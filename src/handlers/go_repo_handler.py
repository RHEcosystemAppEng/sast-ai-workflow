import logging
import os
import re
import threading
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

try:
    import tree_sitter
    import tree_sitter_go
    from tree_sitter import Language, Node, Parser
except ImportError:
    tree_sitter = None
    tree_sitter_go = None
    Language = None
    Parser = None
    Node = None

from common.config import Config
from Utils.repo_utils import download_repo, get_repo_and_branch_from_url

logger = logging.getLogger(__name__)

# Global initialization
_tree_sitter_lock = threading.Lock()
_tree_sitter_initialized = False
_go_language = None
_parser = None


@dataclass(frozen=True, slots=True)
class CodeLocation:
    """Represents a location in source code."""

    file_path: str
    start_line: int
    end_line: int
    start_column: int = 0
    end_column: int = 0


@dataclass(slots=True)
class Symbol:
    """Represents a symbol definition."""

    name: str
    kind: str  # 'function', 'method', 'type', 'variable', 'constant'
    location: CodeLocation
    source_code: str
    package_name: str = ""
    exported: bool = False
    receiver_type: Optional[str] = None  # For methods
    signature: str = ""
    documentation: List[str] = field(default_factory=list)


@dataclass(slots=True)
class Reference:
    """Represents a reference to a symbol."""

    symbol_name: str
    location: CodeLocation
    context: str  # The line containing the reference
    usage_type: str = "reference"  # 'call', 'assignment', 'reference'


class CompactIndex:
    """Memory-efficient index with lazy loading."""

    def __init__(self):
        # String interning for memory efficiency
        self.strings: List[str] = [""]
        self.string_to_id: Dict[str, int] = {"": 0}

        # File management
        self.file_paths: List[str] = []
        self.file_path_to_id: Dict[str, int] = {}

        # Symbol and reference indexes (using interned strings)
        self.symbols: Dict[int, List[int]] = defaultdict(list)  # name_id -> [symbol_indices]
        self.references: Dict[int, List[int]] = defaultdict(list)  # name_id -> [ref_indices]
        self.symbol_data: List[Symbol] = []  # Actual symbol objects
        self.reference_data: List[Reference] = []  # Actual reference objects

        # Lazy-loaded content
        self._file_content_cache: Dict[str, str] = {}

        # Thread safety
        self._lock = threading.RLock()

    def intern_string(self, s: str) -> int:
        """Intern a string and return its ID."""
        if not s:
            return 0
        with self._lock:
            if s not in self.string_to_id:
                string_id = len(self.strings)
                self.strings.append(s)
                self.string_to_id[s] = string_id
                return string_id
            return self.string_to_id[s]

    def get_string(self, string_id: int) -> str:
        """Get string by ID."""
        return self.strings[string_id] if 0 <= string_id < len(self.strings) else ""


class GoRepoHandler:
    """
    Go repository handler focused on SAST finding analysis.

    Provides 4 core functionalities:
    1. Retrieve code block by line
    2. Get definition by name
    3. Get all references
    4. Get documentation/comments
    """

    def __init__(self, config: Config) -> None:
        if tree_sitter is None or tree_sitter_go is None:
            raise ImportError(
                "tree-sitter and tree-sitter-go are required. "
                "Install with: pip install tree-sitter tree-sitter-go"
            )

        self.url, self.branch = get_repo_and_branch_from_url(config.REPO_REMOTE_URL)
        self._report_file_prefix = f"{config.PROJECT_NAME}-{config.PROJECT_VERSION.split('-')[0]}/"

        if config.DOWNLOAD_REPO:
            self.repo_local_path = download_repo(config.REPO_REMOTE_URL)
        else:
            _, self._report_file_prefix = os.path.split(config.REPO_LOCAL_PATH)
            self._report_file_prefix = os.path.join(self._report_file_prefix, "")
            self.repo_local_path = config.REPO_LOCAL_PATH

        # Initialize tree-sitter
        self._init_tree_sitter()

        # Build index
        self.index = CompactIndex()
        self._build_index()

    def _init_tree_sitter(self):
        """Initialize tree-sitter parser and queries."""
        global _tree_sitter_initialized, _go_language, _parser

        with _tree_sitter_lock:
            if not _tree_sitter_initialized:
                _go_language = Language(tree_sitter_go.language())
                _parser = Parser(_go_language)
                _tree_sitter_initialized = True

        self.language = _go_language
        self.parser = _parser

    def _build_index(self):
        """Build the symbol and reference index."""
        logger.info("Building Go project index...")

        go_files = self._find_go_files()
        logger.info(f"Indexing {len(go_files)} Go files")

        # Process files in batches for memory efficiency
        batch_size = 50
        for i in range(0, len(go_files), batch_size):
            batch = go_files[i : i + batch_size]

            with ThreadPoolExecutor(max_workers=4) as executor:
                futures = [executor.submit(self._index_file, file_path) for file_path in batch]
                for future in as_completed(futures):
                    try:
                        future.result()
                    except Exception as e:
                        logger.warning(f"Failed to index file: {e}")

        logger.info(
            f"Index built: {len(self.index.symbol_data)} symbols, "
            f"{len(self.index.reference_data)} references"
        )

    def _find_go_files(self) -> List[str]:
        """Find all Go files in the repository."""
        go_files = []
        repo_path = Path(self.repo_local_path).resolve()  # Resolve to absolute path

        for go_file in repo_path.rglob("*.go"):
            # Check if any part of the path (relative to repo) starts with . or is vendor
            relative_parts = go_file.relative_to(repo_path).parts
            if not any(
                part.startswith(".") or part == "vendor" for part in relative_parts
            ) and not go_file.name.endswith("_test.go"):
                go_files.append(str(go_file))

        return go_files

    def _index_file(self, file_path: str):
        """Index symbols and references in a single file."""
        try:
            with open(file_path, "r", encoding="utf-8") as f:
                content = f.read()

            tree = self.parser.parse(bytes(content, "utf8"))

            # Extract package name
            package_name = self._extract_package_name(tree, content)

            # Extract symbols
            self._extract_symbols(tree, content, file_path, package_name)

            # Extract references
            self._extract_references(tree, content, file_path)

        except Exception as e:
            logger.error(f"Error indexing {file_path}: {e}")

    def _extract_package_name(self, tree: tree_sitter.Tree, content: str) -> str:
        """Extract package name from file."""

        def find_package_name(node):
            if node.type == "package_clause":
                for child in node.children:
                    if child.type == "package_identifier":
                        return child.text.decode("utf-8")

            for child in node.children:
                result = find_package_name(child)
                if result:
                    return result
            return None

        package_name = find_package_name(tree.root_node)
        return package_name if package_name else "unknown"

    def _extract_symbols(
        self, tree: tree_sitter.Tree, content: str, file_path: str, package_name: str
    ):  # todo: check if there is a better way thwn manual traversal
        """Extract symbol definitions from the file using manual traversal."""
        lines = content.split("\n")

        def traverse_for_symbols(node):
            symbols = []

            # Function declarations
            if node.type == "function_declaration":
                name_node = node.child_by_field_name("name")
                if name_node:
                    symbol = self._create_symbol_from_traversal(
                        node,
                        name_node.text.decode("utf-8"),
                        "func",
                        content,
                        file_path,
                        package_name,
                        lines,
                    )
                    if symbol:
                        symbols.append(symbol)

            # Method declarations
            elif node.type == "method_declaration":
                name_node = node.child_by_field_name("name")
                if name_node:
                    # Extract receiver type
                    receiver_type = None
                    receiver_node = node.child_by_field_name("receiver")
                    if receiver_node:
                        receiver_type = self._extract_receiver_type(receiver_node)

                    symbol = self._create_symbol_from_traversal(
                        node,
                        name_node.text.decode("utf-8"),
                        "method",
                        content,
                        file_path,
                        package_name,
                        lines,
                        receiver_type,
                    )
                    if symbol:
                        symbols.append(symbol)

            # Type declarations
            elif node.type == "type_spec":
                name_node = node.child_by_field_name("name")
                if name_node:
                    symbol = self._create_symbol_from_traversal(
                        node,
                        name_node.text.decode("utf-8"),
                        "type",
                        content,
                        file_path,
                        package_name,
                        lines,
                    )
                    if symbol:
                        symbols.append(symbol)

            # Variable declarations
            elif node.type == "var_spec":
                name_node = node.child_by_field_name("name")
                if name_node:
                    symbol = self._create_symbol_from_traversal(
                        node,
                        name_node.text.decode("utf-8"),
                        "var",
                        content,
                        file_path,
                        package_name,
                        lines,
                    )
                    if symbol:
                        symbols.append(symbol)

            # Constant declarations
            elif node.type == "const_spec":
                name_node = node.child_by_field_name("name")
                if name_node:
                    symbol = self._create_symbol_from_traversal(
                        node,
                        name_node.text.decode("utf-8"),
                        "const",
                        content,
                        file_path,
                        package_name,
                        lines,
                    )
                    if symbol:
                        symbols.append(symbol)

            # Recursively traverse children
            for child in node.children:
                symbols.extend(traverse_for_symbols(child))

            return symbols

        # Extract all symbols
        symbols = traverse_for_symbols(tree.root_node)

        # Add to index
        with self.index._lock:
            for symbol in symbols:
                symbol_idx = len(self.index.symbol_data)
                self.index.symbol_data.append(symbol)

                name_id = self.index.intern_string(symbol.name)
                self.index.symbols[name_id].append(symbol_idx)

    def _create_symbol_from_traversal(
        self,
        node: Node,
        name: str,
        kind: str,
        content: str,
        file_path: str,
        package_name: str,
        lines: List[str],
        receiver_type: Optional[str] = None,
    ) -> Optional[Symbol]:
        """Create a Symbol object from manual traversal."""
        if not name or not kind:
            return None

        # Create location
        location = CodeLocation(
            file_path=file_path,
            start_line=node.start_point[0] + 1,
            end_line=node.end_point[0] + 1,
            start_column=node.start_point[1],
            end_column=node.end_point[1],
        )

        # Extract source code
        source_lines = lines[node.start_point[0] : node.end_point[0] + 1]
        numbered_lines = [
            f"{i + location.start_line}| {line}" for i, line in enumerate(source_lines)
        ]
        source_code = "\n".join(numbered_lines)

        # Extract signature for functions/methods
        signature = ""
        if kind in ["func", "method"]:
            signature = self._extract_function_signature(node, content)

        # Extract documentation
        documentation = self._extract_documentation(node, content)

        return Symbol(
            name=name,
            kind=kind,
            location=location,
            source_code=source_code,
            package_name=package_name,
            exported=name[0].isupper() if name else False,
            receiver_type=receiver_type,
            signature=signature,
            documentation=documentation,
        )

    def _extract_references(self, tree: tree_sitter.Tree, content: str, file_path: str):
        """Extract symbol references from the file using manual traversal."""
        lines = content.split("\n")

        def traverse_for_references(node):
            references = []

            # Function calls
            if node.type == "call_expression":
                func_node = node.child_by_field_name("function")
                if func_node:
                    if func_node.type == "identifier":
                        # Direct function call
                        symbol_name = func_node.text.decode("utf-8")
                        references.append(
                            self._create_reference(func_node, symbol_name, file_path, lines, "call")
                        )
                    elif func_node.type == "selector_expression":
                        # Method call
                        field_node = func_node.child_by_field_name("field")
                        if field_node:
                            symbol_name = field_node.text.decode("utf-8")
                            references.append(
                                self._create_reference(
                                    field_node, symbol_name, file_path, lines, "method_call"
                                )
                            )

            # All identifiers (for general references)
            elif node.type == "identifier":
                # Skip if this is a definition
                if not self._is_definition_context(node):
                    symbol_name = node.text.decode("utf-8")
                    references.append(
                        self._create_reference(node, symbol_name, file_path, lines, "reference")
                    )

            # Recursively traverse children
            for child in node.children:
                references.extend(traverse_for_references(child))

            return references

        # Extract all references
        references = traverse_for_references(tree.root_node)

        # Add to index
        with self.index._lock:
            for reference in references:
                if reference:  # Skip None references
                    ref_idx = len(self.index.reference_data)
                    self.index.reference_data.append(reference)

                    name_id = self.index.intern_string(reference.symbol_name)
                    self.index.references[name_id].append(ref_idx)

    def _create_reference(
        self, node: Node, symbol_name: str, file_path: str, lines: List[str], usage_type: str
    ) -> Optional[Reference]:
        """Create a Reference object from a node."""
        if not symbol_name:
            return None

        location = CodeLocation(
            file_path=file_path,
            start_line=node.start_point[0] + 1,
            end_line=node.end_point[0] + 1,
            start_column=node.start_point[1],
            end_column=node.end_point[1],
        )

        # Get context line
        context = lines[node.start_point[0]] if node.start_point[0] < len(lines) else ""

        return Reference(
            symbol_name=symbol_name,
            location=location,
            context=context.strip(),
            usage_type=usage_type,
        )

    # Helper methods for extraction
    def _extract_receiver_type(self, receiver_node: Node) -> str:
        """Extract receiver type from method receiver."""
        return receiver_node.text.decode("utf-8") if receiver_node else ""

    def _extract_function_signature(self, node: Node, content: str) -> str:
        """Extract function signature."""
        try:
            for child in node.children:
                if child.type == "block":
                    sig_end = child.start_byte
                    sig_start = node.start_byte
                    return content[sig_start:sig_end].strip()
            return content.split("\n")[node.start_point[0]].strip()
        except Exception:
            return ""

    def _extract_documentation(self, node: Node, content: str) -> List[str]:
        """Extract documentation comments for a symbol."""
        docs = []

        # Look for comments immediately before the symbol
        target_line = node.start_point[0]
        lines = content.split("\n")

        # Scan backwards for comments
        for i in range(target_line - 1, -1, -1):
            line = lines[i].strip()
            if line.startswith("//"):
                docs.insert(0, line[2:].strip())
            elif line.startswith("/*") and line.endswith("*/"):
                docs.insert(0, line[2:-2].strip())
            elif line == "":
                continue  # Skip empty lines
            else:
                break  # Stop at non-comment, non-empty line

        return docs

    def _is_definition_context(self, node: Node) -> bool:
        """Check if a node is in a definition context."""
        parent = node.parent
        while parent:
            if parent.type in {
                "function_declaration",
                "method_declaration",
                "type_spec",
                "var_spec",
                "const_spec",
                "parameter_declaration",
            }:
                name_field = parent.child_by_field_name("name")
                if name_field and name_field == node:
                    return True
            parent = parent.parent
        return False

    # Public API Methods

    def get_source_code_by_line_number(self, file_path: str, line: int) -> str:
        """
        1. Retrieve code block by line

        Get the source code block (function, method, type) that contains the specified line.
        """
        # Find symbols that contain this line
        containing_symbols = []
        for symbol in self.index.symbol_data:
            if (
                symbol.location.file_path == file_path
                and symbol.location.start_line <= line <= symbol.location.end_line
            ):
                containing_symbols.append(symbol)

        if containing_symbols:
            # Return the most specific (smallest) containing symbol
            best_symbol = min(
                containing_symbols, key=lambda s: s.location.end_line - s.location.start_line
            )
            return best_symbol.source_code

        # Fallback: return context around the line
        return self._get_context_around_line(file_path, line)

    def get_definition_by_name(
        self, symbol_name: str, file_path: Optional[str] = None
    ) -> List[Symbol]:
        """
        2. Get definition by name

        Find all definitions of a symbol by name, optionally filtered by file.
        """
        name_id = self.index.string_to_id.get(symbol_name)
        if name_id is None:
            return []

        symbol_indices = self.index.symbols.get(name_id, [])
        symbols = [self.index.symbol_data[idx] for idx in symbol_indices]

        if file_path:
            symbols = [s for s in symbols if s.location.file_path == file_path]

        return symbols

    def get_all_references(
        self, symbol_name: str, file_path: Optional[str] = None
    ) -> List[Reference]:
        """
        3. Get all references

        Find all references to a symbol by name, optionally filtered by file.
        """
        name_id = self.index.string_to_id.get(symbol_name)
        if name_id is None:
            return []

        ref_indices = self.index.references.get(name_id, [])
        references = [self.index.reference_data[idx] for idx in ref_indices]

        if file_path:
            references = [r for r in references if r.location.file_path == file_path]

        return references

    def get_symbol_documentation(
        self, symbol_name: str, file_path: Optional[str] = None
    ) -> List[str]:
        """
        4. Get documentation/comments

        Get documentation comments associated with a symbol.
        """
        definitions = self.get_definition_by_name(symbol_name, file_path)

        all_docs = []
        for definition in definitions:
            all_docs.extend(definition.documentation)

        return all_docs

    def _get_context_around_line(self, file_path: str, line: int, context_lines: int = 25) -> str:
        """Get context around a specific line."""
        if file_path not in self.index._file_content_cache:
            try:
                with open(file_path, "r", encoding="utf-8") as f:
                    self.index._file_content_cache[file_path] = f.read()
            except Exception as e:
                logger.error(f"Failed to read {file_path}: {e}")
                return ""

        content = self.index._file_content_cache[file_path]
        lines = content.split("\n")

        start = max(0, line - context_lines)
        end = min(len(lines), line + context_lines)

        relevant_lines = lines[start:end]
        numbered_lines = [
            f"{i + start + 1}| {line_content}" for i, line_content in enumerate(relevant_lines)
        ]

        return "\n".join(numbered_lines)

    # Protocol methods (for compatibility with existing system)
    def get_source_code_blocks_from_error_trace(self, error_trace: str) -> Dict[str, str]:
        """Parse error trace and extract relevant code blocks."""
        try:
            source_files = set(re.findall(r"([^\s]+\.go):(\d+):", error_trace))
        except Exception as e:
            logger.warning(f"Failed to parse error trace: {e}")
            return {}

        error_code_sources = defaultdict(set)

        for file_path, line_number in source_files:
            try:
                line_num = int(line_number)
            except ValueError:
                continue

            # Handle absolute paths vs relative paths
            if os.path.isabs(file_path):
                local_file_path = file_path
            else:
                # Remove report prefix if present
                clean_path = file_path.removeprefix(self._report_file_prefix)

                # If the path starts with ../ or ./, resolve it relative to current directory
                if clean_path.startswith("../") or clean_path.startswith("./"):
                    local_file_path = os.path.abspath(clean_path)
                else:
                    local_file_path = os.path.join(self.repo_local_path, clean_path)

            source_code = self.get_source_code_by_line_number(local_file_path, line_num)
            if source_code:
                error_code_sources[file_path].add(source_code)

        return {
            full_file_path: "\n\n".join(code_sections)
            for full_file_path, code_sections in error_code_sources.items()
        }

    def extract_missing_functions_or_macros(
        self, instructions, found_symbols: Set[str]
    ) -> Tuple[str, Set[str]]:
        """Extract missing function definitions."""
        if not instructions:
            return "", found_symbols

        missing_source_codes = ""

        for instruction in instructions:
            try:
                if (
                    hasattr(instruction, "expression_name")
                    and instruction.expression_name not in found_symbols
                ):
                    symbols = self.get_definition_by_name(instruction.expression_name)

                    for symbol in symbols:
                        if symbol.name not in found_symbols:
                            missing_source_codes += (
                                f"\n\n// Definition of {symbol.name} ({symbol.kind}):\n"
                            )
                            missing_source_codes += symbol.source_code
                            found_symbols.add(symbol.name)
            except AttributeError:
                continue

        return missing_source_codes, found_symbols

    def get_source_code_of_called_expressions(self) -> str:
        """Protocol method placeholder."""
        return ""

    def reset_found_symbols(self):
        """Reset handler state."""
        self.index._file_content_cache.clear()
