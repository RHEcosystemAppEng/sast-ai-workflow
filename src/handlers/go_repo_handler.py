import logging
import os
import threading
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import regex
import tree_sitter
import tree_sitter_go
from tree_sitter import Language, Node, Parser

from common.config import Config
from Utils.report_path_utils import normalize_report_source_path

logger = logging.getLogger(__name__)


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
    """One named Go declaration captured for indexing (function, type, const, or var).

    Examples: ``func HandleRequest``, ``type User struct``, ``const MaxRetries = 3``,
    ``func (s *Server) Close() error``.
    """

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
        self.symbols_by_file: Dict[str, List[int]] = defaultdict(
            list
        )  # file_path -> [symbol_indices]
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

    _tree_sitter_lock = threading.Lock()
    _tree_sitter_initialized = False
    _go_language: Optional["Language"] = None
    _parser: Optional["Parser"] = None

    def __init__(self, config: Config) -> None:
        self._report_file_prefix = f"{config.PROJECT_NAME}-{config.PROJECT_VERSION.split('-')[0]}/"
        self.repo_local_path = config.REPO_LOCAL_PATH
        self.project_name = config.PROJECT_NAME

        # Initialize tree-sitter
        self._init_tree_sitter()

        # Build index
        self.index = CompactIndex()
        self._build_index()

    def _init_tree_sitter(self):
        """Initialize tree-sitter parser and queries."""
        with GoRepoHandler._tree_sitter_lock:
            if not GoRepoHandler._tree_sitter_initialized:
                GoRepoHandler._go_language = Language(tree_sitter_go.language())
                GoRepoHandler._parser = Parser(GoRepoHandler._go_language)
                GoRepoHandler._tree_sitter_initialized = True

        self.language = GoRepoHandler._go_language
        self.parser = GoRepoHandler._parser

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
                futures = {
                    executor.submit(self._index_file, file_path): file_path for file_path in batch
                }
                for future in as_completed(futures):
                    try:
                        future.result()
                    except Exception as e:
                        logger.warning(f"Failed to index file {futures[future]}: {e}")

        logger.info(
            f"Index built: {len(self.index.symbol_data)} symbols, "
            f"{len(self.index.reference_data)} references"
        )

    def _find_go_files(self) -> List[str]:
        """Find all Go files in the repository."""
        go_files = []
        repo_path = Path(self.repo_local_path).resolve()

        for go_file in repo_path.rglob("*.go"):
            rel_path = go_file.relative_to(repo_path)
            if not rel_path.match("*_test.go") and not any(
                p.startswith(".") or p == "vendor" for p in rel_path.parts
            ):
                go_files.append(str(go_file))

        return go_files

    def _index_file(self, file_path: str):
        """Index symbols and references in a single file."""
        try:
            with open(file_path, "r", encoding="utf-8") as f:
                file_content = f.read()

            tree = self.parser.parse(bytes(file_content, "utf8"))

            lines = file_content.split("\n")
            self.index._file_content_cache[file_path] = file_content

            package_name = self._extract_package_name(tree)
            self._extract_symbols(tree, file_content, file_path, package_name, lines)
            self._extract_references(tree, file_path, lines)

        except FileNotFoundError:
            logger.error(f"File not found during indexing: {file_path}")
        except PermissionError:
            logger.error(f"Permission denied reading file: {file_path}")
        except UnicodeDecodeError as e:
            logger.error(f"Failed to decode {file_path}: {e}")

    def _extract_package_name(self, tree: tree_sitter.Tree) -> str:
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
        self,
        tree: tree_sitter.Tree,
        content: str,
        file_path: str,
        package_name: str,
        lines: List[str],
    ):
        """Extract symbol definitions from the file using manual traversal."""
        NODE_KIND_MAP = {
            "function_declaration": "func",
            "method_declaration": "method",
            "type_spec": "type",
            "var_spec": "var",
            "const_spec": "const",
        }

        def traverse_for_symbols(node):
            symbols = []
            receiver_type = None
            node_list = []

            if node.type in ["var_spec", "const_spec"]:
                # Multi-name specs: `var a, b = ...` — each identifier is a separate symbol.
                node_list = [n for n in node.children_by_field_name("name") if n.is_named]

            elif node.type in ["function_declaration", "type_spec"]:
                node_list = list(filter(None, [node.child_by_field_name("name")]))

            elif node.type == "method_declaration":
                node_list = list(filter(None, [node.child_by_field_name("name")]))
                receiver_node = node.child_by_field_name("receiver")
                if receiver_node:
                    receiver_type = self._extract_receiver_type(receiver_node)

            for node_name in node_list:
                symbol = self._create_symbol_from_traversal(
                    node,
                    node_name.text.decode("utf-8"),
                    NODE_KIND_MAP[node.type],
                    content,
                    file_path,
                    package_name,
                    lines,
                    receiver_type,
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
                self.index.symbols_by_file[symbol.location.file_path].append(symbol_idx)

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
            signature = self._extract_function_signature(node, content, lines)

        # Extract documentation
        documentation = self._extract_documentation(node, lines)

        return Symbol(
            name=name,
            kind=kind,
            location=location,
            source_code=source_code,
            package_name=package_name,
            exported=name[0].isupper(),
            receiver_type=receiver_type,
            signature=signature,
            documentation=documentation,
        )

    def _extract_references(self, tree: tree_sitter.Tree, file_path: str, lines: List[str]):
        """Extract symbol references from the file using manual traversal."""
        # Extract all references
        references = self._traverse_for_references(tree.root_node, file_path, lines)

        # Add to index
        with self.index._lock:
            for reference in references:
                if reference:  # Skip None references
                    ref_idx = len(self.index.reference_data)
                    self.index.reference_data.append(reference)

                    name_id = self.index.intern_string(reference.symbol_name)
                    self.index.references[name_id].append(ref_idx)

    def _traverse_for_references(
        self, node: Node, file_path: str, lines: List[str]
    ) -> List[Reference]:
        references = []
        skip_node = None

        # Function calls
        if node.type == "call_expression":
            ref, skip_node = self._handle_call_expression(node, file_path, lines)
            if ref:
                references.append(ref)
        # All identifiers (for general references)
        elif node.type == "identifier" and not self._is_definition_context(node):
            symbol_name = node.text.decode("utf-8")
            references.append(
                self._create_reference(node, symbol_name, file_path, lines, "reference")
            )

        # Recursively traverse children
        for child in node.children:
            if child != skip_node:
                references.extend(self._traverse_for_references(child, file_path, lines))

        return references

    def _handle_call_expression(
        self, node: Node, file_path: str, lines: List[str]
    ) -> Tuple[Optional[Reference], Optional[Node]]:
        """Handle call_expression node. Returns (reference, skip_node)."""
        func_node = node.child_by_field_name("function")
        if not func_node:
            return None, None
        if func_node.type == "identifier":
            symbol_name = func_node.text.decode("utf-8")
            return (
                self._create_reference(func_node, symbol_name, file_path, lines, "call"),
                func_node,
            )
        if func_node.type == "selector_expression":
            # Method call — skip the whole selector to avoid double-indexing the
            # field identifier when the generic child traversal visits it again.
            field_node = func_node.child_by_field_name("field")
            if field_node:
                symbol_name = field_node.text.decode("utf-8")
                return (
                    self._create_reference(
                        field_node, symbol_name, file_path, lines, "method_call"
                    ),
                    func_node,
                )
        return None, None

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

    def _extract_function_signature(self, node: Node, content: str, lines: List[str]) -> str:
        """Extract function signature."""
        try:
            for child in node.children:
                if child.type == "block":
                    sig_end = child.start_byte
                    sig_start = node.start_byte
                    return content[sig_start:sig_end].strip()
            row = node.start_point[0]
            if 0 <= row < len(lines):
                return lines[row].strip()
            return ""
        except Exception:
            return ""

    def _extract_documentation(self, node: Node, lines: List[str]) -> List[str]:
        """Extract documentation comments for a symbol.

        Follows Go godoc convention: only comments immediately preceding the
        declaration (no blank lines between) are considered documentation.
        """
        docs = []
        target_line = node.start_point[0]

        i = target_line - 1
        while i >= 0:
            line = lines[i].strip()

            if line.startswith("//"):
                docs.insert(0, line[2:].strip())
                i -= 1

            elif line.endswith("*/"):
                # Collect the full block comment by scanning backwards for /*
                block_lines = []
                while i >= 0:
                    block_line = lines[i].strip()
                    block_lines.insert(0, block_line)
                    i -= 1
                    if block_line.startswith("/*"):
                        break
                # Strip delimiters and prepend the block as a unit to preserve order
                cleaned_block = [
                    bl.lstrip("/*").rstrip("*/").strip()
                    for bl in block_lines
                    if bl.lstrip("/*").rstrip("*/").strip()
                ]
                docs = cleaned_block + docs

            else:
                # Blank line or non-comment line: stop (godoc requires no gap)
                break

        return docs

    def _is_definition_context(self, node: Node) -> bool:
        """Check if a node is in a definition context (declaration LHS, not a usage)."""
        parent = node.parent
        while parent:
            if self._is_named_declaration(parent, node):
                return True
            if self._is_short_var_assignment(parent, node):
                return True
            if self._is_multi_var_assignment(parent, node):
                return True
            parent = parent.parent
        return False

    def _is_named_declaration(self, parent: Node, node: Node) -> bool:
        """Return True if node is the name field of a declaration statement."""
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
        return False

    def _is_short_var_assignment(self, parent: Node, node: Node) -> bool:
        """Return True if node is the LHS of a single-variable := declaration."""
        if parent.type == "short_var_declaration":
            if parent.child_by_field_name("left") == node:
                return True
        return False

    def _is_multi_var_assignment(self, parent: Node, node: Node) -> bool:
        """Return True if node is inside an expression_list on the LHS of a := declaration."""
        if parent.type == "expression_list":
            grandparent = parent.parent
            if (
                grandparent is not None
                and grandparent.type == "short_var_declaration"
                and grandparent.child_by_field_name("left") == parent
            ):
                return True
        return False

    # Public API Methods

    def get_source_code_blocks_from_error_trace(self, error_trace: str) -> Dict[str, str]:
        """Parse error trace and extract relevant code blocks.

        For each file/line reference found in the trace, returns the containing
        symbol's source code. If the symbol has godoc comments, they are prepended
        to the source block as context.
        """
        source_files = set()
        # Path segments may contain dots (e.g. semver dirs like myproject-1.0/).
        # Allow only plausible path characters; '@' covers module-cache paths (…/pkg@v1.2.3/…).
        go_file_line = regex.compile(r"([A-Za-z0-9@./_-]+\.go):(\d+):")
        for line in error_trace.split("\n"):
            matches = go_file_line.findall(line)
            source_files.update(matches)

        if not source_files:
            logger.debug("No Go file references found in error trace")

        error_code_sources: Dict[str, List[str]] = defaultdict(list)

        for file_path, line_number in source_files:
            try:
                line_num = int(line_number)
            except ValueError:
                continue

            local_file_path = self._resolve_local_file_path(file_path)
            symbol = self._get_symbol_at_line(local_file_path, line_num)
            if not symbol:
                source_code = self._get_context_around_line(local_file_path, line_num)
                if source_code:
                    error_code_sources[file_path].append(source_code)
                continue

            block = self._build_symbol_block(symbol)
            if block not in error_code_sources[file_path]:
                error_code_sources[file_path].append(block)

        return {
            full_file_path: "\n\n".join(code_sections)
            for full_file_path, code_sections in error_code_sources.items()
        }

    def _resolve_local_file_path(self, file_path: str) -> str:
        """Resolve a file path from an error trace to a local absolute path."""
        if os.path.isabs(file_path):
            return file_path
        clean_path = normalize_report_source_path(
            file_path,
            report_file_prefix=self._report_file_prefix,
            project_name=self.project_name,
        )
        if clean_path.startswith("../") or clean_path.startswith("./"):
            return os.path.abspath(clean_path)
        return os.path.join(self.repo_local_path, clean_path)

    def _build_symbol_block(self, symbol: Symbol) -> str:
        """Build a source block for a symbol, prepending godoc comments if present."""
        if not symbol.documentation:
            return symbol.source_code
        doc_start_line = symbol.location.start_line - len(symbol.documentation)
        doc_header = "\n".join(
            f"{doc_start_line + i}| // {line}" for i, line in enumerate(symbol.documentation)
        )
        return doc_header + "\n" + symbol.source_code

    def _get_symbol_at_line(self, file_path: str, line: int) -> Optional[Symbol]:
        """Return the most specific (smallest) symbol containing the given line, or None."""
        indices = self.index.symbols_by_file.get(file_path, [])
        containing = [
            self.index.symbol_data[idx]
            for idx in indices
            if self.index.symbol_data[idx].location.start_line
            <= line
            <= self.index.symbol_data[idx].location.end_line
        ]
        if not containing:
            return None
        return min(containing, key=lambda s: s.location.end_line - s.location.start_line)

    def _get_context_around_line(self, file_path: str, line: int, context_lines: int = 25) -> str:
        """Get context around a specific line."""
        if file_path not in self.index._file_content_cache:
            try:
                with open(file_path, "r", encoding="utf-8") as f:
                    self.index._file_content_cache[file_path] = f.read()
            except (FileNotFoundError, PermissionError, UnicodeDecodeError) as e:
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

    def extract_missing_functions_or_macros(
        self, instructions, found_symbols: Set[str]
    ) -> Tuple[str, Set[str]]:
        """Extract missing function definitions."""
        if not instructions:
            return "", found_symbols

        parts: List[str] = []

        for instruction in instructions:
            try:
                if (
                    hasattr(instruction, "expression_name")
                    and instruction.expression_name not in found_symbols
                ):
                    symbols = self.get_definition_by_name(instruction.expression_name)

                    for symbol in symbols:
                        if symbol.name not in found_symbols:
                            parts.append(f"\n\n// Definition of {symbol.name} ({symbol.kind}):\n")
                            parts.append(symbol.source_code)
                            found_symbols.add(symbol.name)
            except AttributeError:
                continue

        return "".join(parts), found_symbols

    def get_definition_by_name(
        self, symbol_name: str, file_path: Optional[str] = None
    ) -> List[Symbol]:
        """
        Get definition by name

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

    def get_source_code_by_line_number(self, file_path: str, line: int) -> str:
        """
        Retrieve code block by line

        Get the source code block (function, method, type) that contains the specified line.
        """
        symbol = self._get_symbol_at_line(file_path, line)
        if symbol:
            return symbol.source_code

        # Fallback: return context around the line
        return self._get_context_around_line(file_path, line)

    def get_all_references(
        self, symbol_name: str, file_path: Optional[str] = None
    ) -> List[Reference]:
        """
        Get all references

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

    def get_symbol_documentation(self, symbol_name: str, file_path: str) -> List[str]:
        """
        Get documentation/comments

        Returns the godoc comment lines for a symbol in a specific file.
        file_path is required to avoid ambiguity when the same symbol name
        exists across multiple packages.
        """
        definitions = self.get_definition_by_name(symbol_name, file_path)

        all_docs = []
        for definition in definitions:
            all_docs.extend(definition.documentation)

        return all_docs
