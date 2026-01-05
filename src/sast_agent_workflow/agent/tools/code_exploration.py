"""
Code exploration tools - NAT-registered tools for codebase exploration.

Provides list_files, read_file, and search_codebase tools for agent investigation.
Uses LangChain's built-in file system tools as base.
"""

import logging
import os
import re
from pathlib import Path
from typing import Optional

from langchain_core.tools import tool
from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.function import FunctionBaseConfig
from pydantic import BaseModel, Field

from common.config import Config

logger = logging.getLogger(__name__)


class ListFilesInput(BaseModel):
    """Input schema for list_files tool."""
    directory: str = Field(
        default=".",
        description="Directory path to list (relative to repository root, default: '.')"
    )
    pattern: Optional[str] = Field(
        default=None,
        description="Optional glob pattern to filter files (e.g., '*.c', '*.py')"
    )


class ReadFileInput(BaseModel):
    """Input schema for read_file tool."""
    file_path: str = Field(
        description=(
            "FILE PATH to read (relative to repository root). "
            "Examples: 'config/settings.py', 'modules/auth.c', 'Makefile'. "
            "Use this for complete files. For symbol definitions (functions/classes), use fetch_code instead."
        )
    )
    start_line: Optional[int] = Field(
        default=None,
        description="Optional starting line number (1-based, inclusive)"
    )
    end_line: Optional[int] = Field(
        default=None,
        description="Optional ending line number (1-based, inclusive)"
    )


class SearchCodebaseInput(BaseModel):
    """Input schema for search_codebase tool."""
    pattern: str = Field(
        description="Regex pattern to search for (e.g., 'sanitize.*input', '@validate')"
    )
    scope: Optional[str] = Field(
        default=None,
        description="Optional directory OR file path to limit search scope (e.g., 'auth/', 'src/main.c')"
    )
    max_results: int = Field(
        default=20,
        description="Maximum number of results to return (default: 20)"
    )


class ListFilesToolConfig(FunctionBaseConfig, name="list_files"):
    """Configuration for list_files tool."""
    description: str = Field(
        default=(
            "Lists files in a directory for discovery. "
            "Use when you need to explore directory structure or find related files. "
            "Returns list of files and directories with basic metadata."
        ),
        description="Tool description",
    )


class ReadFileToolConfig(FunctionBaseConfig, name="read_file"):
    """Configuration for read_file tool."""
    description: str = Field(
        default=(
            "Reads contents of a FILE by its path. "
            "Use when you need to read a complete file (config files, headers, makefiles, etc.). "
            "Parameters: "
            "(1) file_path: FILE PATH to read - e.g., 'config/settings.py', 'modules/auth.c', 'Makefile' (NOT a symbol name!), "
            "(2) start_line (optional): starting line number, "
            "(3) end_line (optional): ending line number. "
            "Returns file contents with line numbers. "
            "IMPORTANT: Use fetch_code for symbol definitions (functions/classes), use read_file for complete files."
        ),
        description="Tool description",
    )


class SearchCodebaseToolConfig(FunctionBaseConfig, name="search_codebase"):
    """Configuration for search_codebase tool."""
    description: str = Field(
        default=(
            "Searches codebase for regex patterns. "
            "Use to find sanitization functions, validation decorators, or any code patterns. "
            "Returns matching lines with context (file path + line number + content). "
            "Example patterns: 'sanitize', 'validate.*username', '@validate_input'"
        ),
        description="Tool description",
    )


@register_function(config_type=ListFilesToolConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN])
async def register_list_files_tool(config: ListFilesToolConfig, builder: Builder):
    """Register the list_files tool with NAT."""
    logger.info("Registering list_files tool...")

    # Get repository path from config
    try:
        global_config = Config()
        repo_path = Path(global_config.REPO_LOCAL_PATH)
        logger.info(f"List files tool will operate on repo: {repo_path}")
    except Exception as e:
        logger.error(f"Failed to get repo path: {e}")
        raise

    def _list_files(directory: str = ".", pattern: Optional[str] = None) -> str:
        """
        List files in a directory.

        Args:
            directory: Directory path relative to repo root
            pattern: Optional glob pattern to filter files

        Returns:
            Formatted list of files and directories
        """
        try:
            target_dir = repo_path / directory
            
            if not target_dir.exists():
                return f"Error: Directory '{directory}' does not exist in repository"
            
            if not target_dir.is_dir():
                return f"Error: Path '{directory}' is not a directory"

            # Get all entries
            entries = sorted(target_dir.iterdir(), key=lambda p: (not p.is_dir(), p.name))
            
            # Filter by pattern if provided
            if pattern:
                entries = [e for e in entries if e.match(pattern)]

            if not entries:
                return f"No files found in '{directory}'" + (f" matching '{pattern}'" if pattern else "")

            # Format output
            lines = [f"=== Directory: {directory} ==="]
            if pattern:
                lines.append(f"Pattern: {pattern}")
            lines.append("")

            dirs = [e for e in entries if e.is_dir()]
            files = [e for e in entries if e.is_file()]

            if dirs:
                lines.append("**Directories:**")
                for d in dirs[:50]:  # Limit to 50
                    lines.append(f"  📁 {d.name}/")
                if len(dirs) > 50:
                    lines.append(f"  ... and {len(dirs) - 50} more directories")
                lines.append("")

            if files:
                lines.append("**Files:**")
                for f in files[:100]:  # Limit to 100
                    size = f.stat().st_size
                    size_str = f"{size:,} bytes" if size < 1024 else f"{size/1024:.1f} KB"
                    lines.append(f"  📄 {f.name} ({size_str})")
                if len(files) > 100:
                    lines.append(f"  ... and {len(files) - 100} more files")

            lines.append("")
            lines.append(f"Total: {len(dirs)} directories, {len(files)} files")

            return "\n".join(lines)

        except Exception as e:
            logger.error(f"list_files error: {e}", exc_info=True)
            return f"Error listing files: {str(e)}"

    # Create async wrapper for NAT
    async def list_files_fn(directory: str = ".", pattern: Optional[str] = None) -> str:
        """Async wrapper for list_files tool."""
        return _list_files(directory, pattern)

    try:
        yield FunctionInfo.from_fn(
            list_files_fn,
            description=config.description,
            input_schema=ListFilesInput,
        )
    except GeneratorExit:
        logger.info("list_files tool exited!")
    finally:
        logger.debug("Cleaning up list_files tool")


@register_function(config_type=ReadFileToolConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN])
async def register_read_file_tool(config: ReadFileToolConfig, builder: Builder):
    """Register the read_file tool with NAT."""
    logger.info("Registering read_file tool...")

    # Get repository path from config
    try:
        global_config = Config()
        repo_path = Path(global_config.REPO_LOCAL_PATH)
        logger.info(f"Read file tool will operate on repo: {repo_path}")
    except Exception as e:
        logger.error(f"Failed to get repo path: {e}")
        raise

    def _read_file(
        file_path: str, 
        start_line: Optional[int] = None, 
        end_line: Optional[int] = None
    ) -> str:
        """
        Read contents of a file.

        Args:
            file_path: Path to file relative to repo root
            start_line: Optional starting line (1-based, inclusive)
            end_line: Optional ending line (1-based, inclusive)

        Returns:
            File contents with line numbers
        """
        try:
            target_file = repo_path / file_path
            
            if not target_file.exists():
                return f"Error: File '{file_path}' does not exist in repository"
            
            if not target_file.is_file():
                return f"Error: Path '{file_path}' is not a file"

            # Read file
            with open(target_file, 'r', encoding='utf-8', errors='replace') as f:
                lines = f.readlines()

            total_lines = len(lines)

            # Apply line range if specified
            if start_line is not None or end_line is not None:
                start_idx = (start_line - 1) if start_line else 0
                end_idx = end_line if end_line else total_lines
                
                # Validate range
                if start_idx < 0:
                    start_idx = 0
                if end_idx > total_lines:
                    end_idx = total_lines
                if start_idx >= end_idx:
                    return f"Error: Invalid line range {start_line}-{end_line} (file has {total_lines} lines)"
                
                lines = lines[start_idx:end_idx]
                line_offset = start_idx
            else:
                line_offset = 0

            # Format output
            result_lines = [f"=== File: {file_path} ==="]
            if start_line or end_line:
                result_lines.append(f"Lines: {start_line or 1}-{end_line or total_lines} (of {total_lines} total)")
            else:
                result_lines.append(f"Total lines: {total_lines}")
            result_lines.append("")

            # Add line numbers
            for idx, line in enumerate(lines, start=line_offset + 1):
                result_lines.append(f"{idx:6}| {line.rstrip()}")

            result_lines.append("")
            result_lines.append(f"=== End of {file_path} ===")

            return "\n".join(result_lines)

        except Exception as e:
            logger.error(f"read_file error: {e}", exc_info=True)
            return f"Error reading file: {str(e)}"

    # Create async wrapper for NAT
    async def read_file_fn(
        file_path: str,
        start_line: Optional[int] = None,
        end_line: Optional[int] = None
    ) -> str:
        """Async wrapper for read_file tool."""
        return _read_file(file_path, start_line, end_line)

    try:
        yield FunctionInfo.from_fn(
            read_file_fn,
            description=config.description,
            input_schema=ReadFileInput,
        )
    except GeneratorExit:
        logger.info("read_file tool exited!")
    finally:
        logger.debug("Cleaning up read_file tool")


@register_function(config_type=SearchCodebaseToolConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN])
async def register_search_codebase_tool(config: SearchCodebaseToolConfig, builder: Builder):
    """Register the search_codebase tool with NAT."""
    logger.info("Registering search_codebase tool...")

    # Get repository path from config
    try:
        global_config = Config()
        repo_path = Path(global_config.REPO_LOCAL_PATH)
        logger.info(f"Search codebase tool will operate on repo: {repo_path}")
    except Exception as e:
        logger.error(f"Failed to get repo path: {e}")
        raise

    def _search_codebase(
        pattern: str,
        scope: Optional[str] = None,
        max_results: int = 20
    ) -> str:
        """
        Search codebase for regex patterns.

        Args:
            pattern: Regex pattern to search for
            scope: Optional directory to limit search
            max_results: Maximum number of results

        Returns:
            Formatted search results with file paths, line numbers, and matching lines
        """
        try:
            # Compile regex pattern
            try:
                regex = re.compile(pattern, re.IGNORECASE)
            except re.error as e:
                return f"Error: Invalid regex pattern '{pattern}': {str(e)}"

            # Determine search scope (can be directory OR file)
            if scope:
                search_path = repo_path / scope
                logger.debug(f"Resolved search path: {search_path} (repo: {repo_path}, scope: {scope})")
                
                if not search_path.exists():
                    # Try without the autofs version prefix (maybe it's stripped in the repo)
                    # E.g., "autofs-5.1.9/modules/file.c" -> "modules/file.c"
                    if '/' in scope:
                        parts = scope.split('/')
                        if parts[0].startswith('autofs-'):
                            alt_scope = '/'.join(parts[1:])
                            search_path = repo_path / alt_scope
                            logger.debug(f"Trying alternate path: {search_path}")
                            
                            if not search_path.exists():
                                return f"Error: Scope path '{scope}' and alternate '{alt_scope}' do not exist in repository"
                        else:
                            return f"Error: Scope path '{scope}' does not exist in repository"
                    else:
                        return f"Error: Scope path '{scope}' does not exist in repository"
                
                # If scope is a file, search only that file
                if search_path.is_file():
                    files_to_search = [search_path]
                elif search_path.is_dir():
                    # If scope is a directory, search recursively
                    files_to_search = None  # Will use rglob below
                else:
                    return f"Error: Scope path '{scope}' is neither a file nor directory"
            else:
                search_path = repo_path
                files_to_search = None  # Will use rglob below

            # Search files
            results = []
            files_searched = 0
            
            # Common source code extensions
            code_extensions = {
                '.c', '.h', '.cpp', '.hpp', '.cc', '.cxx',
                '.py', '.java', '.js', '.ts', '.go',
                '.rs', '.rb', '.php', '.cs', '.swift'
            }

            # If files_to_search is set, use it; otherwise search recursively
            if files_to_search is not None:
                file_iterator = files_to_search
            else:
                file_iterator = search_path.rglob('*')

            for file_path in file_iterator:
                if not file_path.is_file():
                    continue
                
                # Skip if not a code file (only check extension for directory searches)
                if files_to_search is None and file_path.suffix not in code_extensions:
                    continue

                # Skip hidden files and directories
                if any(part.startswith('.') for part in file_path.parts):
                    continue

                files_searched += 1
                
                try:
                    with open(file_path, 'r', encoding='utf-8', errors='replace') as f:
                        for line_num, line in enumerate(f, 1):
                            if regex.search(line):
                                relative_path = file_path.relative_to(repo_path)
                                results.append({
                                    'file': str(relative_path),
                                    'line': line_num,
                                    'content': line.rstrip()
                                })
                                
                                if len(results) >= max_results:
                                    break
                except Exception as e:
                    logger.debug(f"Skipping file {file_path}: {e}")
                    continue

                if len(results) >= max_results:
                    break

            # Format output
            if not results:
                scope_msg = f" in scope '{scope}'" if scope else ""
                return f"No matches found for pattern '{pattern}'{scope_msg}\n(Searched {files_searched} files)"

            output_lines = [f"=== Search Results for: {pattern} ==="]
            if scope:
                output_lines.append(f"Scope: {scope}")
            output_lines.append(f"Found {len(results)} matches (searched {files_searched} files)")
            output_lines.append("")

            # Group by file
            current_file = None
            for result in results:
                if result['file'] != current_file:
                    if current_file is not None:
                        output_lines.append("")
                    current_file = result['file']
                    output_lines.append(f"**{current_file}**")
                
                output_lines.append(f"  Line {result['line']}: {result['content']}")

            if len(results) >= max_results:
                output_lines.append("")
                output_lines.append(f"(Showing first {max_results} results)")

            return "\n".join(output_lines)

        except Exception as e:
            logger.error(f"search_codebase error: {e}", exc_info=True)
            return f"Error searching codebase: {str(e)}"

    # Create async wrapper for NAT
    async def search_codebase_fn(
        pattern: str,
        scope: Optional[str] = None,
        max_results: int = 20
    ) -> str:
        """Async wrapper for search_codebase tool."""
        return _search_codebase(pattern, scope, max_results)

    try:
        yield FunctionInfo.from_fn(
            search_codebase_fn,
            description=config.description,
            input_schema=SearchCodebaseInput,
        )
    except GeneratorExit:
        logger.info("search_codebase tool exited!")
    finally:
        logger.debug("Cleaning up search_codebase tool")

