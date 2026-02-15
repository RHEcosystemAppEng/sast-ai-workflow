"""
Investigation Tools Factory - Creates all investigation tools.

Uses langchain_community tools where possible:
- ListDirectoryTool: List directory contents
- FileSearchTool: Search for files by name pattern

Custom tools retained for specialized functionality:
- fetch_code: Sophisticated function extraction with context
- search_codebase: Regex search within file contents
- read_file: Read file contents with line numbers and optional line range
"""

import logging
from pathlib import Path
from typing import List, Optional

from langchain_core.tools import BaseTool, StructuredTool
from langchain_community.tools.file_management import (
    ListDirectoryTool,
    FileSearchTool,
)

from common.config import Config
from handlers.repo_handler_factory import repo_handler_factory

from .fetch_code import create_fetch_code_tool
from .search_codebase import create_search_codebase_tool
from .schemas import ReadFileInput

logger = logging.getLogger(__name__)


def create_investigation_tools(config: Config) -> List[BaseTool]:
    """
    Create all investigation tools without NAT dependency.

    Custom tools for specialized functionality:
    - fetch_code: Sophisticated function extraction with context
    - search_codebase: Regex search within file contents
    - read_file: Read file contents with line numbers and optional line range

    Uses langchain_community tools where possible:
    - ListDirectoryTool: List directory contents (sandboxed to repo)
    - FileSearchTool: Search for files by name pattern (sandboxed to repo)

    Args:
        config: Config instance with repository settings

    Returns:
        List of LangChain BaseTool instances
    """
    logger.info("Creating investigation tools (no NAT)...")

    # Initialize repo handler once for all tools
    repo_handler = repo_handler_factory(config)
    repo_path = Path(config.REPO_LOCAL_PATH)
    repo_path_str = str(repo_path.resolve())

    logger.info(f"Repository path: {repo_path}")

    # Create custom tools (specialized functionality)
    tools: List[BaseTool] = [
        create_fetch_code_tool(repo_handler, repo_path),
        create_search_codebase_tool(repo_path),
    ]

    # Add custom read_file tool (with line numbers and range support)
    tools.append(_create_read_file_tool(repo_path))

    # Add langchain_community file tools (sandboxed to repo root)
    tools.append(_create_list_directory_tool(repo_path_str))
    tools.append(_create_file_search_tool(repo_path_str))

    logger.info(f"Created {len(tools)} investigation tools")
    return tools


def _create_read_file_tool(repo_path: Path) -> StructuredTool:
    """Create read_file tool (reads file contents with line numbers)."""

    def _read_file(
        file_path: str,
        start_line: Optional[int] = None,
        end_line: Optional[int] = None
    ) -> str:
        """Read file contents with optional line range."""
        logger.info(f"read_file: {file_path} (lines {start_line}-{end_line})")

        try:
            # Strip package prefix if present (e.g., "at-3.2.5/lib/file.c" -> "lib/file.c")
            # The repo root already includes the package name, so paths shouldn't duplicate it
            repo_name = repo_path.name
            if file_path.startswith(f"{repo_name}/"):
                original_file_path = file_path
                file_path = file_path[len(repo_name)+1:]
                logger.debug(f"Stripped package prefix: '{original_file_path}' -> '{file_path}'")

            target_file = repo_path / file_path
            logger.info(f"read_file full resolved path: {target_file.absolute()}")

            if not target_file.exists():
                return f"Error: File '{file_path}' does not exist"

            if not target_file.is_file():
                return f"Error: '{file_path}' is not a file"

            with open(target_file, 'r', encoding='utf-8', errors='replace') as f:
                lines = f.readlines()

            total_lines = len(lines)

            # Handle line range
            if start_line is not None:
                start_idx = max(0, start_line - 1)
            else:
                start_idx = 0

            if end_line is not None:
                end_idx = min(total_lines, end_line)
            else:
                end_idx = total_lines

            # Format output with line numbers
            result_lines = [f"=== File: {file_path} ==="]
            result_lines.append(f"Total Lines: {total_lines}")
            if start_line or end_line:
                result_lines.append(f"Showing Lines: {start_idx + 1}-{end_idx}")
            result_lines.append("")

            for i in range(start_idx, end_idx):
                line_num = i + 1
                result_lines.append(f"{line_num:6d}| {lines[i].rstrip()}")

            result_lines.append("")
            result_lines.append(f"=== End of {file_path} ===")

            return "\n".join(result_lines)

        except Exception as e:
            logger.error(f"read_file error: {e}", exc_info=True)
            return f"Error: {str(e)}"

    return StructuredTool.from_function(
        func=_read_file,
        name="read_file",
        description=(
            "Read contents of a file with optional line range.\n\n"
            "Use this when you need to:\n"
            "- Examine specific files or sections\n"
            "- Read configuration files, headers, or includes\n"
            "- Get context around specific line numbers\n"
            "- Review code when fetch_code doesn't find a function\n\n"
            "Parameters:\n"
            "- file_path: Path to file relative to repo root (e.g., 'src/main.c')\n"
            "- start_line: Optional starting line number (1-based)\n"
            "- end_line: Optional ending line number (1-based)\n\n"
            "Examples:\n"
            "- read_file(file_path='src/config.h') - Read entire file\n"
            "- read_file(file_path='src/auth.c', start_line=50, end_line=100) - Read lines 50-100\n"
            "- read_file(file_path='include/defs.h', start_line=1, end_line=50) - Read first 50 lines"
        ),
        args_schema=ReadFileInput,
    )


def _create_list_directory_tool(repo_path_str: str) -> ListDirectoryTool:
    """Create ListDirectoryTool sandboxed to repository."""
    return ListDirectoryTool(
        root_dir=repo_path_str,
        description=(
            "List files and directories in a path.\n\n"
            "Use this when you need to:\n"
            "- Explore the codebase structure\n"
            "- Find files in a directory before fetching\n"
            "- Discover related source files\n"
            "- Navigate the project organization\n\n"
            "Parameters:\n"
            "- dir_path: Directory path relative to repo root (default: '.')\n\n"
            "Examples:\n"
            "- list_directory(dir_path='src') - List all files in src directory\n"
            "- list_directory(dir_path='lib') - List files in lib\n"
            "- list_directory() - List files in root directory"
        ),
    )


def _create_file_search_tool(repo_path_str: str) -> FileSearchTool:
    """Create FileSearchTool sandboxed to repository."""
    return FileSearchTool(
        root_dir=repo_path_str,
        description=(
            "Search for files in the repository by name pattern.\n\n"
            "Use this when you need to:\n"
            "- Find files matching a glob pattern (e.g., all .c files)\n"
            "- Locate specific file types across the codebase\n"
            "- Find configuration or security-related files\n\n"
            "Parameters:\n"
            "- pattern: Unix shell glob pattern (e.g., '*.c', 'auth*.py', 'config*')\n"
            "- dir_path: Directory to search in (default: '.')\n\n"
            "Note: This searches file NAMES, not file contents.\n"
            "Use search_codebase for searching within file contents.\n\n"
            "Examples:\n"
            "- file_search(pattern='*.c') - Find all C source files\n"
            "- file_search(pattern='*sanitize*', dir_path='src') - Find files with 'sanitize' in name\n"
            "- file_search(pattern='*.config') - Find all config files"
        ),
    )