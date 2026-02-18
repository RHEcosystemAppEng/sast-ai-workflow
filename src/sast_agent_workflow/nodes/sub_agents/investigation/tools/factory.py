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
from typing import List

from langchain_community.tools.file_management import (
    FileSearchTool,
    ListDirectoryTool,
)
from langchain_core.tools import BaseTool

from common.config import Config
from handlers.repo_handler_factory import repo_handler_factory

from .fetch_code import create_fetch_code_tool
from .read_file import create_read_file_tool
from .search_codebase import create_search_codebase_tool

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
    tools.append(create_read_file_tool(repo_path))

    # Add langchain_community file tools (sandboxed to repo root)
    tools.append(_create_list_directory_tool(repo_path_str))
    tools.append(_create_file_search_tool(repo_path_str))

    logger.info(f"Created {len(tools)} investigation tools")
    return tools


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
            "- file_search(pattern='*sanitize*', dir_path='src') "
            "- Find files with 'sanitize' in name\n"
            "- file_search(pattern='*.config') - Find all config files"
        ),
    )
