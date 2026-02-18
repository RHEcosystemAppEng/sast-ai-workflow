"""Input schemas for investigation tools."""

from typing import Optional

from pydantic import BaseModel, Field

from ..constants import (
    DEFAULT_CONTEXT_LINES,
    DEFAULT_MAX_SEARCH_RESULTS,
)


class FetchCodeInput(BaseModel):
    """Input schema for fetch_code tool."""

    file_path: str = Field(
        description=(
            "Path to the file to fetch code from, relative to codebase root "
            "(e.g., 'src/auth.c', 'lib/parser.c')"
        )
    )
    function_name: str = Field(
        description=(
            "Name of the specific function to extract "
            "(e.g., 'validate_input', 'process_request'). "
            "REQUIRED - use read_file for entire files."
        )
    )
    context_lines: int = Field(
        default=DEFAULT_CONTEXT_LINES,
        description=(
            "Number of context lines to include before/after the function "
            f"(default: {DEFAULT_CONTEXT_LINES})"
        ),
    )


class SearchCodebaseInput(BaseModel):
    """Input schema for search_codebase tool."""

    pattern: str = Field(
        description=(
            "Search pattern (regex) to find in code. "
            "Examples: 'sanitize.*input', 'validate_.*', 'malloc'"
        )
    )
    file_pattern: str = Field(
        default="*.c",
        description=(
            "File pattern to search (e.g., '*.c', '*.h', '*.py', '*.java'). Default: '*.c'"
        ),
    )
    max_results: int = Field(
        default=DEFAULT_MAX_SEARCH_RESULTS,
        description=f"Maximum number of results to return (default: {DEFAULT_MAX_SEARCH_RESULTS})",
    )


class ReadFileInput(BaseModel):
    """Input schema for read_file tool."""

    file_path: str = Field(description="Path to file relative to repo root (e.g., 'src/main.c')")
    start_line: Optional[int] = Field(
        default=None, description="Optional starting line number (1-based)"
    )
    end_line: Optional[int] = Field(
        default=None, description="Optional ending line number (1-based)"
    )
