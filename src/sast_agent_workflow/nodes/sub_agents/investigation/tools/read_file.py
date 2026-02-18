"""Read file tool for reading source files with line numbers."""

import logging
from pathlib import Path
from typing import Optional

from langchain_core.tools import StructuredTool

from .schemas import ReadFileInput

logger = logging.getLogger(__name__)


def _resolve_file_path(repo_path: Path, file_path: str):
    """Strip package prefix if present and resolve to absolute path."""
    repo_name = repo_path.name
    if file_path.startswith(f"{repo_name}/"):
        original = file_path
        file_path = file_path[len(repo_name) + 1 :]
        logger.debug(f"Stripped package prefix: '{original}' -> '{file_path}'")

    target_file = repo_path / file_path
    logger.info(f"read_file full resolved path: {target_file.absolute()}")
    return target_file, file_path


def _calculate_line_range(total_lines, start_line, end_line):
    """Compute 0-based start/end indices from 1-based line numbers."""
    start_idx = max(0, start_line - 1) if start_line is not None else 0
    end_idx = min(total_lines, end_line) if end_line is not None else total_lines
    return start_idx, end_idx


def _format_file_output(lines, file_path, start_line, end_line, start_idx, end_idx):
    """Format file contents with line numbers and metadata header."""
    total_lines = len(lines)
    result = [f"=== File: {file_path} ===", f"Total Lines: {total_lines}"]
    if start_line or end_line:
        result.append(f"Showing Lines: {start_idx + 1}-{end_idx}")
    result.append("")

    for i in range(start_idx, end_idx):
        result.append(f"{i + 1:6d}| {lines[i].rstrip()}")

    result.append("")
    result.append(f"=== End of {file_path} ===")
    return "\n".join(result)


def create_read_file_tool(repo_path: Path) -> StructuredTool:
    """Create read_file tool (reads file contents with line numbers)."""

    def _read_file(
        file_path: str, start_line: Optional[int] = None, end_line: Optional[int] = None
    ) -> str:
        """Read file contents with optional line range."""
        logger.info(f"read_file: {file_path} (lines {start_line}-{end_line})")
        try:
            target_file, file_path = _resolve_file_path(repo_path, file_path)

            if not target_file.exists():
                return f"Error: File '{file_path}' does not exist"
            if not target_file.is_file():
                return f"Error: '{file_path}' is not a file"

            with open(target_file, "r", encoding="utf-8", errors="replace") as f:
                lines = f.readlines()

            start_idx, end_idx = _calculate_line_range(len(lines), start_line, end_line)
            return _format_file_output(lines, file_path, start_line, end_line, start_idx, end_idx)
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
            "- read_file(file_path='include/defs.h', start_line=1, end_line=50) "
            "- Read first 50 lines"
        ),
        args_schema=ReadFileInput,
    )
