"""Fetch code tool for extracting functions from source files."""

import logging
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

from langchain_core.tools import StructuredTool

from sast_agent_workflow.investigation.constants import (
    DEFAULT_CONTEXT_LINES,
    MAX_FUNCTION_SCAN_LINES,
)

from .schemas import FetchCodeInput

logger = logging.getLogger(__name__)


@dataclass
class Instruction:
    """Instruction for extracting code from repository."""

    expression_name: str
    referring_source_code_path: str


def create_fetch_code_tool(repo_handler, repo_path: Path) -> StructuredTool:
    """
    Create fetch_code tool with hybrid approach (sophisticated + fallback).

    Args:
        repo_handler: Repository handler for sophisticated code extraction
        repo_path: Path to the repository root

    Returns:
        StructuredTool instance for fetching code
    """

    def _fetch_code(
        file_path: str,
        function_name: str,
        context_lines: int = DEFAULT_CONTEXT_LINES,
    ) -> str:
        """
        Fetch source code for a specific function from a file.

        Uses hybrid approach:
        1. Try sophisticated repo_handler extraction for functions
        2. Fallback to simple file reading (POC approach) if that fails
        """
        logger.info(f"fetch_code: {file_path} function={function_name}")

        # Try sophisticated approach first
        result = _try_sophisticated_extraction(repo_handler, file_path, function_name)
        if result:
            return result

        # Fallback to simple file reading
        return _simple_file_extraction(repo_path, file_path, function_name, context_lines)

    return StructuredTool.from_function(
        func=_fetch_code,
        name="fetch_code",
        description=(
            "Fetch source code for a specific function from a file.\n\n"
            "Use this when you need to:\n"
            "- Retrieve implementation of functions mentioned in SAST trace\n"
            "- Examine code around source/sink points\n"
            "- Get full context of a specific function\n\n"
            "Parameters:\n"
            "- file_path: Relative path (e.g., 'src/main.c', 'lib/auth.c')\n"
            "- function_name: Required function name (e.g., 'validate_input')\n"
            f"- context_lines: Context around function (default: {DEFAULT_CONTEXT_LINES})\n\n"
            "Examples:\n"
            "- fetch_code(file_path='src/auth.c', function_name='sanitize_input')\n"
            "- fetch_code(file_path='lib/utils.c', function_name='process_data')\n\n"
            "Note: For reading entire files, use read_file tool instead."
        ),
        args_schema=FetchCodeInput,
    )


def _try_sophisticated_extraction(
    repo_handler, file_path: str, function_name: str
) -> Optional[str]:
    """
    Try sophisticated function extraction using repo_handler.

    Returns:
        Formatted code string if successful, None otherwise
    """
    try:
        instruction = Instruction(
            expression_name=function_name,
            referring_source_code_path=file_path,
        )

        missing_code, new_symbols = repo_handler.extract_missing_functions_or_macros(
            [instruction], set()
        )

        if missing_code and function_name in new_symbols:
            # Success with sophisticated approach
            code = missing_code
            file_match = re.search(r"code of (.+?) file:", code)
            extracted_file = file_match.group(1) if file_match else file_path

            line_numbers = re.findall(r"^(\d+)\|", code, re.MULTILINE)
            line_range = f"{line_numbers[0]}-{line_numbers[-1]}" if line_numbers else "unknown"

            result = f"""=== Fetched Code: {function_name} ===
File Path: {extracted_file}
Line Range: {line_range}

{code}

=== End of {function_name} ==="""

            logger.info(
                f"Successfully fetched (sophisticated): {function_name} "
                f"({extracted_file}:{line_range})"
            )
            return result

        # Sophisticated approach didn't find it
        logger.info(f"Sophisticated extraction failed for {function_name}, trying simple fallback")
        return None

    except Exception as e:
        logger.warning(
            f"Sophisticated extraction error for {function_name}: {e}, " "trying simple fallback"
        )
        return None


def _simple_file_extraction(
    repo_path: Path, file_path: str, function_name: Optional[str], context_lines: int
) -> str:
    """
    Simple file-based code extraction (fallback approach).

    Returns:
        Formatted code string or error message
    """
    try:
        # Handle repo name suffix edge case
        repo_name = repo_path.name
        if file_path.endswith(f"{repo_name}"):
            file_path = file_path[: -len(repo_name)]

        full_path = repo_path / file_path

        if not full_path.exists():
            return (
                f"Error: File not found: {file_path}\n\n"
                "Suggested recovery:\n"
                f"1. Use search_codebase(pattern='{function_name or file_path}', "
                "file_pattern='*.c')\n"
                f"2. Use list_directory(dir_path='{Path(file_path).parent}')\n"
            )

        with open(full_path, "r", encoding="utf-8", errors="ignore") as f:
            lines = f.readlines()

        # If no function specified, return entire file
        if not function_name:
            result = f"=== {file_path} ===\n"
            result += "".join(f"{i+1:4d}: {line}" for i, line in enumerate(lines))
            logger.info(f"Successfully fetched entire file: {file_path} ({len(lines)} lines)")
            return result

        # Simple function extraction using brace counting
        return _extract_function_simple(lines, file_path, function_name, context_lines)

    except Exception as e:
        logger.error(f"fetch_code error: {e}", exc_info=True)
        return f"Error: Failed to fetch from '{file_path}': {str(e)}"


def _extract_function_simple(
    lines: list, file_path: str, function_name: str, context_lines: int
) -> str:
    """
    Extract a function using simple brace-counting heuristic.

    Returns:
        Formatted code string or error message
    """
    function_start = None
    brace_count = 0
    in_function = False
    # Pattern to match function definition (not just calls)
    # Looks for function_name followed by ( but not preceded by assignment or member access
    func_def_pattern = re.compile(
        r'(?<![=.>-])\b' + re.escape(function_name) + r'\s*\('
    )

    for i, line in enumerate(lines):
        # Match function definition, not function calls
        if func_def_pattern.search(line) and not in_function:
            function_start = max(0, i - context_lines)
            in_function = True
            brace_count = 0

        if in_function:
            # Count braces to find function end
            brace_count += line.count("{") - line.count("}")

            if brace_count == 0 and "{" in "".join(lines[function_start:i]):
                # Function complete
                function_end = min(len(lines), i + context_lines + 1)
                result = (
                    f"=== {file_path}:{function_name} "
                    f"(lines {function_start+1}-{function_end}) ===\n"
                )
                result += "".join(
                    f"{j+1:4d}: {lines[j]}" for j in range(function_start, function_end)
                )
                result += f"\n=== End of {function_name} ==="

                logger.info(
                    f"Successfully fetched (simple): {function_name} "
                    f"({file_path}:{function_start+1}-{function_end})"
                )
                return result

    # Function not found or incomplete
    if function_start is not None:
        # Return partial match
        function_end = min(len(lines), function_start + MAX_FUNCTION_SCAN_LINES)
        result = f"=== {file_path} (partial match for '{function_name}') ===\n"
        result += "".join(f"{i+1:4d}: {lines[i]}" for i in range(function_start, function_end))
        result += "\n... Function may be incomplete or definition not found"
        return result
    else:
        return (
            f"Error: Function '{function_name}' not found in {file_path}\n\n"
            "Suggested recovery:\n"
            f"1. Use search_codebase(pattern='{function_name}', "
            "file_pattern='*.c') to find where it's defined\n"
            f"2. Try read_file(file_path='{file_path}') to see the entire file"
        )