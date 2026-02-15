"""Search codebase tool for finding patterns in source files."""

import logging
import re
from pathlib import Path
from typing import Optional

from langchain_core.tools import StructuredTool

from sast_agent_workflow.investigation.constants import DEFAULT_MAX_SEARCH_RESULTS

from .schemas import SearchCodebaseInput

logger = logging.getLogger(__name__)


def create_search_codebase_tool(repo_path: Path) -> StructuredTool:
    """
    Create search_codebase tool for regex searching within files.

    Args:
        repo_path: Path to the repository root

    Returns:
        StructuredTool instance for searching the codebase
    """

    def _search_codebase(
        pattern: str,
        file_pattern: str = "*.c",
        max_results: int = DEFAULT_MAX_SEARCH_RESULTS,
    ) -> str:
        """Search codebase for patterns. Direct regex search on specified file types."""
        logger.info("search_codebase: pattern='%s' file_pattern='%s'", pattern, file_pattern)
        logger.info("search_codebase full resolved path: %s", repo_path.absolute())

        try:
            regex, err = _compile_search_regex(pattern)
            if err:
                return err

            ext = file_pattern.replace("*", "").replace(".", "")
            results = []
            file_count = 0

            for file_path in repo_path.rglob("*"):
                if not _should_search_file(file_path, ext):
                    continue
                file_count += 1
                if _search_file(file_path, regex, repo_path, results, max_results):
                    break

            return _format_search_results(results, pattern, file_pattern, file_count, max_results)
        except Exception as e:
            logger.error("search_codebase error: %s", e, exc_info=True)
            return "Error: Search failed: %s" % e

    return StructuredTool.from_function(
        func=_search_codebase,
        name="search_codebase",
        description=(
            "Search the codebase for patterns (function names, keywords, etc.).\n\n"
            "Use this when you need to:\n"
            "- Find where functions are defined\n"
            "- Search for sanitization/validation functions\n"
            "- Find usages of variables or APIs\n"
            "- Locate specific code patterns\n\n"
            "Parameters:\n"
            "- pattern: Regex pattern to search "
            "(e.g., 'sanitize.*input', 'validate_.*', 'malloc')\n"
            "- file_pattern: File types to search (e.g., '*.c', '*.h', '*.py'). Default: '*.c'\n"
            f"- max_results: Limit results (default: {DEFAULT_MAX_SEARCH_RESULTS})\n\n"
            "Examples:\n"
            "- search_codebase(pattern='sanitize_input', file_pattern='*.c')\n"
            "- search_codebase(pattern='malloc|calloc|realloc', file_pattern='*.c')\n"
            "- search_codebase(pattern='def validate_.*', file_pattern='*.py')"
        ),
        args_schema=SearchCodebaseInput,
    )


def _format_search_results(
    results: list, pattern: str, file_pattern: str, file_count: int, max_results: int
) -> str:
    """Format search results into a readable string."""
    if not results:
        return (
            f"No matches found for pattern '{pattern}' "
            f"in {file_count} files matching '{file_pattern}'"
        )

    output = [f"=== Search Results: '{pattern}' ({len(results)} matches) ==="]
    output.append(f"File pattern: {file_pattern}")
    output.append("")

    for i, r in enumerate(results, 1):
        output.append(f"{i}. {r['file']}:{r['line']}")
        output.append(f"   {r['content']}")
        output.append("")

    if len(results) >= max_results:
        output.append(f"(Showing first {max_results} matches, there may be more)")

    return "\n".join(output)


def _compile_search_regex(pattern: str) -> tuple[Optional[re.Pattern], Optional[str]]:
    """Compile pattern; return (regex, None) or (None, error_message)."""
    try:
        return (re.compile(pattern, re.IGNORECASE), None)
    except re.error as e:
        return (None, "Error: Invalid regex pattern '%s': %s" % (pattern, e))


def _should_search_file(file_path: Path, ext: str) -> bool:
    """True if path is a searchable file (regular, not hidden, matches extension)."""
    if not file_path.is_file():
        return False
    if any(part.startswith(".") for part in file_path.parts):
        return False
    if ext and not str(file_path).endswith(ext):
        return False
    return True


def _search_file(
    file_path: Path,
    regex: re.Pattern,
    repo_path: Path,
    results: list,
    max_results: int,
) -> bool:
    """Search one file for regex; append matches. Return True if len(results) >= max_results."""
    try:
        with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
            for line_num, line in enumerate(f, 1):
                if regex.search(line):
                    rel_path = file_path.relative_to(repo_path)
                    results.append(
                        {
                            "file": str(rel_path),
                            "line": line_num,
                            "content": line.strip(),
                        }
                    )
                    if len(results) >= max_results:
                        return True
    except Exception as e:
        logger.debug("Error reading %s: %s", file_path, e)
    return False