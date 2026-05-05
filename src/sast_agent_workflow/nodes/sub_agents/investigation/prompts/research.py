"""Research agent prompt builder.

All prompt text lives in research/. This module is a thin render layer:
it loads the template and language-specific YAML files, then uses
Jinja2 to substitute variables and return the final prompt string.

Adding support for a new language requires only a new YAML file in
research/ — no Python changes needed.
"""

import logging
import re
from functools import lru_cache
from pathlib import Path
from typing import Any, Dict, List

import yaml
from jinja2 import Environment, StrictUndefined

from .checklist_loader import format_checklist

logger = logging.getLogger(__name__)

_CONTEXT_DIR = Path(__file__).parent / "research"


# ---------------------------------------------------------------------------
# Lazy singletons — cached on first call via lru_cache
# ---------------------------------------------------------------------------


@lru_cache(maxsize=1)
def _get_jinja_env() -> Environment:
    return Environment(keep_trailing_newline=True, undefined=StrictUndefined)


@lru_cache(maxsize=1)
def _get_template() -> Dict[str, str]:
    with open(_CONTEXT_DIR / "prompt_template.yaml", "r") as f:
        return yaml.safe_load(f)


@lru_cache(maxsize=None)
def _get_language_context(language: str) -> Dict[str, str]:
    """Load the YAML context file for a specific language, cached per language.

    Falls back to 'generic' if language is empty or no matching file exists.
    """
    path = _CONTEXT_DIR / f"{language}.yaml"
    if not path.exists():
        logger.warning("No research context for language '%s', falling back to generic.", language)
        language = "generic"
        path = _CONTEXT_DIR / f"{language}.yaml"
    try:
        with open(path, "r") as f:
            return yaml.safe_load(f) or {}
    except Exception as e:
        logger.error("Failed to load research context from %s: %s", path, e)
        raise


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _render(template_str: str, **context: Any) -> str:
    return _get_jinja_env().from_string(template_str).render(**context)


def build_code_bank(fetched_files: Dict[str, List[str]]) -> str:
    """
    Build CODE BANK content from fetched files.

    The CODE BANK contains all previously fetched code for reference
    during the research phase.

    Args:
        fetched_files: Dictionary mapping tool names to lists of fetched code

    Returns:
        Formatted string containing all fetched code
    """
    if not fetched_files:
        return "[CODE BANK: No code fetched yet. Use tools to gather relevant code.]"

    code_sections = []
    for tool_name, results in fetched_files.items():
        for result in results:
            code_sections.append(result)

    separator = "\n\n─────────────────────────────────────────────────────────────────────\n\n"

    code_bank = "═══════════════════════════════════════════════════════════════════\n"
    code_bank += "                           CODE BANK\n"
    code_bank += "    All previously fetched code for reference during analysis\n"
    code_bank += "═══════════════════════════════════════════════════════════════════\n\n"
    code_bank += separator.join(code_sections)
    code_bank += "\n\n═══════════════════════════════════════════════════════════════════"

    return code_bank


def build_research_instructions(state: Dict[str, Any]) -> str:
    """
    Build research instructions based on current state.

    Returns iteration-dependent instructions for the research agent.
    First iteration gets initial investigation guidance, subsequent
    iterations get feedback-based instructions.

    Args:
        state: Current investigation state containing issue details
               and progress.

               Required keys (iteration 1): issue_description, issue_cwe
               Required keys (iteration 2+): issue_description
               Optional keys: fetched_files, tool_call_history, iteration,
                              evaluation_feedback, required_information,
                              repo_language

    Returns:
        Formatted instruction string for the research agent
    """
    fetched_files = state.get("fetched_files", {})
    tool_call_history = state.get("tool_call_history", [])
    iteration = state.get("iteration", 1)
    language = state.get("repo_language") or "generic"

    lang_ctx = _get_language_context(language)
    tool_history_hints = lang_ctx.get("tool_history_hints", "").rstrip()

    code_bank_files = _build_code_bank_files_summary(fetched_files)
    tool_history = _build_tool_history(tool_call_history, tool_history_hints)

    if iteration == 1:
        return _build_initial_instructions(state, lang_ctx, code_bank_files, tool_history)
    else:
        return _build_continuation_instructions(state, code_bank_files, tool_history)


def _extract_fetch_info(content: str) -> str:
    """
    Extract file:function identifier from fetched code content.

    Parses headers like:
    - "=== Fetched Code: parseConfig ===" + "File Path: /repo/pkg/config/parse.go"
    - "=== handler.go (from error trace) ==="
    - "=== File: /repo/internal/auth/token.go ==="

    Returns:
        String like "parse.go:parseConfig" or "handler.go" if no function
    """
    file_name = None
    func_name = None

    # Try to extract function name from "=== Fetched Code: {name} ==="
    func_match = re.search(r"=== Fetched Code: (\w+) ===", content)
    if func_match:
        func_name = func_match.group(1)

    # Try to extract file path
    path_match = re.search(r"File Path: ([^\n]+)", content)
    if path_match:
        file_path = path_match.group(1).strip()
        file_name = file_path.split("/")[-1]  # Get just filename

    # Fallback: try "=== {filename} (from error trace) ==="
    if not file_name:
        trace_match = re.search(r"=== (\S+\.\w+) \(from error trace\) ===", content)
        if trace_match:
            file_name = trace_match.group(1)

    # Fallback: try "=== File: {path} ==="
    if not file_name:
        file_match = re.search(r"=== File: ([^\n]+) ===", content)
        if file_match:
            file_path = file_match.group(1).strip()
            file_name = file_path.split("/")[-1]

    # Build identifier
    if file_name and func_name:
        return f"{file_name}:{func_name}"
    elif file_name:
        return file_name
    elif func_name:
        return func_name
    else:
        return "unknown"


def _build_code_bank_files_summary(fetched_files: Dict[str, List[str]]) -> str:
    """
    Build summary of files in the CODE BANK.

    Shows what code is available so the model knows what's already fetched.
    Only includes actual code fetches (fetch_code, read_file), not search results.
    """
    if not fetched_files:
        return "None"

    # Tools that fetch actual code (exclude search results)
    code_fetch_tools = {"fetch_code", "read_file", "fetch_code_from_error_trace"}

    # Extract identifiers from code fetch results only
    fetched_items = []
    for tool_name, results in fetched_files.items():
        if tool_name not in code_fetch_tools:
            continue  # Skip search results
        for content in results:
            item = _extract_fetch_info(content)
            if item != "unknown" and item not in fetched_items:
                fetched_items.append(item)

    if not fetched_items:
        return "None"

    # Format as comma-separated list
    return ", ".join(fetched_items)


def _build_tool_history(tool_call_history: List[str], tool_history_hints: str) -> str:
    """
    Build formatted tool call history with strong language for failed calls.

    Separates successful and failed calls, with explicit warnings that
    retrying failed calls with identical parameters will always fail.
    The language-specific retry hints are injected via tool_history_hints.
    """
    if not tool_call_history:
        return "None yet."

    successful = []
    failed = []

    for entry in tool_call_history:
        if entry.startswith("✓"):
            successful.append(entry)
        elif entry.startswith("✗"):
            failed.append(entry)
        else:
            successful.append(entry)  # Default to successful

    lines = []

    if successful:
        lines.append("Successful (code in CODE BANK):")
        lines.extend(f"  {s}" for s in successful)

    if failed:
        if lines:
            lines.append("")
        lines.append("FAILED (DO NOT RETRY - same parameters = same failure):")
        lines.extend(f"  {f}" for f in failed)
        lines.append("")
        t = _get_template()
        failed_suffix = _render(
            t["tool_history_failed_suffix"],
            tool_history_hints=tool_history_hints.rstrip(),
        )
        lines.append(failed_suffix.rstrip())

    return "\n".join(lines)


def _build_initial_instructions(
    state: Dict[str, Any],
    lang_ctx: Dict[str, str],
    code_bank_files: str,
    tool_history: str,
) -> str:
    """Build instructions for the first research iteration."""
    issue_cwe = state.get("issue_cwe", "N/A")
    repo_language = state.get("repo_language") or "generic"
    checklist_section = format_checklist(
        issue_cwe, repo_language
    )  # NOSONAR S1481: clearer than inlining

    t = _get_template()
    return _render(
        t["initial"],
        issue_description=state.get("issue_description", "N/A"),
        context_gathering=lang_ctx.get("context_gathering", "").rstrip(),
        checklist_section=checklist_section,
        code_bank_files=code_bank_files,
        tool_history=tool_history,
    )


def _build_continuation_instructions(
    state: Dict[str, Any], code_bank_files: str, tool_history: str
) -> str:
    """Build instructions for subsequent research iterations."""
    required_info = state.get("required_information", [])
    unknowns_list = (
        "\n".join(f"- {u}" for u in required_info) if required_info else "None specified"
    )

    t = _get_template()
    return _render(
        t["continuation"],
        evaluation_feedback=state.get("evaluation_feedback", "Need more evidence"),
        unknowns_list=unknowns_list,
        code_bank_files=code_bank_files,
        tool_history=tool_history,
    )
