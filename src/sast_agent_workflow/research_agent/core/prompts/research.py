"""Research agent prompt templates.

These prompts guide the research agent in gathering code evidence
for SAST vulnerability investigation.
"""

from typing import Any, Dict, List

from .checklist_loader import format_checklist


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
        state: Current investigation state containing issue details and progress

    Returns:
        Formatted instruction string for the research agent
    """
    fetched_files = state.get("fetched_files", {})
    tool_call_history = state.get("tool_call_history", [])
    iteration = state.get("iteration", 1)

    # Build summaries
    code_bank_files = _build_code_bank_files_summary(fetched_files)
    tool_history = _build_tool_history(tool_call_history)

    if iteration == 1:
        return _build_initial_instructions(state, code_bank_files, tool_history)
    else:
        return _build_continuation_instructions(state, code_bank_files, tool_history)


def _extract_fetch_info(content: str) -> str:
    """
    Extract file:function identifier from fetched code content.

    Parses headers like:
    - "=== Fetched Code: unix_name ===" + "File Path: /path/to/file.c"
    - "=== vfat.c (from error trace) ==="
    - "=== File: /path/to/file.c ==="

    Returns:
        String like "file.c:function_name" or "file.c" if no function
    """
    import re

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
        trace_match = re.search(r"=== (\S+\.c\w*) \(from error trace\) ===", content)
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


def _build_tool_history(tool_call_history: List[str]) -> str:
    """
    Build formatted tool call history with strong language for failed calls.

    Separates successful and failed calls, with explicit warnings that
    retrying failed calls with identical parameters will always fail.
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
        lines.append("If you need this information, try a DIFFERENT approach:")
        lines.append("  - Different regex pattern (simpler or broader)")
        lines.append("  - Different file extension (*.h instead of *.c)")
        lines.append("  - fetch_code on a specific file you know exists")

    return "\n".join(lines)


def _build_initial_instructions(
    state: Dict[str, Any], code_bank_files: str, tool_history: str
) -> str:
    """Build instructions for the first research iteration."""
    issue_description = state.get("issue_description", "N/A")

    # Get vuln-type specific checklist
    checklist_section = format_checklist(issue_description)

    return f"""You are a code gatherer for SAST vulnerability triage. \
Your ONLY job is to fetch relevant code using tools.

**FINDING:**
{issue_description}

{checklist_section}

**CODE BANK FILES:**
{code_bank_files}

**TOOL CALL HISTORY:**
{tool_history}

Full code is in CODE BANK below.

**CRITICAL RULES:**
1. Call ONE tool per response to gather code
2. Use `fetch_code` with `function_name` to get specific functions
3. Use `search_codebase` to find patterns in code
4. **NEVER retry a failed tool call with the same parameters** - it will ALWAYS fail again
   - If a search returned "No matches", that pattern does NOT exist in the codebase
   - Try a DIFFERENT pattern, file type, or approach instead
5. Do NOT analyze - just gather code. Analysis happens later.
6. When done gathering code for ALL checklist items, respond with ONLY: RESEARCH_COMPLETE

**RESPONSE FORMAT:**
Either:
  Reasoning: [One sentence - which checklist item does this address?]
  [Tool call]
Or (when done):
  RESEARCH_COMPLETE"""


def _build_continuation_instructions(
    state: Dict[str, Any], code_bank_files: str, tool_history: str
) -> str:
    """Build instructions for subsequent research iterations."""
    required_info = state.get("required_information", [])
    unknowns_list = (
        "\n".join(f"- {u}" for u in required_info) if required_info else "None specified"
    )

    return f"""Gather more code based on feedback.

**FEEDBACK:** {state.get('evaluation_feedback', 'Need more evidence')}

**MISSING:**
{unknowns_list}

**CODE BANK FILES:**
{code_bank_files}

**TOOL CALL HISTORY:**
{tool_history}

Full code is in CODE BANK below.

**CRITICAL RULES:**
1. Call ONE tool per response to get the missing information
2. **NEVER retry a failed tool call with the same parameters** - it will ALWAYS fail again
   - If a search returned "No matches", that pattern does NOT exist in the codebase
   - Try a DIFFERENT pattern, file type, or approach instead
3. Do NOT re-analyze code in the CODE BANK
4. When done gathering code for ALL missing items, respond with ONLY: RESEARCH_COMPLETE

**RESPONSE FORMAT:**
Either:
  Reasoning: [One sentence - which checklist item?]
  [Tool call]
Or (when done):
  RESEARCH_COMPLETE"""
