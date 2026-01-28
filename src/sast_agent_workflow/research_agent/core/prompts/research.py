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
    fetched_files = state.get('fetched_files', {})
    failed_fetches = state.get('failed_fetches', [])
    iteration = state.get('iteration', 1)
    
    # Build fetched files summary
    fetched_summary = _build_fetched_summary(fetched_files)
    failed_summary = _build_failed_summary(failed_fetches)
    
    if iteration == 1:
        return _build_initial_instructions(state, fetched_summary, failed_summary)
    else:
        return _build_continuation_instructions(state, fetched_summary, failed_summary)


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
    func_match = re.search(r'=== Fetched Code: (\w+) ===', content)
    if func_match:
        func_name = func_match.group(1)
    
    # Try to extract file path
    path_match = re.search(r'File Path: ([^\n]+)', content)
    if path_match:
        file_path = path_match.group(1).strip()
        file_name = file_path.split('/')[-1]  # Get just filename
    
    # Fallback: try "=== {filename} (from error trace) ==="
    if not file_name:
        trace_match = re.search(r'=== (\S+\.c\w*) \(from error trace\) ===', content)
        if trace_match:
            file_name = trace_match.group(1)
    
    # Fallback: try "=== File: {path} ==="
    if not file_name:
        file_match = re.search(r'=== File: ([^\n]+) ===', content)
        if file_match:
            file_path = file_match.group(1).strip()
            file_name = file_path.split('/')[-1]
    
    # Build identifier
    if file_name and func_name:
        return f"{file_name}:{func_name}"
    elif file_name:
        return file_name
    elif func_name:
        return func_name
    else:
        return "unknown"


def _build_fetched_summary(fetched_files: Dict[str, List[str]]) -> str:
    """
    Build summary of fetched files with explicit file:function identifiers.
    
    Shows what was fetched so the model knows NOT to re-fetch.
    Only includes actual code fetches (fetch_code, read_file), not search results.
    """
    if not fetched_files:
        return "None"
    
    # Tools that fetch actual code (exclude search results)
    code_fetch_tools = {'fetch_code', 'read_file', 'fetch_code_from_error_trace'}
    
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
    
    # Format as numbered list
    lines = [f"{i+1}. {item}" for i, item in enumerate(fetched_items)]
    lines.append(f"\nDo NOT re-fetch any of these.\nFull code is in CODE BANK below.\n")
    
    return "\n".join(lines)


def _build_failed_summary(failed_fetches: List[str]) -> str:
    """Build summary of failed fetches."""
    if not failed_fetches:
        return "None"
    return "\n".join(f"- {f}" for f in failed_fetches)


def _build_initial_instructions(
    state: Dict[str, Any],
    fetched_summary: str,
    failed_summary: str
) -> str:
    """Build instructions for the first research iteration."""
    issue_description = state.get('issue_description', 'N/A')
    
    # Get vuln-type specific checklist
    checklist_section = format_checklist(issue_description)
    
    return f"""You are a code gatherer for SAST vulnerability triage. Your ONLY job is to fetch relevant code using tools.

**FINDING:**
{issue_description}

{checklist_section}

**ALREADY FETCHED:** 
{fetched_summary}

**UNAVAILABLE:** 
{failed_summary}

**RULES:**
- Call ONE tool per response
- Use `fetch_code` with `function_name` to get specific functions
- Use `search_codebase` to find patterns in code
- Do NOT analyze - just gather code. Analysis happens later.

**RESPONSE FORMAT:**
Reasoning: [One sentence - which checklist item does this address?]
[Tool call]"""


def _build_continuation_instructions(
    state: Dict[str, Any],
    fetched_summary: str,
    failed_summary: str
) -> str:
    """Build instructions for subsequent research iterations."""
    issue_description = state.get('issue_description', 'N/A')
    required_info = state.get('required_information', [])
    unknowns_list = "\n".join(f"- {u}" for u in required_info) if required_info else "None specified"
    
    # Get vuln-type specific checklist for reference
    checklist_section = format_checklist(issue_description)
    
    return f"""Gather more code based on feedback.

**FEEDBACK:** {state.get('evaluation_feedback', 'Need more evidence')}

**MISSING:** 
{unknowns_list}

{checklist_section}

**ALREADY FETCHED:** 
{fetched_summary}

**UNAVAILABLE:** 
{failed_summary}

**RULES:**
- Call ONE tool to get the missing information
- Do NOT re-fetch items in ALREADY FETCHED list
- Do NOT re-analyze code in the CODE BANK

**RESPONSE FORMAT:**
Reasoning: [One sentence - which checklist item?]
[Tool call]"""
