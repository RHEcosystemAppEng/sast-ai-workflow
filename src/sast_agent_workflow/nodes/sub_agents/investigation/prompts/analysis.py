"""Analysis node prompt templates.

These prompts guide the analysis LLM in making verdict decisions
based on gathered code evidence.
"""

from typing import Any, Dict


def build_analysis_prompt(state: Dict[str, Any]) -> str:
    """
    Build the analysis prompt for verdict decision.

    Args:
        state: Current investigation state containing
               gathered code and issue details.

               Required keys: issue_description
               Optional keys: gathered_code, needs_reanalysis,
                              evaluation_feedback, analysis

    Returns:
        Formatted prompt string for the analysis LLM
    """
    gathered_code_section = state.get("gathered_code", "(No code gathered)")
    is_reanalysis = state.get("needs_reanalysis", False)

    # Build evaluator feedback section for reanalysis
    evaluator_feedback_section = ""
    if is_reanalysis and state.get("evaluation_feedback"):
        evaluator_feedback_section = _build_feedback_section(state["evaluation_feedback"])

    previous = state.get("analysis") or "None - this is the first analysis."

    return (
        _PROMPT_HEADER
        + f"""
---

**SAST FINDING:**
{state['issue_description']}

**CODE GATHERED DURING RESEARCH:**
{gathered_code_section}

**PREVIOUS ANALYSIS (if any):**
{previous}
{evaluator_feedback_section}"""
        + _PROMPT_BODY
    )


def _build_feedback_section(feedback: str) -> str:
    """Build the evaluator feedback block for reanalysis."""
    return f"""
---

**EVALUATOR FEEDBACK (Address this in your reanalysis):**
{feedback}

The evaluator disagreed with your previous analysis.
Please carefully reconsider your verdict based on this
feedback. Pay special attention to any control flow,
guards, or logic issues the evaluator identified.

---
"""


_PROMPT_HEADER = """\
You are an expert security analyst determining if a \
SAST finding is a FALSE_POSITIVE or TRUE_POSITIVE.

**Your Task:**
Analyze the reported security finding and the provided \
source code to determine if the vulnerability is real \
(TRUE_POSITIVE) or a false alarm (FALSE_POSITIVE).\
"""

_PROMPT_BODY = """
---

**HOW TO REASON (Chain of Draft):**
Think in brief, bulleted drafts — do NOT write them out. \
Internalize your reasoning across these checkpoints, \
then output only the final, polished verdict.

Draft checkpoints to work through silently:
- Source: Where does the issue originate?
- Sink: Where is the vulnerable use?
- Control flow between source and sink: any \
`if`/`goto`/`return`/`exit`/`break`/`continue`?
- Do any guards prevent reaching the sink in the \
vulnerable state?
- Is the vulnerable path actually reachable?

**Guidlines to apply during your internal drafts:**
- Code-based evidence only — reference specific line numbers
- Read EVERY line between source and sink before \
concluding "no guards exist"
- One REACHABLE vulnerable path is enough for TRUE_POSITIVE
- Syntax issues are always TRUE_POSITIVE
- Issue-type patterns:
  - **UNINIT**: `if (condition) goto/return/exit` after \
the uninitialized assignment but before the use often \
guards against it
  - **RESOURCE_LEAK**: `exit()` shortly after → OS \
cleanup → FALSE_POSITIVE for CLI tools
  - **NULL_DEREF**: Null checks before the dereference
  - **OVERRUN**: Buffer size at declaration vs. size \
used in callee

---

**REQUIRED OUTPUT:**
After completing your internal drafts, provide:
- **verdict**: TRUE_POSITIVE or FALSE_POSITIVE
- **confidence**: HIGH (all paths traced, clear evidence),\
 MEDIUM (most paths traced, minor gaps), or LOW \
(significant uncertainty)
- **reasoning**: 2–3 sentences — your polished conclusion \
stating what you found and why it leads to the verdict
- **justifications**: Specific code-based evidence points \
with line references

**Begin your analysis.**"""
