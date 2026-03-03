"""Evaluation node prompt templates.

These prompts guide the evaluation LLM in critiquing analysis quality
and determining if more research is needed.
"""

from typing import Any, Dict


def build_evaluation_prompt(state: Dict[str, Any]) -> str:
    """
    Build evaluation prompt with iteration context and
    clear criteria.

    Args:
        state: Current investigation state containing
               analysis and gathered code

    Returns:
        Formatted prompt string for the evaluation LLM
    """
    iteration = state["iteration"]
    max_iterations = state["max_iterations"]
    rejection_streak = state.get("evaluation_rejection_streak", 0)
    no_progress_streak = state.get("no_progress_streak", 0)

    # Build justifications string
    justifications = state.get("justifications", [])
    justifications_str = "\n".join(f"- {j}" for j in justifications)

    dynamic_section = f"""\
**INVESTIGATION PROGRESS:**
- Iteration: {iteration} of {max_iterations}
- Consecutive rejections: {rejection_streak}
- Iterations without new code: {no_progress_streak}

---

**SAST FINDING:**
{state['issue_description']}

**GATHERED CODE:**
{state['gathered_code']}

**ANALYSIS:**
{state['analysis']}

**PROPOSED VERDICT:** {state['proposed_verdict']}

**JUSTIFICATIONS:**
{justifications_str}

---

**ANTI-LOOP GUIDANCE:**
- The code in "GATHERED CODE" has ALREADY been fetched\
—do NOT request it again
- If no new code has been gathered for \
{no_progress_streak} iteration(s), the approach is wrong,\
 not that more code is needed
- Focus on whether EXISTING evidence supports the verdict

---"""

    dynamic_decisions = f"""\

**DECISION RULES:**

**-> APPROVE when ANY of these are true:**
- Evidence supports the verdict even with minor gaps
- Same information requested multiple times without \
progress (current streak: {no_progress_streak})
- At iteration {iteration}/{max_iterations} with \
reasonable evidence
- Missing code is peripheral (helper functions, edge \
cases that don't change the verdict)
- Code was requested but couldn't be found—accept \
the uncertainty"""

    return (
        _EVAL_HEADER
        + "\n\n---\n\n"
        + dynamic_section
        + _EVAL_STANDARDS
        + dynamic_decisions
        + _EVAL_FOOTER
    )


_EVAL_HEADER = """\
You are a senior security reviewer evaluating whether \
a SAST investigation has gathered sufficient evidence \
for a confident verdict.

**YOUR ROLE:** Decide if the analysis is ready for a \
final verdict (APPROVED) or needs more research \
(NEEDS_MORE_RESEARCH). Be rigorous but pragmatic\
—investigations should conclude when sufficient evidence \
exists, not when perfect understanding is achieved."""

_EVAL_STANDARDS = """

**VERDICT-SPECIFIC EVALUATION STANDARDS:**

For **TRUE_POSITIVE** verdicts — VERIFY THESE THOROUGHLY:
- The analysis must show at least ONE vulnerable \
execution path that is ACTUALLY REACHABLE
- **CRITICAL**: Check if there are guards, early exits, \
or error handling that PREVENT reaching the vulnerable \
code
- Look for patterns like: \
`if (error_condition) goto exit;` or \
`if (!valid) return;` BEFORE the vulnerable line
- For UNINIT findings: Is there a check that ensures \
initialization before use?
- For RESOURCE_LEAK: Does the program exit shortly after,\
 making the leak irrelevant?
- Standard: "Is the vulnerable path actually reachable, \
or do guards prevent it?"

For **FALSE_POSITIVE** verdicts:
- Requires evidence that the vulnerability CANNOT occur \
on ANY reachable path
- Must identify specific guards/checks that prevent \
the vulnerability
- Standard: "What specific code prevents the \
vulnerability?"

**BOTH verdicts require equal scrutiny.** Do not \
rubber-stamp TRUE_POSITIVE without verifying reachability.

---

**EVALUATION CHECKLIST:**

1. **Evidence Validation**: Does each justification \
reference code that is ACTUALLY PRESENT in \
"GATHERED CODE"? Justifications citing code not in \
context are gaps.

2. **Data Flow Traced**: Is the path from source to sink \
documented with specific line references?

3. **Security Controls Examined**: Are validation/\
sanitization functions either (a) shown and analyzed, or \
(b) noted as unavailable with uncertainty reflected in \
the verdict?

4. **Confidence Appropriate**:
   - High (0.8-1.0): ALL steps passed, ALL code present, \
clear evidence — APPROVE
   - Medium (0.5-0.8): Most paths traced, minor gaps that \
don't affect verdict — APPROVE
   - Low (0.0-0.5): Failed any step above → MUST use \
NEEDS_MORE_RESEARCH

---"""

_EVAL_FOOTER = """

**-> NEEDS_MORE_RESEARCH only when ALL of these are true:**
- A justification references a critical function/symbol \
NOT in gathered code
- That missing code could realistically change the verdict
- The code hasn't been requested before \
(or was found but not analyzed)
- You can name SPECIFIC items to add to \
required_information

---

**TWO WAYS TO REQUEST CHANGES:**

1. **Need more code** -> NEEDS_MORE_RESEARCH with \
specific items in required_information
   - Use when: A critical function/symbol is referenced \
but NOT in gathered code
   - System will: Gather more code, then re-analyze

2. **Disagree with analysis** -> NEEDS_MORE_RESEARCH \
with EMPTY required_information
   - Use when: All code is present, but the analysis \
reasoning is flawed
   - System will: Send your feedback to analysis for \
reconsideration (no new research)
   - Your feedback should clearly explain what the \
analysis got wrong

---

**OUTPUT OPTIONS:**

**Option 1 - Analysis is correct:**
{{
  "result": "APPROVED",
  "feedback": "Explanation of why the analysis is \
sufficient",
  "required_information": [],
  "confidence": 0.85
}}

**Option 2 - Need more code:**
{{
  "result": "NEEDS_MORE_RESEARCH",
  "feedback": "Explanation of what code is missing and \
why it matters",
  "required_information": ["function_name", "symbol_name"],
  "confidence": 0.4
}}

**Option 3 - Analysis is flawed (trigger reanalysis):**
{{
  "result": "NEEDS_MORE_RESEARCH",
  "feedback": "Detailed explanation of what the analysis \
got wrong - e.g., missed a guard at line X, didn't check \
buffer sizes, etc.",
  "required_information": [],
  "confidence": 0.3
}}

Note: For Option 2, required_information must list ONLY \
items that are (1) mentioned in justifications, \
(2) NOT in gathered code, and (3) critical to the verdict.
"""
