"""Analysis node prompt templates.

These prompts guide the analysis LLM in making verdict decisions
based on gathered code evidence.
"""

from typing import Any, Dict


def build_analysis_prompt(state: Dict[str, Any]) -> str:
    """
    Build the analysis prompt for verdict decision.
    
    Args:
        state: Current investigation state containing gathered code and issue details
    
    Returns:
        Formatted prompt string for the analysis LLM
    """
    gathered_code_section = state.get('gathered_code', '(No code gathered)')
    is_reanalysis = state.get('needs_reanalysis', False)
    
    # Build evaluator feedback section for reanalysis
    evaluator_feedback_section = ""
    if is_reanalysis and state.get('evaluation_feedback'):
        evaluator_feedback_section = f"""
---

**⚠️ EVALUATOR FEEDBACK (IMPORTANT - Address this in your reanalysis):**
{state['evaluation_feedback']}

The evaluator disagreed with your previous analysis. Please carefully reconsider your verdict based on this feedback. Pay special attention to any control flow, guards, or logic issues the evaluator identified.

---
"""
    
    return f"""You are an expert security analyst determining if a SAST (Static Application Security Testing) finding is a FALSE_POSITIVE or TRUE_POSITIVE.

**Your Task:**
Analyze the reported security finding and the provided source code to determine if the vulnerability is real (TRUE_POSITIVE) or a false alarm (FALSE_POSITIVE).

---

**SAST FINDING:**
{state['issue_description']}

**CODE GATHERED DURING RESEARCH:**
{gathered_code_section}

**PREVIOUS ANALYSIS (if any):**
{state.get('analysis') or 'None - this is the first analysis.'}
{evaluator_feedback_section}
---

**MANDATORY ANALYSIS STEPS:**

Before reaching a verdict, you MUST complete these steps IN ORDER:

**Step 1: Identify Source and Sink**
- SOURCE: Where does the issue originate? (e.g., variable declaration, user input, allocation)
- SINK: Where is the vulnerable use? (e.g., line number where uninitialized value is used)

**Step 2: Trace ALL Code Between Source and Sink**
- List EVERY line of code between source and sink
- For EACH line, note if it contains: `if`, `goto`, `return`, `exit`, `break`, `continue`
- DO NOT skip any lines - read them ALL

**Step 3: Analyze Each Control Flow Statement**
For each `if`/`goto`/`return`/`exit` found, ask:
- What condition triggers it?
- Does it exit/skip BEFORE reaching the vulnerable line?
- Could it act as a guard that prevents the vulnerability?

**Step 4: Determine Reachability**
- Can the SINK actually be reached without the issue being resolved?
- If ANY guard prevents reaching the sink in the vulnerable state → FALSE_POSITIVE

---

**ANALYSIS GUIDELINES:**

1. **Code-Based Evidence Only**: Reference specific line numbers. Do not assume behavior - verify it in the code.

2. **READ EVERY LINE**: Before saying "there are no guards", you must have examined EVERY line between source and sink. List the control flow statements you found.

3. **Verify Path Reachability**: For TRUE_POSITIVE, prove the vulnerable path is reachable by showing no guard prevents it.

4. **One REACHABLE Vulnerable Path is Enough**: If even one REACHABLE path triggers the vulnerability → TRUE_POSITIVE.

5. **Syntax Issues are TRUE_POSITIVE**: Syntax errors are always real issues.

6. **Issue-Type Specific Patterns**:
   - **UNINIT**: Look for `if (condition) goto/return/exit` AFTER the loop/conditional that should initialize, but BEFORE the use. This pattern often guards against uninitialized use.
   - **RESOURCE_LEAK**: Check if `exit()` is called shortly after - OS cleanup makes this FALSE_POSITIVE for CLI tools.
   - **NULL_DEREF**: Look for null checks before the dereference.
   - **OVERRUN**: Track buffer size at declaration vs size used in callee functions.

---

**REQUIRED OUTPUT:**
Provide your analysis with the following:
- **verdict**: TRUE_POSITIVE or FALSE_POSITIVE
- **confidence**: HIGH (all paths traced, clear evidence), MEDIUM (most paths traced, minor gaps), or LOW (significant uncertainty)
- **reasoning**: Your step-by-step analysis tracing the data flow
- **justifications**: List of specific code-based evidence points with line references

**Begin your analysis.**"""
