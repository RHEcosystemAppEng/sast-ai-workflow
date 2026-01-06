# ADR 0002: Analysis-Driven Investigation Agent for SAST Triage

---

## CONTEXT & PROBLEM STATEMENT

The current investigation implementation behaves like a **static loop**:
- Fetch some code context (often brittle / identifier-driven)
- Run a single-pass analysis
- Run an evaluation step
- Repeat for a fixed number of iterations (or stop early in ad-hoc ways)

This structure has predictable failure modes:
- **Incorrect verdicts**: Analysis is performed on incomplete context.
- **Brittle retrieval**: Context acquisition relies on exact identifiers.
- **Non-deterministic stopping**: Fixed iteration limits or confidence-driven stops lead to premature finalization or inefficient loops.

We need an **agentic analysis node** that owns the investigation end-to-end, fetching additional evidence as needed and finalizing only when specific sufficiency criteria are met.

---

## THE DECISION

Implement an **Analysis-Driven Investigation Agent (Agentic Analysis Node)** that replaces the static loop with a single agentic controller. The agent's policy is driven by its own analysis state (claims, evidence, unknowns). It iterates by choosing retrieval actions until it can satisfy an evaluator with concrete proof.

---

## AGENT ARCHITECTURE

### Core Loop (The Invariant)

The agent operates on a strict invariant:
> **If the current verdict relies on assumptions or missing evidence, the next action is code retrieval, not finalization.**

#### Process Flow
```
  +------------------------------+
  | START: issue + initial input |
  +--------------+---------------+
                 |
                 v
  +------------------------------+
  | Initialize state             |
  |  - claims                    |
  |  - evidence                  |
  |  - unknowns                  |
  +--------------+---------------+
                 |
                 v
  +------------------------------+
  | LOOP                         |
  +--------------+---------------+
                 |
                 v
  /----------------------------------------------\
  | Decision: do unknowns block correctness /     |
  |          evaluator verification?              |
  \----------------------+-----------------------/
                         |
            +------------+------------+
            |                         |
           YES                     NO (try evaluator)
            |                         |
            v                         v
  +------------------------------+   +------------------------------+
  | Retrieve more context        |   | Build evidence_package       |
  | Choose ONE:                  |   |  - analysis (narrative)      |
  |  - fetch_code                |   |  - claims (structured)       |
  |  - search_codebase           |   |  - evidence (citations)      |
  |  - list_files                |   |  - unknowns                  |
  +--------------+---------------+   |  - contract mapping          |
                 |                   +--------------+---------------+
                 v                                  |
  +------------------------------+                  v
  | Update state                 |   +------------------------------+
  |  - add evidence              |   | evaluator(evidence_package)   |
  |  - update claims             |   +--------------+---------------+
  |  - add/remove unknowns       |                  |
  +--------------+---------------+        +---------+----------+
                 |                        |                    |
                 +-----> back to LOOP     PASS                 FAIL
                                          |                    |
                                          v                    v
                                 +----------------+   +------------------------------+
                                 | FINALIZE: TP/FP|   | Update unknowns from evaluator|
                                 +----------------+   |  - blocking_gaps             |
                                                      |  - required_next_fetches     |
                                                      +--------------+---------------+
                                                                     |
                                                                     v
                                                             back to LOOP
```

### State Management

The agent maintains a clear split between system bookkeeping and investigative reasoning.

#### 1. Infrastructure State (System Bookkeeping)
- **`messages`**: Full conversation history.
- **`fetched_files`**: Full content of all retrieved code blocks.
- **`last_error`**: Specific failure messages from previous tool calls to prevent loops.
- **`iteration_counters`**: Metrics for circuit breakers.

#### 2. Analysis State (Investigative Reasoning)
- **`claims`**: Atomic, checkable assertions about data flow, sanitization, or framework behavior. Status: `supported` | `tentative` | `rejected` | `conflicting`.
- **`evidence`**: Concrete citations from the code (ID, path, line range, and **raw snippet**).
- **`unknowns`**: Prioritized unanswered questions blocking a final verdict.

#### 3. Initialization (Cold Start)
The state is seeded from **preloaded inputs** before any retrieval calls:
- **SAST finding**: Metadata, issue type, file/line, scanner message.
- **Finding trace**: Error/taint trace.
- **Error-trace source bundle**: Source code for all files/functions referenced by the trace.

### Tool Set (Minimum)

1. **`fetch_code(identifier)`**: Preferred when concrete symbols or paths are known.
2. **`search_codebase(pattern, scope)`**: Fallback when names are unknown or likely incorrect.
3. **`list_files(directory)`**: Discovery tool for narrowing scope and finding config/middleware.
4. **`evaluator(evidence_package)`**: LLM verification gate (adversarial persona).

---

## QUALITY & RELIABILITY

### Adversarial Evaluation

Finalization is gated by an **Evaluator LLM** that the analysis agent must convince.
- **Input**: An `evidence_package` containing the analysis narrative plus structured claims/evidence/unknowns.
- **Skepticism**: The evaluator assumes the agent is missing something and rejects unless the **raw code snippets** explicitly prove the claims.
- **Gate**: The agent may only finalize a `TRUE_POSITIVE` or `FALSE_POSITIVE` verdict if `verification_passed == true`.

### Circuit Breakers

To prevent runaway costs and stalled progress, hard limits are enforced:
- **Execution Limits**: Max tool calls (e.g., 15) or max wall time (e.g., 5 mins).
- **Stalled Progress**: Triggered if 3 consecutive retrieval calls add no new claims or evidence.
- **Evaluator Rejection Limit**: Triggered if the evaluator rejects the analysis 3 times for the same gaps or if no progress is made between rejections.
- **Result on Breaker**: Finalize as `NEEDS_REVIEW` with remaining unknowns and recommended next fetches.

---

## OBSERVABILITY (LANGFUSE V3)

Each issue investigation runs under a single per-issue trace. Key metrics recorded as scores/metadata:
- `verdict`, `stop_reason`, `iteration_count`.
- `evidence_sufficiency_passed`, `retrieval_fallback_used`.
- `guard_verification_passed`, `guard_attempt_count`.

---

## IMPLEMENTATION GUIDELINES

### Prompt Strategies
1. **The "Honest Investigator"**: Handle contradictory evidence with a `conflicting` status rather than forcing a verdict.
2. **Discovery-First Retrieval**: Prioritize `list_files` and `search_codebase` in early iterations to avoid guessing symbol names.
3. **Adversarial Persona Split**: 
   - **Agent**: Investigative Reporter (gather all facts).
   - **Evaluator**: Skeptical Auditor (look for leaps of logic).

---

## OPTIONAL / FUTURE IMPROVEMENTS

1. **Agentic Evaluator**: Transition the evaluator to a full agent with same toolset to allow independent verification of the analysis.
2. **Evaluator History**: Provide past evaluator reports in the evaluator's context to track the resolution of previously identified "blocking gaps".
3. **CWE-Specific Evidence Contracts & False Alarm Guidelines**: Transition to gated finalization using detailed, vulnerability-specific "False Alarm" guidelines derived from process mining.
   - **Explanation**: Standard triage often follows predictable "checklist" patterns per CWE (e.g., for XSS: "Is the output rendered in a JS context? If so, is it JSON-serialized?").
   - **Usage**: The Evaluator uses these CWE-specific guidelines as a rigorous audit checklist. It rejects any verdict that hasn't explicitly cleared the known False Positive criteria for that specific vulnerability class, forcing the agent to retrieve the necessary proof.
4. **Project Context Knowledge (Pre-initialization)**: Provide a pre-generated "Project Context" summary (architecture, middleware, docs) to mitigate cold-start inefficiency.
   - Perform an analysis of the prevalence of documentation and middleware across our package library to quantify potential effectiveness.

---

## REFERENCES

- `DESIGN_PER_ISSUE_TRACING_LANGFUSE_V3.md`
- Existing code: `src/sast_agent_workflow/`
