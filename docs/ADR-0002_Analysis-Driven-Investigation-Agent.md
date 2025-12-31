# ADR 0002: Analysis-Driven Investigation Agent for SAST Triage

**Status:** Proposed
**Date:** 2025-12-30
**Decision:** Pending approval

---

## CONTEXT

The current investigation implementation behaves like a **static loop**:
- Fetch some code context (often brittle / identifier-driven)
- Run a single-pass analysis
- Run an evaluation step
- Repeat for a fixed number of iterations (or stop early in ad-hoc ways)

This structure has predictable failure modes:
- Stops too early for complex findings (insufficient evidence)
- Dead-ends when the system cannot name the exact symbol/file it needs

We want to replace the static loop with an **agentic analysis node** that:
- Owns the analysis end-to-end
- Uses the current analysis state to decide which tool to call next
- Fetches additional code/config when evidence is missing
- Finalizes only when evidence sufficiency criteria are met

---

## PROBLEM STATEMENT

The static loop structure leads to:
- **Incorrect verdicts** when analysis is performed on incomplete context
- **Brittle retrieval** when context acquisition relies on exact identifiers
- **Non-deterministic stopping behavior** (fixed iteration limits or confidence-driven stops)

We need an agent architecture that is:
- **Evidence-grounded** (final decisions justified by cited code/config)
- **Adaptive** (fetch more context when needed)
- **Cost-bounded** (no infinite loops)
- **Observable** per issue (Langfuse v3 per-issue traces and scoring)

---

## DECISION

Implement an **Analysis-Driven Investigation Agent (Agentic Analysis Node)** that iterates:

1. Maintain a structured analysis state (claims, evidence, unknowns).
2. Choose a retrieval action when unknowns block correctness.
3. Update analysis with newly fetched evidence.
4. Attempt verification with a **guard verifier** using an evidence package.
5. If verification fails, fetch missing evidence and repeat; if it passes, finalize.
6. If circuit breakers trigger, finalize as `NEEDS_REVIEW` with explicit remaining unknowns.

This replaces the static “fetch → analyze → evaluate (k times)” loop with a **single agentic analysis controller** whose policy is driven by the analysis state.

---

## PROPOSED SOLUTION

### Core Agent Loop (Invariant)

The agent must operate on this invariant:

> **If the current verdict relies on assumptions or missing evidence, the next action is code retrieval (not finalization).**

### Cold Start (Initialization)

On cold start, the agent state is initialized from **preloaded inputs** (before any additional retrieval tool calls):
- **SAST finding** (issue metadata, issue type, file/line, scanner message)
- **Finding trace** (error/taint trace)
- **Error-trace source bundle**: source code for **all files/functions referenced by the trace**

Initialization rules:
- **evidence**: seeded from the trace bundle as citations/excerpts (file + function + relevant snippet).
- **claims**: start as empty or minimal “trace-observed” claims, and are promoted only when anchored to cited evidence.
- **unknowns**: seeded from (a) the evidence contract for the vulnerability class and (b) what the trace bundle does *not* prove yet (e.g., missing sanitizer implementation, missing callers/callees, missing framework/global config).

If the trace bundle is incomplete (missing files/symbols, truncated context), the agent immediately enters the normal loop and uses retrieval tools to fill gaps:
- Prefer `fetch_code` when a concrete file/function identifier is known.
- Use `search_codebase` when names are unknown or likely incorrect.
- Use `list_files` to discover likely config/middleware/framework locations and to narrow search scope.

#### Example: cold start state for a Resource Leak finding (CWE-772)

Finding (input):
- Error: `RESOURCE_LEAK` (CWE-772)
- Location: `wpa_supplicant-2.10/wpa_supplicant/dbus/dbus_new_handlers_p2p.c:2703`
- Trace summary: `query = wpabuf_alloc_copy(...)` overwrites an existing `query` pointer, leaking the previous allocation.

Cold start state (initialized from the trace bundle):

- `claims` (initial)
  - `C1` (trace-observed): At `dbus_new_handlers_p2p.c:2703`, `query` is assigned the result of `wpabuf_alloc_copy(entry.bytearray_value, entry.array_len)`.
  - `C2` (trace-observed): The assignment overwrites a previous value of `query` without an intervening free/reset, implying a potential leak of the previously-referenced storage.
  - `C3` (tentative, needs evidence): The overwritten storage is heap-allocated and must be released (e.g., via `wpabuf_free(query)` or equivalent) before overwrite or along all control-flow paths.

- `evidence` (initial)
  - `E1`: `wpa_supplicant-2.10/wpa_supplicant/dbus/dbus_new_handlers_p2p.c` snippet around line ~2703 showing:
    - `query = wpabuf_alloc_copy(entry.bytearray_value, entry.array_len);`
    - and the surrounding control flow (e.g., `goto error_clear;`) if included in the trace bundle.
  - `E2`: Trace metadata that labels `wpabuf_alloc_copy` as an allocation function and flags an overwrite leak on `query` at line 2703.

- `unknowns` (initial)
  - `U1`: What is the lifecycle contract of `wpabuf_alloc_copy`? (Does it return a `struct wpabuf*` that must be freed by `wpabuf_free`?)
  - `U2`: Where is `query` first assigned/initialized in this function, and can it already hold allocated storage before line 2703?
  - `U3`: Are there any frees/resets of `query` on all paths (success path and `error_clear`/error handling paths) before it can be overwritten or before the function returns?
  - `U4`: Is `query` an alias to another owner (not owned here), or does this function own it? (ownership determines whether overwrite is a real leak)

From this cold start, the next retrieval step is usually to fetch:
- The full containing function around line 2703 (to answer `U2`/`U3`), and
- The definition/contract of `wpabuf_alloc_copy` and its free function (to answer `U1`).

### Flow Chart (How the Agent Runs)

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
  |          guard verification?                  |
  \----------------------+-----------------------/
                         |
            +------------+------------+
            |                         |
           YES                       NO (try guard)
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
  |  - add evidence              |   | guard_verify(evidence_package)|
  |  - update claims             |   +--------------+---------------+
  |  - add/remove unknowns       |                  |
  +--------------+---------------+        +---------+----------+
                 |                        |                    |
                 +-----> back to LOOP     PASS                 FAIL
                                          |                    |
                                          v                    v
                                 +----------------+   +------------------------------+
                                 | FINALIZE: TP/FP|   | Update unknowns from guard   |
                                 +----------------+   |  - blocking_gaps             |
                                                      |  - required_next_fetches     |
                                                      +--------------+---------------+
                                                                     |
                                                                     v
                                                             back to LOOP

  Circuit breakers (max calls / max time / stalled progress) can stop at any point:
    -> FINALIZE: NEEDS_REVIEW (include remaining unknowns + top next fetches)
```

### Tool Set (Minimum)

The agent must have at least:

1. **fetch_code(identifier)** (exact) – preferred when a concrete symbol or file path is known.
2. **search_codebase(pattern, scope)** (fallback) – required when symbol names are unknown or guessed incorrectly.
3. **list_files(directory)** (discovery) – required for framework/config/middleware discovery and narrowing search scope.
4. **guard_verify(evidence_package)** (LLM verifier / guard) – a skeptical verifier the agent must convince using only evidence.

Note: `guard_verify` is a **gate** for finalization. It must not accept confidence; it must accept only when the evidence contract is satisfied.

---

## KEY DECISIONS

### Decision 1: Analysis State Is First-Class (Consolidated)

The agent maintains a clear split between **Infrastructure State** (managed by the framework) and **Analysis State** (managed by the agent reasoning).

#### 1. Infrastructure State (System Bookkeeping)
- **`messages`**: Full conversation history.
- **`fetched_files`**: Full content of all retrieved code blocks (passed to LLM as context).
- **`last_error`**: The specific failure message from the previous tool call (if any), provided to the agent to prevent loops.
- **`iteration_counters`**: Execution metrics for circuit breakers.

#### 2. Analysis State (The Reasoning Engine)
The agent updates this structured state in its output to track findings:
- **`claims`**: Atomic, checkable assertions about data flow, sanitization, or framework behavior. Includes status (`supported` | `tentative` | `rejected`) and links to evidence IDs.
- **`evidence`**: Concrete citations from the code. Each item includes a unique ID, file path, line range, and the **raw code snippet** proving the claim.
- **`unknowns`**: Prioritized unanswered questions blocking a final verdict. Each unknown should include a suggested retrieval action (e.g., `fetch_code` for a specific function).

#### Update flow (how the state is created/updated each cycle)

**Cold start:**
- See `### Cold Start (Initialization)` above for how **evidence**, **claims**, and **unknowns** are seeded from the finding trace + trace source bundle.

**Each retrieval cycle:**
- The system provides the `last_error` and any new `fetched_files`.
- After a retrieval tool call, the agent:
  - appends/updates **evidence** with new snippets,
  - promotes/refines/downgrades **claims** based on the new evidence,
  - removes resolved **unknowns** and adds/refines new ones uncovered by the new context.
- The agent attempts `guard_verify` only when it believes unknowns are sufficiently resolved.
- After `guard_verify`:
  - on PASS: finalize verdict,
  - on FAIL: translate `blocking_gaps` into new/high-priority **unknowns** and continue retrieval.

**Rationale:**
- Prevents "token fog" by keeping the reasoning state lean and distinct from full-file context.
- Ensures the agent can recover from tool failures by explicitly surfacing `last_error`.
- Forces explicit linkage from verdict → claims → evidence snippets.

---

### Decision 2: Finalization Is Gated by Evidence Sufficiency (Checklist)

Define an “evidence contract” per vulnerability class (examples):

- **Injection (SQL/Command/LDAP/etc.)**
  - Identify **source** of tainted input (or explain why input is controlled).
  - Identify **sink** and show the call site path to sink.
  - Identify and validate **sanitizers/validators/parameterization** (or prove absence).
  - Confirm **framework defaults** and global middleware/config (if applicable).

- **XSS**
  - Identify render context (HTML/attribute/JS/URL).
  - Confirm escaping/encoding function and its correctness for that context.
  - Check templating engine auto-escaping defaults and override paths.

Finalization requires:
- No critical unknowns remain for the selected issue type’s evidence contract, OR
- Circuit breakers force `NEEDS_REVIEW` with explicit remaining unknowns and recommended next fetches.

Rationale:
- Avoids “LLM confidence == correctness”.
- Improves consistency and debuggability.

---

### Decision 3: Guard Verifier LLM (Adversarial Verification Gate)

Replace the “evaluation/critique” role with a **Guard Verifier LLM** that the analysis agent must convince using an evidence package.

**Behavior**
- `guard_verify` is invoked as a tool call.
- Input: `evidence_package` (structured, including the agent’s analysis narrative plus structured claims/evidence/unknowns), plus the selected evidence contract for the issue type.
- **CRITICAL**: The `evidence_package` MUST contain the **raw code snippets** associated with each claim. The Guard verifies the agent's logic against the actual source code, not just the agent's summary of it.
- Output: a structured verification result.
- The guard is instructed to be **skeptical** and to reject unless the evidence meets the contract and is clearly supported by the provided snippets.
- The guard does not use tools; it can only reject with explicit missing evidence categories and concrete next fetch suggestions.

Rationale:
- Reduces self-confirmation and confidence-driven finalization.
- Prevents "grounding hallucination" where the agent misrepresents what a snippet proves.
- Forces explicit linkage from verdict → claims → evidence.
- Makes “stop” measurable and auditable.

**Evidence package (minimum)**
- Claims: explicit statements about sources, sinks, dataflow, sanitizers/validators, framework defaults.
- Evidence: file paths + **raw code snippets** that directly support each claim.
- Unknowns: any remaining unanswered questions.
- Proposed verdict: `TRUE_POSITIVE`, `FALSE_POSITIVE`, or `NEEDS_REVIEW`.
- Contract mapping: for each required evidence-contract item, link the supporting evidence (or mark missing).

**Guard output schema (minimum)**
- `verification_passed`: bool
- `verification_reasoning`: short explanation for pass/fail, grounded in the provided evidence (cite which claims/evidence items were decisive; no speculation)
- `blocking_gaps`: typed list (mapped to the evidence contract)
- `rejected_claims`: which claims are not supported by evidence
- `required_next_fetches`: concrete file/symbol/pattern + scope
- `stop_reason_if_any`: e.g., “contract_unsatisfied”, “evidence_conflict”

**Finalization rule**
- The agent may finalize a `TRUE_POSITIVE` / `FALSE_POSITIVE` verdict only if:
  - the evidence contract is satisfied AND
  - `verification_passed == true`.

If the guard rejects, the agent must fetch missing evidence (or terminate with `NEEDS_REVIEW` on circuit breaker).

---

### Decision 4: Circuit Breakers and Duplicate Suppression Are Mandatory

Hard limits (defaults; configurable):
- Max tool calls per issue (e.g., 15)
- Max wall time per issue (e.g., 5 minutes)
- Duplicate detection: prevent fetching the same file/symbol/pattern repeatedly unless new scope/rationale exists

Verification and Stalled Progress limits:
- Run `guard_verify` only when the agent believes it is ready to finalize (not after every retrieval).
- **Guard Rejection Limit**: Track repeated rejections by the Guard. If the Guard rejects the analysis 3 times for the same missing evidence categories or if no new evidence is added between rejections, terminate with `NEEDS_REVIEW`.
- **Duplicate Detection**: Prevent fetching the same file/symbol/pattern repeatedly unless new scope/rationale exists.
- **Stalled Progress**: If the agent makes 3 consecutive retrieval calls without adding new `claims` or `evidence` to the state, trigger a circuit breaker.

On breaker:
- Final verdict becomes `NEEDS_REVIEW` (or equivalent), with:
  - remaining unknowns (including the Guard's latest `blocking_gaps`)
  - last known evidence
  - top recommended next fetches

Rationale:
- Prevents runaway cost.
- Produces useful partial results.

---

## OBSERVABILITY (LANGFUSE V3 PER-ISSUE)

This ADR assumes the per-issue tracing model from `DESIGN_PER_ISSUE_TRACING_LANGFUSE_V3.md`.

Requirements:
- Each issue investigation runs under a single per-issue trace (session = package run, user_id = issue_id).
- Add scores:
  - `verdict`
  - `iteration_count`
  - `stop_reason` (string enum, recorded as metadata and/or score)
  - `evidence_sufficiency_passed` (0/1)
  - `retrieval_fallback_used` (0/1)
  - `guard_verification_passed` (0/1)
  - `guard_attempt_count` (int)
  - `guard_rejection_categories` (list-like string / tags)

Rationale:
- Enables measuring whether improvements come from better retrieval vs. better reasoning.

---

## CONSEQUENCES

### Benefits
- More consistent and evidence-grounded verdicts
- Better recovery from missing/unknown identifiers (via fallback retrieval)
- Improved debuggability through structured unknowns and stop reasons

### Costs / Risks
- Higher LLM and retrieval cost per issue (mitigated by circuit breakers)
- Requires careful design of evidence contracts per issue type
- Poorly specified `guard_verify` can still introduce noise (mitigated by typed gaps, strict evidence-only acceptance, and bounded retries)

---

---

## PROMPT IMPLEMENTATION GUIDELINES

To ensure the Agent and Guard perform as intended, the prompts must emphasize the following strategies:

### 1. Handling Evidence Conflict (The "Honest Investigator" Pattern)
If the agent encounters contradictory evidence (e.g., a global sanitizer exists but a specific route bypasses it), it must not "choose a winner" to force a PASS.
- **Claims Status**: Introduce a `conflicting` status for claims where evidence exists for both sides.
- **Finalization**: If a critical path has unresolved conflicting evidence, the agent should finalize as `NEEDS_REVIEW` and explicitly describe the contradiction in the narrative.

### 2. Retrieval Strategy (Discovery-First)
The agent should be prompted to avoid "guessing" symbol names.
- **Tiered Approach**: In the first 2 iterations, if the framework or project structure is unclear, prioritize `list_files` and `search_codebase` over repetitive `fetch_code` attempts.
- **Scope Awareness**: Use `list_files` to discover middleware, configuration, and utility directories early in the investigation.

### 3. Adversarial Persona Split
- **Agent Persona**: An **Investigative Reporter**. The goal is to gather all facts, both for and against the vulnerability. It is successful if it provides a complete picture, not just if it finds a "bug."
- **Guard Persona**: A **Skeptical Auditor**. The Guard assumes the Agent is missing something. It should specifically look for "leaps of logic" where a claim is made (e.g., "The input is sanitized") but the provided snippet doesn't explicitly show that specific input being sanitized.

### 4. Default Evidence Contract
For CWEs without a specific contract, use the **Standard Taint-Flow Contract**:
1. **Source**: Is the entry point user-controlled?
2. **Dataflow**: Is the taint preserved through all assignments/calls?
3. **Sink**: Does the taint reach a dangerous function?
4. **Sanitization**: Is there any evidence of validation or encoding on all paths to the sink?

---

## IMPLEMENTATION NOTES (NON-NORMATIVE)

1. Implement/extend agent state schema to include claims/evidence/unknowns.
2. Implement `guard_verify` prompt and parser to produce structured fields.
3. Ensure the agent chooses between:
   - `fetch_code` (exact) → when identifier is known
   - `search_codebase` → when identifiers are unknown/guessed
   - `list_files` → when discovery is needed to narrow scope
4. Integrate per-issue Langfuse v3 config pattern and scoring from `DESIGN_PER_ISSUE_TRACING_LANGFUSE_V3.md`.

---

## REFERENCES

- `DESIGN_PER_ISSUE_TRACING_LANGFUSE_V3.md`
- Existing code: `src/sast_agent_workflow/`


