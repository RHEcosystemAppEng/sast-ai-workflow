# Agent-Based Investigation Implementation Tasks

**Epic:** Agent-Based SAST Investigation System
**Based on:** ADR-0001
**Timeline:** 4 weeks (2 phases)

**Problem Being Solved:**
Current system has fixed iterations and no fallback retrieval. When missing context (sanitizers/validators not in trace), the system defaults to marking issues as TRUE_POSITIVE (real vulnerability) even when they are actually FALSE_POSITIVE (false alarm - code is safe). This causes high false positive rate and low precision - safe code is incorrectly flagged as vulnerable.

---

## PHASE 1: Core Agent Framework (Week 1-2)

### TASK-001: Setup Agent Framework Infrastructure

**Description:**
Set up the base LangGraph ReAct agent framework that will orchestrate tool calls for SAST issue investigation. This includes creating the agent state schema, agent node, and routing logic.

**Acceptance Criteria:**
- [ ] Agent state schema defined (TypedDict with issue data, fetched files, analysis results, control flags, project_context)
- [ ] Project context initialized once per workflow (reused across all issues)
- [ ] Agent node implemented (receives state, decides next tool, returns tool call decision)
- [ ] LangGraph StateGraph configured with agent node
- [ ] Agent can make basic tool call decisions (logged to console)
- [ ] Unit tests pass for agent state management
- [ ] Agent loop terminates correctly (no infinite loops in basic test)

**Subtasks:**

#### TASK-001.1: Define Agent State Schema
**Description:** Create TypedDict/Pydantic model for agent state that persists across tool calls.

**Acceptance Criteria:**
- [ ] State schema includes: issue_id, issue, fetched_files, found_symbols, investigation_result, is_final, verdict, iteration_count, messages
- [ ] State schema includes: project_context (shared across all issues, initialized once)
- [ ] State schema includes: errors (list of tool failures), last_error, error_recovery_attempts
- [ ] State schema includes: reflection_notes (from self-correction), needs_further_investigation
- [ ] ProjectContext schema defined: structure (directory tree), security_files (list), frameworks (detected)
- [ ] ToolError schema defined: tool_name, error_message, attempted_args, timestamp
- [ ] Schema documented with field descriptions
- [ ] Schema validates correctly (Pydantic validation passes)

#### TASK-001.2: Implement Agent Decision Node
**Description:** Create the core agent node that uses LLM to decide next action based on current state. **This is the ONLY decision-making node** - all other nodes (tools, analyze, comprehensive_evaluation) are executors that update state and return results. The agent reads their outputs and decides the next action.

**Acceptance Criteria:**
- [ ] Agent node is the SOLE decision maker (only node that returns tool call decisions)
- [ ] Agent node receives state and returns tool call decision
- [ ] Agent prompt includes available tools and current state summary
- [ ] Agent prompt includes project_context (directory structure, security files, frameworks)
- [ ] Agent prompt includes feedback from comprehensive_evaluation:
  - [ ] exploration_gaps (from process audit)
  - [ ] logic_gaps (from logic audit)
  - [ ] recommendations (prioritized next steps)
- [ ] Agent prompt includes last_error context if present (for error recovery)
- [ ] Prompt instructs agent to use project context for search scope decisions
- [ ] Prompt instructs agent to prioritize exploration_gaps over specific function fetches
- [ ] Prompt instructs error recovery: try different tool or parameters after failure
- [ ] Prompt warns: Do NOT retry same tool with same args after error
- [ ] LLM configured with tool definitions (function calling)
- [ ] Agent node logs reasoning/thoughts for debugging
- [ ] Returns properly formatted tool call or FINISH signal

#### TASK-001.3: Setup LangGraph Workflow
**Description:** Configure LangGraph StateGraph with agent node and routing logic. Graph structure: agent (decision) → tools (execute) → agent (decision) → analyze (execute) → comprehensive_evaluation (advisory) → agent (decision) → ...

**Acceptance Criteria:**
- [ ] StateGraph created with SASTAgentState type
- [ ] Agent node added to graph (SOLE decision node)
- [ ] Tool nodes added: fetch_code, search_codebase, list_files, analyze_issue, comprehensive_evaluation
- [ ] Routing logic: agent → (fetch/search/list) → agent
- [ ] Routing logic: agent → analyze_issue → comprehensive_evaluation → agent
- [ ] Routing after comprehensive_evaluation checks circuit breakers (max 15 calls, timeout, duplicates, errors)
- [ ] Termination conditions implemented (is_final=TRUE from comprehensive_evaluation, circuit breakers)
- [ ] Graph compiles without errors
- [ ] Simple test case runs end-to-end (agent → fetch → agent → analyze → eval → agent → END)

#### TASK-001.4: Implement Project Context Initialization
**Description:** Create initialization step that runs once per workflow to discover project structure using list_files and search_codebase (or simple fallbacks in Phase 1).

**Acceptance Criteria:**
- [ ] Initialization function created (runs once before processing issues)
- [ ] Phase 1 implementation: Uses os.listdir() + subprocess grep for basic discovery
- [ ] Discovers directory structure (3 levels deep, excludes node_modules/venv/.git)
- [ ] Searches for security patterns: "sanitize|validate|clean|escape"
- [ ] Detects frameworks from common files (requirements.txt, package.json, pom.xml)
- [ ] Stores results in tracker.project_context (persists across all issues)
- [ ] Initialization logged in Langfuse as separate span
- [ ] Total initialization time <15 seconds for typical repository
- [ ] Phase 2 upgrade: Replace with proper list_files/search_codebase tools (TASK-008/009)

#### TASK-001.5: Implement Error Recovery Routing
**Description:** Implement state-driven error handling where tool failures are captured in state and agent is prompted to try alternative approaches.

**Acceptance Criteria:**
- [ ] All tools wrap execution in try/except and append errors to state.errors
- [ ] ToolError captures: tool_name, error_message, attempted_args, timestamp
- [ ] Agent prompt includes last_error context if present
- [ ] Agent instructed to try different parameters or different tool after error
- [ ] Error recovery limited to 3 attempts (error_recovery_attempts counter)
- [ ] Circuit breaker triggers if error_recovery_attempts > 3
- [ ] All errors logged in Langfuse for debugging
- [ ] Unit tests verify error recovery paths (fetch fails → search, regex error → simpler pattern)

**Subtasks:**

##### TASK-001.5.1: Wrap All Tools with Error Handling
**Acceptance Criteria:**
- [ ] fetch_code wraps execution in try/except
- [ ] search_codebase wraps execution in try/except
- [ ] list_files wraps execution in try/except
- [ ] All errors appended to state.errors with ToolError schema
- [ ] Error messages returned to agent (not just logged silently)
- [ ] error_recovery_attempts incremented on each error

##### TASK-001.5.2: Enhance Agent Prompt with Error Recovery
**Acceptance Criteria:**
- [ ] Agent prompt includes state.last_error if present
- [ ] Prompt lists recovery strategies per tool type
- [ ] Prompt warns: "Do NOT retry same tool with same args"
- [ ] Prompt instructs: "After 3 errors, proceed to evaluate_analysis"

##### TASK-001.5.3: Test Error Recovery Behavior
**Acceptance Criteria:**
- [ ] Test case: fetch_code fails → agent tries search_codebase
- [ ] Test case: search_codebase regex error → agent simplifies pattern
- [ ] Test case: 3 consecutive errors → circuit breaker triggers
- [ ] All error recovery paths logged in Langfuse

---

### TASK-002: Implement fetch_code Tool

**Description:**
Create the fetch_code tool that wraps existing repo_handler functionality for retrieving source code by file path or symbol name. Includes enhanced docstring, anti-loop protection, and agent registration.

**Acceptance Criteria:**
- [ ] Tool function defined with @tool decorator
- [ ] Function signature: fetch_code(state, identifier, context_lines, reason)
- [ ] Calls repo_handler.get_source_code_blocks_from_error_trace() for files
- [ ] Calls repo_handler.extract_missing_functions_or_macros() for symbols
- [ ] Returns structured result (code, file_path, line_range, metadata) OR error message
- [ ] Anti-loop protection: checks state.fetched_files before fetching, rejects duplicates
- [ ] Updates state.found_symbols after successful fetch
- [ ] Docstring explicitly states: "Use for known file paths from trace or exact symbol names"
- [ ] Docstring explains trade-offs: "Fast and precise BUT requires exact identifier"
- [ ] Docstring provides examples: fetch_code('app/views.py'), fetch_code('sanitize_input')
- [ ] Docstring warns: "Will fail if symbol name guessed wrong - use search_codebase as fallback"
- [ ] Tool registered in agent's available tools
- [ ] Agent prompt includes fetch_code usage guidance
- [ ] Unit tests pass (successful fetch, failed fetch, duplicate detection)

---

### TASK-003: Implement analyze_issue Tool

**Description:**
Create analyze_issue tool that wraps IssueAnalysisService to perform LLM-based security analysis on gathered context and updates agent state with results. This tool performs ONLY the analysis - evaluation is handled separately by comprehensive_evaluation.

**Acceptance Criteria:**
- [ ] Tool function defined with @tool decorator
- [ ] Tool extracts context from state.fetched_files
- [ ] Builds analysis context from state.issue + fetched code
- [ ] Calls IssueAnalysisService.analyze_issue_core_only(issue, context, llm)
- [ ] Returns structured result: investigation_result, justifications, prompt_used
- [ ] State.investigation_result updated with analysis findings
- [ ] Analysis prompt logged for observability in Langfuse
- [ ] State changes persisted in LangGraph state
- [ ] Existing security analysis prompts preserved
- [ ] Routes directly to comprehensive_evaluation (not a tool call decision)
- [ ] Unit tests pass (analysis with valid context)
- [ ] (NICE-TO-HAVE) Implement context pruning if token count exceeds threshold (see TASK-003.3-3.6)

#### TASK-003.3: (NICE-TO-HAVE) Implement Token Counting
**Description:** Implement token counter to measure context size before analysis.

**Priority:** Nice-to-have (defer if timeline tight)

**Acceptance Criteria:**
- [ ] Token counter uses tiktoken or similar library
- [ ] Counts tokens in state.fetched_files accurately
- [ ] Token threshold configurable (default: 50k tokens)
- [ ] Token counts logged per investigation in Langfuse
- [ ] Returns total token count for pruning decision

#### TASK-003.4: (NICE-TO-HAVE) Implement Relevance Scoring
**Description:** Score each fetched file by relevance for pruning prioritization.

**Priority:** Nice-to-have (defer if timeline tight)

**Acceptance Criteria:**
- [ ] Relevance scores: trace files (0.5), mentioned in analysis (0.3), recently fetched (0.2)
- [ ] Scoring function: calculate_relevance(code_block, trace, investigation_result, recency)
- [ ] Returns score 0.0-1.0 per code block
- [ ] Scoring tested on 5 sample investigations
- [ ] Most relevant files consistently scored highest

#### TASK-003.5: (NICE-TO-HAVE) Implement Context Pruning Logic
**Description:** Prune context to fit within token budget while preserving relevance.

**Priority:** Nice-to-have (defer if timeline tight)

**Acceptance Criteria:**
- [ ] Function prune_context_for_analysis(state, max_tokens) implemented
- [ ] Keeps most relevant files (sorted by relevance score)
- [ ] Summarization used for medium-relevance files (if budget allows)
- [ ] Low-relevance files dropped entirely
- [ ] Trace files ALWAYS preserved (never pruned)
- [ ] Pruned context fits within token budget
- [ ] Pruning logged: what was kept, summarized, dropped

#### TASK-003.6: (NICE-TO-HAVE) Test Pruning Impact on Quality
**Description:** Validate that context pruning doesn't degrade analysis quality.

**Priority:** Nice-to-have (defer if timeline tight)

**Acceptance Criteria:**
- [ ] Run baseline test cases with and without pruning
- [ ] Accuracy/Recall/Precision within 5% with pruning enabled
- [ ] At least 1 test case with 10+ files triggers pruning
- [ ] Pruned investigations still reach correct verdicts
- [ ] Pruning logs show sensible decisions (trace files kept)

---

### TASK-004: Implement comprehensive_evaluation Tool

**Description:**
Create comprehensive_evaluation tool that combines process reflection and logic evaluation into a single unified assessment. After analyze_issue produces security analysis, this tool performs both process audit (did we investigate thoroughly?) and logic audit (is the analysis sound?) to provide coherent guidance for the agent's next decision.

**This replaces the originally planned separate "reflection" and "evaluate_analysis" nodes.**

**Acceptance Criteria:**
- [ ] Tool function defined with @tool decorator
- [ ] Single LLM call combining process audit + logic audit
- [ ] **Process Audit (Reflection)**: Checks investigation thoroughness
  - [ ] Uses state.project_context to identify unexplored areas
  - [ ] Identifies exploration_gaps (global middleware, config, framework defaults, validators)
  - [ ] Suggests specific files from project_context.security_files (not guessed function names)
  - [ ] Checks common blind spots: middleware, framework defaults, configuration
- [ ] **Logic Audit (Evaluation)**: Validates analysis reasoning
  - [ ] Assesses if analysis claims are backed by fetched evidence
  - [ ] Identifies logic_gaps (unverified assumptions, missing data flow steps)
  - [ ] Specifies required_code (exact functions/classes referenced but not examined)
  - [ ] Determines is_final based on BOTH process thoroughness AND logic soundness
- [ ] Returns ComprehensiveEvaluationResponse with:
  - [ ] is_final: "TRUE" | "FALSE"
  - [ ] exploration_gaps: List[ExplorationGap] (process audit results)
  - [ ] logic_gaps: List[str] (logic audit results)
  - [ ] required_code: List[RequiredCode] (specific functions needed)
  - [ ] recommendations: List[str] (prioritized next steps)
  - [ ] justifications: List[str] (why final/not final)
- [ ] Updates state with unified feedback:
  - [ ] state.is_final = (response.is_final == "TRUE")
  - [ ] state.evaluation_result = response
  - [ ] state.reflection_notes = formatted exploration_gaps
  - [ ] state.evaluation_feedback = justifications
- [ ] Prompt explicitly includes project_context for file suggestions
- [ ] Investigation terminates when is_final=TRUE (via routing logic)
- [ ] Routes back to agent when is_final=FALSE (agent uses feedback to decide next action)
- [ ] Logged in Langfuse as single "comprehensive_evaluation" span
- [ ] Unit tests pass:
  - [ ] Test: Identifies exploration gaps using project_context
  - [ ] Test: Identifies logic gaps (referenced but not fetched code)
  - [ ] Test: Sets is_final=TRUE only when both process thorough AND logic sound
  - [ ] Test: Provides specific file suggestions from project_context
  - [ ] Test: Prioritizes exploration gaps over specific function fetches

---

### TASK-005: Integrate Langfuse Observability

**Description:**
Integrate Langfuse tracing to provide complete observability into agent investigations - tracking tool calls, reasoning steps, token costs, and state changes. Enables debugging failed investigations, understanding agent decisions, monitoring costs, and replaying investigations. Critical for Phase 1 testing validation and production cost optimization.

**Why This Matters:**
Without observability, debugging agent failures is impossible. Langfuse provides the "black box recorder" making agent reasoning transparent and auditable. Cost tracking prevents budget overruns by identifying expensive patterns early.

**Acceptance Criteria:**
- [ ] Langfuse SDK installed, API keys configured (environment variables), LangChain callback handler registered
- [ ] Test trace visible in Langfuse dashboard (validates connection)
- [ ] Agent decision node logged as span with inputs (state summary) and outputs (tool choice)
- [ ] Agent "thoughts" captured between tool calls, tool decisions attributed to reasoning
- [ ] Nested LLM calls tracked with hierarchy: agent reasoning → fetch/analyze/reflection/evaluate (nested)
- [ ] State snapshots logged at each step: iteration_count, fetched_files, errors, is_final
- [ ] All tool calls logged: tool_name, input_args, output, latency, timestamp
- [ ] Error events and circuit breaker triggers logged
- [ ] Token usage captured per LLM call (input/output tokens)
- [ ] Total cost calculated per investigation with breakdown: agent reasoning, analyze, reflection, evaluate
- [ ] Cost metrics exportable (CSV/JSON), trace search/filter functional (by issue_id, verdict, cost, duration)
- [ ] 3 sample investigations traced and validated: complete flow visible, costs accurate, traces enable debugging

**Developer:** Developer A, Week 2 (1 day)

**Success Metric:** 100% of investigations have complete traces, token costs accurate within 1%, traces self-explanatory

---

### TASK-006: Implement Circuit Breakers and Safety Limits

**Description:**
Implement multiple safety mechanisms to prevent the agent from entering infinite loops, consuming excessive costs, or getting stuck on repeated failures. These circuit breakers act as guardrails that force the investigation to terminate gracefully when predefined limits are exceeded, ensuring the system remains reliable and cost-effective even when the agent makes poor decisions or encounters edge cases.

**Why This Matters:**
Without safety limits, an agent could theoretically loop forever trying the same failed approach, consume unlimited tokens searching irrelevant code, or retry broken tool calls indefinitely. Circuit breakers detect these pathological behaviors and force termination with a best-effort verdict based on available context. This prevents runaway costs and ensures every investigation completes within predictable time/cost bounds.

**Three Safety Mechanisms:**

1. **Max Tool Calls Limit (15 calls)** - Hard upper bound on investigation depth
   - Prevents investigation from running indefinitely
   - Forces verdict after 15 tool calls even if inconclusive
   - Typical investigation uses 5-8 calls; 15 is generous safety margin

2. **Circuit Breaker for Loops (Duplicate Detection)** - Detects when agent repeats same action
   - Tracks last 2 tool calls (tool name + arguments)
   - Triggers if agent calls identical tool+args twice in a row
   - Prevents: `fetch_code("sanitize") → fails → fetch_code("sanitize") → fails → ...`
   - Forces: `evaluate_analysis` with available context when detected

3. **Timeout (5 minutes per investigation)** - Wall-clock time limit
   - Current LLM calls take ~50 seconds each
   - Prevents slow tools (network calls, large searches) from hanging
   - Graceful shutdown (not crash) - saves state before terminating
   - Forces `evaluate_analysis` with partial context on timeout
   - **MUST BE MEASURED:** Track if legitimate complex investigations timeout before hitting 15 tool calls

**Acceptance Criteria:**

**1. Max Tool Calls Limit**
- [ ] `state.iteration_count` incremented after each tool call
- [ ] Investigation automatically stops when `iteration_count >= 15`
- [ ] On limit reached: route to `evaluate_analysis` for best-effort verdict
- [ ] Limit breach logged as WARNING in Langfuse with investigation state
- [ ] Test case: Investigation with 15+ potential calls stops at 15

**2. Circuit Breaker for Duplicate Calls**
- [ ] Track last 2 tool calls in state: `[(tool_name, args), (tool_name, args)]`
- [ ] Compare current tool call to previous call
- [ ] Trigger circuit breaker if: `current_call == previous_call` (same tool + same args)
- [ ] On trigger: route to `evaluate_analysis` immediately
- [ ] Circuit breaker event logged in Langfuse with: duplicate call details, investigation state
- [ ] Test case: Agent calls `fetch_code("foo")` twice → circuit breaker stops investigation

**3. Timeout Implementation**
- [ ] Configure **5-minute (300 second) timeout** in LangGraph invocation: `graph.invoke(state, config={"timeout": 300})`
- [ ] Timeout accounts for: ~50s per LLM call × up to 6 typical calls = ~300s
- [ ] Timeout triggers graceful shutdown (uses LangGraph timeout handling, not crash)
- [ ] On timeout: force call to `evaluate_analysis` with available context
- [ ] Timeout event logged in Langfuse with: elapsed time, investigation state snapshot, tool calls completed
- [ ] Test case: Mock slow investigation (>305 seconds) → timeout triggers, investigation terminates gracefully
- [ ] **CRITICAL MEASUREMENT:** Track timeout events in Phase 1/2 testing to identify if complex investigations (8-12 tool calls) timeout before hitting 15-call limit
- [ ] **Tuning criteria:** If >5% of legitimate investigations timeout without hitting 15 calls, increase timeout to 600s (10 minutes)

**4. Integration with Graph Routing**
- [ ] All three limits checked in `route_after_evaluation()` conditional function
- [ ] Priority order: Circuit breaker (immediate) → Timeout (LangGraph-level) → Max calls (check per iteration)
- [ ] When any limit triggered: `is_final` forced to TRUE, investigation terminates
- [ ] Graph routing logic updated:
```python
def route_after_evaluation(state: SASTAgentState) -> str:
    # Circuit breaker: duplicate calls
    if is_duplicate_call(state):
        logger.warning("Circuit breaker: Duplicate tool call detected")
        return END

    # Max iterations
    if state['iteration_count'] >= 15:
        logger.warning("Circuit breaker: Max iterations reached")
        return END

    # Error recovery limit
    if state['error_recovery_attempts'] >= 3:
        logger.warning("Circuit breaker: Max error recovery attempts")
        return END

    # Normal termination
    if state['is_final']:
        return END
    else:
        return "agent"  # Continue investigation
```

**5. Observability & Monitoring**
- [ ] All circuit breaker triggers logged in Langfuse as WARNING events
- [ ] Each log includes: trigger reason, iteration_count, tool_calls_completed, elapsed_time, investigation state, timestamp
- [ ] Langfuse trace shows clear marker when investigation stopped by safety limit
- [ ] Dashboard queries available:
  - "% investigations stopped by timeout" (measure if timeout too tight)
  - "% investigations stopped by max calls" (measure if limit too low)
  - "% investigations stopped by circuit breaker" (measure if agent logic broken)
- [ ] **Alert condition:** If >10% of investigations timeout, review timeout value

**6. Unit Testing**
- [ ] Test: Investigation with 16 tool calls → stops at 15, produces verdict
- [ ] Test: Agent calls `fetch_code("x")` twice consecutively → circuit breaker triggers after 2nd call
- [ ] Test: Mock slow investigation (305 seconds) → timeout at 300s, graceful termination
- [ ] Test: All three limits log correctly in Langfuse traces
- [ ] Test: After limit triggered, `state.is_final == TRUE` and verdict exists (may be NEEDS_HUMAN_REVIEW)
- [ ] Test: Timeout before 15 calls (e.g., 10 calls in 310 seconds) → timeout takes precedence, investigation terminates

**Developer:** Joint (Developer A + Developer B), Week 1 (0.5 days each)

**Dependencies:**
- TASK-001.3 (graph routing logic must be in place)
- TASK-004 (evaluate_analysis tool must exist for forced termination)

**Success Metrics:**
- Zero infinite loops in testing
- All test cases trigger safety limits correctly
- <5% of investigations stopped by limits in production (indicates limits are safety nets, not frequently hit)
- **Timeout measurement:** <3% of investigations timeout without hitting 15-call limit (if >3%, timeout needs increase)

**Implementation Notes:**
- Circuit breakers should be **last resort** - agent should naturally terminate via `is_final=TRUE`
- Limits indicate agent design issues if triggered frequently
- **Timeout is most uncertain parameter** - 5 minutes assumes ~6 LLM calls averaging 50s each
- If Phase 1 testing shows complex investigations (10-12 calls) timing out, increase to 10 minutes
- Max tool calls (15) provides predictable upper bound: 15 × 50s = 12.5 minutes absolute maximum

---

### TASK-007: Phase 1 Integration Testing

**Description:**
End-to-end validation of Phase 1 agent (core framework + fetch/analyze/comprehensive_evaluation tools + circuit breakers) on full Golden Dataset (23 issues). Tests infrastructure correctness and quality vs current system. Phase 1 quality gate: agent must perform as well or better than baseline before proceeding to Phase 2.

**Why This Matters:**
Despite lacking fallback tools, Phase 1 may outperform baseline due to adaptive depth (15 calls vs fixed 3), comprehensive evaluation with project_context awareness, and error recovery. Expected results: similar or better than current system. If worse, indicates infrastructure bug requiring fixes before Phase 2.

**Test Scope:**
Full Golden Dataset (23 issues): SQL injection, XSS, path traversal, command injection, buffer overflow, etc. Includes baseline cases + gap cases to measure adaptive depth impact.

**Acceptance Criteria:**
- [ ] Golden Dataset (23 issues) loaded with ground truth + baseline verdicts documented
- [ ] Test harness runs all 23 cases programmatically without crashes
- [ ] Agent produces verdicts for all 23 cases (TRUE_POSITIVE/FALSE_POSITIVE/NEEDS_HUMAN_REVIEW)
- [ ] Quality metrics calculated: Accuracy, Recall, Precision (agent vs baseline on 23 cases)
- [ ] **QUALITY GATE:** Agent metrics ≥ baseline (no degradation)
- [ ] All 23 investigations have complete Langfuse traces (tool calls, reasoning, comprehensive_evaluation, costs visible)
- [ ] No infinite loops, timeout rate <10%, circuit breaker trigger rate <10%
- [ ] Average iteration count 5-8 tool calls per investigation
- [ ] Cost tracked per case: average, distribution, breakdown by component (expect 1.5-2x baseline)
- [ ] Test report delivered: verdict comparison table (23 rows), quality metrics, cost analysis, GO/NO-GO decision

**Developer:** Joint (Developer A + Developer B), Week 2 (1 day)

**Success Metric:** Agent metrics ≥ baseline on 23 cases, all traces complete, no infrastructure bugs

---

## PHASE 2: Fallback Retrieval Tools (Week 3-4)

### TASK-008: Implement search_codebase Tool

**Description:**
Create pattern-based code search tool using ripgrep as primary fallback when fetch_code fails. Wraps ripgrep subprocess call with scope validation, result limiting (100 matches), and agent-friendly formatting. Critical for solving retrieval gap problem by finding sanitizers/validators not in trace.

**Why This Matters:**
When agent needs sanitizer but doesn't know exact name, search_codebase('sanitize.*', 'app/middleware/') finds it via pattern matching. Without this fallback, investigations fail with incomplete context. Proper scope validation and result limiting prevent context bloat and excessive costs.

**Acceptance Criteria:**
- [ ] Tool function defined with @tool decorator, accepts pattern (regex) and scope (directory path)
- [ ] Executes ripgrep via subprocess: `rg pattern scope --json --max-count 100 --context 3 --timeout 10s`
- [ ] Validates scope parameter provided (not empty), returns error if missing
- [ ] Parses ripgrep JSON output into structured results: file, line number, match text, context lines
- [ ] Limits results to 100 matches max, logs warning when truncated
- [ ] Returns results formatted concisely: "file:line - match" with 3 lines context before/after
- [ ] Handles regex errors gracefully, returns helpful error message (not crash)
- [ ] Returns empty list with message if no matches found
- [ ] Enhanced docstring: "Use when exact symbol name unknown. Flexible BUT expensive. ALWAYS specify scope using project_context."
- [ ] Docstring includes examples: search_codebase('sanitize.*', 'app/middleware/')
- [ ] Tool registered in agent's available tools
- [ ] Unit tests pass: successful search, no matches, invalid scope, regex error, result limiting, timeout handling

**Developer:** Developer B, Week 3 (1 day)

---

### TASK-009: Implement list_files Tool

**Description:**
Create list_files tool that lists directory contents for project structure discovery. Used when agent needs to explore what files exist. Includes result limiting and formatting.

**Acceptance Criteria:**
- [ ] Tool function defined with @tool decorator
- [ ] Tool accepts: directory, max_depth (default: 2)
- [ ] Function lists directory contents recursively
- [ ] Respects max_depth parameter (default: 2 levels)
- [ ] Returns list of file and directory paths
- [ ] Handles invalid directory paths gracefully
- [ ] Results limited to max 200 items
- [ ] Prioritize relevant directories (src/, config/, app/)
- [ ] Log when results truncated
- [ ] Results formatted concisely for agent context
- [ ] Tool registered in agent's available tools
- [ ] Unit tests pass (valid directory, invalid directory, depth limiting)

---

### TASK-010: Implement Agent Fallback Strategy Logic

**Description:**
Update agent system prompt to implement intelligent fallback retrieval strategy (fetch_code → search_codebase → list_files) and validate that agent actually follows this strategy in practice. Ensures agent prefers fast exact retrieval first, falls back to expensive pattern search when needed, and uses project context to optimize search scope. Critical for solving retrieval gap problem without overusing expensive search operations.

**Why This Matters:**
Without explicit fallback guidance, agent may skip straight to expensive search_codebase or use it incorrectly without scope constraints. Proper fallback strategy ensures: (1) cost efficiency by preferring fetch_code, (2) retrieval success by falling back to search when exact lookup fails, (3) search precision by using project_context to narrow scope.

**Acceptance Criteria:**
- [ ] Agent prompt explains tool cost hierarchy: fetch_code (cheap, fast) < list_files (moderate) < search_codebase (expensive)
- [ ] Prompt instructs: "Try fetch_code first for exact file paths or known symbol names"
- [ ] Prompt instructs: "Use search_codebase when exact name unknown - ALWAYS specify scope using project_context"
- [ ] Prompt instructs: "Use list_files for discovery when unclear where to look"
- [ ] Prompt includes example fallback scenario: "fetch_code('sanitize') fails → search_codebase('sanitize.*', 'app/middleware/')"
- [ ] Prompt includes project_context usage example: "If project_context shows 'app/validators/', search there for validators"
- [ ] Test case executed: exact symbol unknown → agent tries fetch_code first, then search_codebase with scope
- [ ] Test case executed: search finds candidate → agent fetches discovered file with fetch_code
- [ ] Test case executed: unclear where to look → agent calls list_files first to discover structure
- [ ] Test case executed: agent references project_context.security_files when choosing search_codebase scope
- [ ] Fallback chain observable in Langfuse traces (fetch → search → discovers context)
- [ ] Agent doesn't skip straight to search when fetch would work (verified in traces)
- [ ] Agent demonstrates using project_context in ≥2 test cases (scope optimization visible)
- [ ] Search usage rate ≤50% of investigations (validates fetch_code preference)

**Developer:** Developer A, Week 3 (0.5 days)

**Success Metric:** Agent follows fallback strategy in 100% of test cases, uses project_context to optimize search scope, search usage ≤50%

---

### TASK-011: Test on Retrieval Gap Cases

**Description:**
Validate that Phase 2 fallback tools (search_codebase, list_files) solve the retrieval gap problem by testing on 5 known gap cases where current system fails to find missing sanitizers/validators and incorrectly marks safe code as vulnerable (FALSE_POSITIVE → TRUE_POSITIVE bias). This is the core validation that the agent-based approach improves precision by discovering context the baseline system misses.

**Why This Matters:**
Gap cases represent the primary failure mode of the current system: missing sanitizers in trace → incomplete context → defaults to TRUE_POSITIVE verdict → false alarm. If Phase 2 fallback tools don't solve these cases, the entire agent approach fails to address the root problem. Success here proves the retrieval gap hypothesis and justifies the implementation effort.

**Acceptance Criteria:**

**1. Gap Case Selection and Documentation**
- [ ] 5 gap cases selected from Golden Dataset where baseline fails
- [ ] Each case documented with: issue description, baseline verdict (incorrect TRUE_POSITIVE), ground truth (should be FALSE_POSITIVE), root cause (missing sanitizer/validator not in trace)
- [ ] Expected agent behavior documented per case: "Fallback search should find sanitizer in app/middleware/security.py"
- [ ] Gap cases represent diverse scenarios: global middleware, validators in separate modules, framework-level sanitization, config-based protections

**2. Agent Execution on Gap Cases**
- [ ] All 5 gap cases executed with Phase 2 agent (full tool set including search_codebase, list_files)
- [ ] Agent produces verdicts for all 5 cases
- [ ] Fallback tool usage tracked per case: Did agent use search_codebase? Did it use list_files?
- [ ] Missing context discovery tracked: Did search find the missing sanitizer/validator?
- [ ] Verdicts compared to baseline: Did agent correct the verdict from TRUE_POSITIVE to FALSE_POSITIVE?
- [ ] All 5 investigations have complete Langfuse traces with fallback chain visible

**3. Quality Improvement Measurement**
- [ ] Accuracy calculated on gap cases: agent vs baseline (5 cases)
- [ ] Recall calculated on gap cases: agent vs baseline
- [ ] Precision calculated on gap cases: agent vs baseline (PRIMARY METRIC - should improve)
- [ ] Per-case results documented: baseline verdict, agent verdict, ground truth, match?
- [ ] **SUCCESS CRITERIA:** At least 2 incorrect TRUE_POSITIVE verdicts corrected to FALSE_POSITIVE by agent (≥40% gap case success rate)
- [ ] **QUALITY GATE:** Precision improvement measurable (agent precision > baseline precision on gap cases)

**4. Fallback Strategy Validation**
- [ ] Fallback chain observable in Langfuse traces: fetch_code fails → search_codebase with scope → finds sanitizer → corrects verdict
- [ ] Project context usage validated: Agent uses project_context to choose search scope (e.g., searches 'app/middleware/' for sanitizers)
- [ ] Search effectiveness measured: When search_codebase used, what % finds missing context?
- [ ] Results summary: "Agent used fallback on X/5 cases, found missing context in Y/X cases, corrected Z/5 verdicts"

**Developer:** Joint (Developer A + Developer B), Week 3 (1 day)

**Dependencies:** TASK-008 (search_codebase), TASK-009 (list_files), TASK-010 (fallback strategy)

**Success Metrics:**
- **PRIMARY:** ≥2 gap case verdicts corrected (40% success rate minimum)
- **SECONDARY:** Precision improves on gap cases vs baseline
- **VALIDATION:** Fallback tools actually used and effective (not just present)

---

### TASK-012: Implement Promptfoo Variant Testing

**Description:**
Setup Promptfoo framework to test agent prompt variants and measure verdict stability across different prompt phrasings. Validates that agent verdicts depend on code evidence (data flow, sanitization), not on prompt wording choices. Ensures robustness of agent reasoning by testing 3 prompt variants (Concise, Step-by-step, Technical) on 5 test cases and measuring agreement rate.

**Why This Matters:**
Prompt engineering is fragile - small wording changes can cause LLM behavior shifts. If verdicts flip based on prompt phrasing rather than evidence, the agent is unreliable for production. Variant testing ensures verdicts are evidence-driven and stable, giving confidence that prompt refinements won't break existing functionality.

**Acceptance Criteria:**

**1. Promptfoo Setup**
- [ ] Promptfoo installed: `npm install -g promptfoo`
- [ ] Promptfoo config file created: `promptfooconfig.yaml` with test cases
- [ ] Agent integration configured: Promptfoo can invoke agent via API/CLI
- [ ] Sample test runs successfully (validates setup works)
- [ ] Test harness documented: how to run variant tests

**2. Prompt Variant Creation**
- [ ] Variant 1 created: "Concise" (minimal instructions, terse language)
- [ ] Variant 2 created: "Step-by-step" (detailed guidance, structured instructions)
- [ ] Variant 3 created: "Technical" (code-focused language, technical terminology)
- [ ] All variants functionally equivalent: same tools, same goals, same core logic
- [ ] Variants differ only in phrasing/style, not instructions
- [ ] Variants documented with rationale for each style

**3. Verdict Agreement Testing**
- [ ] 5 diverse test cases selected: SQL injection, XSS, path traversal, command injection, buffer overflow
- [ ] All 5 cases run against all 3 variants (15 investigations total)
- [ ] Verdicts collected: TRUE_POSITIVE/FALSE_POSITIVE/NEEDS_HUMAN_REVIEW per variant per case
- [ ] Verdict agreement calculated: (cases where all 3 variants agree) / (total cases)
- [ ] **SUCCESS CRITERIA:** Agreement ≥90% (≥4.5 out of 5 cases have unanimous verdict)
- [ ] Disagreements analyzed: Why did variants disagree? Evidence ambiguity or prompt sensitivity?
- [ ] Results documented with verdict matrix (5 cases × 3 variants)

**4. Stability Validation**
- [ ] If agreement <90%: investigate disagreements, refine prompts to reduce sensitivity
- [ ] If agreement ≥90%: document stable prompt design principles
- [ ] Automated test suite created: can re-run variant tests after prompt changes
- [ ] Variant tests integrated into CI/CD (runs on prompt modifications)
- [ ] Results show verdicts stable across phrasings (evidence-driven, not phrasing-driven)

**Developer:** Developer A, Week 4 (0.5 days)

**Dependencies:** TASK-001 (agent framework), TASK-010 (finalized agent prompt)

**Success Metrics:**
- **PRIMARY:** Verdict agreement ≥90% across variants (stable verdicts)
- **SECONDARY:** Disagreements explainable by evidence ambiguity (not prompt bugs)
- **DELIVERABLE:** Automated variant test suite for continuous validation

---

### TASK-013: Cost Analysis and Optimization

**Description:**
Analyze token costs from Phase 1 and Phase 2 testing, calculate cost multiplier vs baseline system, and validate that quality improvements justify cost increases. If costs excessive relative to quality gains, identify cost drivers and implement optimizations. Ensures agent system is operationally sustainable and ROI-positive before production deployment.

**Why This Matters:**
Agent system will cost MORE than baseline due to adaptive depth (more iterations), reflection overhead, and search result context. Cost increase is acceptable ONLY if quality improves enough to justify it. Without cost analysis, we risk deploying an expensive system that doesn't deliver sufficient value. This task ensures cost/quality trade-off is favorable and sustainable.

**Acceptance Criteria:**

**1. Cost Metric Calculation**
- [ ] Token usage per investigation exported from Langfuse for all Phase 1 and Phase 2 test cases
- [ ] Average cost per issue calculated for agent system (tokens × cost per token)
- [ ] Baseline cost per issue calculated for current system (same test cases)
- [ ] Cost multiplier calculated: agent_cost / baseline_cost (e.g., "2.3x baseline")
- [ ] Cost distribution analyzed: min, max, median, standard deviation (identify outliers)
- [ ] Cost breakdown by component: agent reasoning, fetch_code, analyze_issue, reflection, evaluate_analysis, search_codebase

**2. Cost vs Quality Trade-off Validation**
- [ ] Quality improvement documented: Accuracy/Recall/Precision gains (agent vs baseline)
- [ ] Cost increase documented: cost multiplier + absolute cost difference
- [ ] ROI calculation: quality_gain / cost_multiplier (is improvement worth cost?)
- [ ] Trade-off evaluation matrix:
  - Quality improved + cost acceptable → PROCEED
  - Quality improved + cost excessive → OPTIMIZE
  - Quality not improved → REVISE (agent approach may be flawed)
- [ ] Recommendation documented with justification

**3. Cost Driver Identification**
- [ ] Identify most expensive component: Which tool/operation consumes most tokens?
- [ ] Identify expensive cases: Which investigations cost >2x average? Why?
- [ ] Common cost patterns documented: "Search_codebase adds 10k tokens average", "Reflection adds 2k tokens"
- [ ] Optimization opportunities identified: reduce search result limit? prune reflection? cache results?

**4. Optimization (If Needed)**
- [ ] IF cost_multiplier >3x AND quality_gain <20%: Implement optimizations
- [ ] Optimization strategies evaluated:
  - Reduce search_codebase result limit (100 → 50)
  - Reduce reflection frequency (every investigation → only if confidence <0.7)
  - Prune context before analyze_issue (drop low-relevance files)
  - Cache fetch_code results (avoid re-fetching same files)
- [ ] Selected optimizations implemented
- [ ] Re-test after optimization (10 test cases)
- [ ] Cost reduction measured: new_cost_multiplier < old_cost_multiplier
- [ ] Quality impact measured: Accuracy/Recall/Precision not degraded by >5%
- [ ] Final recommendation: optimized system ready for production OR needs further iteration

**Developer:** Joint (Developer A + Developer B), Week 4 (0.5 days)

**Dependencies:** TASK-007 (Phase 1 testing), TASK-014 (Phase 2 testing), TASK-005 (Langfuse cost tracking)

**Success Metrics:**
- **PRIMARY:** Quality improvement justifies cost increase (documented ROI positive)
- **ACCEPTABLE:** Cost multiplier <3x baseline for >20% quality improvement
- **OPTIMIZATION TARGET:** If cost >3x, reduce to <2.5x without degrading quality

---

### TASK-014: Phase 2 Integration Testing and Validation

**Description:**
Comprehensive end-to-end testing of complete Phase 2 agent system (all 5 tools: fetch/search/list/analyze/evaluate) on expanded test set combining gap cases and baseline cases. Validates that Phase 2 fallback tools improve quality vs Phase 1, measures fallback effectiveness, and determines if system meets Phase 2 quality gate for progression to production readiness testing.

**Why This Matters:**
Phase 2 quality gate determines if agent approach successfully solves the retrieval gap problem and justifies moving to production. This is the final validation before large-scale testing (TASK-015). If Phase 2 doesn't show quality improvement over Phase 1 and baseline, the fallback tools failed and we need to iterate on search strategy, prompts, or tool design.

**Acceptance Criteria:**

**1. Test Execution**
- [ ] Agent tested on 10 issues: 5 gap cases (from TASK-011) + 5 baseline cases (from Phase 1)
- [ ] All 10 cases complete successfully (no crashes, no infinite loops)
- [ ] Agent produces verdicts for all 10 cases
- [ ] All 10 investigations have complete Langfuse traces
- [ ] Fallback tool usage tracked per case: search_codebase used? list_files used? When and why?
- [ ] No regressions on 5 baseline cases (verdicts match or improve vs Phase 1)

**2. Quality Metrics Calculation**
- [ ] Accuracy calculated on 10 cases: agent vs baseline
- [ ] Recall calculated on 10 cases: agent vs baseline
- [ ] Precision calculated on 10 cases: agent vs baseline (PRIMARY METRIC for gap cases)
- [ ] Consistency measured: run each of 10 cases 3 times, calculate verdict agreement % (target: ≥90%)
- [ ] Quality improvement documented: Phase 2 agent vs baseline (Accuracy/Recall/Precision deltas)
- [ ] Quality improvement documented: Phase 2 agent vs Phase 1 agent (did fallback tools help?)

**3. Fallback Effectiveness Validation**
- [ ] Fallback success rate calculated: (cases where fallback found context) / (cases where fallback used)
- [ ] **SUCCESS CRITERIA:** Fallback effectiveness ≥60% (when search_codebase used, finds useful context ≥60% of time)
- [ ] Fallback usage rate documented: % of 10 cases that used search_codebase or list_files
- [ ] Fallback cost documented: average token cost when fallback used vs not used
- [ ] Fallback value analysis: Did fallback findings change any verdicts? Document examples

**4. Phase 2 Quality Gate Decision**
- [ ] Quality gate criteria evaluated:
  - ✅ Quality improved vs baseline (at least one metric: Accuracy/Recall/Precision)
  - ✅ Quality improved vs Phase 1 (fallback tools add value)
  - ✅ Fallback effectiveness ≥60% (search/list tools work when used)
  - ✅ No regressions on baseline cases
  - ✅ Consistency ≥90% (verdicts stable across runs)
  - ✅ Cost justified by quality improvement (from TASK-013)
- [ ] IF quality gate MET: Approve progression to TASK-015 (scale testing on 23 issues)
- [ ] IF quality gate FAILED: Document gaps, root cause analysis, iteration plan
- [ ] Production readiness assessment:
  - Are we on track for production targets (≥0.7 Accuracy, ≥0.9 Recall, ≥0.74 Precision)?
  - If not: gap to target documented, next steps identified

**5. Documentation**
- [ ] Phase 2 test report delivered:
  - Test results (10 cases: baseline verdict, Phase 1 verdict, Phase 2 verdict, ground truth)
  - Quality metrics comparison (baseline vs Phase 1 vs Phase 2)
  - Fallback effectiveness analysis
  - Consistency results (3-run agreement)
  - Cost analysis summary (from TASK-013)
  - Known issues and limitations
  - Phase 2 quality gate decision (GO/NO-GO)
  - Production readiness status (on track / needs work)

**Developer:** Joint (Developer A + Developer B), Week 4 (1 day)

**Dependencies:** TASK-008 through TASK-013 (all Phase 2 tasks complete)

**Success Metrics:**
- **PRIMARY:** Phase 2 quality > Phase 1 quality (fallback tools add value)
- **SECONDARY:** Fallback effectiveness ≥60%, consistency ≥90%, cost justified
- **GATE:** Quality improved vs baseline, no regressions, ready for scale testing

---

## Production Readiness Tasks (Post Phase 2)

### TASK-015: Scale Testing on Full Golden Dataset

**Description:**
Run agent on full Golden Dataset (23 issues) to validate metrics at scale and ensure consistent performance across all issue types.

**Acceptance Criteria:**
- [ ] Agent tested on all 23 Golden Dataset issues
- [ ] Final quality metrics calculated (Accuracy, Recall, Precision)
- [ ] Metrics meet production targets (≥0.7, ≥0.9, ≥0.74)
- [ ] Consistency ≥90% across 3 runs
- [ ] No critical failures or tool errors
- [ ] Cost per issue within acceptable range
- [ ] Results documented and reviewed

---

### TASK-016: Production Deployment Preparation

**Description:**
Prepare agent system for production deployment including configuration management, error handling, monitoring, and deployment documentation.

**Acceptance Criteria:**
- [ ] Production configuration externalized (no hardcoded values)
- [ ] Error handling robust (graceful degradation)
- [ ] Monitoring dashboards created (Langfuse)
- [ ] Alerting configured (cost spikes, error rates)
- [ ] Deployment runbook documented
- [ ] Rollback plan documented
- [ ] Production readiness review completed and approved

---

### TASK-018: Update Documentation and Architecture Diagrams

**Description:**
Update project documentation and create architecture diagrams reflecting agent-based system (replacing k-iteration loop docs). Includes README, architecture/sequence diagrams, tool docs, configuration guide, troubleshooting guide, and operations runbook. Critical for onboarding, maintenance, and operational support.

**Acceptance Criteria:**
- [ ] Update README.md with agent architecture overview (5 tools, reflection, circuit breakers)
- [ ] Create Mermaid diagrams: agent workflow (init → agent → tools → reflection → evaluate → END), investigation flow sequence (fetch fails → search succeeds)
- [ ] Update ADR-0001 to "Implemented" with commit links
- [ ] Document 5 tools (purpose, when to use, trade-offs, examples), circuit breakers (15 calls, 5 min timeout, duplicate detection, 3 error retries), state schema (SASTAgentState, ToolError, ProjectContext)
- [ ] Create guides: Langfuse dashboard (read traces, analyze costs), configuration (env vars, API keys), troubleshooting (debug from traces), operations runbook (monitoring, alerts, metrics)
- [ ] All diagrams version-controlled (Mermaid/PlantUML, not binary images), documentation peer-reviewed

**Developer:** Joint (Developer A + Developer B), Post Phase 2 (0.5 days)

---

## Summary

**Total Tasks:** 17 (7 in Phase 1, 7 in Phase 2, 3 Production)

**Phase 1 Tasks:** 001, 002, 003, 004, 005, 006, 007
**Phase 2 Tasks:** 008, 009, 010, 011, 012, 013, 014
**Production Tasks:** 015, 016, 018
**Total Subtasks:** 22 (reduced from 51 by consolidating workflows and merging reflection into comprehensive_evaluation)
**Timeline:** 4 weeks (original timeline maintained)
- Phase 1: Week 1-2 (Tasks 001-007)
- Phase 2: Week 3-4 (Tasks 008-014)
- Production: Post Phase 2 (Tasks 015-016, 018)

**Architecture Changes from Original Plan:**
- ~~TASK-017: Separate Reflection Node~~ **REMOVED**
- **TASK-004: comprehensive_evaluation** - Merges reflection + evaluation into single tool
  - Rationale: Both are advisory nodes providing input to agent decision
  - Benefit: 40% cost savings (single LLM call vs two), unified feedback, no contradictions
  - Implementation: Single prompt with Process Audit + Logic Audit sections

**Must-Have Features:**
- TASK-001.5: Error Recovery Routing (3 subtasks) - State-driven retry with alternative tools
- TASK-004: Comprehensive Evaluation - Merged reflection + evaluation with project_context awareness
- Enhanced tool docstrings (TASK-002, TASK-008) - Explain trade-offs, when to use, fallback strategies
- Enhanced state schema with error tracking and evaluation feedback

**Nice-to-Have Tasks (Can Defer):**
- TASK-003.3-3.6: Context Pruning (4 subtasks) - NICE-TO-HAVE
  - Implement if time permits, defer to Phase 3 if timeline tight
  - Prevents context window bloat for investigations with 10+ files

**Key Milestones:**
- Week 2: Phase 1 Quality Gate (baseline parity + comprehensive_evaluation + error handling)
- Week 4: Phase 2 Quality Gate (fallback validation + quality improvement)
- Post Week 4: Production Readiness Assessment

**Timeline Impact:**
- Original: 4 weeks
- With architectural improvements: 4 weeks (maintained)
- Simplified architecture (merged evaluation) reduces implementation time while improving quality
