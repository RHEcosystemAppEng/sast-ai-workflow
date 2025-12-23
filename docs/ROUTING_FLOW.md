# Agent Graph Routing Flow

This document explains how routing works in the SAST Investigation Agent graph, including node transitions, termination conditions, and the role of `is_final`.

## Overview

The agent uses a **LangGraph** state machine with conditional routing. The graph has:
- **1 decision node**: `agent` (decides which tool to call)
- **3 tool nodes**: `fetch_code`, `analyze_issue`, `comprehensive_evaluation`
- **2 routing functions**: `route_agent_decision`, `route_after_evaluation`
- **Termination state**: `END`

## Graph Structure

```
START
  ↓
agent (decision node)
  ↓ (route_agent_decision)
  ├─→ fetch_code → agent (loop)
  ├─→ analyze_issue → comprehensive_evaluation
  └─→ comprehensive_evaluation
       ↓ (route_after_evaluation)
       ├─→ agent (continue investigation)
       └─→ END (investigation complete)
```

## Routing Functions

### 1. `route_agent_decision(state)`

**When Called:** AFTER agent node executes (routes FROM agent TO tools)

**Purpose:** Examine agent's response to determine which tool to invoke

**Logic:**
```python
if no messages yet:
    return "agent"  # First call, initialize

last_message = messages[-1]  # Agent's AIMessage

if last_message has tool_calls:
    return tool_name  # Route to the tool agent chose
else:
    return "agent"  # LLM error - retry
```

**Key Point:** This function examines the **agent's output** (AIMessage with tool_calls), NOT tool results.

**Why is_final is NOT checked here:**
- `is_final` is only set by `comprehensive_evaluation` tool
- After `comprehensive_evaluation`, `route_after_evaluation` checks `is_final`
- If `is_final=TRUE`, routes to END (never returns to agent)
- Therefore, if we're IN agent node, `is_final` must be FALSE
- Checking `is_final` here would be **dead code**

---

### 2. `route_after_evaluation(state, max_iterations, max_error_recovery)`

**When Called:** AFTER comprehensive_evaluation tool executes (routes FROM comprehensive_evaluation)

**Purpose:** Decide whether investigation is complete or should continue

**Logic:**
```python
# Circuit breaker 1: Max iterations
if iteration_count >= max_iterations:
    is_final = True
    return END

# Circuit breaker 2: Duplicate tool calls
if is_duplicate_call(state):
    is_final = True
    return END

# Circuit breaker 3: Error recovery limit
if error_recovery_attempts >= max_error_recovery:
    is_final = True
    return END

# Normal termination: evaluation decided investigation is complete
if is_final:
    return END

# Continue investigation
return "agent"
```

**Key Point:** This is the **ONLY place** where `is_final` determines routing.

---

## Message Flow

Understanding message types is critical for routing:

### Message Types in state.memory.messages

1. **HumanMessage**: User prompt to agent ("What should I do next?")
2. **AIMessage**: Agent's response with `tool_calls` attribute
3. **ToolMessage**: Tool execution result

### Typical Message Sequence

```
Iteration 1:
  HumanMessage("What should I do next?")
  AIMessage(tool_calls=[{name: "fetch_code", args: {...}}])
  ToolMessage(content="=== app/views.py ===\n...")

  HumanMessage("What should I do next?")
  AIMessage(tool_calls=[{name: "analyze_issue", args: {...}}])
  ToolMessage(content='{"investigation_result": "TRUE_POSITIVE", ...}')

  # analyze_issue routes to comprehensive_evaluation (no agent decision)
  ToolMessage(content='{"is_final": "FALSE", "exploration_gaps": [...]}')

  # route_after_evaluation sees is_final=FALSE, routes to agent
  HumanMessage("What should I do next?")
  AIMessage(tool_calls=[{name: "fetch_code", args: {...}}])
  ...

Final Iteration:
  ToolMessage(content='{"is_final": "TRUE", ...}')
  # route_after_evaluation sees is_final=TRUE, routes to END
  # Agent never called again
```

---

## Complete Flow Examples

### Example 1: Simple Investigation (3 iterations)

```
1. START → agent
   Agent decides: fetch_code("app/views.py")
   route_agent_decision → fetch_code

2. fetch_code → agent
   Agent decides: analyze_issue()
   route_agent_decision → analyze_issue

3. analyze_issue → comprehensive_evaluation (automatic)
   Evaluation: is_final=FALSE, exploration_gaps=[...]
   route_after_evaluation → agent

4. agent
   Agent decides: fetch_code("app/middleware/security.py")
   route_agent_decision → fetch_code

5. fetch_code → agent
   Agent decides: analyze_issue()
   route_agent_decision → analyze_issue

6. analyze_issue → comprehensive_evaluation
   Evaluation: is_final=TRUE
   route_after_evaluation → END ✅
```

### Example 2: Circuit Breaker (Max Iterations)

```
1-14. (14 iterations of fetch_code → agent → ...)

15. fetch_code → agent
    Agent decides: analyze_issue()
    route_agent_decision → analyze_issue

16. analyze_issue → comprehensive_evaluation
    Evaluation: is_final=FALSE (still has gaps)
    BUT: iteration_count=15 >= max_iterations
    route_after_evaluation → END ⚠️ (circuit breaker)
```

---

## Termination Conditions

Investigation terminates when ANY of these conditions is met:

### 1. **Normal Termination** (is_final=TRUE)
- `comprehensive_evaluation` sets `state.is_final = TRUE`
- `route_after_evaluation` checks `is_final` → returns END
- **Log:** "Investigation complete (is_final=TRUE)"

### 2. **Circuit Breaker: Max Iterations**
- `state.iteration_count >= max_iterations` (default: 15)
- `route_after_evaluation` sets `is_final = TRUE` → returns END
- **Log:** "Circuit breaker: Max iterations (15) reached"

### 3. **Circuit Breaker: Duplicate Calls**
- Last 2 tool calls are identical (same tool + same args)
- `route_after_evaluation` sets `is_final = TRUE` → returns END
- **Log:** "Circuit breaker: Duplicate tool call detected"

### 4. **Circuit Breaker: Error Recovery**
- `error_recovery_attempts >= max_error_recovery` (default: 3)
- `route_after_evaluation` sets `is_final = TRUE` → returns END
- **Log:** "Circuit breaker: Max error recovery attempts (3)"

---

## Special Routing Rules

### analyze_issue → comprehensive_evaluation (Automatic)

```python
if tool_name == "analyze_issue":
    graph.add_edge("analyze_issue", "comprehensive_evaluation")
```

**Why:** Every analysis must be evaluated for completeness. This enforces:
1. Agent cannot analyze without evaluation
2. Evaluation always happens after analysis
3. No agent decision needed between analyze and evaluate

### fetch_code → agent (Loop)

```python
else:  # Other tools (fetch_code, etc.)
    graph.add_edge(tool_name, "agent")
```

**Why:** After fetching code, agent must decide next action:
- Fetch more code?
- Ready to analyze?
- Continue investigation?

---

## Key Invariants

### 1. is_final is ONLY set by comprehensive_evaluation
- Never set by agent node
- Never set by other tools
- Only set by `ComprehensiveEvaluationStateUpdater` or circuit breakers

### 2. route_after_evaluation is the ONLY place is_final determines routing
- Agent node doesn't check `is_final`
- Tool nodes don't check `is_final`
- Only `route_after_evaluation` uses it for routing decisions

### 3. Agent always makes tool calls
- Agent has tools bound via `llm.bind_tools(tools)`
- LLM is trained to use tools
- If agent responds without tool call → LLM error (should not happen)

### 4. Messages are append-only
- Never delete messages during investigation
- History maintains full conversation context
- Agent sees last 10 messages in context (line 192 of agent_node.py)

---

## Debugging Routing Issues

### Check Logs

Routing decisions are logged:
```
[issue-001] Routing to tool: fetch_code
[issue-001] Tool fetch_code succeeded
[issue-001] Routing to tool: analyze_issue
[issue-001] Investigation complete (is_final=TRUE)
```

### Common Issues

**Problem:** Agent keeps calling same tool repeatedly
- **Cause:** Duplicate detection not triggering
- **Check:** `state.memory.tool_call_history` length and contents

**Problem:** Investigation never terminates
- **Cause:** `is_final` never set to TRUE
- **Check:** `comprehensive_evaluation` output and state updater

**Problem:** Agent makes decision when it shouldn't
- **Cause:** `route_after_evaluation` routing to agent instead of END
- **Check:** `state.is_final` value after comprehensive_evaluation

**Problem:** "Agent failed to make tool call" error
- **Cause:** LLM responded without tool_calls
- **Check:** Agent prompt, tool definitions, LLM configuration

---

## State Mutation Points

Understanding when state changes:

| **Location** | **What Changes** | **When** |
|-------------|------------------|----------|
| `agent_node` | `memory.messages` (adds HumanMessage, AIMessage) | Every agent decision |
| `tool_wrapper_node` | `iteration_count++`, `memory.messages` (adds ToolMessage) | Every tool call |
| `FetchCodeStateUpdater` | `context.fetched_files`, `context.found_symbols` | After fetch_code |
| `AnalyzeIssueStateUpdater` | `analysis.investigation_result` | After analyze_issue |
| `ComprehensiveEvaluationStateUpdater` | `analysis.evaluation_result`, **`is_final`**, `analysis.verdict` | After comprehensive_evaluation |
| `route_after_evaluation` | **`is_final`** (circuit breakers only) | When circuit breaker triggers |

---

## Related Files

- **Routing logic:** `src/sast_agent_workflow/agent/agent_graph.py`
- **Agent decision:** `src/sast_agent_workflow/agent/agent_node.py`
- **State updaters:** `src/sast_agent_workflow/agent/tools/state_updaters.py`
- **State definition:** `src/sast_agent_workflow/agent/agent_state.py`
- **Agent prompt:** `src/templates/prompts/agent_decision_prompt.yaml`
