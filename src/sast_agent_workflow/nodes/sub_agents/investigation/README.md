# Investigation Package

Autonomous multi-stage SAST vulnerability investigation using a LangGraph subgraph with iterative research, analysis, and evaluation.

## Overview

The investigation package implements a **research → analysis → evaluation** loop that runs per-issue. A ReAct agent gathers code evidence, an LLM produces a verdict, and a critique step decides whether the analysis is sufficient or more research is needed.

## Architecture

### Component Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                     Parent LangGraph Workflow                       │
│                      (SASTWorkflowTracker)                          │
└──────────────────────────────┬──────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────────┐
│  investigation/                                                     │
│                                                                     │
│  ┌───────────────────────────────────────────────────────────────┐  │
│  │  ORCHESTRATOR  (orchestrator.py)                               │  │
│  │  Loops over non-final issues, invokes subgraph per issue      │  │
│  └───────────────────────────┬───────────────────────────────────┘  │
│                              │                                      │
│       ┌──────────────────────┼───────────────────────┐              │
│       │  INVESTIGATION       │ SUBGRAPH              │              │
│       │  (subgraph.py)       ▼                       │              │
│       │                                              │              │
│       │  ┌─────────────┐  ┌──────────────┐  ┌──────┐ │              │
│       │  │  RESEARCH   │─▶│   ANALYSIS   │─▶│ EVAL │ │              │
│       │  │  (ReAct     │  │  (LLM verdict│  │      │ │              │
│       │  │   agent)    │  │   decision)  │  │      │ │              │
│       │  └─────────────┘  └──────────────┘  └──┬───┘ │              │
│       │        ▲               ▲               │     │              │
│       │        │               │               │     │              │
│       │        └────────┐  ┌───┘               │     │              │
│       │           ┌─────┴──┴──┐                │     │              │
│       │           │ INCREMENT │◀── loop back ──┘     │              │
│       │           └───────────┘                │     │              │
│       │                             ┌──────────┘     │              │
│       │                             ▼ safety limit   │              │
│       │                     ┌─────────────────┐      │              │
│       │                     │ CIRCUIT BREAKER │──▶END│              │
│       │                     └─────────────────┘      │              │
│       └──────────────────────────────────────────────┘              │
└─────────────────────────────────────────────────────────────────────┘
```

| Component | Directory | Role |
|-----------|-----------|------|
| **Orchestrator** | `orchestrator.py` | Entry point -- loops over non-final issues, invokes the subgraph, writes verdicts back |
| **Subgraph** | `subgraph.py` | Builds the Research → Analysis → Evaluation StateGraph loop |
| **Tools** | `tools/` | Code-gathering tools the ReAct agent calls (fetch_code, read_file, search_codebase, list_directory, file_search) — see [Tools & Schemas Reference](../../../../../docs/tools_and_schemas.md) |
| **Prompts** | `prompts/` | Prompt builders for each node + per-vulnerability-type YAML checklists |
| **Observability** | `observability/` | Langfuse tracing/scoring and ground-truth verdict loading |
| **Constants** | `constants.py` | All thresholds, limits, tool names -- single source of truth for tuning |

---

### Investigation Subgraph: The Core Loop

This is the LangGraph `StateGraph` compiled in `subgraph.py`. All nodes share an `InvestigationState` dict.

```
                        ┌──────────────────────┐
                        │       START          │
                        └──────────┬───────────┘
                                   │
                                   ▼
                        ┌──────────────────────┐
                   ┌───▶│      RESEARCH        │
                   │    │   (ReAct agent       │
                   │    │    gathers code)     │
                   │    └──────────┬───────────┘
                   │               │
                   │               ▼
                   │    ┌──────────────────────┐
                   │ ┌─▶│      ANALYSIS        │
                   │ │  │  (LLM structured     │
                   │ │  │   output: verdict,   │
                   │ │  │   confidence)        │
                   │ │  └──────────┬───────────┘
                   │ │             │
                   │ │             ▼
                   │ │  ┌──────────────────────┐
                   │ │  │     EVALUATION       │
                   │ │  │  (quality critique,  │
                   │ │  │   loop control)      │
                   │ │  └───┬────┬────┬────┬───┘
                   │ │      │    │    │    │
                   │ │      │    │    │    │  "approved"
                   │ │      │    │    │    └──────────────────▶ END
                   │ │      │    │    │
                   │ │      │    │    │  "safety limit"
                   │ │      │    │    └──────┐
                   │ │      │    │           ▼
                   │ │      │    │  ┌────────────────┐
                   │ │      │    │  │ CIRCUIT BREAKER│──────▶ END
                   │ │      │    │  │ (force verdict │
                   │ │      │    │  │  NEEDS_REVIEW) │
                   │ │      │    │  └────────────────┘
                   │ │      │    │
                   │ │      │    │  "reanalyze"   "needs more code"
                   │ │      │    │       │              │
                   │ │      │    │       ▼              ▼
                   │ │      │    │  ┌──────────────────────┐
                   │ │      │    └─▶│     INCREMENT        │
                   │ │      │       │   (iteration += 1)   │
                   │ │      └──────▶└───────┬────┬─────────┘
                   │ │                      │    │
                   │ │  needs_reanalysis    │    │  needs_reanalysis
                   │ │     = true           │    │     = false
                   │ └──────────────────────┘    │
                   └─────────────────────────────┘
```

## Research Agent

The research node is a **ReAct agent** built with LangChain's `create_agent` API and a middleware stack. It is **conversation-free per model call** -- instead of replaying the raw message history, the middleware rebuilds the system prompt from scratch with a structured summary of accumulated state (fetched files, tool-call history with success/fail status, evaluation feedback).

```
┌──────────────────────────────────────────────────────────────┐
│  RESEARCH NODE  (nodes/research.py)                          │
│                                                              │
│  Middleware Stack (applied per model call):                  │
│    1. ModelRetryMiddleware      retry on 500/429/timeout     │
│    2. ModelCallLimitMiddleware  max 15 calls, graceful exit  │
│    3. stateless_model_middleware                             │
│         rebuilds SystemMessages with accumulated context:    │
│           - instructions + fetched files + tool history      │
│           - full CODE BANK (all gathered code)               │
│    4. code_gathering_middleware                              │
│         intercepts tool results → Command(update state)      │
│                         │                                    │
│                         ▼                                    │
│               ┌──────────────────┐                           │
│               │  LLM (tool-call) │                           │
│               └────────┬─────────┘                           │
│                        │                                     │
│     ┌────┬────────────┬────────────┬────────────┐            │
│     ▼    ▼            ▼            ▼            ▼            │
│  fetch_code  read_file  search_codebase  list_directory      │
│                                          file_search*        │
│                        │                                     │
│                        ▼                                     │
│             ResearchAgentState                               │
│          (fetched_files, tool_call_history, messages)        │
│                                                              │
│  * list_directory and file_search conditionally included     │
└──────────────────────────────────────────────────────────────┘
```

### Key Behaviors

- **Conversation-free prompts**: Each model call rebuilds the system prompt from scratch (middleware #3), embedding a structured summary of accumulated state (CODE BANK files, tool-call history with success/fail, evaluation feedback on iteration 2+) rather than replaying raw message history.
- **Graceful degradation**: If the recursion limit is hit, the `InMemorySaver` checkpointer preserves accumulated code so the analysis node still has evidence.

### Evaluation Error Handling

If the evaluation node raises an unrecoverable exception after all retries, the investigation terminates early:

```
proposed_verdict  = "NEEDS_REVIEW"
stop_reason       = "evaluation_error"
is_complete       = True
```

This is consistent with the circuit breaker's forced-termination behavior.

## Key Constants

| Constant | Default | Description |
|----------|---------|-------------|
| `INVESTIGATION_SUBGRAPH_RECURSION_LIMIT` | 100 | Max LangGraph recursion steps for the subgraph |
| `RESEARCH_AGENT_RECURSION_LIMIT` | 80 | Max steps for the ReAct research agent per iteration |
| `MAX_REJECTION_STREAK` | 3 | Stop after N consecutive evaluation rejections |
| `MAX_NO_PROGRESS_STREAK` | 2 | Stop after N iterations without new code gathered |
| `MAX_MODEL_CALLS` | 15 | Max LLM calls per research iteration |

## Tracked State Fields (Observability)

These `InvestigationState` fields are not tuning knobs but are recorded as Langfuse scores after each issue is investigated:

| Field | Set by | Description |
|-------|--------|-------------|
| `reanalysis_count` | Evaluation node | Number of times the evaluator disagreed with the analysis verdict but had no missing evidence, triggering a reanalysis loop (no new research). Incremented when `NEEDS_MORE_RESEARCH` is returned with an empty `required_information` list. |
| `total_tool_calls` | Research node | Cumulative count of `ToolMessage` instances across all research iterations, including duplicate tool calls. Accumulated at the end of each research phase. |
| `stop_reason` | Research / Evaluation / Circuit Breaker nodes | Human-readable string describing why the investigation ended early (e.g. `"research_recursion_limit_hit"`, `"evaluation_rejection_streak"`, `"no_progress_detected"`, `"max_iterations"`, `"evaluation_error"`, `"circuit_breaker_unknown"`). `None` when the investigation ended normally via evaluator approval. |
