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
│  │  ORCHESTRATOR  (nodes/orchestrator.py)                        │  │
│  │  Loops over non-final issues, invokes subgraph per issue      │  │
│  └───────────────────────────┬───────────────────────────────────┘  │
│                              │                                      │
│       ┌──────────────────────┼───────────────────────┐              │
│       │  INVESTIGATION       │ SUBGRAPH              │              │
│       │  (nodes/subgraph.py) ▼                       │              │
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
| **Orchestrator** | `nodes/orchestrator.py` | Entry point -- loops over non-final issues, invokes the subgraph, writes verdicts back |
| **Subgraph** | `nodes/subgraph.py` | Builds the Research → Analysis → Evaluation StateGraph loop |
| **Tools** | `tools/` | Code-gathering tools the ReAct agent calls (fetch_code, read_file, search_codebase) |
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
│          ┌─────────────┼─────────────┐                       │
│          ▼             ▼             ▼                       │
│    fetch_code     read_file   search_codebase                │
│                        │                                     │
│                        ▼                                     │
│             ResearchAgentState                               │
│          (fetched_files, tool_call_history, messages)        │
└──────────────────────────────────────────────────────────────┘
```

### Key Behaviors

- **Conversation-free prompts**: Each model call rebuilds the system prompt from scratch (middleware #3), embedding a structured summary of accumulated state (CODE BANK files, tool-call history with success/fail, evaluation feedback on iteration 2+) rather than replaying raw message history.
- **Graceful degradation**: If the recursion limit is hit, the `InMemorySaver` checkpointer preserves accumulated code so the analysis node still has evidence.

## Key Constants

| Constant | Default | Description |
|----------|---------|-------------|
| `INVESTIGATION_SUBGRAPH_RECURSION_LIMIT` | 100 | Max LangGraph recursion steps for the subgraph |
| `RESEARCH_AGENT_RECURSION_LIMIT` | 80 | Max steps for the ReAct research agent per iteration |
| `MAX_REJECTION_STREAK` | 3 | Stop after N consecutive evaluation rejections |
| `MAX_NO_PROGRESS_STREAK` | 2 | Stop after N iterations without new code gathered |
| `MAX_MODEL_CALLS` | 15 | Max LLM calls per research iteration |
