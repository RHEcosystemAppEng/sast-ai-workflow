# SAST AI Workflow Architecture

## Overview

The SAST AI Workflow supports two execution approaches:

1. **NAT Framework** - Modern NeMo-based agent architecture with LangGraph workflow management
2. **Traditional Python** - Direct execution approach

## NAT Framework Architecture

### Workflow Type: `sast_agent`

The NAT-based workflow is implemented as a custom `sast_agent` type, registered in `src/sast_agent_workflow/register.py`. This approach provides:

- **Modular Design**: Each analysis step is implemented as a separate node
- **LangGraph Integration**: Workflow orchestration using LangGraph
- **Enhanced Configuration**: YAML-based configuration with environment variable overrides

### Core Components

#### Nodes (`src/sast_agent_workflow/nodes/`)
- `pre_process`: Initializes the SAST workflow
- `filter`: Filters known false positives and retrieves similar issues
- `investigate`: Investigates each SAST issue using a multi-stage research → analysis → evaluation subgraph (see [Investigation Package](../src/sast_agent_workflow/nodes/sub_agents/investigation/README.md))
- `summarize_justifications`: Summarizes analysis justifications
- `calculate_metrics`: Calculates performance metrics for SAST analysis
- `write_results`: Writes final analysis results to specified outputs

#### Tools & State Schemas
- [Tools & Schemas Reference](tools_and_schemas.md) — 5 research tools (purpose, parameters, trade-offs, examples), circuit breaker constants, and full state schema documentation (`SASTWorkflowTracker`, `PerIssueData`, `InvestigationState`)

#### Configuration
- **Main Config**: `src/sast_agent_workflow/configs/config.yml`
- **Package Config**: `pyproject.toml` - Defines NAT component registration
- **Environment Variables**: Detailed in [setup.md](setup.md)

#### LLM and Embedders
- **Main LLM**: Configurable LLM for primary analysis
- **Embedding Model**: Extended OpenAI-compatible embedder for similarity matching

### Workflow Execution Flow

The NAT framework uses LangGraph with a linear pipeline. All iterative analysis logic is encapsulated inside the `investigate` node:

```
Input → pre_process → filter → investigate → summarize_justifications → calculate_metrics → write_results → Output
```

The `investigate` node internally runs a **Research → Analysis → Evaluation loop** per issue, with a circuit breaker for safety limits. See the [Investigation Package README](../src/sast_agent_workflow/nodes/sub_agents/investigation/README.md) for the full inner-loop design.

#### Guides
- [Langfuse Guide](langfuse_guide.md) — trace reading, cost analysis, debugging
- [Operations Runbook](operations_runbook.md) — monitoring, alerts, performance tuning

## Traditional Python Architecture

The traditional approach uses direct Python execution via `run.py`, maintaining the original workflow structure with sequential processing.

## Key Differences

| Aspect | NAT Framework | Traditional Python |
|--------|---------------|-------------------|
| **Execution** | `nat run --config_file ...` | `python run.py` |
| **Architecture** | Agent-based with LangGraph | Sequential processing |
| **Configuration** | YAML + Environment variables | Environment variables only |
| **Modularity** | High (node-based) | Moderate |
| **Testing** | `pytest tests/nat_tests/` | Standard pytest |
