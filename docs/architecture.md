# SAST AI Workflow Architecture

## Overview

The SAST AI Workflow supports two execution approaches:

1. **AIQ Framework** - Modern NeMo-based agent architecture with LangGraph workflow management
2. **Traditional Python** - Direct execution approach

## AIQ Framework Architecture

### Workflow Type: `sast_agent`

The AIQ-based workflow is implemented as a custom `sast_agent` type, registered in `src/sast_agent_workflow/register.py`. This approach provides:

- **Modular Design**: Each analysis step is implemented as a separate tool
- **LangGraph Integration**: Workflow orchestration using LangGraph
- **Enhanced Configuration**: YAML-based configuration with environment variable overrides

### Core Components

#### Tools Package (`src/sast_agent_workflow/tools/`)
- `pre_process`: Initializes the SAST workflow
- `filter`: Filters known false positives and retrieves similar issues  
- `data_fetcher`: Fetches required source code for analysis
- `judge_llm_analysis`: Performs LLM-based analysis of SAST issues
- `evaluate_analysis`: Evaluates analysis results and provides next step recommendations
- `summarize_justifications`: Summarizes analysis justifications
- `calculate_metrics`: Calculates performance metrics for SAST analysis
- `write_results`: Writes final analysis results to specified outputs

#### Configuration
- **Main Config**: `src/sast_agent_workflow/configs/config.yml`
- **Package Config**: `pyproject.toml` - Defines AIQ component registration
- **Environment Variables**: Detailed in [setup.md](setup.md)

#### LLM and Embedders
- **Main LLM**: Configurable LLM for primary analysis
- **Embedding Model**: Extended OpenAI-compatible embedder for similarity matching

### Workflow Execution Flow

The AIQ framework uses LangGraph with conditional logic for iterative analysis:

```
Input → pre_process → filter → data_fetcher → judge_llm_analysis → evaluate_analysis
                                    ↑                                     ↓
                                    └─────── (conditional loop) ──────────┘
                                                     ↓
                              summarize_justifications → calculate_metrics → write_results → Output
```

**Conditional Edge Logic** (`graph_builder.py:should_continue_analysis`):
- Loops back to `data_fetcher` if issues need re-analysis and under `MAX_ANALYSIS_ITERATIONS`
- Proceeds to `summarize_justifications` when analysis is complete or max iterations reached
- If `data_fetcher` fails to retrieve new data despite instructions, the issue is marked as final to skip further analysis iterations

## Traditional Python Architecture

The traditional approach uses direct Python execution via `run.py`, maintaining the original workflow structure with sequential processing.

## Key Differences

| Aspect | AIQ Framework | Traditional Python |
|--------|---------------|-------------------|
| **Execution** | `aiq run --config_file ...` | `python run.py` |
| **Architecture** | Agent-based with LangGraph | Sequential processing |
| **Configuration** | YAML + Environment variables | Environment variables only |
| **Modularity** | High (tool-based) | Moderate |
| **Testing** | `pytest tests/aiq_tests/` | Standard pytest |

