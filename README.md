# SAST-AI-Workflow 
[![SonarQube Cloud](https://sonarcloud.io/images/project_badges/sonarcloud-highlight.svg)](https://sonarcloud.io/summary/new_code?id=RHEcosystemAppEng_sast-ai-workflow)
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=RHEcosystemAppEng_sast-ai-workflow&metric=alert_status)](https://sonarcloud.io/summary/new_code?id=RHEcosystemAppEng_sast-ai-workflow)
[![Code Smells](https://sonarcloud.io/api/project_badges/measure?project=RHEcosystemAppEng_sast-ai-workflow&metric=code_smells)](https://sonarcloud.io/summary/new_code?id=RHEcosystemAppEng_sast-ai-workflow)
[![Maintainability Rating](https://sonarcloud.io/api/project_badges/measure?project=RHEcosystemAppEng_sast-ai-workflow&metric=sqale_rating)](https://sonarcloud.io/summary/new_code?id=RHEcosystemAppEng_sast-ai-workflow)
[![Vulnerabilities](https://sonarcloud.io/api/project_badges/measure?project=RHEcosystemAppEng_sast-ai-workflow&metric=vulnerabilities)](https://sonarcloud.io/summary/new_code?id=RHEcosystemAppEng_sast-ai-workflow)

[![main branch Image](https://github.com/RHEcosystemAppEng/sast-ai-workflow/actions/workflows/build-dev-image.yml/badge.svg)](https://github.com/RHEcosystemAppEng/sast-ai-workflow/actions/workflows/build-dev-image.yml) [![Build Release Image](https://github.com/RHEcosystemAppEng/sast-ai-workflow/actions/workflows/build-release-image.yml/badge.svg)](https://github.com/RHEcosystemAppEng/sast-ai-workflow/actions/workflows/build-release-image.yml) [![Quay.io](https://img.shields.io/badge/Quay.io-sast--ai--workflow-blue)](https://quay.io/repository/ecosystem-appeng/sast-ai-workflow)


SAST-AI-Workflow is a LLM-based tool designed to detect and flag suspected vulnerabilities through 
SAST(Static Application Security Testing). It inspects suspicious lines of code in a given repository and 
deeply review the legitimacy of errors. This workflow involves existing SAST reports, source code analysis, CWE data 
and other known examples. 

### Purpose
The SAST-AI-Workflow can be integrated into the vulnerability detection process as an AI-assisted tool. It offers 
enhanced insights that may be overlooked during manual verification, while also reducing the time required by engineers.

As an initial step, we applied the workflow to the SAST scanning of the RHEL **systemd** project 
(source: [systemd GitHub](https://github.com/redhat-plumbers/systemd-rhel10)). We intend to extend this approach to support additional 
C-based projects in the future.

## Architecture

The workflow uses a **LangGraph-based agent architecture** (NAT framework). A linear pipeline of nodes processes each SAST report, with the `investigate` node running an autonomous multi-stage research loop per issue.

```
Input → pre_process → filter → investigate → summarize_justifications → calculate_metrics → write_results → Output
```

See [Architecture Details](./docs/architecture.md) and the [diagrams](./docs/diagrams/) (Mermaid + Excalidraw) for the full picture.

### Agent Pipeline Nodes

| Node | Role |
|------|------|
| `pre_process` | Initialize workflow, load config, set up vector store |
| `filter` | Filter known false positives using embedding similarity |
| `investigate` | Autonomously investigate each SAST issue (see below) |
| `summarize_justifications` | Generate human-readable analysis summaries |
| `calculate_metrics` | Compute precision/recall and performance metrics |
| `write_results` | Export final verdicts to configured outputs |

### The `investigate` Node: Research → Analysis → Evaluation

Each SAST issue is handled by a subgraph that loops until the evaluator approves a verdict or a safety limit is reached:

```
┌─ RESEARCH (ReAct agent) ─────────────────────────────────┐
│  Calls tools to gather code evidence:                     │
│    fetch_code · read_file · search_codebase               │
│    list_directory · file_search                           │
└──────────────────────────────────────────────────────────┘
         │
         ▼
┌─ ANALYSIS (LLM) ─────────────────────────────────────────┐
│  Produces structured verdict + confidence score           │
└──────────────────────────────────────────────────────────┘
         │
         ▼
┌─ EVALUATION (reflection) ────────────────────────────────┐
│  Critiques the analysis:                                  │
│    • approved       → END                                 │
│    • needs more code → loop back to RESEARCH              │
│    • reanalyze      → loop back to ANALYSIS               │
│    • safety limit   → CIRCUIT BREAKER → END               │
└──────────────────────────────────────────────────────────┘
```

### Research Tools

| Tool | Purpose |
|------|---------|
| `fetch_code` | Retrieve a specific file/function from the target repository |
| `read_file` | Read a file by path from the source tree |
| `search_codebase` | Semantic search across the codebase for relevant code patterns |
| `list_directory` | List directory contents to explore unfamiliar code structure |
| `file_search` | Search for files by name pattern (conditionally included) |

### Circuit Breakers

The investigation subgraph has multiple safety limits to prevent runaway LLM usage:

| Limit | Value | Behavior |
|-------|-------|----------|
| Max LLM calls per research iteration | 15 | Graceful exit; accumulated code is preserved |
| Max consecutive evaluation rejections | 3 | Forces `NEEDS_REVIEW` verdict |
| Max iterations without new code | 2 | Forces `NEEDS_REVIEW` verdict |
| Evaluation error (after retries) | — | Forces `NEEDS_REVIEW` + `stop_reason = "evaluation_error"` |

For full details see the [Investigation Package README](./src/sast_agent_workflow/nodes/sub_agents/investigation/README.md).

### Input Sources
- **SAST HTML Reports:** Processes scan results from SAST HTML reports.
- **Source Code:** Requires access to the exact source code used to generate the SAST HTML report.
- **Verified Data:** Incorporates known error cases for continuous learning and better results.
- **CWE Information:** Embeds CWE (Common Weakness Enumeration) data to enrich vulnerability analysis context.

### Embeddings & Vector Store
- Converts input data (verified data, source code) into embeddings using a specialized sentence transformer HuggingFace model ([all-mpnet-base-v2](https://huggingface.co/sentence-transformers/all-mpnet-base-v2)) and stores them in an in-memory vector store (FAISS).

### LLM Integration
- Supports OpenAI-compatible AI models.
- Supports NVIDIA NIMs via the `ChatNVIDIA` LangChain integration.

### Evaluation
- Applies metrics (from Ragas library) to assess the quality of model outputs.
- **Note:** SAST-AI-Workflow is primarily focused on identifying false alarms (False Positives).

  
## 🔌 Installation & Setup
Please refer to [how to run](./docs/setup.md) guideline.

## 📊 Observability
- [Langfuse Guide](./docs/langfuse_guide.md) — reading traces, analyzing costs, debugging from trace data

## 📋 Operations
- [Operations Runbook](./docs/operations_runbook.md) — monitoring, alerts, performance tuning, credential rotation

## 🚀 CI/CD Pipeline
We provide a Tekton pipeline and helper scripts for deploying the SAST‑AI‑Workflow on an OpenShift cluster. 
Please refer to [how to execute](./deploy/README.md) the pipeline.

For triggering workflows via the orchestrator API, see the [Triggering Workflows Guide](./docs/triggering_workflows.md).


