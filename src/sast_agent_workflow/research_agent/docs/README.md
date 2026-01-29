# Research Agent POC - Manual Setup Guide

This guide explains how to manually run the SAST Research Agent workflow, including local Langfuse setup for observability.

## Table of Contents

- [Prerequisites](#prerequisites)
- [Langfuse Setup (Local)](#langfuse-setup-local)
- [Environment Configuration](#environment-configuration)
- [Workflow Configuration](#workflow-configuration)
- [Running the Workflow](#running-the-workflow)
- [Tracing Options](#tracing-options)
- [Troubleshooting](#troubleshooting)

---

## Prerequisites

- **Python 3.11+** (tested with 3.13)
- **Docker & Docker Compose** (for local Langfuse)
- **Git** (for repository analysis)
- **libclang** (for C/C++ code parsing)
  - macOS: `brew install llvm`
  - Linux: `apt install libclang-dev`

---

## Langfuse Setup (Local)

Langfuse provides observability and tracing for LLM workflows. The project includes a pre-configured Docker Compose setup for Langfuse v3.

### 1. Start Langfuse Services

Navigate to the langfuse deployment directory and start the services:

```bash
cd src/sast_agent_workflow/research_agent/langfuse_deployment
docker-compose -f docker-compose.langfuse-v3.yml up -d
```

This starts the following services:
- **langfuse-web**: Web UI at http://localhost:3000
- **langfuse-worker**: Background worker
- **postgres**: Database
- **clickhouse**: Analytics database
- **redis**: Cache
- **minio**: S3-compatible storage

### 2. Create Langfuse Account & Project

1. Open http://localhost:3000 in your browser
2. Create an account (first time only)
3. Create a new project (e.g., "sast-workflow")
4. Navigate to **Settings → API Keys**
5. Click **"Create new API key"**
6. Copy the **Public Key** (`pk-lf-...`) and **Secret Key** (`sk-lf-...`)

### 3. Configure Langfuse Credentials

Create a local environment file from the template:

```bash
cd src/sast_agent_workflow/research_agent/langfuse_deployment
cp .env.langfuse.local.template .env.langfuse.local
```

Edit `.env.langfuse.local` with your API keys:

```env
LANGFUSE_PUBLIC_KEY=pk-lf-your-public-key-here
LANGFUSE_SECRET_KEY=sk-lf-your-secret-key-here
LANGFUSE_HOST=http://localhost:3000
```

### 4. Stop Langfuse (When Done)

```bash
docker-compose -f docker-compose.langfuse-v3.yml down
```

To completely remove data volumes:

```bash
docker-compose -f docker-compose.langfuse-v3.yml down -v
```

---

## Environment Configuration

The workflow requires several environment variables for LLM access and optional tracing.

### Required Environment Variables

Create a `.env` file in the project root or export these variables:

```bash
# LLM Configuration (Required)
export LLM_API_KEY="your-llm-api-key"
export LLM_MODEL_NAME="your-model-name"              # e.g., gpt-4, llama-3.1-70b
export EMBEDDINGS_LLM_API_KEY="your-embeddings-key"
export EMBEDDINGS_LLM_MODEL_NAME="your-embedding-model"

# Langfuse Tracing (Optional)
export LANGFUSE_PUBLIC_KEY="pk-lf-..."
export LANGFUSE_SECRET_KEY="sk-lf-..."
export LANGFUSE_HOST="http://localhost:3000"

# Per-issue tracing (Optional - creates separate traces per issue)
export LANGFUSE_TRACE_PER_ISSUE="false"  # Set to "true" for individual traces
```

### Environment Variables Reference

| Variable | Required | Description |
|----------|----------|-------------|
| `LLM_API_KEY` | Yes | API key for the main LLM |
| `LLM_MODEL_NAME` | Yes | Model name (e.g., `gpt-4`, `llama-3.1-70b`) |
| `EMBEDDINGS_LLM_API_KEY` | Yes | API key for embeddings model |
| `EMBEDDINGS_LLM_MODEL_NAME` | Yes | Embedding model name |
| `LANGFUSE_PUBLIC_KEY` | No | Langfuse public key for tracing |
| `LANGFUSE_SECRET_KEY` | No | Langfuse secret key for tracing |
| `LANGFUSE_HOST` | No | Langfuse server URL (default: `http://localhost:3000`) |
| `LANGFUSE_TRACE_PER_ISSUE` | No | Enable per-issue traces (`true`/`false`) |

---

## Workflow Configuration

The workflow configuration is defined in `config/default_config.yaml`. Key settings include:

### Project Settings

```yaml
PROJECT_NAME: my-project
PROJECT_VERSION: 1.0.0
TEST_RUN_ID: test_0  # Auto-generated at runtime
```

### LLM Settings

```yaml
LLM_URL: http://your-llm-endpoint
LLM_API_TYPE: nim  # or "openai"
LLM_MODEL_NAME: llm-model
EMBEDDINGS_LLM_URL: http://your-embeddings-endpoint
EMBEDDINGS_LLM_MODEL_NAME: embedding-model
```

### Input/Output

```yaml
INPUT_REPORT_FILE_PATH: /path/to/sast-report.xlsx  # or Google Sheets URL
OUTPUT_FILE_PATH: /path/to/output.xlsx
REPO_LOCAL_PATH: /path/to/analyzed/repository
```

### Investigation Settings

```yaml
investigation:
  max_iterations: 5              # Max investigation loops per issue
  max_rejection_streak: 3        # Max consecutive rejections before abort
  max_no_progress_streak: 2      # Max iterations without progress
  research_recursion_limit: 30   # Max recursion for research agent
  structured_output_max_retries: 3
```

### Google Sheets Integration

If using Google Sheets for input reports:

```yaml
INPUT_REPORT_FILE_PATH: https://docs.google.com/spreadsheets/d/...
SERVICE_ACCOUNT_JSON_PATH: /path/to/service_account.json
```

---

## Running the Workflow

### 1. Install Dependencies

Create and activate a virtual environment:

```bash
cd src/sast_agent_workflow/research_agent
python -m venv .venv
source .venv/bin/activate  # Linux/macOS
# or: .venv\Scripts\activate  # Windows

pip install -r requirements.txt
```

### 2. Set Environment Variables

Load environment variables (choose one method):

```bash
# Option A: Source from file
source .env

# Option B: Source Langfuse-specific vars
source langfuse_deployment/.env.langfuse.local

# Option C: Export directly
export LLM_API_KEY="..."
export LLM_MODEL_NAME="..."
# ... etc
```

### 3. Run the Workflow

```bash
python scripts/run_workflow.py
```

### Expected Output

```
================================================================================
SAST Workflow - Direct Python Execution (No NAT)
================================================================================
Loading configuration...
Generated unique TEST_RUN_ID: test_1737398400
Added timestamp prefix to output file: /path/to/20250120_143200_output.xlsx
Input report: /path/to/input.xlsx
Repository: /path/to/repo
Output file: /path/to/output.xlsx
LLM: nim/llama-3.1-70b

Building workflow graph...
✓ Langfuse workflow tracing enabled (single trace for all issues)

Initializing workflow state...
✓ Tracing session: my-project_test_1737398400

Executing workflow...
--------------------------------------------------------------------------------
... (workflow execution logs)
--------------------------------------------------------------------------------

Workflow execution complete!

Results:
  Total issues: 42
  Investigated: 38
  Output file: /path/to/output.xlsx

================================================================================
SAST Workflow Complete
================================================================================
```

---

## Tracing Options

The workflow supports two tracing modes:

### 1. Workflow-Level Tracing (Default)

A single trace captures the entire workflow execution:

```bash
export LANGFUSE_TRACE_PER_ISSUE="false"  # or don't set
python scripts/run_workflow.py
```

**Pros**: Single trace view, easier to see overall workflow  
**Cons**: Large traces for many issues

### 2. Per-Issue Tracing

Separate traces for each issue investigation:

```bash
export LANGFUSE_TRACE_PER_ISSUE="true"
python scripts/run_workflow.py
```

**Pros**: Isolated traces, easier to debug individual issues  
**Cons**: Many traces to navigate

### Viewing Traces

1. Open Langfuse UI at http://localhost:3000
2. Navigate to your project
3. Click **Traces** in the sidebar
4. Filter by:
   - **Session ID**: `{project_name}_{test_run_id}`
   - **Tags**: `project:my-project`, `model:llm-model`

---

## Troubleshooting

### Langfuse Issues

**Traces not appearing:**
- Verify Langfuse services are running: `docker-compose ps`
- Check API keys are correctly set
- Ensure `LANGFUSE_HOST` matches the running instance

**"Failed to upload JSON to S3" errors:**
- The minio bucket may not be initialized
- Restart services: `docker-compose down && docker-compose up -d`

### Workflow Issues

**Configuration errors:**
- Ensure all required environment variables are set
- Verify paths in `config/default_config.yaml` exist
- Check Google Sheets service account has access

**LLM connection errors:**
- Verify `LLM_URL` is accessible
- Check API key is valid
- Ensure model name is correct for your endpoint

### Port Conflicts

If ports are in use:

| Service | Default Port | Alternative |
|---------|-------------|-------------|
| Langfuse Web | 3000 | Edit docker-compose.yml |
| Postgres | 5432 | Edit docker-compose.yml |
| Redis | 6379 | Edit docker-compose.yml |
| MinIO | 9090 | Edit docker-compose.yml |

---

## Directory Structure

```
research_agent/
├── core/                    # Core workflow components
│   ├── llm_factory.py       # LLM client factory
│   ├── tools.py             # Agent tools
│   └── workflow.py          # LangGraph workflow definition
├── docs/                    # Documentation
│   └── README.md            # This file
├── langfuse_deployment/     # Langfuse setup
│   ├── docker-compose.langfuse-v3.yml
│   ├── .env.langfuse.local.template
│   └── export_*.py          # Trace export utilities
├── nodes/                   # Workflow nodes
│   ├── investigation/       # Investigation subgraph
│   ├── filter.py
│   ├── pre_process.py
│   └── ...
├── observability/           # Tracing utilities
├── scripts/
│   └── run_workflow.py      # Main entry point
└── requirements.txt         # Python dependencies
```
