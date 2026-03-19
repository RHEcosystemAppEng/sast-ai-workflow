# Operations Runbook

This runbook covers day-to-day operational tasks for the SAST AI Workflow: monitoring a run, interpreting results, tuning performance, and managing alerts.

---

## Starting a Run

### NAT Framework (recommended)

```bash
source .venv/bin/activate
nat run --config_file src/sast_agent_workflow/configs/config.yml --input "sast_analysis"
```

### Container

```bash
podman run -d --name sast-ai-app \
  -e PROJECT_NAME=systemd \
  -e PROJECT_VERSION=257-9 \
  -e LLM_URL=<llm-endpoint> \
  -e LLM_MODEL_NAME=<model> \
  -e LLM_API_KEY=<key> \
  -e LLM_API_TYPE=nim \
  -e EMBEDDINGS_LLM_URL=<embeddings-endpoint> \
  -e EMBEDDINGS_API_KEY=<key> \
  -e EMBEDDINGS_LLM_MODEL_NAME=all-mpnet-base-v2 \
  -e INPUT_REPORT_FILE_PATH=<report-path-or-sheet-url> \
  -e KNOWN_FALSE_POSITIVE_FILE_PATH=<ignore.err> \
  -e OUTPUT_FILE_PATH=<output-path-or-sheet-url> \
  -e LANGFUSE_PUBLIC_KEY=<key> \
  -e LANGFUSE_SECRET_KEY=<key> \
  quay.io/ecosystem-appeng/sast-ai-workflow:latest
```

See [setup.md](setup.md) for the full environment variable reference.

---

## Monitoring a Live Run

### Log output

Set `LOG_LEVEL=INFO` (default) for operational output. Key log lines to watch:

| Log pattern | Meaning |
|-------------|---------|
| `Building SAST workflow graph...` | Startup successful |
| `[<issue_id>] Starting investigation` | Per-issue investigation begins |
| `[<issue_id>] Langfuse tracing enabled` | Tracing active for this issue |
| `[<issue_id>] Circuit breaker triggered` | Safety limit hit (check `stop_reason`) |
| `[<issue_id>] Investigation complete` | Issue resolved |
| `Writing results to <path>` | Final output step |

For maximum detail: `LOG_LEVEL=DEBUG`.

### Langfuse dashboard

See [Langfuse Guide](langfuse_guide.md) for full instructions. During a live run:

1. Open Langfuse → **Sessions** and filter by `test_run:<TEST_RUN_ID>`.
2. Watch traces appear as each issue is investigated.
3. Click any in-progress trace to see live tool calls in the `research_node` span.

---

## Interpreting Results

### Output Excel file

The `write_results` node writes an Excel file with one row per SAST issue. Key columns:

| Column | Values | Meaning |
|--------|--------|---------|
| `verdict` | `TRUE_POSITIVE` / `FALSE_POSITIVE` / `NEEDS_REVIEW` | AI verdict |
| `confidence` | `HIGH` / `MEDIUM` / `LOW` | Model confidence in the verdict |
| `justifications` | Text list | Key findings supporting the verdict |
| `is_final` | `TRUE` / `FALSE` | Whether the verdict is considered final |
| `stop_reason` | String or blank | Why investigation ended early (blank = normal) |

**Operational thresholds to watch:**
- More than 20% `NEEDS_REVIEW` in a batch → investigate `stop_reason` distribution in Langfuse.
- Many `stop_reason = "research_recursion_limit_hit"` → agent is hitting model call limits; consider increasing `MAX_MODEL_CALLS`.
- Many `stop_reason = "evaluation_rejection_streak"` → evaluator and analyzer are misaligned; review prompts.

### Metrics output

The `calculate_metrics` node computes precision and recall (if `HUMAN_VERIFIED_FILE_PATH` is set) and stores them in the `metrics` field of `SASTWorkflowTracker`. These are also written to the output Excel.

---

## Performance Tuning

### Key constants (in `constants.py`)

| Constant | Default | Effect of increasing | Effect of decreasing |
|----------|---------|----------------------|----------------------|
| `MAX_MODEL_CALLS` | 15 | More thorough research, higher cost | Faster, cheaper, may miss evidence |
| `MAX_REJECTION_STREAK` | 3 | More chances to improve analysis | Faster termination on stalled issues |
| `MAX_NO_PROGRESS_STREAK` | 2 | More attempts when tools fail | Faster termination on unreachable code |
| `INVESTIGATION_SUBGRAPH_RECURSION_LIMIT` | 100 | Longer loops possible | Earlier hard stop |

### Key env vars

| Variable | Default | Tuning notes |
|----------|---------|--------------|
| `MAX_ANALYSIS_ITERATIONS` | `4` | Outer loop per issue; increase for more thorough analysis at higher cost |
| `SIMILARITY_ERROR_THRESHOLD` | `2` | Documents returned from vector DB; increase for more context in filter |
| `CALCULATE_RAGAS_METRICS` | `false` | Enable only when evaluating model quality; adds LLM cost |

---

## Alerts and Health Checks

### Pre-run health check

Before starting a production run, verify all dependencies:

```bash
# Check LLM endpoint
curl -s -o /dev/null -w "LLM: %{http_code}\n" $LLM_URL/models

# Check embeddings endpoint
curl -s -o /dev/null -w "Embeddings: %{http_code}\n" $EMBEDDINGS_LLM_URL/models

# Check repo is accessible
ls $REPO_LOCAL_PATH | head -5

# Check input report exists
ls -la $INPUT_REPORT_FILE_PATH 2>/dev/null || echo "WARNING: input report not found locally (may be a URL)"
```

### Signs a run needs intervention

| Signal | Action |
|--------|--------|
| No log output after 60s | Check process is running; try `LOG_LEVEL=DEBUG` |
| Investigation stuck on one issue > 10 min | Check Langfuse trace for the issue; may need to kill and rerun with `MAX_MODEL_CALLS` reduced |
| LLM `429 Too Many Requests` | Rate limit hit; reduce parallelism or wait |
| Output file not written at end | Check `write_results` logs; verify `OUTPUT_FILE_PATH` is writable |

---

## Scheduled / CI Runs (Tekton)

For runs triggered via the orchestrator API or Tekton pipeline:

- See [Triggering Workflows Guide](triggering_workflows.md) for API parameters and endpoints.
- See [deploy/README.md](../deploy/README.md) for OpenShift pipeline operations.
- See [deploy/tekton/eventlistener/README.md](../deploy/tekton/eventlistener/README.md) for EventListener webhook setup.

### DVC metadata tracking (MLOps runs)

When triggered via the `/api/v1/mlops-batch` endpoint, set:

| Variable | Description |
|----------|-------------|
| `DVC_GIT_COMMIT_HASH` | Git commit hash for data versioning |
| `DVC_REPO_BRANCH` | Branch name for reproducibility |
| `DVC_REPO_URL` | Source repository URL for lineage |

These are automatically set by the Tekton pipeline.

---

## Rotating Credentials

| Credential | Variable | Steps |
|------------|----------|-------|
| LLM API key | `LLM_API_KEY` | Update `.env` or OpenShift secret, restart workflow |
| Embeddings API key | `EMBEDDINGS_API_KEY` | Update `.env` or OpenShift secret, restart workflow |
| Langfuse keys | `LANGFUSE_PUBLIC_KEY`, `LANGFUSE_SECRET_KEY` | Rotate in Langfuse dashboard → update env vars → restart |
| Google service account | `SERVICE_ACCOUNT_JSON_PATH` | Generate new key in GCP console → update file path → restart |
