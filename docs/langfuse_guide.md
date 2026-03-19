# Langfuse Observability Guide

This guide covers how to read traces, analyze costs, and interpret scores in the Langfuse dashboard for the SAST AI Workflow.

---

## Local Installation

The quickest way to run Langfuse locally is with Docker Compose. This is suitable for development and testing; it is not recommended for high-availability production use.

### Requirements

- [Docker Desktop](https://www.docker.com/products/docker-desktop/) (Mac/Windows) or Docker + Docker Compose (Linux)
- Git

### Steps

**1. Clone the Langfuse repository:**
```bash
git clone https://github.com/langfuse/langfuse.git
cd langfuse
```

**2. Update secrets in `docker-compose.yml`:**

All lines that need changing are marked with `# CHANGEME`. Replace the placeholder values with long, random strings:
```yaml
# Example fields to change
NEXTAUTH_SECRET: replace-this-with-a-random-secret   # CHANGEME
SALT: replace-this-with-a-random-salt                # CHANGEME
```

**3. Start the containers:**
```bash
docker compose up
```

Wait 2–3 minutes. When `langfuse-web-1` logs `Ready`, the UI is available at **http://localhost:3000**.

**4. Create an account and project:**

- Open http://localhost:3000 and register a new account.
- Create a new **project**.
- Go to **Settings → API Keys** and create a key pair.
- Copy the **Public Key** and **Secret Key** — these go into `LANGFUSE_PUBLIC_KEY` and `LANGFUSE_SECRET_KEY`.

**5. Point the workflow at your local instance:**
```bash
LANGFUSE_PUBLIC_KEY=pk-lf-...
LANGFUSE_SECRET_KEY=sk-lf-...
LANGFUSE_HOST=http://localhost:3000
```

### Stopping and Upgrading

```bash
# Stop
docker compose down

# Upgrade to latest Langfuse version
docker compose down
docker compose up --pull always
```

> **Note:** `docker compose down -v` also removes volumes (all stored traces). Omit `-v` to keep data.

---

## Setup

### Environment Variables

Set these variables before running the workflow to enable tracing:

| Variable | Required | Description |
|----------|----------|-------------|
| `LANGFUSE_PUBLIC_KEY` | Yes (to enable) | Your Langfuse project public key |
| `LANGFUSE_SECRET_KEY` | Yes (to enable) | Your Langfuse project secret key |
| `LANGFUSE_HOST` | No | Langfuse server URL (default: cloud.langfuse.com) |
| `LANGFUSE_TRACE_PER_ISSUE` | No | `"true"` for one trace per issue; `"false"` (default) for a single trace per workflow run |

If `LANGFUSE_PUBLIC_KEY` is not set, all observability is silently skipped — the workflow runs normally without tracing.

### Trace Modes

**Single-trace mode** (default, `LANGFUSE_TRACE_PER_ISSUE=false`):
- One Langfuse trace covers the entire workflow run
- Useful for overview / cost analysis across a full batch

**Per-issue mode** (`LANGFUSE_TRACE_PER_ISSUE=true`):
- One trace per SAST issue investigated
- Trace ID is logged at `INFO` level: `[<issue_id>] trace_id: <uuid>`
- Session ID format: `<PROJECT_NAME>_<TEST_RUN_ID>_<issue_id>`
- Useful for debugging individual issue investigations

---

## Reading Traces

### Trace Structure

Each per-issue trace contains three named runs that correspond to the investigation subgraph nodes:

| Run Name | Constant | What it captures |
|----------|----------|-----------------|
| `research_node` | `LANGFUSE_RESEARCH_TRACE_NAME` | All ReAct agent steps: LLM calls, tool calls, tool results |
| `analysis_node` | `LANGFUSE_ANALYSIS_TRACE_NAME` | LLM call that produces the structured verdict |
| `evaluation_node` | `LANGFUSE_EVALUATION_TRACE_NAME` | LLM call that critiques the analysis |

### Navigation Tips

1. Open the **Traces** tab and filter by `session_id` to see all issues from a single run.
2. Click into a trace and expand the `research_node` run to see every tool call the agent made.
3. Look at the `INPUT` and `OUTPUT` fields on each span to see exactly what was sent to / returned from the LLM or tool.
4. The `Tags` sidebar lets you filter by `project:`, `test_run:`, `model:`, `issue_type:`, and `stage:investigate`.

---

## Scores

After each issue investigation, the following scores are written to the trace:

| Score Name | Type | Description |
|-----------|------|-------------|
| `tool_call_count` | Numeric | Total tool executions during the investigation |
| `reanalysis_count` | Numeric | Times evaluation triggered reanalysis without new research |
| `stop_reason` | Categorical | Why the investigation ended (see below) |
| `verdict` | Categorical | `FALSE_POSITIVE`, `TRUE_POSITIVE`, or `NEEDS_REVIEW` |
| `ground_truth_verdict` | Categorical | Human-verified verdict (if `HUMAN_VERIFIED_FILE_PATH` is set) |
| `verdict_correct` | Boolean (`0`/`1`) | Whether `verdict` matches `ground_truth_verdict` |

### `stop_reason` Values

| Value | Meaning |
|-------|---------|
| `completed` | Investigation ended normally (evaluator approved) |
| `research_recursion_limit_hit` | Research agent hit the 15-call limit |
| `evaluation_rejection_streak` | Evaluator rejected 3 consecutive times |
| `no_progress_detected` | Code bank did not grow for 2 consecutive iterations |
| `max_iterations` | Hit the configured `MAX_ANALYSIS_ITERATIONS` limit |
| `evaluation_error` | Evaluation node raised an unrecoverable exception |
| `circuit_breaker_unknown` | Circuit breaker triggered for an unexpected reason |

---

## Debugging from Traces

### Issue gives wrong verdict

1. Find the trace for the issue using the `issue:<issue_id>` tag.
2. Open the `analysis_node` span and inspect the `INPUT` — check which files are in the CODE BANK.
3. Open the `research_node` spans and look for tool failures (tool results starting with `Error:`).
4. If critical files were missing, check `tool_call_history` in the research spans — was the right file ever requested?

### Investigation terminates early

1. Check the `stop_reason` score on the trace.
2. If `evaluation_rejection_streak`: open each `evaluation_node` span and read the `required_information` field in the output to understand what the evaluator kept asking for.
3. If `research_recursion_limit_hit`: the research agent ran out of model calls. Consider increasing `MAX_MODEL_CALLS` or simplifying the research prompt.
4. If `no_progress_detected`: the agent kept failing to fetch new code. Look at tool results in the research spans for repeated `Error: File not found` messages — the path format may be wrong.

### High cost per issue

1. Sort traces by total tokens descending.
2. Open the most expensive trace and look at the number of `research_node` iterations.
3. Check if `gathered_code` is very large — this inflates every subsequent prompt via the CODE BANK.
