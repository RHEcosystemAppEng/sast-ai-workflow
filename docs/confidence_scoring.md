# Confidence Scoring

> **Note:** The confidence scoring system is under active improvement. Weights and calibration are being refined as more data is collected. This document reflects the current implementation.

## Overview

Every issue analyzed by the pipeline receives a **confidence score** (0–100%) that quantifies how much trust to place in the verdict.

There are two scoring paths depending on how the issue was handled:

**Path 1 — Known false positive (short-circuit):**
The filter matched a known false positive in the vector DB. No investigation runs. The score is simply the filter's match confidence:
```
final_confidence = filter_confidence * 100
```

**Path 2 — Investigated issue (standard):**
The issue went through the full research-analysis-evaluation loop. The score combines three components:
```
final_confidence = (0.375 * agent_confidence
                  + 0.25  * evidence_strength
                  + 0.375 * investigation_depth) * 100
```

The scoring logic lives in [`src/Utils/confidence_scoring.py`](../src/Utils/confidence_scoring.py). All weights and normalization caps are configurable in [`config/default_config.yaml`](../config/default_config.yaml).

## Interpreting Scores

| Range | Level | Recommended Action |
|---|---|---|
| **80–100%** | High | Verdict can generally be trusted without further review. |
| **50–79%** | Medium | Consider spot-checking, especially for security-critical code. |
| **0–49%** | Low | Manual review recommended regardless of verdict. |

### Common Patterns

| Pattern | Meaning |
|---|---|
| High confidence + FALSE_POSITIVE | Confident false alarm — code clearly handles the flagged condition. |
| High confidence + TRUE_POSITIVE | Concrete evidence of an exploitable path without adequate mitigation. |
| Low confidence + any verdict | Investigation struggled — manual review recommended. |
| Medium confidence + "max_iterations" stop | Substantial evidence gathered but hit the iteration limit. Quick manual check advised. |

### In the Output Reports

- **Excel report:** The `Confidence` column shows the final percentage (0–100) for each issue, color-coded red (< 50%) or green (>= 50%).
- **SARIF file:** Confidence scores are not yet included in the SARIF output.

### Aggregate Statistics

Aggregate confidence metrics (mean, min, max, high/medium/low counts) are computed during the `calculate_metrics` node and logged, but are not currently included in the Excel or SARIF output.

---

## Score Components (investigated issues)

### 1. Agent Confidence (37.5% of final score)

The LLM's self-assessed certainty in its verdict, produced by the analysis node.

- **Source:** `AnalysisResultOutput.confidence`
- **Range:** 0.0–1.0
- **Calibration:**
  - **0.8–1.0:** All critical paths traced, clear evidence.
  - **0.5–0.8:** Most paths traced, minor gaps that don't affect verdict.
  - **0.0–0.5:** Significant uncertainty or critical gaps remain.

### 2. Evidence Strength (25% of final score)

Quality and quantity of evidence supporting the verdict.

| Sub-component | Weight | What it measures | Cap |
|---|---|---|---|
| FAISS similarity | 0.40 | How closely the issue matches known examples in the vector DB | Already 0–1 |
| Files fetched | 0.30 | Number of source files retrieved during investigation | 5 files = max |
| Evidence count | 0.30 | Concrete evidence in justifications (code blocks, CVE refs, file:line refs) | 3 items = max |

```
evidence_strength = 0.40 * faiss_score + 0.30 * files_score + 0.30 * evidence_score
```

### 3. Investigation Depth (37.5% of final score)

How thoroughly the issue was investigated before reaching a verdict.

| Sub-component | Weight | What it measures | Cap |
|---|---|---|---|
| Symbols explored | 0.25 | Unique code symbols/functions examined beyond initial trace | 3 symbols = max |
| Tool calls | 0.25 | Total research tool calls made | 8 calls = max |
| Reanalysis cycles | 0.25 | Times the evaluator requested re-analysis (iterative refinement) | 2 cycles = max |
| Stop reason | 0.25 | How the investigation ended (see below) | Categorical |

**Stop reason scores:**

| Reason | Score | Meaning |
|---|---|---|
| `approved` | 1.0 | Evaluator approved the verdict |
| `max_iterations` | 0.6 | Hit iteration limit but gathered evidence |
| `no_progress` | 0.3 | Investigation stalled |
| Unknown | 0.2 | Default low confidence |

```
investigation_depth = 0.25 * depth + 0.25 * tool_calls + 0.25 * reanalysis + 0.25 * stop_reason
```

---

## Data Flow

```
filter node
  ├── Sets filter_confidence and faiss_similarity_score (all issues)
  └── Known FPs: is_final=TRUE → short-circuit scoring (Path 1)
         │
         ▼
investigate node (per issue, Path 2 only)
  ├── RESEARCH → gathers code, fetched_files, found_symbols, tool_call_count
  ├── ANALYSIS → produces agent_confidence, justifications, verdict
  └── EVALUATION → sets stop_reason, tracks reanalysis_count
         │
         ▼
calculate_metrics node
  ├── Calls calculate_final_confidence() per issue
  ├── Stores final_confidence_score (0-100) in PerIssueData
  └── Computes aggregate statistics (mean, min, max, high/medium/low counts)
         │
         ▼
write_results node
  └── Outputs final_confidence_score in Excel/Google Sheets
```

---

## Configuration

All values in `config/default_config.yaml`.

### Path 1 — Known False Positives

No configurable weights. Score = `filter_confidence * 100` (always 100% filter).

### Path 2 — Investigated Issues

#### Main Component Weights (must sum to 1.0)

| Parameter | Default | Description |
|---|---|---|
| `CONFIDENCE_WEIGHT_AGENT` | 0.375 | LLM's verdict confidence |
| `CONFIDENCE_WEIGHT_EVIDENCE` | 0.25 | Evidence quality/quantity |
| `CONFIDENCE_WEIGHT_INVESTIGATION` | 0.375 | Investigation thoroughness |

#### Evidence Sub-Weights (must sum to 1.0)

| Parameter | Default |
|---|---|
| `EVIDENCE_WEIGHT_FAISS_SCORE` | 0.40 |
| `EVIDENCE_WEIGHT_FILES_FETCHED` | 0.30 |
| `EVIDENCE_WEIGHT_EVIDENCE_COUNT` | 0.30 |

#### Investigation Sub-Weights (must sum to 1.0)

| Parameter | Default |
|---|---|
| `INVESTIGATION_WEIGHT_DEPTH` | 0.25 |
| `INVESTIGATION_WEIGHT_TOOL_CALLS` | 0.25 |
| `INVESTIGATION_WEIGHT_REANALYSIS` | 0.25 |
| `INVESTIGATION_WEIGHT_STOP_REASON` | 0.25 |

#### Normalization Caps

| Parameter | Default | Description |
|---|---|---|
| `CONFIDENCE_MAX_FILES_FOR_NORMALIZATION` | 5 | Files fetched cap |
| `CONFIDENCE_MAX_EVIDENCE_ITEMS_FOR_NORMALIZATION` | 3 | Evidence items cap |
| `CONFIDENCE_MAX_SYMBOLS_FOR_NORMALIZATION` | 3 | Symbols explored cap |
| `CONFIDENCE_MAX_TOOL_CALLS_FOR_NORMALIZATION` | 8 | Tool calls cap |
| `CONFIDENCE_MAX_REANALYSIS_FOR_NORMALIZATION` | 2 | Reanalysis cycles cap |

#### Stop Reason Scores

| Parameter | Default |
|---|---|
| `STOP_REASON_SCORE_APPROVED` | 1.0 |
| `STOP_REASON_SCORE_MAX_ITERATIONS` | 0.6 |
| `STOP_REASON_SCORE_NO_PROGRESS` | 0.3 |
| `STOP_REASON_SCORE_UNKNOWN` | 0.2 |

## Safety

- **Weight validation:** Raises `ValueError` at scoring time if any weight group doesn't sum to 1.0.
- **Clamping:** All scores clamped to valid ranges. Out-of-range values trigger a warning log.
- **Error isolation:** Confidence scoring failures don't block other metrics — errors are captured separately.

## Key Source Files

| File | Role |
|---|---|
| `src/Utils/confidence_scoring.py` | Core scoring logic and formula |
| `config/default_config.yaml` | All weights, caps, and stop reason scores |
| `src/sast_agent_workflow/nodes/calculate_metrics.py` | Orchestrates scoring across all issues |
| `src/sast_agent_workflow/nodes/sub_agents/investigation/orchestrator.py` | Transfers investigation metrics to PerIssueData |
| `src/sast_agent_workflow/nodes/sub_agents/investigation/nodes/analysis.py` | Produces agent_confidence |
| `src/sast_agent_workflow/nodes/sub_agents/investigation/nodes/evaluation.py` | Sets stop_reason, tracks reanalysis |
| `src/sast_agent_workflow/nodes/sub_agents/investigation/nodes/schemas.py` | Schema definitions |
| `src/dto/SASTWorkflowModels.py` | PerIssueData model with confidence fields |
