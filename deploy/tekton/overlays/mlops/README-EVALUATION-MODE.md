# SAST AI Workflow - MLOps Evaluation Mode Implementation

## Overview

This document describes the implementation of on-the-fly XLSX parsing and evaluation mode support for the SAST AI Workflow in OpenShift/Tekton pipelines. This feature allows running individual evaluation nodes (filter, summary, judge) instead of the full SAST AI workflow, with dynamic parsing of ground truth Excel files.

## Table of Contents

- [Implementation Summary](#implementation-summary)
- [Architecture](#architecture)
- [Files Created/Modified](#files-createdmodified)
- [Issues Encountered and Resolutions](#issues-encountered-and-resolutions)
- [Docker Build Process](#docker-build-process)
- [Usage Guide](#usage-guide)
- [Troubleshooting](#troubleshooting)

---

## Implementation Summary

### Objectives

1. Support evaluation mode for individual NAT evaluation nodes (filter, summary, judge)
2. Parse XLSX ground truth files on-the-fly during pipeline execution
3. Print evaluation results to terminal in JSON format
4. Support flexible container image versions via parameters
5. Maintain compatibility with existing full workflow mode

### What Was Built

1. **MLOps Kustomize Overlay** - Evaluation-specific patches for Tekton pipeline
2. **Dynamic XLSX Parser Integration** - On-the-fly conversion from Excel to NAT-compatible JSON
3. **Evaluation Runner Script** - Shell script logic to route between full workflow and evaluation modes
4. **Parameter Passing System** - Proper Tekton parameter substitution and image selection
5. **Docker Image Build** - Multi-platform container with evaluation directory included

---

## Architecture

### Evaluation Flow

```
┌─────────────────────────────────────────────────────────────┐
│ PipelineRun (systemd-summary-eval-test)                     │
│  - EVALUATE_SPECIFIC_NODE: "summary"                         │
│  - CONTAINER_IMAGE: "quay.io/.../sast-ai-workflow:486fb4a"  │
│  - INPUT_REPORT_FILE_PATH: "http://minio.../systemd.xlsx"   │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│ Pipeline: sast-ai-workflow-pipeline (MLOps overlay)         │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│ Task: execute-sast-ai-workflow                               │
│  Step 1: Validate URLs                                      │
│  Step 2: Validate Report File                               │
│  Step 3: Prepare Source (SRPM → extracted source)           │
│  Step 4: Transform Report (DVC fetch XLSX from MinIO)       │
│  Step 5: Fetch False Positives (DVC)                        │
│  Step 6: Run Analysis/Evaluation ← EVALUATION LOGIC HERE    │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│ Step 6 Decision Logic (run-analysis-eval-patch.yaml)        │
│                                                              │
│ if EVALUATE_SPECIFIC_NODE == "all":                         │
│   ├─► Run full SAST AI workflow (aiq run)                   │
│   └─► Output: /shared-data/output/sast_ai_output.xlsx      │
│                                                              │
│ else (e.g., "summary", "filter", "judge"):                  │
│   ├─► Parse XLSX to JSON (parse_excel_to_json.py)          │
│   ├─► Export EVALUATION_DATASET_PATH                        │
│   ├─► Run evaluation runner (run_summarize_evaluation.py)   │
│   └─► Output: === EVALUATION_RESULTS_JSON ===               │
└─────────────────────────────────────────────────────────────┘
```

### Data Flow

```
XLSX File (MinIO)
    │
    ├─► DVC Fetch → /shared-data/downloaded_report.xlsx
    │
    └─► parse_excel_to_json.py
            │
            ├─► Extract columns: Finding, Comment, Hint
            ├─► Map: Comment → generated_answer
            ├─► Map: Hint → expected_output
            └─► Generate: /shared-data/evaluation_dataset/parsed_dataset.json
                    │
                    └─► run_summarize_evaluation.py
                            │
                            ├─► Update NAT config with dataset path
                            ├─► Run NAT evaluation (nat eval)
                            ├─► Generate quality metrics (judge LLM)
                            └─► Print JSON results to terminal
```

---

## Files Created/Modified

### 1. Containerfile (Modified)

**Purpose**: Include evaluation directory in Docker image

**Changes**:
```dockerfile
COPY config ./config/
COPY src ./src/
COPY evaluation ./evaluation/    # ← ADDED: Contains parse_excel_to_json.py
COPY pyproject.toml .
```

**Why**: The evaluation scripts (parsers, runners, converters) were not being included in the container image, causing file-not-found errors during pipeline execution.

**Location**: `/Users/gziv/Dev/sast-ai-workflow/Containerfile`

---

### 2. run-analysis-eval-patch.yaml (Created)

**Purpose**: Replace the run-analysis step script to add evaluation mode routing logic

**Key Features**:
- Detects evaluation mode via `EVALUATE_SPECIFIC_NODE` parameter
- Routes to full workflow if `all`, or evaluation mode for specific nodes
- Parses XLSX files on-the-fly using `parse_excel_to_json.py`
- Maps node names (summary → summarize) to match parser expectations
- Explicitly sets container image from parameter

**Critical Sections**:

```yaml
- op: replace
  path: /spec/steps/5/image
  value: "$(params.CONTAINER_IMAGE)"  # Ensures correct image version
```

```bash
# Parameter reading (Tekton syntax, not bash)
EVAL_NODES="$(params.EVALUATE_SPECIFIC_NODE)"

# Node type mapping
case "$node" in
  summary)
    PARSER_NODE_TYPE="summarize"  # Maps to parser's expected name
    ;;
  filter|judge)
    PARSER_NODE_TYPE="$node"
    ;;
esac

# On-the-fly XLSX parsing
python /app/evaluation/utils/parse_excel_to_json.py \
  --node-type "$PARSER_NODE_TYPE" \
  single \
  --excel-file "$GROUND_TRUTH_PATH" \
  --package-name "$NVR" \
  --output-file "/shared-data/evaluation_dataset/parsed_dataset.json"
```

**Location**: `/Users/gziv/Dev/sast-ai-workflow/deploy/tekton/overlays/mlops/run-analysis-eval-patch.yaml`

---

### 3. pipeline-params-patch.yaml (Created)

**Purpose**: Add new pipeline parameters for evaluation mode and container image selection

**Parameters Added**:

```yaml
# Evaluation node selection
- name: EVALUATE_SPECIFIC_NODE
  type: string
  description: "Comma-separated list: 'filter', 'summary', 'judge', 'all'"
  default: "all"

# Container image version
- name: CONTAINER_IMAGE
  type: string
  description: "Container image to use for SAST AI analysis"
  default: "quay.io/ecosystem-appeng/sast-ai-workflow:latest"
```

**Why**: Allows PipelineRuns to specify which evaluation node to run and which container image version to use, without modifying the base pipeline definition.

**Location**: `/Users/gziv/Dev/sast-ai-workflow/deploy/tekton/overlays/mlops/pipeline-params-patch.yaml`

---

### 4. kustomization.yaml (Modified)

**Purpose**: Apply evaluation patches to base Tekton resources

**Patches Applied**:
```yaml
patches:
  - path: pipeline-params-patch.yaml
    target:
      kind: Pipeline
      name: sast-ai-workflow-pipeline

  - path: run-analysis-eval-patch.yaml
    target:
      kind: Task
      name: execute-sast-ai-workflow
```

**Location**: `/Users/gziv/Dev/sast-ai-workflow/deploy/tekton/overlays/mlops/kustomization.yaml`

---

### 5. Test PipelineRun (Created)

**Purpose**: Example PipelineRun demonstrating evaluation mode usage

**Key Parameters**:
```yaml
apiVersion: tekton.dev/v1
kind: PipelineRun
metadata:
  name: systemd-summary-eval-test
  namespace: sast-ai-s3-tests
spec:
  pipelineRef:
    name: sast-ai-workflow-pipeline
  params:
    - name: PROJECT_NAME
      value: "systemd"
    - name: PROJECT_VERSION
      value: "257-9"
    - name: CONTAINER_IMAGE
      value: "quay.io/ecosystem-appeng/sast-ai-workflow:486fb4a"
    - name: EVALUATE_SPECIFIC_NODE
      value: "summary"  # Run only summary evaluation
    - name: INPUT_REPORT_FILE_PATH
      value: "http://minio-api.../systemd_257_input_file.xlsx"
```

**Location**: `/Users/gziv/Dev/sast-ai-workflow/deploy/tekton/pipelinerun-mlops-test-2.yaml`

---

## Issues Encountered and Resolutions

### Issue 1: EVALUATE_SPECIFIC_NODE Parameter Defaulting to "all"

**Symptom**: Pipeline logs showed `Evaluation mode: all` instead of `summary`, causing the full workflow to run instead of just the summary evaluation.

**Root Cause**: Using bash environment variable syntax `${EVALUATE_SPECIFIC_NODE:-all}` instead of Tekton parameter substitution.

**Solution**: Changed to Tekton parameter syntax:
```bash
# ❌ WRONG (bash default value)
EVAL_NODES="${EVALUATE_SPECIFIC_NODE:-all}"

# ✅ CORRECT (Tekton parameter substitution)
EVAL_NODES="$(params.EVALUATE_SPECIFIC_NODE)"
```

**File**: `run-analysis-eval-patch.yaml:33`

**Validation**: Logs now correctly show `Evaluation mode: summary`

---

### Issue 2: parse_excel_to_json.py File Not Found

**Symptom**:
```
python: can't open file '/app/evaluation/utils/parse_excel_to_json.py':
[Errno 2] No such file or directory
```

**Root Cause**: Containerfile didn't include the `evaluation` directory.

**Solution**: Added `COPY evaluation ./evaluation/` to Containerfile between `src` and `pyproject.toml`.

**File**: `Containerfile:14`

**Validation**: File exists in container at path `/app/evaluation/utils/parse_excel_to_json.py`

---

### Issue 3: Wrong Docker Image Being Used

**Symptom**: Pod was using `quay.io/ecosystem-appeng/sast-ai-workflow:latest` instead of `486fb4a`, even though PipelineRun specified `486fb4a`.

**Root Cause**: Pipeline wasn't passing `CONTAINER_IMAGE` parameter to the task, and the run-analysis step didn't have an explicit image field patch.

**Solution**:
1. Added `CONTAINER_IMAGE` parameter to pipeline-params-patch.yaml (both pipeline params and task params)
2. Added explicit image field patch to run-analysis-eval-patch.yaml:
```yaml
- op: replace
  path: /spec/steps/5/image
  value: "$(params.CONTAINER_IMAGE)"
```

**Files**:
- `pipeline-params-patch.yaml:63-72`
- `run-analysis-eval-patch.yaml:1-3`

**Validation**: Pod now uses specified image version `486fb4a`

---

### Issue 4: Node Type Mismatch

**Symptom**: Parser failed with:
```
argument --node-type: invalid choice: 'summary'
(choose from filter, summarize, judge)
```

**Root Cause**: Pipeline parameter uses `summary` but parser expects `summarize`.

**Solution**: Added node type mapping in run-analysis-eval-patch.yaml:
```bash
case "$node" in
  summary)
    PARSER_NODE_TYPE="summarize"  # Map user-friendly name to parser name
    ;;
  filter|judge)
    PARSER_NODE_TYPE="$node"
    ;;
esac
```

**File**: `run-analysis-eval-patch.yaml:70-81`

**Validation**: XLSX parsing completed successfully with "Created 27 test cases"

---

### Issue 5: Stuck PipelineRuns with Finalizer Errors

**Symptom**: Multiple PipelineRuns stuck with "Failed to update finalizers" errors.

**Root Cause**: Multiple controllers trying to update the same resource simultaneously.

**Solution**: Force deleted stuck runs:
```bash
kubectl delete pipelinerun <name> -n sast-ai-s3-tests --force --grace-period=0
```

**Validation**: New PipelineRuns create and execute successfully

---

### Issue 6: Outdated LLM Model in Kubernetes Secret

**Symptom**: Secret contained old model name `nvidia/llama-3.1-nemotron-70b-instruct` instead of `meta/llama-3.1-70b-instruct`.

**Root Cause**: Secret wasn't updated when LLM model changed.

**Solution**: Updated Kubernetes secret with new model name:
```bash
cat <<'EOF' | kubectl patch secret sast-ai-default-llm-creds \
  -n sast-ai-s3-tests --type='json' --patch-file=/dev/stdin
[{
  "op": "replace",
  "path": "/data/llm_model_name",
  "value": "bWV0YS9sbGFtYS0zLjEtNzBiLWluc3RydWN0"
}]
EOF
```

**Validation**:
```bash
kubectl get secret sast-ai-default-llm-creds -n sast-ai-s3-tests \
  -o jsonpath='{.data.llm_model_name}' | base64 -d
# Output: meta/llama-3.1-70b-instruct
```

---

### Issue 7: Quality Metrics All 0.0 (Data Quality Issue)

**Symptom**: JSON output shows all quality metrics as 0.0, despite judge LLM evaluator running successfully.

**Root Cause**: Ground truth XLSX file's `Hint` column contains incomplete prompt templates instead of actual expected summary content:
- `"Here is a concise summary of the justifications in two sentences:"`
- `"Here are concise justifications for the investigation result:"`

**Solution**: XLSX file needs correction. The `Hint` column should contain actual expected summary sentences, not prompt fragments.

**Status**: Awaiting XLSX file update

**Expected Behavior**: After XLSX correction, quality metrics should show non-zero scores from judge LLM comparison.

---

## Docker Build Process

### Build Configuration

**Platform**: `linux/amd64` (required for OpenShift AMD64 nodes)

**Base Image**: `registry.access.redhat.com/ubi9/python-312`

**Build Context**: Project root directory

### Build Commands

```bash
# 1. Build image for AMD64 platform
docker build --platform linux/amd64 \
  -t quay.io/ecosystem-appeng/sast-ai-workflow:486fb4a \
  -f Containerfile .

# 2. Tag with semantic version
docker tag quay.io/ecosystem-appeng/sast-ai-workflow:486fb4a \
  quay.io/ecosystem-appeng/sast-ai-workflow:eval-node-support

# 3. Push both tags to Quay.io
docker push quay.io/ecosystem-appeng/sast-ai-workflow:486fb4a
docker push quay.io/ecosystem-appeng/sast-ai-workflow:eval-node-support
```

### Image Contents

Critical directories included:
- `/app/src/` - SAST AI workflow source code
- `/app/evaluation/` - Evaluation scripts (parsers, runners, converters)
- `/app/config/` - Configuration files
- `/app/pyproject.toml` - Python package metadata

Installed packages:
- NAT framework (Natural Language Assessment Tool)
- LangGraph
- FAISS
- DVC
- All requirements from `requirements.txt`

### Git Commit Reference

**Commit**: `486fb4a`

**Changes**:
- Added `COPY evaluation ./evaluation/` to Containerfile
- Fixed Tekton parameter reading in run-analysis-eval-patch.yaml
- Added node type mapping (summary → summarize)

**Committed Files**:
- `Containerfile`
- `deploy/tekton/overlays/mlops/run-analysis-eval-patch.yaml`
- `deploy/tekton/overlays/mlops/pipeline-params-patch.yaml`

---

## Usage Guide

### Running Summary Evaluation

```bash
# 1. Apply the MLOps overlay (if not already applied)
kubectl apply -k deploy/tekton/overlays/mlops -n sast-ai-s3-tests

# 2. Create a PipelineRun for summary evaluation
cat <<EOF | kubectl apply -f -
apiVersion: tekton.dev/v1
kind: PipelineRun
metadata:
  name: my-package-summary-eval
  namespace: sast-ai-s3-tests
spec:
  pipelineRef:
    name: sast-ai-workflow-pipeline
  params:
    - name: PROJECT_NAME
      value: "mypackage"
    - name: PROJECT_VERSION
      value: "1.0.0"
    - name: CONTAINER_IMAGE
      value: "quay.io/ecosystem-appeng/sast-ai-workflow:486fb4a"
    - name: EVALUATE_SPECIFIC_NODE
      value: "summary"  # or "filter", "judge", "all"
    - name: INPUT_REPORT_FILE_PATH
      value: "http://minio-api.../ground_truth.xlsx"
    - name: LLM_URL
      value: "https://integrate.api.nvidia.com/v1"
    - name: LLM_MODEL_NAME
      value: "meta/llama-3.1-70b-instruct"
  workspaces:
    - name: gitlab-token-ws
      secret:
        secretName: sast-ai-gitlab-token
    - name: google-sa-json-ws
      secret:
        secretName: sast-ai-google-service-account
    - name: s3-input-creds-ws
      secret:
        secretName: sast-ai-s3-input-creds
        optional: true
EOF

# 3. Watch the pipeline run
kubectl get pipelinerun my-package-summary-eval -n sast-ai-s3-tests -w

# 4. View logs
kubectl logs -f $(kubectl get pods -n sast-ai-s3-tests \
  -l tekton.dev/pipelineRun=my-package-summary-eval \
  --sort-by=.metadata.creationTimestamp \
  -o jsonpath='{.items[-1].metadata.name}') \
  -c step-run-analysis -n sast-ai-s3-tests
```

### Running Filter Evaluation

Change `EVALUATE_SPECIFIC_NODE` to `"filter"`:

```yaml
- name: EVALUATE_SPECIFIC_NODE
  value: "filter"
```

### Running Judge LLM Evaluation

Change `EVALUATE_SPECIFIC_NODE` to `"judge"`:

```yaml
- name: EVALUATE_SPECIFIC_NODE
  value: "judge"
```

**What Judge LLM Evaluation Measures:**
- Quality of security analysis justifications
- Clarity of explanations (35% weight)
- Completeness of reasoning (30% weight)
- Technical accuracy (25% weight)
- Logical flow (10% weight)
- Overall weighted quality score

**Expected Output Structure:**
```json
{
  "node_type": "judge_llm_analysis",
  "package_info": {
    "name": "systemd-257-9",
    "version": "257-9",
    "total_issues": 27
  },
  "aggregated_metrics": {
    "quality_metrics": {
      "overall_score": 0.75,
      "clarity": 0.80,
      "completeness": 0.72,
      "technical_accuracy": 0.78,
      "logical_flow": 0.68
    },
    "performance_metrics": {
      "total_tokens": 45000,
      "avg_time_per_request": 0.0,
      "llm_call_count": 27
    }
  },
  "issues": [
    {
      "id": "systemd-257-9_0_ISSUE_TYPE_file.c",
      "quality_metrics": {
        "overall_score": 0.82,
        "clarity": 0.85,
        "completeness": 0.80,
        "technical_accuracy": 0.85,
        "logical_flow": 0.75
      },
      "performance_metrics": {
        "tokens": 1500,
        "time": 0.0,
        "llm_calls": 1
      }
    }
  ]
}
```

### Running Full Workflow (Normal Mode)

Set `EVALUATE_SPECIFIC_NODE` to `"all"` (or omit parameter to use default):

```yaml
- name: EVALUATE_SPECIFIC_NODE
  value: "all"
```

### Extracting Results from Logs

Results are printed to stdout with markers:

```bash
# Extract JSON results
kubectl logs <pod-name> -c step-run-analysis -n sast-ai-s3-tests \
  | sed -n '/=== EVALUATION_RESULTS_JSON ===/,/=== END_EVALUATION_RESULTS_JSON ===/p' \
  | grep -v "===" \
  > evaluation_results.json

# Pretty-print results
cat evaluation_results.json | jq .
```

---

## Troubleshooting

### Pipeline Shows "Evaluation mode: all" Instead of Specific Node

**Check**: Verify parameter is being passed correctly in PipelineRun

```bash
kubectl get pipelinerun <name> -n sast-ai-s3-tests \
  -o jsonpath='{.spec.params[?(@.name=="EVALUATE_SPECIFIC_NODE")].value}'
```

**Expected Output**: Should show your specified node (e.g., `summary`)

**If Wrong**: Check that pipeline-params-patch.yaml includes parameter passing to task

---

### Parser Error: "invalid choice: 'summary'"

**Check**: Verify node type mapping in run-analysis-eval-patch.yaml

**Solution**: Ensure mapping exists:
```bash
summary) PARSER_NODE_TYPE="summarize" ;;
```

---

### Quality Metrics All 0.0

**Check 1**: Verify ground truth XLSX file has valid expected summaries in `Hint` column

**Check 2**: Verify judge LLM ran:
```bash
kubectl logs <pod> -c step-run-analysis | grep "summarization_quality_eval"
```

**Check 3**: Look for archived files including `summarization_quality_eval_output.json`:
```bash
kubectl logs <pod> -c step-run-analysis | grep "Archived files"
```

---

### Wrong Container Image Version

**Check**: Verify pod is using correct image:
```bash
kubectl get pod <pod-name> -n sast-ai-s3-tests \
  -o jsonpath='{.spec.containers[?(@.name=="step-run-analysis")].image}'
```

**Expected**: Should match `CONTAINER_IMAGE` parameter value

**If Wrong**: Check that pipeline-params-patch.yaml includes `CONTAINER_IMAGE` parameter

---

### Secrets Not Found

**Check**: Verify all required secrets exist:
```bash
kubectl get secret sast-ai-default-llm-creds -n sast-ai-s3-tests
kubectl get secret sast-ai-s3-input-creds -n sast-ai-s3-tests
kubectl get secret sast-ai-google-service-account -n sast-ai-s3-tests
```

**Solution**: Create missing secrets or mark as optional in PipelineRun

---

## Summary of Achievements

✅ **On-the-fly XLSX parsing** - Ground truth files are parsed dynamically during pipeline execution

✅ **Evaluation mode routing** - Pipeline can run individual nodes or full workflow based on parameter

✅ **Container image parameterization** - Easy version selection via PipelineRun parameter

✅ **JSON output to terminal** - Results printed with markers for easy extraction

✅ **Multi-platform Docker build** - Image works on AMD64 OpenShift nodes

✅ **Kubernetes secret updates** - LLM model configuration updated successfully

✅ **27 test cases processed** - Successful XLSX parsing with 27 issues from systemd package

✅ **Judge LLM integration** - Quality evaluator runs and generates output (awaiting proper test data)

## Next Steps

1. **Fix ground truth XLSX files** - Replace prompt templates in `Hint` column with actual expected summaries
2. **Validate quality scores** - Re-run evaluation with corrected XLSX to verify non-zero quality metrics
3. **Document XLSX format** - Create specification for ground truth file structure
4. **Add more test cases** - Expand evaluation coverage to other packages
5. **CI/CD integration** - Automate evaluation runs on code changes

---

## References

- **Base Pipeline**: `deploy/tekton/base/pipeline.yaml`
- **Base Task**: `deploy/tekton/base/tasks/execute_sast_ai_workflow.yaml`
- **XLSX Parser**: `evaluation/utils/parse_excel_to_json.py`
- **Summary Runner**: `evaluation/runners/run_summarize_evaluation.py`
- **NAT Config**: `evaluation/configs/summarize_justifications_eval.yml`
- **Docker Registry**: https://quay.io/repository/ecosystem-appeng/sast-ai-workflow

---

**Document Version**: 1.0
**Last Updated**: November 2, 2025
**Author**: Guy Ziv
**Commit Reference**: 486fb4a