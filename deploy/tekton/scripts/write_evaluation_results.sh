#!/usr/bin/env bash
set -e
echo "=== STEP 8: WRITE ALL EVALUATION RESULTS ==="

# Load repo path for DVC commit hash
if [[ -f "/shared-data/env.txt" ]]; then
  . /shared-data/env.txt
  export REPO_LOCAL_PATH
fi

# Write filter evaluation if exists
if [[ -f "/shared-data/eval_results/filter.json" ]]; then
  cat /shared-data/eval_results/filter.json > "$TEKTON_RESULTS_DIR/filter-evaluation-results"
  FILTER_SIZE=$(wc -c < "$TEKTON_RESULTS_DIR/filter-evaluation-results")
  echo "filter-evaluation-results: $FILTER_SIZE bytes"
else
  echo "{}" > "$TEKTON_RESULTS_DIR/filter-evaluation-results"
  echo "filter-evaluation-results: {} (empty)"
fi

# Write judge evaluation if exists
if [[ -f "/shared-data/eval_results/judge.json" ]]; then
  cat /shared-data/eval_results/judge.json > "$TEKTON_RESULTS_DIR/judge-evaluation-results"
  JUDGE_SIZE=$(wc -c < "$TEKTON_RESULTS_DIR/judge-evaluation-results")
  echo "judge-evaluation-results: $JUDGE_SIZE bytes"
else
  echo "{}" > "$TEKTON_RESULTS_DIR/judge-evaluation-results"
  echo "judge-evaluation-results: {} (empty)"
fi

# Write summary evaluation result
if [[ -f "/shared-data/eval_results/summary.json" ]]; then
  cat /shared-data/eval_results/summary.json > "$TEKTON_RESULTS_DIR/summary-evaluation-results"
  SUMMARY_SIZE=$(wc -c < "$TEKTON_RESULTS_DIR/summary-evaluation-results")
  echo "summary-evaluation-results: $SUMMARY_SIZE bytes"
else
  echo "{}" > "$TEKTON_RESULTS_DIR/summary-evaluation-results"
  echo "summary-evaluation-results: {} (empty)"
fi

# Write workflow metrics
if [[ -f "/shared-data/output/workflow_metrics.json" ]]; then
  cat /shared-data/output/workflow_metrics.json > "$TEKTON_RESULTS_DIR/workflow-metrics"
  METRICS_SIZE=$(wc -c < "$TEKTON_RESULTS_DIR/workflow-metrics")
  echo "workflow-metrics: $METRICS_SIZE bytes"
else
  echo "{}" > "$TEKTON_RESULTS_DIR/workflow-metrics"
  echo "workflow-metrics: {} (empty)"
fi

# Write DVC results
echo -n "$PROJECT_VERSION" > "$TEKTON_RESULTS_DIR/dvc-data-version"

# For git repos, extract commit hash; for SRPM/non-git, leave empty
if [[ -d "$REPO_LOCAL_PATH/.git" ]]; then
  cd "$REPO_LOCAL_PATH" && git rev-parse HEAD > "$TEKTON_RESULTS_DIR/dvc-commit-hash" || echo -n "" > "$TEKTON_RESULTS_DIR/dvc-commit-hash"
else
  echo -n "" > "$TEKTON_RESULTS_DIR/dvc-commit-hash"
fi

# Pipeline stage identifier
echo -n "sast_ai_analysis" > "$TEKTON_RESULTS_DIR/dvc-pipeline-stage"

echo "All evaluation results written successfully"