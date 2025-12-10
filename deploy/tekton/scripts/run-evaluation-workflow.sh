#!/usr/bin/env bash
set -e

echo "=== STEP 6: RUN SAST AI ANALYSIS OR EVALUATION ==="

# Inline S3 availability check (from s3-helper.sh check mode)
if [[ -f "/shared-data/s3-available.txt" ]]; then
  S3_AVAILABLE=$(cat /shared-data/s3-available.txt)
  if [[ "$S3_AVAILABLE" = "false" ]]; then
    echo "Cannot run evaluation without ground truth data from S3"
    echo "Skipped: S3 endpoint not available (see Step 4)"

    # Create empty results so pipeline continues
    mkdir -p /shared-data/eval_results /shared-data/output
    echo "{}" > /shared-data/output/workflow_metrics.json

    exit 0
  fi
else
  echo "Warning: S3 availability status file not found, proceeding with caution"
fi

# Load the repo path from step 4
if [[ -f "/shared-data/env.txt" ]]; then
  . /shared-data/env.txt
  export REPO_LOCAL_PATH
else
  echo "Error: No environment file found from prepare-source step" >&2
  exit 1
fi

# Use the transformed report file path if available
if [[ -f "/shared-data/report-file-path.txt" ]]; then
  TRANSFORMED_REPORT_PATH=$(cat /shared-data/report-file-path.txt)
  export INPUT_REPORT_FILE_PATH="$TRANSFORMED_REPORT_PATH"
  export HUMAN_VERIFIED_FILE_PATH="$TRANSFORMED_REPORT_PATH"
  echo "Using transformed report file path: $INPUT_REPORT_FILE_PATH"
  echo "Using transformed ground truth file path: $HUMAN_VERIFIED_FILE_PATH"
else
  echo "Using original report file path: $INPUT_REPORT_FILE_PATH"
fi

# Create directories
mkdir -p "/cache-data/tmp" "/shared-data/output"

# Parse and normalize EVALUATE_SPECIFIC_NODE parameter
EVAL_NODES="$EVALUATE_SPECIFIC_NODE"
EVAL_NODES_NORMALIZED=$(echo "$EVAL_NODES" | tr '[:upper:]' '[:lower:]' | sed 's/ //g')
echo "Evaluation nodes: $EVAL_NODES_NORMALIZED"

# Check if full workflow should run
if echo ",$EVAL_NODES_NORMALIZED," | grep -q ",all,"; then
  RUN_FULL_WORKFLOW=true
  echo "Mode: Individual evaluations + full workflow"
else
  RUN_FULL_WORKFLOW=false
  echo "Mode: Individual evaluations only"
fi

# PHASE 1: Individual node evaluations
mkdir -p /shared-data/eval_results

# Parse comma-separated list and run each evaluation runner
echo "$EVAL_NODES_NORMALIZED" | tr ',' '\n' | while read -r node; do
  node=$(echo "$node" | xargs)  # trim whitespace

  # Skip "all" - it's handled by full workflow in Phase 2
  [ "$node" = "all" ] && continue

  # Parse ground truth XLSX file if needed for this evaluation node
  if [[ -f "/shared-data/report-file-path.txt" ]]; then
    GROUND_TRUTH_PATH=$(cat /shared-data/report-file-path.txt)

    # Check if it's an XLSX file that needs parsing
    case "$GROUND_TRUTH_PATH" in
      *.xlsx)
        mkdir -p /shared-data/evaluation_dataset
        NVR="${PROJECT_NAME}-${PROJECT_VERSION}"

        # Map node names to parser node types
        case "$node" in
          summary)
            PARSER_NODE_TYPE="summarize"
            ;;
          filter|judge)
            PARSER_NODE_TYPE="$node"
            ;;
          *)
            continue
            ;;
        esac

      # Parse XLSX to JSON
      python /app/evaluation/utils/parse_excel_to_json.py \
        --node-type "$PARSER_NODE_TYPE" \
        single \
        --excel-file "$GROUND_TRUTH_PATH" \
        --package-name "$NVR" \
        --output-file "/shared-data/evaluation_dataset/parsed_dataset.json"

        if [[ $? -eq 0 ]]; then
          export EVALUATION_DATASET_PATH="/shared-data/evaluation_dataset/parsed_dataset.json"
        else
          echo "Error: XLSX parsing failed for $node, results of eval flow for $node is not saved."
          exit 0
        fi
        ;;
    esac
  fi

  case "$node" in
    filter)
      echo "Evaluating: filter"
      echo "Running: python -u /app/evaluation/runners/run_filter_evaluation.py"
      echo "EVALUATION_JSON_OUTPUT=/shared-data/eval_results/filter.json"
      echo "EVALUATION_DATASET_PATH=$EVALUATION_DATASET_PATH"
      echo "---START PYTHON OUTPUT---"
      EVALUATION_JSON_OUTPUT=/shared-data/eval_results/filter.json \
        python -u /app/evaluation/runners/run_filter_evaluation.py 2>&1
      EXIT_CODE=$?
      echo "---END PYTHON OUTPUT---"
      echo "Filter eval exit code: $EXIT_CODE"
      if [[ $EXIT_CODE -ne 0 ]]; then
        echo "WARNING: Filter evaluation failed with exit code $EXIT_CODE, continuing to next node"
      fi
      ;;
    summary)
      echo "Evaluating: summary"
      echo "Running: python -u /app/evaluation/runners/run_summarize_evaluation.py"
      echo "EVALUATION_JSON_OUTPUT=/shared-data/eval_results/summary.json"
      echo "EVALUATION_DATASET_PATH=$EVALUATION_DATASET_PATH"
      echo "---START PYTHON OUTPUT---"
      EVALUATION_JSON_OUTPUT=/shared-data/eval_results/summary.json \
        python -u /app/evaluation/runners/run_summarize_evaluation.py 2>&1
      EXIT_CODE=$?
      echo "---END PYTHON OUTPUT---"
      echo "Summary eval exit code: $EXIT_CODE"
      if [[ $EXIT_CODE -ne 0 ]]; then
        echo "WARNING: Summary evaluation failed with exit code $EXIT_CODE, continuing to next node"
      fi
      ;;
    judge)
      echo "Evaluating: judge"
      echo "Running: python -u /app/evaluation/runners/run_judge_llm_evaluation.py"
      echo "EVALUATION_JSON_OUTPUT=/shared-data/eval_results/judge.json"
      echo "EVALUATION_DATASET_PATH=$EVALUATION_DATASET_PATH"
      echo "---START PYTHON OUTPUT---"
      EVALUATION_JSON_OUTPUT=/shared-data/eval_results/judge.json \
        python -u /app/evaluation/runners/run_judge_llm_evaluation.py 2>&1
      EXIT_CODE=$?
      echo "---END PYTHON OUTPUT---"
      echo "Judge eval exit code: $EXIT_CODE"
      if [[ $EXIT_CODE -ne 0 ]]; then
        echo "WARNING: Judge evaluation failed with exit code $EXIT_CODE, continuing to next node"
      fi
      ;;
  esac
done

# PHASE 2: Run full workflow AFTER evaluations (if "all" was present)
if [[ "$RUN_FULL_WORKFLOW" = "true" ]]; then
  echo "Running full SAST workflow..."
  export WORKFLOW_JSON_OUTPUT="/shared-data/output/workflow_metrics.json"
  echo "---START FULL WORKFLOW OUTPUT---"
  aiq run --config_file /app/src/sast_agent_workflow/configs/config.yml --input "sast_agent"
  EXIT_CODE=$?
  echo "---END FULL WORKFLOW OUTPUT---"
  echo "Full workflow exit code: $EXIT_CODE"

  # Validate workflow succeeded
  if [[ $EXIT_CODE -ne 0 ]]; then
    echo "Error: Full workflow failed with exit code $EXIT_CODE" >&2
    exit 1
  fi

  # Validate output file exists
  if [[ ! -f "/shared-data/output/sast_ai_output.xlsx" ]]; then
    echo "Error: Output file not found!" >&2
    exit 1
  fi

  echo "Full workflow completed successfully"
else
  # Create empty workflow metrics when full workflow doesn't run
  echo "{}" > /shared-data/output/workflow_metrics.json
  echo "Skipped full workflow - evaluation mode only"
fi

echo "Evaluation workflow completed successfully"