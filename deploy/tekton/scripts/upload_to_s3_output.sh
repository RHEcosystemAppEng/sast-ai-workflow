#!/bin/bash
set -e
echo "=== UPLOAD TO S3/MINIO OUTPUT ==="

# Inline S3 availability check (from s3-helper.sh check mode)
if [[ -f "/shared-data/s3-available.txt" ]]; then
  S3_AVAILABLE=$(cat /shared-data/s3-available.txt)
  if [[ "$S3_AVAILABLE" = "false" ]]; then
    echo "Skipped: S3 endpoint not available (see Step 4)"
    exit 0
  fi
else
  echo "Warning: S3 availability status file not found, proceeding with caution"
fi

# Skip if "all" is not present in EVALUATE_SPECIFIC_NODE
EVAL_NODES_NORMALIZED=$(echo "$EVALUATE_SPECIFIC_NODE" | tr '[:upper:]' '[:lower:]' | sed 's/ //g')
if ! echo ",$EVAL_NODES_NORMALIZED," | grep -q ",all,"; then
  echo "Skipping S3 output upload in evaluation mode (EVALUATE_SPECIFIC_NODE=$EVALUATE_SPECIFIC_NODE)"
  exit 0
fi

# Check if we have required parameters
if [[ -z "$S3_OUTPUT_BUCKET_NAME" ]]; then
  echo "Skipping S3 output upload - no bucket name provided"
  echo "This is not an error - pipeline continues gracefully"
  exit 0
fi

# Check if credentials are available
if [[ -z "$AWS_ACCESS_KEY_ID" ] || [ -z "$AWS_SECRET_ACCESS_KEY" ]]; then
  echo "Skipping S3 upload - credentials not available"
  echo "This is not an error - pipeline continues gracefully"
  exit 0
fi

# Check if output file exists
EXCEL_FILE="/shared-data/output/sast_ai_output.xlsx"
if [[ ! -f "$EXCEL_FILE" ]]; then
  echo "ERROR: Excel file not found at $EXCEL_FILE"
  echo "Available files in output directory:"
  ls -la /shared-data/output/ || echo "Output directory is empty or inaccessible"
  exit 1
fi

# Construct S3 key using unique pipeline run ID
# Use PIPELINE_RUN_ID if provided, otherwise fallback to timestamp
if [[ -n "$PIPELINE_RUN_ID" ]]; then
  PIPELINE_ID="$PIPELINE_RUN_ID"
else
  PIPELINE_ID=$(date -u +"%Y%m%d-%H%M%S")
fi

# Structure: {pipeline-id}/{repo-name}_sast_ai_output.xlsx
REPO_NAME="${PROJECT_NAME}"
S3_KEY="${PIPELINE_ID}/${REPO_NAME}_sast_ai_output.xlsx"

echo "File to upload: $EXCEL_FILE"
echo "S3 Output Bucket: $S3_OUTPUT_BUCKET_NAME"
echo "S3 Key: $S3_KEY"

# Upload to S3
echo "Executing S3 output upload..."
if [[ -n "$S3_ENDPOINT_URL" ]]; then
  python /scripts/s3-output/s3_upload.py "$EXCEL_FILE" "$S3_OUTPUT_BUCKET_NAME" "$S3_KEY" "$S3_ENDPOINT_URL"
else
  python /scripts/s3-output/s3_upload.py "$EXCEL_FILE" "$S3_OUTPUT_BUCKET_NAME" "$S3_KEY"
fi

if [[ $? -eq 0 ]]; then
  echo "=== S3 upload completed successfully! ==="
else
  echo "=== S3 upload failed ==="
  exit 0
fi