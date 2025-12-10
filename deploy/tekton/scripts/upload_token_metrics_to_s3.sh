#!/bin/bash
set -e
echo "=== STEP 9: UPLOAD TOKEN METRICS TO S3/MINIO ==="

# Check if S3 is available
if [[ -f "/shared-data/s3-available.txt" ]]; then
  S3_AVAILABLE=$(cat /shared-data/s3-available.txt)
  if [[ "$S3_AVAILABLE" = "false" ]]; then
    echo "Skipped: S3 endpoint not available"
    exit 0
  fi
fi

# Skip if not running in "all" mode
EVAL_NODES_NORMALIZED=$(echo "$EVALUATE_SPECIFIC_NODE" | tr '[:upper:]' '[:lower:]' | sed 's/ //g')
if ! echo ",$EVAL_NODES_NORMALIZED," | grep -q ",all,"; then
  echo "Skipping token metrics upload in evaluation mode (EVALUATE_SPECIFIC_NODE=$EVALUATE_SPECIFIC_NODE)"
  exit 0
fi

# Check if bucket name is provided
if [[ -z "$S3_OUTPUT_BUCKET_NAME" ]]; then
  echo "Skipping token metrics upload - no bucket name provided"
  exit 0
fi

# Check if credentials are available
if [[ -z "$AWS_ACCESS_KEY_ID" ] || [ -z "$AWS_SECRET_ACCESS_KEY" ]]; then
  echo "Skipping token metrics upload - credentials not available"
  exit 0
fi

# Check if token metrics file exists
TOKEN_METRICS_FILE="/shared-data/token_usage.json"
if [[ ! -f "$TOKEN_METRICS_FILE" ]]; then
  echo "WARNING: Token metrics file not found at $TOKEN_METRICS_FILE"
  echo "This may indicate the workflow did not use LLM nodes or metrics tracking failed"
  echo "Continuing pipeline execution gracefully"
  exit 0
fi

# Install required packages
echo "Installing required packages..."
pip install --quiet boto3 >/dev/null 2>&1
echo "Dependencies installed successfully"

# Construct S3 key
if [[ -n "$PIPELINE_RUN_ID" ]]; then
  PIPELINE_ID="$PIPELINE_RUN_ID"
else
  PIPELINE_ID=$(date -u +"%Y%m%d-%H%M%S")
fi

REPO_NAME="${PROJECT_NAME}"
S3_KEY="${PIPELINE_ID}/${REPO_NAME}_token_usage.json"

echo "File to upload: $TOKEN_METRICS_FILE"
echo "S3 Output Bucket: $S3_OUTPUT_BUCKET_NAME"
echo "S3 Key: $S3_KEY"

# Upload to S3
echo "Executing token metrics upload..."
if [[ -n "$S3_ENDPOINT_URL" ]]; then
  python /scripts/s3-output/s3_upload.py "$TOKEN_METRICS_FILE" "$S3_OUTPUT_BUCKET_NAME" "$S3_KEY" "$S3_ENDPOINT_URL"
else
  python /scripts/s3-output/s3_upload.py "$TOKEN_METRICS_FILE" "$S3_OUTPUT_BUCKET_NAME" "$S3_KEY"
fi

if [[ $? -eq 0 ]]; then
  echo "=== Token metrics upload completed successfully! ==="
else
  echo "WARNING: Token metrics upload failed, but continuing pipeline"
  exit 0
fi