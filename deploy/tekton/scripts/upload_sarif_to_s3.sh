#!/bin/bash
set -e
echo "=== STEP 9: UPLOAD SARIF TO S3 BUCKET ==="

# Check if we have required parameters
if [[ -z "$S3_OUTPUT_BUCKET_NAME" ]]; then
  echo "Skipping S3 upload - no bucket name provided"
  exit 0
fi

# Check S3 credentials
if [[ -z "$AWS_ACCESS_KEY_ID" ]] || [[ -z "$AWS_SECRET_ACCESS_KEY" ]]; then
  echo "Skipping S3 upload - credentials not available"
  exit 0
fi

# Look for the SARIF file in output directory
SARIF_FILE=$(find /shared-data/output -name "*.sarif" -type f 2>/dev/null | head -1)

if [[ -z "$SARIF_FILE" ]]; then
  echo "ERROR: No SARIF file found in output directory" >&2
  echo "Available files in output directory:"
  ls -la /shared-data/output/ || echo "Output directory is empty or inaccessible"
  exit 1
fi

echo "Found SARIF file: $SARIF_FILE"

# Generate timestamp for folder organization
TIMESTAMP=$(date -u +"%Y-%m-%dT%H-%M-%S")

# Upload the SARIF file
SCAN_FILENAME=$(basename "$SARIF_FILE")

# Create organized path: sarif-reports/timestamp/scan_filename
DESTINATION_PATH="sarif-reports/${TIMESTAMP}/${SCAN_FILENAME}"

echo "Uploading: $SARIF_FILE"
echo "Destination: s3://$S3_OUTPUT_BUCKET_NAME/$DESTINATION_PATH"
if [[ -n "$S3_ENDPOINT_URL" ]]; then
  echo "Endpoint: $S3_ENDPOINT_URL"
fi

# Call Python S3 upload script
if python3 /scripts/s3-upload/s3_upload.py "$SARIF_FILE" "$S3_OUTPUT_BUCKET_NAME" "$DESTINATION_PATH" "${S3_ENDPOINT_URL:-}"; then
  echo "✓ Successfully uploaded: $SCAN_FILENAME"
  echo "=== SARIF file uploaded to S3 successfully! ==="
else
  echo "✗ Failed to upload: $SCAN_FILENAME"
  echo "=== SARIF upload failed, but pipeline continues ==="
  # Don't fail the pipeline for upload issues
fi
