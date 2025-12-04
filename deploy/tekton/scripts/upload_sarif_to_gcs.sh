#!/bin/bash
set -e
echo "=== STEP 8: UPLOAD SARIF TO GCS BUCKET ==="

# Check if we have required parameters
if [ -z "$GCS_BUCKET_NAME" ]; then
  echo "Skipping GCS upload - no bucket name provided"
  exit 0
fi

# Check service account
if [ ! -f "$GOOGLE_APPLICATION_CREDENTIALS" ]; then
  echo "Skipping GCS upload - service account not available"
  exit 0
fi

# Look for the SARIF file in output directory
SARIF_FILE=$(find /shared-data/output -name "*.sarif" -type f 2>/dev/null | head -1)

if [ -z "$SARIF_FILE" ]; then
  echo "ERROR: No SARIF file found in output directory"
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
echo "Destination: gs://$GCS_BUCKET_NAME/$DESTINATION_PATH"

if python /scripts/gcs/gcs_upload.py "$SARIF_FILE" "$GCS_BUCKET_NAME" "$DESTINATION_PATH"; then
  echo "✓ Successfully uploaded: $SCAN_FILENAME"
  echo "=== SARIF file uploaded to GCS successfully! ==="
else
  echo "✗ Failed to upload: $SCAN_FILENAME"
  echo "=== SARIF upload failed, but pipeline continues ==="
  # Don't fail the pipeline for upload issues
fi