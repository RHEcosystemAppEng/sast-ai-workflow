#!/bin/bash
set -e
echo "=== STEP 7: UPLOAD TO GOOGLE DRIVE ==="

# Check if we have required parameters
if [ -z "$GDRIVE_FOLDER_ID" ]; then
  # Try ConfigMap environment variable
  if [ -n "$GDRIVE_FOLDER_ID_FROM_CM" ]; then
    GDRIVE_FOLDER_ID="$GDRIVE_FOLDER_ID_FROM_CM"
    echo "Using Google Drive folder ID from ConfigMap: $GDRIVE_FOLDER_ID"
  else
    echo "Skipping Google Drive upload - no folder ID available"
    echo "This is not an error - pipeline continues gracefully"
    exit 0
  fi
else
  echo "Using Google Drive folder ID from parameter: $GDRIVE_FOLDER_ID"
fi

# Check service account
if [ ! -f "$GOOGLE_APPLICATION_CREDENTIALS" ]; then
  echo "Skipping Google Drive upload - service account not available"
  echo "This is not an error - pipeline continues gracefully"
  exit 0
fi

# Check if output file exists
EXCEL_FILE="/shared-data/output/sast_ai_output.xlsx"
if [ ! -f "$EXCEL_FILE" ]; then
  echo "ERROR: Excel file not found at $EXCEL_FILE"
  echo "Available files in output directory:"
  ls -la /shared-data/output/ || echo "Output directory is empty or inaccessible"
  exit 1
fi

# Set filename
EXCEL_FILENAME="${PROJECT_NAME}-${PROJECT_VERSION}"
if [ -z "$EXCEL_FILENAME" ] || [ "$EXCEL_FILENAME" = "-" ]; then
  EXCEL_FILENAME="sast_ai_output"
fi

echo "File to upload: $EXCEL_FILE"
echo "Remote filename: $EXCEL_FILENAME"
echo "Target folder ID: $GDRIVE_FOLDER_ID"

echo "Executing Google Drive upload..."
python /scripts/gdrive/gdrive_upload.py "$EXCEL_FILE" "$EXCEL_FILENAME" "$GDRIVE_FOLDER_ID"

if [ $? -eq 0 ]; then
  echo "=== Google Drive upload completed successfully! ==="
else
  echo "=== Google Drive upload failed ==="
  exit 1
fi