#!/bin/bash
set -e
echo "=== STEP 4: TRANSFORM SAST REPORT TO SARIF FORMAT ==="

REPORT_PATH="$INPUT_REPORT_FILE_PATH"
WORKSPACE_PATH="/shared-data"

echo "Processing report path: $REPORT_PATH"
echo "Workspace path: $WORKSPACE_PATH"

# Check if the input is a URL of json file
if [[ "$REPORT_PATH" =~ ^https?://.*\.(json|js|sarif)$ ]]; then
  echo "Detected JSON URL - using direct download..."

  # Download the JSON file
  JSON_FILE="$WORKSPACE_PATH/downloaded_report.json"
  echo "Downloading file to: $JSON_FILE"
  curl -kL "$REPORT_PATH" -o "$JSON_FILE"

  # Verify the file was downloaded
  if [[ ! -f "$JSON_FILE" ]]; then
    echo "Error: Failed to download file" >&2
    exit 1
  fi

  echo "File downloaded successfully ($(wc -c < "$JSON_FILE") bytes)"

  # Check if it's a JSON file and convert to SARIF
  if  [[ "$JSON_FILE" =~ \.(json|js)$ ]] || file "$JSON_FILE" | grep -q "JSON"; then
    echo "Detected JSON file - converting to SARIF format..."

    # Convert to SARIF format using csgrep
    SARIF_FILE="$WORKSPACE_PATH/report.sarif"
    echo "Converting to SARIF format: $SARIF_FILE"
    csgrep "$JSON_FILE" --mode sarif > "$SARIF_FILE"

    # Verify the SARIF file was created
    if [[ ! -f "$SARIF_FILE" ]]; then
      echo "Error: Failed to create SARIF file" >&2
      exit 1
    fi

    echo "SARIF file created successfully ($(wc -c < "$SARIF_FILE") bytes)"

    # Clean up the downloaded JSON file
    rm -f "$JSON_FILE"

    # Set the result to the SARIF file path
    echo -n "$SARIF_FILE" > "${TEKTON_RESULTS_DIR}"
    echo -n "$SARIF_FILE" > /shared-data/report-file-path.txt
    echo "Report converted and saved as: $SARIF_FILE"
    ls -l $SARIF_FILE
  else
    echo "File is already a SARIF file - using as-is"
    # Set the result to the downloaded file path
    echo -n "$JSON_FILE" > "${TEKTON_RESULTS_DIR}"
    echo -n "$JSON_FILE" > /shared-data/report-file-path.txt
    echo "Report saved as: $JSON_FILE"
  fi
else
  echo "Input is not a JSON URL - passing through unchanged"
  # For non-URL paths (Google Sheets, local files), pass the original path through
  echo -n "$REPORT_PATH" > "${TEKTON_RESULTS_DIR}"
  echo -n "$REPORT_PATH" > /shared-data/report-file-path.txt
  echo "Report path unchanged: $REPORT_PATH"
fi

echo "Report preparation completed"