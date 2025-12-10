#!/bin/bash
set -e
echo "=== STEP 2: VALIDATE REPORT FILE ==="

# Check if it's a URL (starts with http:// or https://)
if [[ "$INPUT_REPORT_FILE_PATH" =~ ^https?:// ]]; then

    # Skip validation for SARIF reports (JSON URLs) - they should not be validated as spreadsheets
    if [[ "$INPUT_REPORT_FILE_PATH" =~ ^https?://.*\.(json|js)$ ]]; then
      echo "Detected JSON URL - skipping validation"
      exit 0
    fi

    # Extract Sheet ID from URL
    SHEET_ID=$(echo "$INPUT_REPORT_FILE_PATH" | sed -n 's/.*\/spreadsheets\/d\/\([a-zA-Z0-9_-]*\).*/\1/p')

    if [[ -z "$SHEET_ID" ]]; then
        echo "Error: Invalid Google Sheets URL format"
        echo "Expected format: https://docs.google.com/spreadsheets/d/{SHEET_ID}/..."
        exit 1
    fi

    # Check if credentials workspace is provided
    if [[ -f "$GOOGLE_SA_JSON_PATH" ]]; then
        # Run validation script
        python3 /scripts/validate_sheets.py "$SHEET_ID" "$GOOGLE_SA_JSON_PATH"
    else
        echo "Error: No service account credentials provided for Google Sheets validation"
        exit 1
    fi
else
    # Check if local file exists
    if [[ -f "$INPUT_REPORT_FILE_PATH" ]]; then
        echo "Local file validated successfully"
    else
        echo "Error: File does not exist: $INPUT_REPORT_FILE_PATH"
        exit 1
    fi
fi