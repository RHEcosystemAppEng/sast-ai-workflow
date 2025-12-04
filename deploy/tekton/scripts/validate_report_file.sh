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

    if [ -z "$SHEET_ID" ]; then
        echo "Error: Invalid Google Sheets URL format"
        echo "Expected format: https://docs.google.com/spreadsheets/d/{SHEET_ID}/..."
        exit 1
    fi

    # Check if credentials workspace is provided
    if [ -f "$GOOGLE_SA_JSON_PATH" ]; then

        # Create and run validation script
        cat > /tmp/validate_sheets.py << 'PYTHON_SCRIPT'
import sys
from google.oauth2 import service_account
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError

def validate_sheet_access(sheet_id, credentials_path):
    try:
        credentials = service_account.Credentials.from_service_account_file(
            credentials_path,
            scopes=['https://www.googleapis.com/auth/spreadsheets.readonly']
        )
        service = build('sheets', 'v4', credentials=credentials)
        spreadsheet = service.spreadsheets().get(
            spreadsheetId=sheet_id,
            fields='properties.title'
        ).execute()
        title = spreadsheet.get('properties', {}).get('title', 'Unknown')
        print(f"Successfully validated access to spreadsheet: '{title}'")
    except HttpError as e:
        if e.resp.status == 403:
            print("Error: Access denied - service account lacks permissions")
        elif e.resp.status == 404:
            print("Error: Spreadsheet not found - check the URL or permissions")
        else:
            print(f"Error: HTTP error {e.resp.status}: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    sheet_id = sys.argv[1]
    credentials_path = sys.argv[2]
    validate_sheet_access(sheet_id, credentials_path)
PYTHON_SCRIPT

        python3 /tmp/validate_sheets.py "$SHEET_ID" "$GOOGLE_SA_JSON_PATH"
    else
        echo "Error: No service account credentials provided for Google Sheets validation"
        exit 1
    fi
else
    # Check if local file exists
    if [ -f "$INPUT_REPORT_FILE_PATH" ]; then
        echo "Local file validated successfully"
    else
        echo "Error: File does not exist: $INPUT_REPORT_FILE_PATH"
        exit 1
    fi
fi