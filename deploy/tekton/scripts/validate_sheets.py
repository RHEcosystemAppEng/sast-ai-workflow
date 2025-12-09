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