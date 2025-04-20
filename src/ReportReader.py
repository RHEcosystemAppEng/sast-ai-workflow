import re
import gspread
from oauth2client.service_account import ServiceAccountCredentials
from bs4 import BeautifulSoup
from typing import List

from common.config import Config
from dto.Issue import Issue



def read_sast_report(config:Config) -> List[Issue]:
    print(f"Reading => {config.INPUT_REPORT_FILE_PATH}")
    if config.INPUT_REPORT_FILE_PATH.startswith("https"):
        return read_sast_report_google_sheet(config)
    return read_sast_report_html(config.INPUT_REPORT_FILE_PATH)



def read_sast_report_google_sheet(config:Config) -> List[Issue]:
    """
    Reads a Google Sheet and creates a list of Issue objects based on the 'Finding' column.
    NOTE: Assumes issue details are in the 'Finding' 
          column of the first sheet (sheet name doesn't matter).

    :param config: Config object containing configuration details, including:
                   - INPUT_REPORT_FILE_PATH: URL of the Google Sheet.
                   - SERVICE_ACCOUNT_JSON_PATH: Path to the service account JSON file for authentication.
    :return: List of Issue objects.
    """
    # Define the scope for Google Sheets API
    scope = ['https://spreadsheets.google.com/feeds', 'https://www.googleapis.com/auth/drive']

    # Authenticate using the service account JSON file
    credentials = ServiceAccountCredentials.from_json_keyfile_name(config.SERVICE_ACCOUNT_JSON_PATH, scope)
    client = gspread.authorize(credentials)

    sheet = client.open_by_url(config.INPUT_REPORT_FILE_PATH).sheet1  # Assumes the data is in the first sheet
    rows = sheet.get_all_records()

    # Create a list of Issue objects
    issue_list = []
    for idx, row in enumerate(rows):
        finding = row.get('Finding')
        if not finding:
            continue
        issue = Issue(idx)
        lines = finding.split("\n")
        issue.issue_type = lines[0].split("Error:")[1].strip().split()[0]
        match = re.search(r'CWE-\d+', lines[0])
        issue.issue_cve = match.group() if match else ""
        issue.issue_cve_link = f"https://cwe.mitre.org/data/definitions/{issue.issue_cve.split('-')[1]}.html" if match else ""
        issue.trace = "\n".join(lines[1:])
        issue_list.append(issue)

    return issue_list

def read_sast_report_html(file_path) -> List[Issue]:
    issue_list = []
    with open(file_path, "r", encoding='utf-8') as f:
        soup = BeautifulSoup(f.read(), 'html.parser')
        all_pre_tags = soup.findAll('pre')
        cur_issue = Issue(-1)
        for tag in all_pre_tags[0].children:
            if tag.name == 'a' and tag.has_attr('id'):
                if cur_issue.id != -1:
                    issue_list.append(cur_issue)
                cur_issue = Issue(tag['id'])
            else:
                if tag.name == 'b' and tag.find('span') and tag.find('a'):
                    try:
                        cur_issue.issue_type = tag.find('span').text
                        cur_issue.issue_cve = tag.find('a').text
                        cur_issue.issue_cve_link = tag.find('a')['href']
                    except AttributeError:
                        print(f"Exception when parsing tag: {tag}")
                else:
                    cur_issue.trace += tag.text

    return issue_list




