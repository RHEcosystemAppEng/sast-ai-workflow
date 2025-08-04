import logging
import re
from typing import List

from bs4 import BeautifulSoup

from common.config import Config
from dto.Issue import Issue
from Utils.file_utils import get_google_sheet

logger = logging.getLogger(__name__)


def read_sast_report(config: Config) -> List[Issue]:
    logger.info(f"Reading => {config.INPUT_REPORT_FILE_PATH}")
    if config.INPUT_REPORT_FILE_PATH.startswith("https"):
        return read_sast_report_google_sheet(
            config.SERVICE_ACCOUNT_JSON_PATH, config.INPUT_REPORT_FILE_PATH
        )
    return read_sast_report_local_html(config.INPUT_REPORT_FILE_PATH)


def read_sast_report_google_sheet(service_account_file_path, google_sheet_url) -> List[Issue]:
    """
    Reads a Google Sheet and creates a list of Issue objects based on the 'Finding' column.
    NOTE: Assumes issue details are in the 'Finding'
          column of the first sheet (sheet name doesn't matter).

    :param config: Config object containing configuration details, including:
            - INPUT_REPORT_FILE_PATH: URL of the Google Sheet.
            - SERVICE_ACCOUNT_JSON_PATH: Path to the service account JSON file for authentication.
    :return: List of Issue objects.
    """
    sheet = get_google_sheet(google_sheet_url, service_account_file_path, ignore_error=False)
    rows = sheet.get_all_records()

    # Check for empty sheet
    if not rows:
        logger.warning(f"No rows found in Google Sheet: {google_sheet_url}")
        return []

    # Create a list of Issue objects
    issue_list = []
    for idx, row in enumerate(rows, start=1):
        finding = row.get("Finding")
        if not finding:
            continue

        issue = Issue(f"def{idx}")
        # TODO - please leave a example string for finding
        lines = finding.split("\n")
        issue.issue_type = lines[0].split("Error:")[1].strip().split()[0]
        match = re.search(r"CWE-\d+", lines[0])
        issue.issue_cve = match.group() if match else ""
        issue.issue_cve_link = (
            f"https://cwe.mitre.org/data/definitions/{issue.issue_cve.split('-')[1]}.html"
            if match
            else ""
        )
        issue.trace = "\n".join(lines[1:])
        issue_list.append(issue)

    return issue_list


def read_sast_report_local_html(file_path) -> List[Issue]:
    issue_list = []
    
    # Check if file exists
    import os
    if not os.path.exists(file_path):
        logger.error(f"File not found: {file_path}")
        raise FileNotFoundError(f"File not found: {file_path}")
    
    with open(file_path, "r", encoding="utf-8") as f:
        content = f.read()
        
        # Check for empty file
        if len(content.strip()) == 0:
            logger.warning(f"Empty HTML file detected: {file_path}")
            return []
            
        try:
            soup = BeautifulSoup(content, "html.parser")
        except Exception as e:
            logger.error(f"BeautifulSoup parsing failed for {file_path}: {e}")
            raise ValueError(f"HTML parsing failed: {e}")
            
        all_pre_tags = soup.find_all("pre")
        
        # Check if no <pre> tags found (corrupted/invalid structure)
        if not all_pre_tags:
            logger.warning(f"No <pre> tags found in HTML file - possibly corrupted: {file_path}")
            return []
            
        cur_issue = Issue(-1)
        tags_processed = 0
        valid_tags_found = 0
        
        for tag in all_pre_tags[0].children:
            tags_processed += 1
            
            if tag.name == "a" and tag.has_attr("id"):
                valid_tags_found += 1
                if cur_issue.id != -1:
                    issue_list.append(cur_issue)
                cur_issue = Issue(tag["id"])
            else:
                if tag.name == "b" and tag.find("span") and tag.find("a"):
                    valid_tags_found += 1
                    try:
                        cur_issue.issue_type = tag.find("span").text
                        cur_issue.issue_cve = tag.find("a").text
                        cur_issue.issue_cve_link = tag.find("a")["href"]
                    except AttributeError:
                        logger.error(f"Exception when parsing tag: {tag}")
                else:
                    cur_issue.trace += tag.text
        
        # Log if very few valid tags were found (possible corruption)
        if tags_processed > 0 and valid_tags_found == 0:
            logger.warning(f"No valid SAST tags found in {tags_processed} processed tags - file may be corrupted: {file_path}")
        elif tags_processed > 10 and valid_tags_found < (tags_processed * 0.1):
            logger.warning(f"Very few valid SAST tags found ({valid_tags_found}/{tags_processed}) - file may be corrupted: {file_path}")

    return issue_list
