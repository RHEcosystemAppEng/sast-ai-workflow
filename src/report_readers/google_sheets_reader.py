import logging
import re
from typing import List

from common.config import Config
from dto.Issue import Issue
from report_readers.base_reader import BaseReportReader
from Utils.file_utils import get_google_sheet

logger = logging.getLogger(__name__)


class GoogleSheetsReportReader(BaseReportReader):
    """
    Reader for Google Sheets SAST reports.
    """

    def can_handle(self, file_path: str, config: Config) -> bool:
        """
        Check if the provided file_path is a Google Sheets URL.
        """
        try:
            # Check if it's a Google Sheets URL
            if not file_path.startswith("https://"):
                return False

            # Check if it contains Google Sheets URL patterns
            google_patterns = ["docs.google.com/spreadsheets", "sheets.google.com"]

            google_patterns = ["docs.google.com/spreadsheets", "sheets.google.com"]

            return any(pattern in file_path for pattern in google_patterns)

        except Exception as e:
            logger.debug(f"Error checking Google Sheets URL: {e}")
            return False

    def read_report(self, file_path: str, config: Config) -> List[Issue]:
        """
        Read and parse Google Sheets SAST report.
        """
        logger.info(f"Reading Google Sheets report from: {file_path}")

        try:
            return self._read_sast_report_google_sheet(config.SERVICE_ACCOUNT_JSON_PATH, file_path)
            return self._read_sast_report_google_sheet(config.SERVICE_ACCOUNT_JSON_PATH, file_path)
        except Exception as e:
            logger.error(f"Error reading Google Sheets report {file_path}: {e}")
            raise

    def _read_sast_report_google_sheet(
        self, service_account_file_path: str, google_sheet_url: str
    ) -> List[Issue]:
        """
        Read a Google Sheet and create a list of Issue objects based on the 'Finding' column.


        NOTE: Assumes issue details are in the 'Finding' column of the first sheet.


        Args:
            service_account_file_path: Path to the service account JSON file for authentication
            google_sheet_url: URL of the Google Sheet


        Returns:
            List[Issue]: List of Issue objects
        """
        sheet = get_google_sheet(google_sheet_url, service_account_file_path, ignore_error=False)
        if sheet is None:
            raise ValueError(f"Failed to access Google Sheet: {google_sheet_url}")

        rows = sheet.get_all_records()

        # Check for empty sheet
        if not rows:
            logger.warning(f"No rows found in Google Sheet: {google_sheet_url}")
            return []

        # Create a list of Issue objects
        issue_list = []
        for idx, row in enumerate(rows, start=1):
            finding = row.get("Finding", None)

            if finding is None:
                continue

            # Convert finding to string if it's not already
            finding_str = str(finding)

            issue = Issue(f"def{idx}")

            # Parse with error handling - always create Issue object to preserve indexing
            try:
                self._parse_finding_with_error_handling(finding_str, issue)
            except ValueError as e:
                logger.warning(f"Failed to parse finding for issue {issue.id}: {e}")
                # Mark as having parsing errors for validation
                issue.parsing_errors = True

            issue_list.append(issue)

        logger.info(f"Successfully parsed {len(issue_list)} issues from Google Sheet")
        return issue_list

    def _parse_finding_with_error_handling(self, finding_str: str, issue: Issue) -> None:
        """
        Parse finding string into Issue object with comprehensive error handling.

        Expected format: "Error: IssueType (CWE-XXX)\nTrace line 1\nTrace line 2"
        or "Error: IssueType CWE-XXX\nTrace line 1"

        Example: "Error: BUFFER_OVERFLOW (CWE-120):\nBuffer overflow in main() function"
        """
        lines = finding_str.split("\n")

        if not lines or not lines[0]:
            raise ValueError("Empty finding content")

        first_line = lines[0].strip()

        # Extract issue type
        if "Error:" not in first_line:
            raise ValueError("Missing 'Error:' prefix in finding")

        error_part = first_line.split("Error:", 1)[1].strip()
        if not error_part:
            raise ValueError("No content after 'Error:' prefix")

        # Extract issue type (first word after Error:, before any parentheses or CWE)
        # Handle formats like "BUFFER_OVERFLOW (CWE-120)" or "BUFFER_OVERFLOW CWE-120"
        type_match = re.match(r"(\w+)", error_part)
        if not type_match:
            raise ValueError("No issue type found after 'Error:'")

        issue.issue_type = type_match.group(1)

        # Extract CWE information
        cwe_match = re.search(r"CWE-(\d+)", first_line, re.IGNORECASE)
        if cwe_match:
            issue.issue_cwe = f"CWE-{cwe_match.group(1)}"
            issue.issue_cwe_link = (
                f"https://cwe.mitre.org/data/definitions/{cwe_match.group(1)}.html"
            )
        else:
            issue.issue_cwe = ""
            issue.issue_cwe_link = ""

        # Set trace (remaining lines or full content if single line)
        if len(lines) > 1:
            issue.trace = "\n".join(lines[1:]).strip()
        else:
            issue.trace = finding_str.strip()

        # Mark as successfully parsed
        issue.parsing_errors = False
