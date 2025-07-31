import logging
import re
from typing import List

from report_readers.base_reader import BaseReportReader
from dto.Issue import Issue
from common.config import Config
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
            google_patterns = [
                "docs.google.com/spreadsheets",
                "sheets.google.com"
            ]
            
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
            return self._read_sast_report_google_sheet(
                config.SERVICE_ACCOUNT_JSON_PATH,
                file_path
            )
        except Exception as e:
            logger.error(f"Error reading Google Sheets report {file_path}: {e}")
            raise
    
    def _read_sast_report_google_sheet(self, service_account_file_path: str, google_sheet_url: str) -> List[Issue]:
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
        
        # Create a list of Issue objects
        issue_list = []
        for idx, row in enumerate(rows, start=1):
            finding = row.get('Finding')
            if not finding:
                continue
            
            # Convert finding to string if it's not already
            finding_str = str(finding)
            
            issue = Issue(f"def{idx}")
            # TODO - please leave a example string for finding
            lines = finding_str.split("\n")
            issue.issue_type = lines[0].split("Error:")[1].strip().split()[0]
            match = re.search(r'CWE-\d+', lines[0])
            issue.issue_cve = match.group() if match else ""
            issue.issue_cve_link = f"https://cwe.mitre.org/data/definitions/{issue.issue_cve.split('-')[1]}.html" if match else ""
            issue.trace = "\n".join(lines[1:])
            issue_list.append(issue)
        
        logger.info(f"Successfully parsed {len(issue_list)} issues from Google Sheet")
        return issue_list 