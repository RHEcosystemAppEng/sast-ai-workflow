import logging
import os
import re
from typing import List

import pandas as pd

from common.config import Config
from dto.Issue import Issue
from report_readers.base_reader import BaseReportReader

logger = logging.getLogger(__name__)


class ExcelReportReader(BaseReportReader):
    """
    Reader for local Excel (.xlsx) SAST reports.
    Reads reports with 'Finding' column, similar to GoogleSheetsReportReader.
    """

    def can_handle(self, file_path: str, config: Config) -> bool:
        """
        Check if the provided file_path is a local .xlsx file.
        """
        if not file_path:
            return False

        if not os.path.isfile(file_path):
            return False

        return file_path.lower().endswith('.xlsx')

    def read_report(self, file_path: str, config: Config) -> List[Issue]:
        """
        Read and parse Excel SAST report.
        """
        logger.info(f"Reading Excel report from: {file_path}")

        try:
            return self._read_sast_report_excel(file_path)
        except Exception as e:
            logger.error(f"Error reading Excel report {file_path}: {e}")
            raise

    def _read_sast_report_excel(self, excel_file_path: str) -> List[Issue]:
        """
        Read an Excel file and create a list of Issue objects based on the 'Finding' column.

        NOTE: Assumes issue details are in the 'Finding' column of the first sheet.

        Args:
            excel_file_path: Path to the Excel file

        Returns:
            List[Issue]: List of Issue objects
        """
        df = pd.read_excel(excel_file_path, sheet_name=0, engine='openpyxl')

        if df.empty:
            logger.warning(f"No rows found in Excel file: {excel_file_path}")
            return []

        if 'Finding' not in df.columns:
            raise ValueError(f"Excel file does not contain 'Finding' column: {excel_file_path}")

        issue_list = []
        for idx, row in df.iterrows():
            finding = row.get("Finding", None)

            if pd.isna(finding) or finding is None:
                continue

            finding_str = str(finding)

            issue = Issue(id=f"def{idx + 1}")

            try:
                self._parse_finding_with_error_handling(finding_str, issue)
            except ValueError as e:
                logger.warning(f"Failed to parse finding for issue {issue.id}: {e}")
                issue.parsing_errors = True

            issue_list.append(issue)

        logger.info(f"Successfully parsed {len(issue_list)} issues from Excel file")
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

        if "Error:" not in first_line:
            raise ValueError("Missing 'Error:' prefix in finding")

        error_part = first_line.split("Error:", 1)[1].strip()
        if not error_part:
            raise ValueError("No content after 'Error:' prefix")

        type_match = re.match(r"(\w+)", error_part)
        if not type_match:
            raise ValueError("No issue type found after 'Error:'")

        issue.issue_type = type_match.group(1)

        cwe_match = re.search(r"CWE-(\d+)", first_line, re.IGNORECASE)
        if cwe_match:
            issue.issue_cwe = f"CWE-{cwe_match.group(1)}"
            issue.issue_cwe_link = (
                f"https://cwe.mitre.org/data/definitions/{cwe_match.group(1)}.html"
            )
        else:
            issue.issue_cwe = ""
            issue.issue_cwe_link = ""

        if len(lines) > 1:
            issue.trace = "\n".join(lines[1:]).strip()
        else:
            issue.trace = finding_str.strip()

        issue.parsing_errors = False
