"""
ProcessMiningService - Extract data from Excel ground-truth files.
Reuses existing infrastructure from ExcelReportReader and file_utils.
"""

import pandas as pd
import logging
from typing import List, Dict, Optional
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from Utils.file_utils import get_header_row
from report_readers.excel_reader import ExcelReportReader
from dto.Issue import Issue
from process_mining.src.common.process_mining_config import ProcessMiningConfig

logger = logging.getLogger(__name__)


class ProcessMiningService:
    """Service for extracting data from Excel ground-truth files."""

    def __init__(self, config_path: Optional[str] = None):
        self.excel_reader = ExcelReportReader()

        config = ProcessMiningConfig(config_path)
        prep_config = config.get_preparation_config()
        self.false_positive_markers = [
            marker.lower() for marker in prep_config.get('false_positive_markers', ['y', 'yes', 'true'])
        ]

    def read_ground_truth_excel(self, excel_path: str) -> List[Dict]:
        """
        Read Excel ground-truth file and extract all relevant fields.

        Combines:
        - ExcelReportReader._parse_finding_with_error_handling() for Finding column
        - file_utils logic for reading Excel with pandas
        - Custom extraction for Hint, Comment, AI prediction columns

        Args:
            excel_path: Path to .xlsx file

        Returns:
            List of dicts with all fields needed for ProcessMiningEntry
        """
        filename = Path(excel_path).name
        header_row = get_header_row(excel_path)
        df = pd.read_excel(excel_path, header=header_row, engine='openpyxl')

        if df.empty:
            logger.warning(f"No rows found in Excel file: {excel_path}")
            return []

        required_cols = ['Finding', 'False Positive?']
        missing_required = [col for col in required_cols if col not in df.columns]
        if missing_required:
            raise ValueError(f"Excel file missing required columns: {missing_required}")

        has_hint = 'Hint' in df.columns
        has_comment = 'Comment' in df.columns
        if not has_hint and not has_comment:
            raise ValueError(f"Excel file must have at least one justification column: 'Hint' or 'Comment'")

        entries = []
        for idx, row in df.iterrows():
            finding = row.get("Finding", None)
            if pd.isna(finding) or finding is None:
                logger.warning(f"{filename} - Row {idx + 1}: Skipping empty Finding")
                continue

            finding_str = str(finding)

            issue = Issue(id=f"def{idx + 1}")
            try:
                self.excel_reader._parse_finding_with_error_handling(finding_str, issue)
                issue_type = issue.issue_type
                issue_cwe = issue.issue_cwe
                error_trace = issue.trace
            except Exception as e:
                logger.warning(f"{filename} - Row {idx + 1}: Failed to parse finding: {e}")
                continue

            fp_raw = row.get("False Positive?", None)
            if pd.isna(fp_raw):
                logger.debug(f"{filename} - Row {idx + 1}: Skipping entry with missing 'False Positive?' annotation")
                continue

            fp_value = str(fp_raw).strip().lower()
            if not fp_value:
                logger.debug(f"{filename} - Row {idx + 1}: Skipping entry with empty 'False Positive?' annotation")
                continue

            is_false_positive = fp_value in self.false_positive_markers
            true_positive_markers = ['n', 'no', 'false']
            is_true_positive = fp_value in true_positive_markers

            if not is_false_positive and not is_true_positive:
                logger.debug(f"{filename} - Row {idx + 1}: Skipping entry with unrecognized 'False Positive?' value: '{fp_value}'")
                continue

            hint = row.get("Hint", "")
            hint_str = str(hint) if pd.notna(hint) else ""

            comment = row.get("Comment", "")
            comment_str = str(comment) if pd.notna(comment) else ""

            ai_pred = row.get("AI prediction", None)
            ai_prediction = str(ai_pred) if pd.notna(ai_pred) else None

            entries.append({
                "issue_type": issue_type,
                "issue_cwe": issue_cwe,
                "error_trace": error_trace,
                "is_false_positive": is_false_positive,
                "human_justification": hint_str,
                "comment": comment_str or None,
                "ai_prediction": ai_prediction,
                "raw_finding": finding_str
            })

        return entries
