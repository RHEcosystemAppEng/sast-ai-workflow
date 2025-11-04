#!/usr/bin/env python3
"""
Parse Excel ground truth sheets to NAT-compatible evaluation JSON format.

This module provides base and specialized classes for converting Excel ground truth
data into JSON datasets for NAT evaluation framework.
"""

import json
import logging
import os
import sys
from abc import ABC, abstractmethod
from pathlib import Path
from typing import List, Dict, Any, Optional, Set
import pandas as pd

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from evaluation.constants import (
    DATASET_FILTER_DIR,
    DATASET_SUMMARIZATION_DIR,
    DATASET_JUDGE_LLM_DIR,
    DYNAMIC_EVAL_DATASET_PATH
)

logger = logging.getLogger(__name__)


class BaseExcelParser(ABC):
    """Base class for parsing Excel ground truth to evaluation JSON."""

    def __init__(self, excel_file: str, nvr: str, output_path: Optional[str] = None):
        self.excel_file = excel_file
        self.nvr = nvr
        self.output_path = output_path
        self.df = None

    def parse(self) -> Optional[str]:
        """Main method to parse Excel and generate JSON."""
        if not self._load_excel():
            return None

        if not self._validate_columns():
            return None

        test_cases = self._extract_test_cases()

        if not test_cases:
            logger.warning(f"No valid test cases found in {self.excel_file}")
            return None

        output_file = self._write_output(test_cases)
        return output_file

    def _load_excel(self) -> bool:
        """Load Excel file into DataFrame."""
        try:
            self.df = pd.read_excel(self.excel_file)
            return True
        except Exception as e:
            logger.error(f"Error reading {self.excel_file}: {e}")
            return False

    def _validate_columns(self) -> bool:
        """Validate required columns exist in the DataFrame."""
        required_columns = self._get_required_columns()
        missing_columns = [col for col in required_columns if col not in self.df.columns]
        if missing_columns:
            logger.warning(f"Missing columns {missing_columns} in {self.excel_file}")
            return False
        return True

    @abstractmethod
    def _get_required_columns(self) -> List[str]:
        """Get list of required column names for this parser type."""
        pass

    @abstractmethod
    def _extract_test_cases(self) -> List[Dict[str, Any]]:
        """Extract test cases from DataFrame. Node-specific implementation."""
        pass

    @abstractmethod
    def _get_default_output_filename(self) -> str:
        """Get default output filename for this parser type."""
        pass

    def _write_output(self, test_cases: List[Dict[str, Any]]) -> str:
        """Write test cases to JSON file."""
        if self.output_path:
            output_file = self.output_path
            os.makedirs(os.path.dirname(output_file), exist_ok=True)
        else:
            output_file = self._get_default_output_filename()
            os.makedirs(os.path.dirname(output_file), exist_ok=True)

        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(test_cases, f, indent=2, ensure_ascii=False)

        logger.info(f"  Created {len(test_cases)} test cases")
        logger.info(f"  Output: {output_file}")

        return output_file

    @staticmethod
    def _extract_issue_id_from_trace(finding_text: str) -> str:
        """Extract issue type and file from the finding trace for unique ID generation."""
        lines = finding_text.strip().split('\n')
        if not lines:
            return "unknown"

        first_line = lines[0]
        issue_type = "UNKNOWN"
        if "Error:" in first_line:
            parts = first_line.split("Error:")
            if len(parts) > 1:
                error_part = parts[1].strip()
                if "(" in error_part:
                    issue_type = error_part.split("(")[0].strip()

        file_path = "unknown"
        if len(lines) > 1:
            second_line = lines[1]
            if "/" in second_line and ":" in second_line:
                file_path = second_line.split(":")[0].strip()
                if "/" in file_path:
                    file_path = file_path.split("/")[-1]

        return f"{issue_type}_{file_path}"

    @staticmethod
    def _extract_issue_type_from_finding(finding: str) -> str:
        """Extract issue type from finding text."""
        issue_type = "SECURITY_ISSUE"
        if "Error:" in finding:
            parts = finding.split("Error:")
            if len(parts) > 1:
                error_part = parts[1].strip()
                if "(" in error_part:
                    issue_type = error_part.split("(")[0].strip()
        return issue_type

    @staticmethod
    def _extract_source_file_from_finding(finding: str) -> str:
        """Extract source file from finding text."""
        source_file = "unknown.c"
        lines = finding.strip().split('\n')
        if len(lines) > 1:
            second_line = lines[1]
            if "/" in second_line and ":" in second_line:
                source_file = second_line.split(":")[0].strip()
        return source_file


class SummarizeExcelParser(BaseExcelParser):
    """Parser for summarize_justifications evaluation."""

    def _get_required_columns(self) -> List[str]:
        return ['Comment', 'Finding', 'Hint']

    def _extract_test_cases(self) -> List[Dict[str, Any]]:
        """Extract test cases for summarize evaluation."""
        test_cases = []
        skipped_count = 0

        for idx, row in self.df.iterrows():
            finding = str(row['Finding']) if pd.notna(row['Finding']) else ""
            comment = str(row['Comment']) if pd.notna(row['Comment']) else ""
            hint = str(row['Hint']) if pd.notna(row['Hint']) else ""

            if not finding or not comment or finding == "nan" or comment == "nan":
                skipped_count += 1
                continue

            if not hint or hint == "nan":
                skipped_count += 1
                continue

            issue_identifier = self._extract_issue_id_from_trace(finding)
            test_case_id = f"{self.nvr}_{idx}_{issue_identifier}"

            issue_type = self._extract_issue_type_from_finding(finding)
            source_file = self._extract_source_file_from_finding(finding)

            test_case = {
                "id": test_case_id,
                "question": json.dumps({
                    "id": test_case_id,
                    "full_justification": comment,
                    "issue_type": issue_type,
                    "severity": "MEDIUM",
                    "source_file": source_file,
                    "investigation_result": "TRUE POSITIVE"
                }),
                "expected_output": hint
            }

            test_cases.append(test_case)

        if skipped_count > 0:
            logger.info(f"  Skipped {skipped_count} rows")

        return test_cases

    def _get_default_output_filename(self) -> str:
        return str(project_root / DATASET_SUMMARIZATION_DIR / f"{self.nvr}_summarize_eval.json")


class FilterExcelParser(BaseExcelParser):
    """Parser for filter evaluation."""

    def _get_required_columns(self) -> List[str]:
        return ['False Positive?', 'Finding']

    def _extract_test_cases(self) -> List[Dict[str, Any]]:
        """Extract test cases for filter evaluation."""
        test_cases = []
        skipped_count = 0

        for idx, row in self.df.iterrows():
            finding = str(row['Finding']) if pd.notna(row['Finding']) else ""
            is_false_positive = str(row['False Positive?']) if pd.notna(row['False Positive?']) else ""

            if not finding or finding == "nan":
                skipped_count += 1
                continue

            if not is_false_positive or is_false_positive == "nan":
                skipped_count += 1
                continue

            issue_identifier = self._extract_issue_id_from_trace(finding)
            test_case_id = f"{self.nvr}_{idx}_{issue_identifier}"

            issue_type = self._extract_issue_type_from_finding(finding)
            source_file = self._extract_source_file_from_finding(finding)

            expected_classification = "FALSE_POSITIVE" if is_false_positive.strip().upper() == "YES" else "TRUE_POSITIVE"

            test_case = {
                "id": test_case_id,
                "question": json.dumps({
                    "id": test_case_id,
                    "finding": finding,
                    "issue_type": issue_type,
                    "source_file": source_file
                }),
                "expected_output": expected_classification
            }

            test_cases.append(test_case)

        if skipped_count > 0:
            logger.info(f"  Skipped {skipped_count} rows")

        return test_cases

    def _get_default_output_filename(self) -> str:
        return str(project_root / DATASET_FILTER_DIR / f"{self.nvr}_filter_eval.json")


class JudgeLLMExcelParser(BaseExcelParser):
    """Parser for judge_llm_analysis evaluation."""

    def _get_required_columns(self) -> List[str]:
        return ['Finding', 'Comment', 'AI prediction']

    def _extract_test_cases(self) -> List[Dict[str, Any]]:
        """Extract test cases for judge LLM evaluation."""
        test_cases = []
        skipped_count = 0

        for idx, row in self.df.iterrows():
            finding = str(row['Finding']) if pd.notna(row['Finding']) else ""
            comment = str(row['Comment']) if pd.notna(row['Comment']) else ""
            ai_prediction = str(row['AI prediction']) if pd.notna(row['AI prediction']) else ""

            if not finding or finding == "nan":
                skipped_count += 1
                continue

            if not comment or comment == "nan":
                skipped_count += 1
                continue

            issue_identifier = self._extract_issue_id_from_trace(finding)
            test_case_id = f"{self.nvr}_{idx}_{issue_identifier}"

            issue_type = self._extract_issue_type_from_finding(finding)
            source_file = self._extract_source_file_from_finding(finding)

            test_case = {
                "id": test_case_id,
                "question": json.dumps({
                    "id": test_case_id,
                    "issue_name": issue_type,
                    "error_description": finding,
                    "source_code_context": "",
                    "ai_prediction": ai_prediction if ai_prediction != "nan" else ""
                }),
                "expected_output": comment
            }

            test_cases.append(test_case)

        if skipped_count > 0:
            logger.info(f"  Skipped {skipped_count} rows")

        return test_cases

    def _get_default_output_filename(self) -> str:
        return str(project_root / DATASET_JUDGE_LLM_DIR / f"{self.nvr}_judge_llm_eval.json")


class BatchExcelParser:
    """Batch processor for parsing multiple Excel files."""

    def __init__(self, parser_class, ground_truth_dir: str, test_set_csv: str, output_dir: str):
        self.parser_class = parser_class
        self.ground_truth_dir = ground_truth_dir
        self.test_set_csv = test_set_csv
        self.output_dir = output_dir
        self.test_packages: Set[str] = set()

    def parse_all(self) -> None:
        """Parse all Excel files in the directory."""
        self._load_test_set()
        excel_files = self._find_excel_files()

        logger.info(f"Found {len(excel_files)} Excel files")
        logger.info(f"Output directory: {self.output_dir}")

        processed = 0
        skipped = 0

        for excel_file in sorted(excel_files):
            nvr = self._extract_nvr_from_filename(excel_file)

            if nvr.lower() not in self.test_packages:
                logger.info(f"Skipping {nvr} - not in test set")
                skipped += 1
                continue

            logger.info(f"Processing {nvr} (in test set)")

            parser = self.parser_class(excel_file, nvr, None)
            parser.output_path = os.path.join(self.output_dir, os.path.basename(parser._get_default_output_filename()))

            result = parser.parse()
            if result:
                processed += 1
            else:
                skipped += 1

        logger.info("Summary:")
        logger.info(f"  Processed: {processed} packages")
        logger.info(f"  Skipped: {skipped} packages (not in test set or invalid)")
        logger.info(f"  Output directory: {self.output_dir}")

    def _load_test_set(self) -> None:
        """Load test set package names from CSV."""
        test_df = pd.read_csv(self.test_set_csv)
        self.test_packages = set(test_df['nvr'].str.strip().str.lower())

    def _find_excel_files(self) -> List[str]:
        """Find all Excel files in the directory."""
        excel_files = []
        for filename in os.listdir(self.ground_truth_dir):
            if filename.endswith('.xlsx') and not filename.startswith('~'):
                excel_files.append(os.path.join(self.ground_truth_dir, filename))
        return excel_files

    @staticmethod
    def _extract_nvr_from_filename(excel_file: str) -> str:
        """Extract package NVR from Excel filename."""
        filename = os.path.basename(excel_file)
        nvr = filename.replace('.xlsx', '').replace('Cold start - ', '').replace('Cold start test - ', '').strip()
        return nvr


def main():
    """Main function to process Excel files."""
    import argparse

    parser = argparse.ArgumentParser(description='Parse Excel ground truth to evaluation JSON')
    parser.add_argument('--node-type', choices=['filter', 'summarize', 'judge'],
                       required=True, help='Evaluation node type')

    subparsers = parser.add_subparsers(dest='mode', help='Processing mode')

    batch_parser = subparsers.add_parser('batch', help='Batch process directory of Excel files')
    batch_parser.add_argument('--ground-truth-dir', required=True,
                             help='Directory containing Excel ground truth files')
    batch_parser.add_argument('--test-set-csv', required=True,
                             help='CSV file containing test set package names')
    batch_parser.add_argument('--output-dir', required=True,
                             help='Output directory for JSON files')

    single_parser = subparsers.add_parser('single', help='Parse single Excel file')
    single_parser.add_argument('--excel-file', required=True,
                              help='Path to Excel ground truth file')
    single_parser.add_argument('--package-name', required=True,
                              help='Package name (NVR)')
    single_parser.add_argument('--output-file', default=None,
                              help='Output JSON file path (default: auto-generated)')

    args = parser.parse_args()

    parser_classes = {
        'filter': FilterExcelParser,
        'summarize': SummarizeExcelParser,
        'judge': JudgeLLMExcelParser
    }

    parser_class = parser_classes[args.node_type]

    if args.mode == 'batch':
        batch_processor = BatchExcelParser(
            parser_class,
            args.ground_truth_dir,
            args.test_set_csv,
            args.output_dir
        )
        batch_processor.parse_all()

    elif args.mode == 'single':
        logger.info(f"Processing {args.package_name}")
        excel_parser = parser_class(args.excel_file, args.package_name, args.output_file)
        result = excel_parser.parse()

        if result:
            logger.info(f"Successfully created: {result}")
        else:
            logger.error(f"Failed to process {args.excel_file}")
            sys.exit(1)

    else:
        parser.print_help()
        sys.exit(1)


if __name__ == "__main__":
    main()
