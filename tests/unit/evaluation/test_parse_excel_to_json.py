#!/usr/bin/env python3
"""
Unit tests for parse_excel_to_json.py

Tests cover:
- Empty DataFrame handling
- Missing column validation
- JSON serialization error handling
- Output directory creation
- File/directory existence checks
- All three parser types (FilterExcelParser, SummarizeExcelParser, JudgeLLMExcelParser)
"""

import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch, MagicMock
import pandas as pd

# Add project root to path
import sys
project_root = Path(__file__).parent.parent.parent.parent
sys.path.insert(0, str(project_root))

from evaluation.utils.parse_excel_to_json import (
    BaseExcelParser,
    FilterExcelParser,
    SummarizeExcelParser,
    JudgeLLMExcelParser,
    BatchExcelParser
)


class TestFilterExcelParser(unittest.TestCase):
    """Test cases for FilterExcelParser."""

    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.test_excel = os.path.join(self.temp_dir, "test_filter.xlsx")
        self.nvr = "test-package-1.0.0"

    def tearDown(self):
        """Clean up test files."""
        import shutil
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)

    def test_valid_filter_data(self):
        """Test parsing valid filter evaluation data."""
        # Create test Excel file
        test_data = pd.DataFrame({
            'False Positive?': ['YES', 'NO', 'YES'],
            'Finding': [
                'Error: Buffer Overflow (CWE-120)\n/path/to/file.c:42',
                'Error: NULL Pointer Dereference (CWE-476)\n/path/to/main.c:100',
                'Error: Use After Free (CWE-416)\n/path/to/util.c:200'
            ],
            'Hint': ['', '', 'the error is similar to one found in the provided known issues']
        })
        test_data.to_excel(self.test_excel, index=False)

        # Parse the file
        parser = FilterExcelParser(self.test_excel, self.nvr, None)
        result = parser.parse()

        # Verify output file was created
        self.assertIsNotNone(result)
        self.assertTrue(os.path.exists(result))

        # Verify JSON content
        with open(result, 'r') as f:
            test_cases = json.load(f)

        self.assertEqual(len(test_cases), 3)

        # Verify first test case (FALSE_POSITIVE)
        self.assertIn('id', test_cases[0])
        self.assertIn('question', test_cases[0])
        self.assertIn('expected_output_obj', test_cases[0])
        self.assertEqual(test_cases[0]['expected_output_obj']['filter_result'], 'FALSE_POSITIVE')
        self.assertFalse(test_cases[0]['expected_output_obj']['has_expected_matches'])

        # Verify second test case (TRUE_POSITIVE)
        self.assertEqual(test_cases[1]['expected_output_obj']['filter_result'], 'TRUE_POSITIVE')

        # Verify third test case (has_expected_matches=True)
        self.assertTrue(test_cases[2]['expected_output_obj']['has_expected_matches'])

    def test_empty_dataframe(self):
        """Test handling of empty DataFrame."""
        # Create empty Excel file
        test_data = pd.DataFrame()
        test_data.to_excel(self.test_excel, index=False)

        parser = FilterExcelParser(self.test_excel, self.nvr, None)
        result = parser.parse()

        # Should return None for empty DataFrame
        self.assertIsNone(result)

    def test_missing_required_columns(self):
        """Test handling of missing required columns."""
        # Create Excel with missing 'Finding' column
        test_data = pd.DataFrame({
            'False Positive?': ['YES', 'NO']
        })
        test_data.to_excel(self.test_excel, index=False)

        parser = FilterExcelParser(self.test_excel, self.nvr, None)
        result = parser.parse()

        # Should return None for missing columns
        self.assertIsNone(result)

    def test_nan_values(self):
        """Test handling of NaN values in required fields."""
        # Create Excel with NaN values
        test_data = pd.DataFrame({
            'False Positive?': ['YES', None, 'NO'],
            'Finding': [
                'Error: Buffer Overflow\n/file.c:42',
                'Error: NULL Pointer\n/main.c:100',
                None
            ]
        })
        test_data.to_excel(self.test_excel, index=False)

        parser = FilterExcelParser(self.test_excel, self.nvr, None)
        result = parser.parse()

        # Should skip rows with NaN values
        self.assertIsNotNone(result)
        with open(result, 'r') as f:
            test_cases = json.load(f)

        # Only first row should be valid
        self.assertEqual(len(test_cases), 1)

    def test_custom_output_path(self):
        """Test using custom output path."""
        test_data = pd.DataFrame({
            'False Positive?': ['YES'],
            'Finding': ['Error: Test\n/test.c:1']
        })
        test_data.to_excel(self.test_excel, index=False)

        custom_output = os.path.join(self.temp_dir, "custom_output.json")
        parser = FilterExcelParser(self.test_excel, self.nvr, custom_output)
        result = parser.parse()

        self.assertEqual(result, custom_output)
        self.assertTrue(os.path.exists(custom_output))


class TestSummarizeExcelParser(unittest.TestCase):
    """Test cases for SummarizeExcelParser."""

    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.test_excel = os.path.join(self.temp_dir, "test_summarize.xlsx")
        self.nvr = "test-package-1.0.0"

    def tearDown(self):
        """Clean up test files."""
        import shutil
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)

    def test_valid_summarize_data(self):
        """Test parsing valid summarize evaluation data."""
        test_data = pd.DataFrame({
            'Comment': ['Full justification text here', 'Another justification'],
            'Finding': [
                'Error: Buffer Overflow (CWE-120)\n/path/to/file.c:42',
                'Error: NULL Pointer Dereference (CWE-476)\n/path/to/main.c:100'
            ],
            'Hint': ['Brief summary', 'Another summary']
        })
        test_data.to_excel(self.test_excel, index=False)

        parser = SummarizeExcelParser(self.test_excel, self.nvr, None)
        result = parser.parse()

        self.assertIsNotNone(result)
        self.assertTrue(os.path.exists(result))

        with open(result, 'r') as f:
            test_cases = json.load(f)

        self.assertEqual(len(test_cases), 2)

        # Verify structure
        self.assertIn('id', test_cases[0])
        self.assertIn('question', test_cases[0])
        self.assertIn('expected_output', test_cases[0])

        # Verify question JSON contains required fields
        question_obj = json.loads(test_cases[0]['question'])
        self.assertIn('full_justification', question_obj)
        self.assertIn('issue_type', question_obj)
        self.assertIn('severity', question_obj)
        self.assertEqual(question_obj['severity'], 'MEDIUM')
        self.assertEqual(question_obj['investigation_result'], 'TRUE POSITIVE')

    def test_missing_hint(self):
        """Test handling of missing hint values."""
        test_data = pd.DataFrame({
            'Comment': ['Full justification'],
            'Finding': ['Error: Test\n/test.c:1'],
            'Hint': [None]
        })
        test_data.to_excel(self.test_excel, index=False)

        parser = SummarizeExcelParser(self.test_excel, self.nvr, None)
        result = parser.parse()

        # Should return None when no valid test cases
        self.assertIsNone(result)

    def test_json_serialization_error(self):
        """Test handling of JSON serialization errors."""
        # This test verifies the try/except block around json.dumps
        test_data = pd.DataFrame({
            'Comment': ['Valid comment'],
            'Finding': ['Error: Test\n/test.c:1'],
            'Hint': ['Valid hint']
        })
        test_data.to_excel(self.test_excel, index=False)

        parser = SummarizeExcelParser(self.test_excel, self.nvr, None)

        # Mock json.dumps to raise TypeError
        with patch('evaluation.utils.parse_excel_to_json.json.dumps', side_effect=TypeError("Test error")):
            result = parser.parse()
            # Should return None when all rows fail serialization
            self.assertIsNone(result)


class TestJudgeLLMExcelParser(unittest.TestCase):
    """Test cases for JudgeLLMExcelParser."""

    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.test_excel = os.path.join(self.temp_dir, "test_judge.xlsx")
        self.nvr = "test-package-1.0.0"

    def tearDown(self):
        """Clean up test files."""
        import shutil
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)

    def test_valid_judge_data(self):
        """Test parsing valid judge LLM evaluation data."""
        test_data = pd.DataFrame({
            'Finding': [
                'Error: Buffer Overflow (CWE-120)\n/path/to/file.c:42',
                'Error: NULL Pointer Dereference (CWE-476)\n/path/to/main.c:100'
            ],
            'Comment': ['Analysis comment 1', 'Analysis comment 2'],
            'AI prediction': ['Prediction 1', 'Prediction 2']
        })
        test_data.to_excel(self.test_excel, index=False)

        parser = JudgeLLMExcelParser(self.test_excel, self.nvr, None)
        result = parser.parse()

        self.assertIsNotNone(result)
        self.assertTrue(os.path.exists(result))

        with open(result, 'r') as f:
            test_cases = json.load(f)

        self.assertEqual(len(test_cases), 2)

        # Verify structure
        question_obj = json.loads(test_cases[0]['question'])
        self.assertIn('issue_name', question_obj)
        self.assertIn('error_description', question_obj)
        self.assertIn('ai_prediction', question_obj)

    def test_missing_ai_prediction(self):
        """Test handling of missing AI prediction."""
        test_data = pd.DataFrame({
            'Finding': ['Error: Test\n/test.c:1'],
            'Comment': ['Comment'],
            'AI prediction': [None]
        })
        test_data.to_excel(self.test_excel, index=False)

        parser = JudgeLLMExcelParser(self.test_excel, self.nvr, None)
        result = parser.parse()

        # Should still create test case with empty ai_prediction
        self.assertIsNotNone(result)
        with open(result, 'r') as f:
            test_cases = json.load(f)

        self.assertEqual(len(test_cases), 1)
        question_obj = json.loads(test_cases[0]['question'])
        self.assertEqual(question_obj['ai_prediction'], '')


class TestBatchExcelParser(unittest.TestCase):
    """Test cases for BatchExcelParser."""

    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.ground_truth_dir = os.path.join(self.temp_dir, "ground_truth")
        self.output_dir = os.path.join(self.temp_dir, "output")
        self.test_set_csv = os.path.join(self.temp_dir, "test_set.csv")
        os.makedirs(self.ground_truth_dir)

    def tearDown(self):
        """Clean up test files."""
        import shutil
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)

    def test_batch_processing_with_test_set(self):
        """Test batch processing with test set filter."""
        # Create test set CSV
        test_set = pd.DataFrame({
            'nvr': ['package-1.0.0', 'package-2.0.0']
        })
        test_set.to_csv(self.test_set_csv, index=False)

        # Create Excel files
        for pkg in ['package-1.0.0', 'package-2.0.0', 'package-3.0.0']:
            excel_file = os.path.join(self.ground_truth_dir, f"{pkg}.xlsx")
            test_data = pd.DataFrame({
                'False Positive?': ['YES'],
                'Finding': ['Error: Test\n/test.c:1']
            })
            test_data.to_excel(excel_file, index=False)

        # Run batch processor
        batch = BatchExcelParser(
            FilterExcelParser,
            self.ground_truth_dir,
            self.test_set_csv,
            self.output_dir
        )
        batch.parse_all()

        # Verify only test set packages were processed
        output_files = os.listdir(self.output_dir)
        self.assertEqual(len(output_files), 2)

    def test_missing_test_set_csv(self):
        """Test handling of missing test set CSV."""
        # Don't create test set CSV
        excel_file = os.path.join(self.ground_truth_dir, "package-1.0.0.xlsx")
        test_data = pd.DataFrame({
            'False Positive?': ['YES'],
            'Finding': ['Error: Test\n/test.c:1']
        })
        test_data.to_excel(excel_file, index=False)

        # Should process all files when test set is missing
        batch = BatchExcelParser(
            FilterExcelParser,
            self.ground_truth_dir,
            "/nonexistent/test_set.csv",
            self.output_dir
        )
        batch.parse_all()

        # Should still complete without error
        self.assertTrue(True)

    def test_missing_ground_truth_directory(self):
        """Test handling of missing ground truth directory."""
        batch = BatchExcelParser(
            FilterExcelParser,
            "/nonexistent/directory",
            self.test_set_csv,
            self.output_dir
        )
        batch.parse_all()

        # Should complete gracefully
        self.assertTrue(True)

    def test_nvr_extraction(self):
        """Test NVR extraction from filename."""
        test_cases = [
            ("package-1.0.0.xlsx", "package-1.0.0"),
            ("Cold start - package-1.0.0.xlsx", "package-1.0.0"),
            ("Cold start test - package-1.0.0.xlsx", "package-1.0.0"),
        ]

        for filename, expected_nvr in test_cases:
            nvr = BatchExcelParser._extract_nvr_from_filename(filename)
            self.assertEqual(nvr, expected_nvr)


class TestBaseExcelParserHelpers(unittest.TestCase):
    """Test cases for BaseExcelParser helper methods."""

    def test_extract_issue_id_from_trace(self):
        """Test issue ID extraction from finding trace."""
        finding = """Error: Buffer Overflow (CWE-120)
/path/to/vulnerable/file.c:42: Memory access error
Details here"""

        issue_id = BaseExcelParser._extract_issue_id_from_trace(finding)
        self.assertIn("Buffer Overflow", issue_id)
        self.assertIn("file.c", issue_id)

    def test_extract_issue_type_from_finding(self):
        """Test issue type extraction."""
        finding = "Error: NULL Pointer Dereference (CWE-476)\n/file.c:100"
        issue_type = BaseExcelParser._extract_issue_type_from_finding(finding)
        self.assertEqual(issue_type, "NULL Pointer Dereference")

    def test_extract_source_file_from_finding(self):
        """Test source file extraction."""
        finding = "Error: Use After Free (CWE-416)\n/path/to/util.c:200: Error details"
        source_file = BaseExcelParser._extract_source_file_from_finding(finding)
        self.assertEqual(source_file, "/path/to/util.c")

    def test_empty_finding_extraction(self):
        """Test extraction from empty finding."""
        issue_id = BaseExcelParser._extract_issue_id_from_trace("")
        self.assertEqual(issue_id, "UNKNOWN_unknown")

        source_file = BaseExcelParser._extract_source_file_from_finding("")
        self.assertEqual(source_file, "unknown.c")


class TestOutputDirectoryCreation(unittest.TestCase):
    """Test cases for output directory creation and permission handling."""

    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.test_excel = os.path.join(self.temp_dir, "test.xlsx")
        self.nvr = "test-package-1.0.0"

    def tearDown(self):
        """Clean up test files."""
        import shutil
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)

    def test_output_directory_created(self):
        """Test that output directory is created if it doesn't exist."""
        test_data = pd.DataFrame({
            'False Positive?': ['YES'],
            'Finding': ['Error: Test\n/test.c:1']
        })
        test_data.to_excel(self.test_excel, index=False)

        nested_output = os.path.join(self.temp_dir, "nested", "dir", "output.json")
        parser = FilterExcelParser(self.test_excel, self.nvr, nested_output)
        result = parser.parse()

        self.assertIsNotNone(result)
        self.assertTrue(os.path.exists(nested_output))
        self.assertTrue(os.path.exists(os.path.dirname(nested_output)))

    def test_permission_error_handling(self):
        """Test handling of permission errors during output."""
        test_data = pd.DataFrame({
            'False Positive?': ['YES'],
            'Finding': ['Error: Test\n/test.c:1']
        })
        test_data.to_excel(self.test_excel, index=False)

        parser = FilterExcelParser(self.test_excel, self.nvr, None)

        # Mock os.makedirs to raise PermissionError
        with patch('os.makedirs', side_effect=PermissionError("No permission")):
            result = parser.parse()
            # Should return None when permission denied
            self.assertIsNone(result)


if __name__ == '__main__':
    unittest.main()
