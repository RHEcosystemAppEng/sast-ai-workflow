"""
Unit tests for input format detection logic in the report writers system.

These tests specifically focus on the _is_sarif_input method and related
input format detection functionality that drives the dual output decision.
"""

import json
import os
import tempfile
import unittest
from unittest.mock import Mock, mock_open, patch

from common.config import Config
from dto.EvaluationSummary import EvaluationSummary
from report_writers.report_builder import ReportBuilder


class TestInputFormatDetection(unittest.TestCase):
    """Test cases for input format detection logic."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_config = Mock(spec=Config)
        self.sample_data = []
        self.evaluation_summary = Mock(spec=EvaluationSummary)

        # Create builder instance for testing
        self.builder = ReportBuilder(self.sample_data, self.evaluation_summary, self.mock_config)

    def test_given_sarif_extension_when_detecting_then_returns_true(self):
        """Test that .sarif extension is correctly detected with various path formats and cases."""
        test_cases = [
            # Basic paths
            "/workspace/input/report.sarif",
            "/home/user/scans/results.sarif",
            "C:\\Reports\\analysis.sarif",
            "./local/scan.sarif",
            "../parent/findings.sarif",
            "report.sarif",  # Just filename
            # Case variations
            "/workspace/input/report.SARIF",
            "/workspace/input/report.Sarif",
            "/workspace/input/report.sArIf",
            # Complex paths
            "/workspace/input/sub dir/report with spaces.sarif",
            "/workspace/input/report-with-dashes.sarif",
            "/workspace/input/report_with_underscores.sarif",
            "/workspace/input/report.v2.sarif",
            "/workspace/input/2023-12-25-scan-results.sarif",
            "/workspace/input/project.name.scan.sarif",
        ]

        for sarif_path in test_cases:
            with self.subTest(sarif_path=sarif_path):
                self.mock_config.INPUT_REPORT_FILE_PATH = sarif_path
                self.assertTrue(
                    self.builder._is_sarif_input(),
                    f"Failed to detect SARIF format for: {sarif_path}",
                )

    def test_given_non_sarif_extension_when_detecting_then_returns_false(self):
        """Test that non-SARIF extensions are correctly identified, including case variations."""
        test_cases = [
            # Various file types
            "/workspace/input/report.html",
            "/workspace/input/report.xlsx",
            "/workspace/input/report.csv",
            "/workspace/input/report.txt",
            "/workspace/input/report.xml",
            "/workspace/input/report.pdf",
            "/workspace/input/report",  # No extension
            "/workspace/input/report.json.backup",  # Multiple extensions
            # Case variations of non-SARIF extensions
            "/workspace/input/report.HTML",
            "/workspace/input/report.Html",
            "/workspace/input/report.XLSX",
        ]

        for non_sarif_path in test_cases:
            with self.subTest(non_sarif_path=non_sarif_path):
                self.mock_config.INPUT_REPORT_FILE_PATH = non_sarif_path
                self.assertFalse(
                    self.builder._is_sarif_input(),
                    f"Incorrectly detected SARIF format for: {non_sarif_path}",
                )

    def test_given_url_input_when_detecting_then_returns_false(self):
        """Test that URL inputs are not considered SARIF regardless of extension."""
        url_test_cases = [
            # URLs with SARIF extensions should still return False
            "https://example.com/report.sarif",
            "http://scanner.com/results.sarif",
            "ftp://files.company.com/report.sarif",
            # URLs with other extensions
            "https://docs.google.com/spreadsheets/d/123/edit",
            "http://internal.company.com/scans/report.html",
            "https://api.scanner.com/reports/456.json",
            # URLs with JSON that might contain SARIF
            "https://api.example.com/sarif-report.json",
        ]

        for url in url_test_cases:
            with self.subTest(url=url):
                self.mock_config.INPUT_REPORT_FILE_PATH = url
                self.assertFalse(
                    self.builder._is_sarif_input(), f"URL should not be detected as SARIF: {url}"
                )

    def test_given_invalid_paths_when_detecting_then_returns_false(self):
        """Test that None, empty, and invalid paths are handled correctly."""
        test_cases = [
            None,
            "",
            "   ",  # Whitespace only
            "\t\n",  # Other whitespace
        ]

        for invalid_path in test_cases:
            with self.subTest(invalid_path=repr(invalid_path)):
                self.mock_config.INPUT_REPORT_FILE_PATH = invalid_path
                self.assertFalse(
                    self.builder._is_sarif_input(),
                    f"Invalid path should not be detected as SARIF: {invalid_path}",
                )

    @patch("builtins.open", new_callable=mock_open, read_data='{"version": "2.1.0", "runs": []}')
    def test_given_json_with_sarif_content_when_detecting_then_returns_true(self, mock_file):
        """Test that .json files with SARIF content are detected, including case variations."""
        # Test different JSON file extensions with case variations
        json_paths = [
            "/workspace/input/report.json",
            "/workspace/input/report.JSON",
            "/workspace/input/report.Json",
            "/workspace/input/report.jSoN",
        ]

        sarif_content_cases = [
            '{"version": "2.1.0", "runs": []}',
            '{"runs": [], "version": "2.1.0"}',  # Different order
            '{\n  "version": "2.1.0",\n  "runs": [\n    {}\n  ]\n}',  # Formatted JSON
            '{"version":"2.1.0","runs":[],"$schema":"https://json.schemastore.org/sarif-2.1.0.json"}',  # noqa
        ]

        for json_path in json_paths:
            for content in sarif_content_cases:
                with self.subTest(path=json_path, content=content[:30] + "..."):
                    mock_file.return_value.read.return_value = content
                    self.mock_config.INPUT_REPORT_FILE_PATH = json_path

                    self.assertTrue(
                        self.builder._is_sarif_input(),
                        f"Failed to detect SARIF content in {json_path}",
                    )

                    # Verify file was opened correctly
                    mock_file.assert_called_with(json_path, "r", encoding="utf-8")

    @patch("builtins.open", new_callable=mock_open)
    def test_given_json_with_non_sarif_content_when_detecting_then_returns_false(self, mock_file):
        """Test that .json files with non-SARIF content are not detected as SARIF."""
        non_sarif_content_cases = [
            '{"results": [], "metadata": {}}',  # Missing version and runs
            '{"version": "1.0", "data": []}',  # Wrong version format, no runs
            '{"runs": []}',  # Missing version
            '{"version": "2.1.0"}',  # Missing runs
            '{"tool": "scanner", "findings": []}',  # Different structure
            "[]",  # Array instead of object
            '"string content"',  # String instead of object
            "123",  # Number instead of object
        ]

        # Test with different JSON file case variations
        json_paths = [
            "/workspace/input/report.json",
            "/workspace/input/report.JSON",
        ]

        for json_path in json_paths:
            for content in non_sarif_content_cases:
                with self.subTest(path=json_path, content=content[:30] + "..."):
                    mock_file.return_value.read.return_value = content
                    self.mock_config.INPUT_REPORT_FILE_PATH = json_path

                    self.assertFalse(
                        self.builder._is_sarif_input(),
                        f"Incorrectly detected non-SARIF content as SARIF in {json_path}",
                    )

    @patch("builtins.open", side_effect=PermissionError("Permission denied"))
    def test_given_json_with_permission_error_when_detecting_then_returns_false(self, mock_file):
        """Test that .json files with permission errors are not detected as SARIF."""
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/protected.json"

        self.assertFalse(
            self.builder._is_sarif_input(),
            "JSON file with permission error should not be detected as SARIF",
        )

    @patch("builtins.open", new_callable=mock_open, read_data="invalid json content")
    def test_given_json_with_invalid_content_when_detecting_then_returns_false(self, mock_file):
        """Test that .json files with invalid JSON are not detected as SARIF."""
        invalid_json_cases = [
            "invalid json content",
            '{"incomplete": json',
            "{malformed json}",
            '{"key": value}',  # Unquoted value
            "{'single': 'quotes'}",  # Single quotes
            "",  # Empty file
        ]

        for invalid_content in invalid_json_cases:
            with self.subTest(invalid_content=invalid_content):
                mock_file.return_value.read.return_value = invalid_content
                self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/invalid.json"

                self.assertFalse(
                    self.builder._is_sarif_input(),
                    f"Invalid JSON should not be detected as SARIF: {invalid_content}",
                )

    @patch("builtins.open", side_effect=FileNotFoundError("File not found"))
    def test_given_missing_json_file_when_detecting_then_returns_false(self, mock_file):
        """Test that missing .json files are not detected as SARIF (mocked version)."""
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/missing.json"

        self.assertFalse(
            self.builder._is_sarif_input(), "Missing JSON file should not be detected as SARIF"
        )

        # Verify file was attempted to be opened
        mock_file.assert_called_once_with("/workspace/input/missing.json", "r", encoding="utf-8")

    @patch(
        "builtins.open", side_effect=UnicodeDecodeError("utf-8", b"", 0, 1, "invalid start byte")
    )
    def test_given_json_with_encoding_error_when_detecting_then_returns_false(self, mock_file):
        """Test that .json files with encoding errors are not detected as SARIF."""
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/binary.json"

        self.assertFalse(
            self.builder._is_sarif_input(),
            "JSON file with encoding error should not be detected as SARIF",
        )

    def test_given_edge_case_extensions_when_detecting_then_handles_correctly(self):
        """Test edge cases with unusual but valid extensions."""
        edge_cases = [
            # Should be False (not ending with .sarif)
            ("/workspace/input/report.sarif.bak", False),
            ("/workspace/input/report.sarif.tmp", False),
            ("/workspace/input/report.sarif.backup", False),
            # Should be True (ending with .sarif)
            ("/workspace/input/report.json.sarif", True),
            ("/workspace/input/report.backup.sarif", True),
            ("/workspace/input/report.v1.sarif", True),
        ]

        for edge_case, expected_result in edge_cases:
            with self.subTest(edge_case=edge_case):
                self.mock_config.INPUT_REPORT_FILE_PATH = edge_case
                actual_result = self.builder._is_sarif_input()

                self.assertEqual(
                    actual_result, expected_result, f"Edge case detection failed for: {edge_case}"
                )


class TestInputDetectionIntegration(unittest.TestCase):
    """Integration tests for input detection with real file operations."""

    def setUp(self):
        """Set up test fixtures with temporary files."""
        self.temp_dir = tempfile.mkdtemp()

        self.mock_config = Mock(spec=Config)
        self.sample_data = []
        self.evaluation_summary = Mock(spec=EvaluationSummary)
        self.builder = ReportBuilder(self.sample_data, self.evaluation_summary, self.mock_config)

    def tearDown(self):
        """Clean up temporary files."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_given_real_sarif_file_when_detecting_then_returns_true(self):
        """Integration test with real SARIF file."""
        # Create real SARIF file
        sarif_path = os.path.join(self.temp_dir, "real_report.sarif")
        sarif_content = {
            "version": "2.1.0",
            "runs": [{"tool": {"driver": {"name": "test-scanner"}}, "results": []}],
        }

        with open(sarif_path, "w") as f:
            json.dump(sarif_content, f)

        self.mock_config.INPUT_REPORT_FILE_PATH = sarif_path
        self.assertTrue(self.builder._is_sarif_input())

    def test_given_real_json_with_sarif_when_detecting_then_returns_true(self):
        """Integration test with real JSON file containing SARIF."""
        # Create real JSON file with SARIF content
        json_path = os.path.join(self.temp_dir, "sarif_report.json")
        sarif_content = {
            "version": "2.1.0",
            "runs": [
                {
                    "tool": {"driver": {"name": "test-scanner"}},
                    "results": [{"ruleId": "test-rule", "message": {"text": "Test finding"}}],
                }
            ],
        }

        with open(json_path, "w") as f:
            json.dump(sarif_content, f)

        self.mock_config.INPUT_REPORT_FILE_PATH = json_path
        self.assertTrue(self.builder._is_sarif_input())

    def test_given_real_json_with_non_sarif_when_detecting_then_returns_false(self):
        """Integration test with real JSON file containing non-SARIF data."""
        # Create real JSON file with non-SARIF content
        json_path = os.path.join(self.temp_dir, "other_report.json")
        non_sarif_content = {
            "tool": "scanner",
            "results": [{"id": 1, "severity": "high", "description": "Issue found"}],
            "metadata": {"scan_date": "2023-12-25"},
        }

        with open(json_path, "w") as f:
            json.dump(non_sarif_content, f)

        self.mock_config.INPUT_REPORT_FILE_PATH = json_path
        self.assertFalse(self.builder._is_sarif_input())

    def test_given_real_html_file_when_detecting_then_returns_false(self):
        """Integration test with real HTML file."""
        # Create real HTML file
        html_path = os.path.join(self.temp_dir, "report.html")
        html_content = """
        <!DOCTYPE html>
        <html>
        <head><title>SAST Report</title></head>
        <body>
            <h1>Security Analysis Results</h1>
            <p>No issues found.</p>
        </body>
        </html>
        """

        with open(html_path, "w") as f:
            f.write(html_content)

        self.mock_config.INPUT_REPORT_FILE_PATH = html_path
        self.assertFalse(self.builder._is_sarif_input())

    def test_given_missing_file_when_detecting_then_handles_correctly(self):
        """Integration test with missing files - behavior differs by extension."""
        # Missing JSON files return False (can't verify SARIF content)
        missing_json = os.path.join(self.temp_dir, "missing.json")
        self.mock_config.INPUT_REPORT_FILE_PATH = missing_json
        self.assertFalse(
            self.builder._is_sarif_input(), "Missing JSON file should not be detected as SARIF"
        )

        # Missing SARIF files return True (extension indicates SARIF format)
        missing_sarif = os.path.join(self.temp_dir, "missing.sarif")
        self.mock_config.INPUT_REPORT_FILE_PATH = missing_sarif
        self.assertTrue(
            self.builder._is_sarif_input(),
            "Missing SARIF file should still be detected as SARIF format",
        )

    def test_given_empty_file_when_detecting_then_returns_false(self):
        """Integration test with empty file."""
        empty_path = os.path.join(self.temp_dir, "empty.json")

        # Create empty file
        with open(empty_path, "w"):
            pass

        self.mock_config.INPUT_REPORT_FILE_PATH = empty_path
        self.assertFalse(self.builder._is_sarif_input())


if __name__ == "__main__":
    unittest.main()
