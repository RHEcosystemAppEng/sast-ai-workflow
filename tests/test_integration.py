import json
import os
import sys
import tempfile
import unittest
from unittest.mock import Mock, patch

# Add src to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

from fixtures import SAMPLE_HTML, SAMPLE_SARIF

from common.config import Config
from dto.Issue import Issue
from ReportReader import read_sast_report


class TestReportReaderIntegration(unittest.TestCase):
    """Integration tests for the complete report reading workflow"""

    def setUp(self):
        """Set up test environment"""
        self.config = Mock(spec=Config)
        self.config.SERVICE_ACCOUNT_JSON_PATH = "/path/to/service_account.json"

    def test_given_sarif_report_file_when_reading_end_to_end_then_returns_parsed_issues(self):
        """Test complete SARIF report reading workflow"""
        with tempfile.NamedTemporaryFile(suffix=".sarif", mode="w", delete=False) as temp_file:
            json.dump(SAMPLE_SARIF, temp_file)
            temp_file.flush()

            try:
                self.config.INPUT_REPORT_FILE_PATH = temp_file.name

                issues = read_sast_report(self.config)

                # Verify results
                self.assertEqual(len(issues), 3)

                # Check first issue
                issue1 = issues[0]
                self.assertEqual(issue1.id, "def1")
                self.assertTrue(
                    "RESOURCE_LEAK" in issue1.issue_type or "Resource Leak" in issue1.issue_type
                )
                self.assertEqual(issue1.issue_cwe, "CWE-772")
                self.assertIn("full_url", issue1.trace)

                # Check second issue
                issue2 = issues[1]
                self.assertEqual(issue2.id, "def2")
                self.assertIn("SQL injection vulnerability", issue2.trace)

            finally:
                os.unlink(temp_file.name)

    def test_given_html_report_file_when_reading_end_to_end_then_returns_parsed_issues(self):
        """Test complete HTML report reading workflow"""
        with tempfile.NamedTemporaryFile(suffix=".html", mode="w", delete=False) as temp_file:
            temp_file.write(SAMPLE_HTML)
            temp_file.flush()

            try:
                self.config.INPUT_REPORT_FILE_PATH = temp_file.name

                issues = read_sast_report(self.config)

                # Verify results
                self.assertEqual(len(issues), 3)

                # Check first issue
                issue1 = issues[0]
                self.assertEqual(issue1.id, "def1")
                self.assertEqual(issue1.issue_type, "RESOURCE_LEAK")
                self.assertEqual(issue1.issue_cwe, "CWE-772")
                self.assertIn("full_url", issue1.trace)

                # Check second issue
                issue2 = issues[1]
                self.assertEqual(issue2.id, "def2")
                self.assertEqual(issue2.issue_type, "SQL_INJECTION")
                self.assertEqual(issue2.issue_cwe, "CWE-89")

            finally:
                os.unlink(temp_file.name)

    @patch(
        "report_readers.google_sheets_reader.GoogleSheetsReportReader."
        "_read_sast_report_google_sheet"
    )
    def test_given_google_sheets_url_when_reading_end_to_end_then_returns_parsed_issues(
        self, mock_read_google_sheet
    ):
        """Test complete Google Sheets report reading workflow"""
        # Mock the expected issues that would be returned by the Google Sheets reader
        mock_issues = [Issue("def1"), Issue("def2")]

        # Set up the mock issues with expected data
        mock_issues[0].issue_type = "BUFFER_OVERFLOW"
        mock_issues[0].issue_cwe = "CWE-120"
        mock_issues[0].issue_cwe_link = "https://cwe.mitre.org/data/definitions/120.html"
        mock_issues[0].trace = (
            "Error: BUFFER_OVERFLOW CWE-120\nBuffer overflow in main() function\n"
            "Stack shows memory corruption"
        )

        mock_issues[1].issue_type = "XSS"
        mock_issues[1].issue_cwe = "CWE-79"
        mock_issues[1].issue_cwe_link = "https://cwe.mitre.org/data/definitions/79.html"
        mock_issues[1].trace = (
            "Error: XSS CWE-79\nCross-site scripting vulnerability\nInput not escaped properly"
        )

        # Mock the internal method to return our test issues
        mock_read_google_sheet.return_value = mock_issues

        # Test Google Sheets URL
        sheets_url = "https://docs.google.com/spreadsheets/d/test123/edit"
        self.config.INPUT_REPORT_FILE_PATH = sheets_url

        issues = read_sast_report(self.config)

        # Verify results
        self.assertEqual(len(issues), 2)

        # Check first issue
        issue1 = issues[0]
        self.assertEqual(issue1.id, "def1")
        self.assertEqual(issue1.issue_type, "BUFFER_OVERFLOW")
        self.assertEqual(issue1.issue_cwe, "CWE-120")
        self.assertIn("Buffer overflow in main()", issue1.trace)

        # Check second issue
        issue2 = issues[1]
        self.assertEqual(issue2.id, "def2")
        self.assertEqual(issue2.issue_type, "XSS")
        self.assertEqual(issue2.issue_cwe, "CWE-79")

        # Verify the mock was called with correct parameters
        mock_read_google_sheet.assert_called_once_with(
            self.config.SERVICE_ACCOUNT_JSON_PATH, sheets_url
        )

    def test_given_unsupported_file_format_when_reading_report_then_raises_value_error(self):
        """Test handling of unsupported file formats"""
        with tempfile.NamedTemporaryFile(suffix=".unknown", mode="w", delete=False) as temp_file:
            temp_file.write("unsupported format content")
            temp_file.flush()

            try:
                self.config.INPUT_REPORT_FILE_PATH = temp_file.name

                with self.assertRaises(ValueError) as context:
                    read_sast_report(self.config)

                self.assertIn("No suitable reader found", str(context.exception))

            finally:
                os.unlink(temp_file.name)

    def test_given_nonexistent_file_when_reading_report_then_raises_value_error(self):
        """Test handling of non-existent files"""
        self.config.INPUT_REPORT_FILE_PATH = "/nonexistent/file.sarif"

        with self.assertRaises(ValueError) as context:
            read_sast_report(self.config)

        self.assertIn("No suitable reader found", str(context.exception))

    def test_given_json_file_with_sarif_content_when_reading_then_handeled_as_sarif(self):
        """Test that SARIF reader takes priority for .json files with SARIF content"""
        # Create a .json file with SARIF content
        with tempfile.NamedTemporaryFile(suffix=".json", mode="w", delete=False) as temp_file:
            json.dump(SAMPLE_SARIF, temp_file)
            temp_file.flush()

            try:
                self.config.INPUT_REPORT_FILE_PATH = temp_file.name

                issues = read_sast_report(self.config)

                # Should be processed by SARIF reader
                self.assertEqual(len(issues), 3)
                # Check for SARIF-specific processing (rule names in issue type)
                self.assertTrue(
                    "RESOURCE_LEAK" in issues[0].issue_type
                    or "Resource Leak" in issues[0].issue_type
                )

            finally:
                os.unlink(temp_file.name)

    def test_given_malformed_sarif_file_when_reading_report_then_handles_gracefully(self):
        """Test handling of malformed SARIF files"""
        malformed_sarif = {
            "version": "2.1.0",
            "runs": [
                {
                    "results": [
                        {
                            # Missing required fields
                            "message": {"text": "Incomplete result"}
                        }
                    ]
                }
            ],
        }

        with tempfile.NamedTemporaryFile(suffix=".sarif", mode="w", delete=False) as temp_file:
            json.dump(malformed_sarif, temp_file)
            temp_file.flush()

            try:
                self.config.INPUT_REPORT_FILE_PATH = temp_file.name

                # Should handle gracefully without crashing
                issues = read_sast_report(self.config)

                # Might return empty list or partial results
                self.assertIsInstance(issues, list)

            finally:
                os.unlink(temp_file.name)

    def test_given_empty_report_files_when_reading_then_returns_empty_list(self):
        """Test handling of empty report files"""
        # Empty SARIF
        empty_sarif = {"version": "2.1.0", "runs": []}

        with tempfile.NamedTemporaryFile(suffix=".sarif", mode="w", delete=False) as temp_file:
            json.dump(empty_sarif, temp_file)
            temp_file.flush()

            try:
                self.config.INPUT_REPORT_FILE_PATH = temp_file.name

                issues = read_sast_report(self.config)
                self.assertEqual(len(issues), 0)

            finally:
                os.unlink(temp_file.name)

        # Empty HTML
        empty_html = "<html><body></body></html>"

        with tempfile.NamedTemporaryFile(suffix=".html", mode="w", delete=False) as temp_file:
            temp_file.write(empty_html)
            temp_file.flush()

            try:
                self.config.INPUT_REPORT_FILE_PATH = temp_file.name

                issues = read_sast_report(self.config)
                self.assertEqual(len(issues), 0)

            finally:
                os.unlink(temp_file.name)

    def test_given_large_report_with_many_issues_when_reading_then_handles_all_issues(self):
        """Test handling of reports with many issues"""
        # Create a SARIF with multiple results
        large_sarif = {
            "version": "2.1.0",
            "runs": [{"tool": {"driver": {"name": "TestTool", "version": "1.0.0"}}, "results": []}],
        }

        # Add 50 results
        for i in range(50):
            result = {
                "ruleId": f"RULE-{i:03d}",
                "message": {"text": f"Issue number {i}"},
                "level": "warning",
                "locations": [
                    {
                        "physicalLocation": {
                            "artifactLocation": {"uri": f"file{i}.c"},
                            "region": {"startLine": i + 1},
                        }
                    }
                ],
            }
            large_sarif["runs"][0]["results"].append(result)

        with tempfile.NamedTemporaryFile(suffix=".sarif", mode="w", delete=False) as temp_file:
            json.dump(large_sarif, temp_file)
            temp_file.flush()

            try:
                self.config.INPUT_REPORT_FILE_PATH = temp_file.name

                issues = read_sast_report(self.config)

                # Should handle all 50 issues
                self.assertEqual(len(issues), 50)

                # Check ID generation
                self.assertEqual(issues[0].id, "def1")
                self.assertEqual(issues[49].id, "def50")

            finally:
                os.unlink(temp_file.name)

    def test_given_invalid_config_when_reading_report_then_raises_value_error(self):
        """Test validation of config object"""
        # Missing INPUT_REPORT_FILE_PATH
        invalid_config = Mock(spec=Config)
        invalid_config.INPUT_REPORT_FILE_PATH = None

        with self.assertRaises(ValueError) as context:
            read_sast_report(invalid_config)

        self.assertIn("No suitable reader found", str(context.exception))

    def test_given_file_path_with_special_characters_when_reading_then_handles_correctly(self):
        """Test handling of file paths with special characters"""
        # Create file with special characters in name
        special_content = SAMPLE_SARIF

        with tempfile.NamedTemporaryFile(
            suffix="_test-file[1].sarif", mode="w", delete=False
        ) as temp_file:
            json.dump(special_content, temp_file)
            temp_file.flush()

            try:
                self.config.INPUT_REPORT_FILE_PATH = temp_file.name

                issues = read_sast_report(self.config)

                # Should handle special characters in filename
                self.assertEqual(len(issues), 3)

            finally:
                os.unlink(temp_file.name)


class TestErrorRecovery(unittest.TestCase):
    """Test error recovery and resilience"""

    def setUp(self):
        """Set up test environment"""
        self.config = Mock(spec=Config)

    def test_given_reader_instantiation_failure_when_reading_report_then_raises_value_error(self):
        """Test handling when reader instantiation fails"""
        # This test verifies the factory's error handling
        with tempfile.NamedTemporaryFile(suffix=".unknown", delete=False) as temp_file:
            temp_file.write(b"unknown content")
            temp_file.flush()

            try:
                self.config.INPUT_REPORT_FILE_PATH = temp_file.name

                with self.assertRaises(ValueError):
                    read_sast_report(self.config)

            finally:
                os.unlink(temp_file.name)


class TestDataConsistency(unittest.TestCase):
    """Test data consistency across different report formats"""

    def setUp(self):
        """Set up test environment"""
        self.config = Mock(spec=Config)
        self.config.SERVICE_ACCOUNT_JSON_PATH = "service_account.json"

        # Reference vulnerability data that should be consistent across formats
        # This matches the first finding in our fixtures (Resource Leak)
        self.reference_vuln = {
            "id": "def1",
            "type": "RESOURCE_LEAK",
            "cve": "CWE-772",
            "file": "src/file.c",
            "line": 946,
            "message": 'Variable "full_url" going out of scope leaks the storage it points to',
        }

    @patch("report_readers.google_sheets_reader.get_google_sheet")
    def test_given_same_vulnerability_across_formats_when_reading_then_produces_consistent_data(
        self, mock_get_sheet
    ):
        """Test that all readers produce consistent Issue data for the same vulnerability"""
        from fixtures import SAMPLE_HTML, SAMPLE_SARIF, SAMPLE_SHEET_DATA

        # Use fixture data directly
        sarif_data = SAMPLE_SARIF
        html_data = SAMPLE_HTML

        # Mock Google Sheets to return fixture data
        mock_sheet = Mock()
        mock_sheet.get_all_records.return_value = SAMPLE_SHEET_DATA
        mock_get_sheet.return_value = mock_sheet

        # 4. Create temporary files and test each format
        sarif_issues = []
        html_issues = []
        sheets_issues = []

        # Test SARIF
        with tempfile.NamedTemporaryFile(suffix=".sarif", mode="w", delete=False) as temp_file:
            json.dump(sarif_data, temp_file)
            temp_file.flush()

            try:
                self.config.INPUT_REPORT_FILE_PATH = temp_file.name
                sarif_issues = read_sast_report(self.config)
            finally:
                os.unlink(temp_file.name)

        # Test HTML
        with tempfile.NamedTemporaryFile(suffix=".html", mode="w", delete=False) as temp_file:
            temp_file.write(html_data)
            temp_file.flush()

            try:
                self.config.INPUT_REPORT_FILE_PATH = temp_file.name
                html_issues = read_sast_report(self.config)
            finally:
                os.unlink(temp_file.name)

        # Test Google Sheets
        sheets_url = "https://docs.google.com/spreadsheets/d/test123/edit"
        self.config.INPUT_REPORT_FILE_PATH = sheets_url
        sheets_issues = read_sast_report(self.config)

        # 5. Validate all readers found the expected issues
        self.assertEqual(len(sarif_issues), 3, "SARIF reader should find exactly three issues")
        self.assertEqual(len(html_issues), 3, "HTML reader should find exactly three issues")
        self.assertEqual(
            len(sheets_issues), 3, "Google Sheets reader should find exactly three issues"
        )

        # Focus on the first issue (buffer overflow) for consistency testing
        sarif_issue = sarif_issues[0]
        html_issue = html_issues[0]
        sheets_issue = sheets_issues[0]

        # 6. Compare core issue data across formats

        # Issue IDs should be consistent
        self.assertEqual(
            sarif_issue.id, self.reference_vuln["id"], "SARIF issue ID should match reference"
        )
        self.assertEqual(
            html_issue.id, self.reference_vuln["id"], "HTML issue ID should match reference"
        )
        self.assertEqual(
            sheets_issue.id, self.reference_vuln["id"], "Sheets issue ID should match reference"
        )

        # Issue types should be consistent (allowing for format variations)
        expected_type_variations = [
            self.reference_vuln["type"],  # "RESOURCE_LEAK"
            "leaked_storage",  # SARIF might use the specific rule name
            "Resource Leak",  # Alternative formatting
        ]

        self.assertTrue(
            any(var in sarif_issue.issue_type for var in expected_type_variations),
            f"SARIF issue type '{sarif_issue.issue_type}' should contain expected type",
        )
        self.assertEqual(
            html_issue.issue_type,
            self.reference_vuln["type"],
            "HTML issue type should match reference exactly",
        )
        self.assertEqual(
            sheets_issue.issue_type,
            self.reference_vuln["type"],
            "Sheets issue type should match reference exactly",
        )

        # CWE information should be consistent
        self.assertEqual(
            sarif_issue.issue_cwe, self.reference_vuln["cve"], "SARIF CWE should match reference"
        )
        self.assertEqual(
            html_issue.issue_cwe, self.reference_vuln["cve"], "HTML CWE should match reference"
        )
        self.assertEqual(
            sheets_issue.issue_cwe, self.reference_vuln["cve"], "Sheets CWE should match reference"
        )

        # CWE links should be consistent
        expected_cwe_link = "https://cwe.mitre.org/data/definitions/772.html"
        self.assertEqual(
            sarif_issue.issue_cwe_link, expected_cwe_link, "SARIF CWE link should match expected"
        )
        self.assertEqual(
            html_issue.issue_cwe_link, expected_cwe_link, "HTML CWE link should match expected"
        )
        self.assertEqual(
            sheets_issue.issue_cwe_link, expected_cwe_link, "Sheets CWE link should match expected"
        )

        # Trace content should contain core vulnerability information
        # Note: Traces may differ in format but should contain the same essential security
        # information
        core_content_checks = [
            "full_url",  # The variable that leaks
            "src/file.c",  # File reference
            "946",  # Line number where the leak occurs
            "g_strdup_inline",  # The allocation function
            "leaked_storage",  # The vulnerability type
        ]

        print("\n=== Trace Analysis ===")
        print(f"SARIF Trace Length: {len(sarif_issue.trace)}")
        print(f"HTML Trace Length: {len(html_issue.trace)}")
        print(f"Sheets Trace Length: {len(sheets_issue.trace)}")

        # All traces should contain the core vulnerability information
        for content in core_content_checks:
            self.assertIn(content, sarif_issue.trace, f"SARIF trace should contain '{content}'")
            self.assertIn(content, html_issue.trace, f"HTML trace should contain '{content}'")
            self.assertIn(content, sheets_issue.trace, f"Sheets trace should contain '{content}'")

        print(f"SARIF Trace: {sarif_issue.trace}")
        print(f"HTML Trace: {html_issue.trace}")
        print(f"Sheets Trace: {sheets_issue.trace}")

        self.assertEqual(sarif_issue.trace, html_issue.trace)
        self.assertEqual(sarif_issue.trace, sheets_issue.trace)

        # 7. Validate that all issues represent the same vulnerability
        # despite format differences
        print("\n=== Data Consistency Validation ===")
        print(
            f"SARIF Issue: ID={sarif_issue.id}, Type={sarif_issue.issue_type}, "
            f"CWE={sarif_issue.issue_cwe}"
        )
        print(
            f"HTML Issue:  ID={html_issue.id}, Type={html_issue.issue_type}, "
            f"CWE={html_issue.issue_cwe}"
        )
        print(
            f"Sheets Issue: ID={sheets_issue.id}, Type={sheets_issue.issue_type}, "
            f"CWE={sheets_issue.issue_cwe}"
        )
        print("=== All formats consistently represent the same vulnerability ===\n")

        # 8. Validate that all three formats found the same set of vulnerabilities
        all_issue_ids = {
            "sarif": [issue.id for issue in sarif_issues],
            "html": [issue.id for issue in html_issues],
            "sheets": [issue.id for issue in sheets_issues],
        }

        # All formats should find the same issue IDs
        expected_ids = {"def1", "def2", "def3"}
        for format_name, ids in all_issue_ids.items():
            self.assertEqual(
                set(ids),
                expected_ids,
                f"{format_name} format should find issues with IDs {expected_ids}",
            )


if __name__ == "__main__":
    unittest.main()
