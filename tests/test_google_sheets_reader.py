import os
import sys
import unittest
from unittest.mock import Mock, patch

# Add src to path for imports (for direct test run)
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "src")))


from fixtures import SAMPLE_SHEET_DATA

from common.config import Config
from dto.Issue import Issue
from report_readers.google_sheets_reader import GoogleSheetsReportReader


class TestGoogleSheetsReportReader(unittest.TestCase):
    """Test cases for GoogleSheetsReportReader"""

    def setUp(self):
        """Set up test environment"""
        self.reader = GoogleSheetsReportReader()
        self.config = Mock(spec=Config)
        self.config.SERVICE_ACCOUNT_JSON_PATH = "/path/to/service_account.json"

        # Mock sheet object
        self.mock_sheet = Mock()
        self.mock_sheet.get_all_records.return_value = SAMPLE_SHEET_DATA

    def test_given_valid_google_sheets_url_when_checking_can_handle_then_returns_true(self):
        """Test can_handle returns True for valid Google Sheets URLs"""
        valid_urls = [
            "https://docs.google.com/spreadsheets/d/abc123/edit#gid=0",
            "https://docs.google.com/spreadsheets/d/xyz789/edit",
            "https://sheets.google.com/spreadsheet/d/test123",
        ]

        for url in valid_urls:
            with self.subTest(url=url):
                result = self.reader.can_handle(url, self.config)
                self.assertTrue(result, f"Should handle URL: {url}")

    def test_given_invalid_urls_when_checking_can_handle_then_returns_false(self):
        """Test can_handle returns False for invalid URLs"""
        invalid_urls = [
            "http://docs.google.com/spreadsheets/d/abc123/edit",  # HTTP not HTTPS
            "https://example.com/spreadsheet.xlsx",  # Not Google domain
            "https://drive.google.com/file/d/abc123/view",  # Google Drive, not Sheets
            "/local/file/path.xlsx",  # Local file
            "ftp://sheets.google.com/file",  # Wrong protocol
            "",  # Empty string
            "not-a-url",  # Invalid URL format
        ]

        for url in invalid_urls:
            with self.subTest(url=url):
                result = self.reader.can_handle(url, self.config)
                self.assertFalse(result, f"Should not handle URL: {url}")

    def test_given_malformed_url_when_checking_can_handle_then_returns_false(self):
        """Test can_handle handles malformed URLs gracefully"""
        malformed_urls = [
            "https://",
            "https:///invalid",
            "https://docs.google.com",  # No path
        ]

        for url in malformed_urls:
            with self.subTest(url=url):
                result = self.reader.can_handle(url, self.config)
                self.assertFalse(result)

    @patch("report_readers.google_sheets_reader.get_google_sheet")
    def test_given_valid_sheets_url_when_reading_report_then_parses_issues_correctly(
        self, mock_get_sheet
    ):
        """Test basic Google Sheets report parsing"""
        mock_get_sheet.return_value = self.mock_sheet

        url = "https://docs.google.com/spreadsheets/d/test123/edit"
        issues = self.reader.read_report(url, self.config)

        # Verify get_google_sheet was called correctly
        mock_get_sheet.assert_called_once_with(
            url, self.config.SERVICE_ACCOUNT_JSON_PATH, ignore_error=False
        )

        # Check parsed issues
        self.assertEqual(len(issues), 3)

        # Check first issue
        issue1 = issues[0]
        self.assertEqual(issue1.id, "def1")
        self.assertEqual(issue1.issue_type, "RESOURCE_LEAK")
        self.assertEqual(issue1.issue_cwe, "CWE-772")
        self.assertIn("772.html", issue1.issue_cwe_link)
        self.assertIn("full_url", issue1.trace)

        # Check second issue
        issue2 = issues[1]
        self.assertEqual(issue2.id, "def2")
        self.assertEqual(issue2.issue_type, "SQL_INJECTION")
        self.assertEqual(issue2.issue_cwe, "CWE-89")

        # Check third issue (CVE format)
        issue3 = issues[2]
        self.assertEqual(issue3.id, "def3")
        self.assertEqual(issue3.issue_type, "UNINIT_VAR")
        self.assertEqual(issue3.issue_cwe, "CWE-457")
        self.assertIn("457", issue3.issue_cwe_link)

    @patch("report_readers.google_sheets_reader.get_google_sheet")
    def test_given_sheet_access_failure_when_reading_report_then_raises_value_error(
        self, mock_get_sheet
    ):
        """Test handling when sheet access fails"""
        mock_get_sheet.return_value = None

        url = "https://docs.google.com/spreadsheets/d/test123/edit"

        with self.assertRaises(ValueError) as context:
            self.reader.read_report(url, self.config)

        self.assertIn("Failed to access Google Sheet", str(context.exception))

    @patch("report_readers.google_sheets_reader.get_google_sheet")
    def test_given_empty_sheet_when_reading_report_then_returns_empty_list(self, mock_get_sheet):
        """Test handling of empty Google Sheets"""
        mock_sheet = Mock()
        mock_sheet.get_all_records.return_value = []
        mock_get_sheet.return_value = mock_sheet

        url = "https://docs.google.com/spreadsheets/d/test123/edit"
        issues = self.reader.read_report(url, self.config)

        self.assertEqual(len(issues), 0)

    @patch("report_readers.google_sheets_reader.get_google_sheet")
    def test_given_sheet_missing_finding_column_when_reading_report_then_returns_empty_list(
        self, mock_get_sheet
    ):
        """Test handling when Finding column is missing"""
        sheet_data_no_finding = [
            {"Status": "Open", "Severity": "High"},
            {"Status": "Fixed", "Severity": "Medium"},
        ]

        mock_sheet = Mock()
        mock_sheet.get_all_records.return_value = sheet_data_no_finding
        mock_get_sheet.return_value = mock_sheet

        url = "https://docs.google.com/spreadsheets/d/test123/edit"
        issues = self.reader.read_report(url, self.config)

        # Should return empty list when no Finding column
        self.assertEqual(len(issues), 0)

    @patch("report_readers.google_sheets_reader.get_google_sheet")
    def test_given_malformed_findings_when_reading_report_then_handles_gracefully(
        self, mock_get_sheet
    ):
        """Test handling of malformed finding entries"""
        malformed_data = [
            {"Finding": ""},  # Empty finding
            {"Finding": "Invalid format"},
            {"Finding": "Error: ValidType CWE-123\nValid trace"},  # Valid one
        ]

        mock_sheet = Mock()
        mock_sheet.get_all_records.return_value = malformed_data
        mock_get_sheet.return_value = mock_sheet

        url = "https://docs.google.com/spreadsheets/d/test123/edit"
        issues = self.reader.read_report(url, self.config)

        self.assertEqual(len(issues), 3)

        # Check that first 2 have parsing errors
        self.assertTrue(issues[0].parsing_errors)
        self.assertEqual(issues[0].issue_type, "")

        self.assertTrue(issues[1].parsing_errors)
        self.assertEqual(issues[1].issue_type, "")

        # Check that the 3th issue is valid
        self.assertFalse(hasattr(issues[2], "parsing_errors") and issues[2].parsing_errors)
        self.assertEqual(issues[2].issue_type, "ValidType")
        self.assertEqual(issues[2].issue_cwe, "CWE-123")

    def test_given_valid_finding_data_when_parsing_with_error_handling_then_returns_correct_issue(
        self,
    ):
        """Test _parse_finding_with_error_handling with valid data"""
        row = {"Finding": "Error: TestType CWE-123\nTest trace content"}
        issue = Issue("def1")
        self.reader._parse_finding_with_error_handling(row["Finding"], issue)

        self.assertEqual(issue.id, "def1")
        self.assertEqual(issue.issue_type, "TestType")
        self.assertEqual(issue.issue_cwe, "CWE-123")
        self.assertIn("Test trace content", issue.trace)

    @patch("report_readers.google_sheets_reader.get_google_sheet")
    def test_given_network_error_when_reading_report_then_raises_exception(self, mock_get_sheet):
        """Test exception handling during sheet processing"""
        mock_get_sheet.side_effect = Exception("Network error")

        url = "https://docs.google.com/spreadsheets/d/test123/edit"

        with self.assertRaises(Exception):
            self.reader.read_report(url, self.config)


if __name__ == "__main__":
    unittest.main()
