import os
import tempfile
import unittest
from unittest.mock import Mock

from fixtures import SAMPLE_HTML

from src.common.config import Config
from src.report_readers.html_reader import HtmlReportReader


class TestHtmlReportReader(unittest.TestCase):
    """Test cases for HtmlReportReader"""

    def setUp(self):
        """Set up test environment"""
        self.reader = HtmlReportReader()
        self.config = Mock(spec=Config)

        # Minimal valid HTML for basic testing
        self.minimal_html_content = """
        <html>
        <pre>
            <a id="def1">Test Issue</a>
            <b><span>TestVuln</span> <a href="test.html">TEST-001</a></b>
            <div>Simple test trace</div>
        </pre>
        </html>
        """

    def test__can_handle__valid_html_file_returns_true(self):
        """Test can_handle returns True for valid HTML files"""
        with tempfile.NamedTemporaryFile(suffix=".html", mode="w", delete=False) as temp_file:
            temp_file.write(SAMPLE_HTML)
            temp_file.flush()

            try:
                result = self.reader.can_handle(temp_file.name, self.config)
                self.assertTrue(result)
            finally:
                os.unlink(temp_file.name)

    def test__can_handle__htm_file_returns_true(self):
        """Test can_handle returns True for .htm files"""
        with tempfile.NamedTemporaryFile(suffix=".htm", mode="w", delete=False) as temp_file:
            temp_file.write(SAMPLE_HTML)
            temp_file.flush()

            try:
                result = self.reader.can_handle(temp_file.name, self.config)
                self.assertTrue(result)
            finally:
                os.unlink(temp_file.name)

    def test__can_handle__unsupported_file_extension_returns_false(self):
        """Test can_handle returns False for unsupported file extensions"""
        with tempfile.NamedTemporaryFile(suffix=".txt", mode="w", delete=False) as temp_file:
            temp_file.write("some text")
            temp_file.flush()

            try:
                result = self.reader.can_handle(temp_file.name, self.config)
                self.assertFalse(result)
            finally:
                os.unlink(temp_file.name)

    def test__can_handle__url_instead_of_file_returns_false(self):
        """Test can_handle returns False for URLs"""
        result = self.reader.can_handle("https://example.com/report.html", self.config)
        self.assertFalse(result)

        result = self.reader.can_handle("http://example.com/report.html", self.config)
        self.assertFalse(result)

    def test__can_handle__nonexistent_file_returns_false(self):
        """Test can_handle returns False for non-existent files"""
        result = self.reader.can_handle("/nonexistent/file.html", self.config)
        self.assertFalse(result)

    def test__can_handle__case_insensitive_extension_returns_true(self):
        """Test can_handle works with case-insensitive extensions"""
        with tempfile.NamedTemporaryFile(suffix=".HTML", mode="w", delete=False) as temp_file:
            temp_file.write(SAMPLE_HTML)
            temp_file.flush()

            try:
                result = self.reader.can_handle(temp_file.name, self.config)
                self.assertTrue(result)
            finally:
                os.unlink(temp_file.name)

    def test__read_report__valid_html_parses_correctly(self):
        """Test basic HTML report parsing"""
        with tempfile.NamedTemporaryFile(suffix=".html", mode="w", delete=False) as temp_file:
            temp_file.write(SAMPLE_HTML)
            temp_file.flush()

            try:
                issues = self.reader.read_report(temp_file.name, self.config)

                self.assertEqual(len(issues), 3)

                # Check first issue
                issue1 = issues[0]
                self.assertEqual(issue1.id, "def1")
                self.assertEqual(issue1.issue_type, "RESOURCE_LEAK")
                self.assertEqual(issue1.issue_cwe, "CWE-772")
                self.assertEqual(
                    issue1.issue_cwe_link, "https://cwe.mitre.org/data/definitions/772.html"
                )
                self.assertIn("full_url", issue1.trace)

                # Check second issue
                issue2 = issues[1]
                self.assertEqual(issue2.id, "def2")
                self.assertEqual(issue2.issue_type, "SQL_INJECTION")
                self.assertEqual(issue2.issue_cwe, "CWE-89")
                self.assertIn("SQL injection vulnerability in login form", issue2.trace)

                # Check third issue
                issue3 = issues[2]
                self.assertEqual(issue3.id, "def3")
                self.assertEqual(issue3.issue_type, "UNINIT_VAR")
                self.assertEqual(issue3.issue_cwe, "CWE-457")
                self.assertIn("Using uninitialized variable", issue3.trace)

            finally:
                os.unlink(temp_file.name)

    def test__read_report__minimal_html_parses_successfully(self):
        """Test parsing minimal HTML structure"""
        with tempfile.NamedTemporaryFile(suffix=".html", mode="w", delete=False) as temp_file:
            temp_file.write(self.minimal_html_content)
            temp_file.flush()

            try:
                issues = self.reader.read_report(temp_file.name, self.config)

                self.assertEqual(len(issues), 1)
                issue = issues[0]
                self.assertEqual(issue.id, "def1")
                self.assertEqual(issue.issue_type, "TestVuln")
                self.assertEqual(issue.issue_cwe, "TEST-001")
                self.assertEqual(issue.issue_cwe_link, "test.html")

            finally:
                os.unlink(temp_file.name)

    def test__read_report__html_without_pre_tags_returns_empty_list(self):
        """Test handling of HTML without <pre> tags"""
        html_no_pre = "<html><body><p>No pre tags here</p></body></html>"

        with tempfile.NamedTemporaryFile(suffix=".html", mode="w", delete=False) as temp_file:
            temp_file.write(html_no_pre)
            temp_file.flush()

            try:
                issues = self.reader.read_report(temp_file.name, self.config)
                self.assertEqual(len(issues), 0)
            finally:
                os.unlink(temp_file.name)

    def test__read_report__empty_pre_tag_returns_empty_list(self):
        """Test handling of empty <pre> tag"""
        html_empty_pre = "<html><body><pre></pre></body></html>"

        with tempfile.NamedTemporaryFile(suffix=".html", mode="w", delete=False) as temp_file:
            temp_file.write(html_empty_pre)
            temp_file.flush()

            try:
                issues = self.reader.read_report(temp_file.name, self.config)
                self.assertEqual(len(issues), 0)
            finally:
                os.unlink(temp_file.name)

    def test__read_report__issues_with_missing_metadata_handles_gracefully(self):
        """Test handling of issues with missing metadata"""
        html_missing_metadata = """
        <html>
        <pre>
            <a id="def1">Issue without metadata</a>
            Some trace content without proper metadata tags
            <a id="def2">Issue with partial metadata</a>
            <b><span>OnlyType</span></b>
            More trace content
        </pre>
        </html>
        """

        with tempfile.NamedTemporaryFile(suffix=".html", mode="w", delete=False) as temp_file:
            temp_file.write(html_missing_metadata)
            temp_file.flush()

            try:
                issues = self.reader.read_report(temp_file.name, self.config)

                self.assertEqual(len(issues), 2)

                # First issue should have no metadata
                issue1 = issues[0]
                self.assertEqual(issue1.id, "def1")
                self.assertEqual(issue1.issue_type, "")  # No metadata extracted

                # Second issue should have partial metadata
                issue2 = issues[1]
                self.assertEqual(issue2.id, "def2")
                # Should extract type but no CVE

            finally:
                os.unlink(temp_file.name)

    def test__read_report__missing_file_raises_file_not_found_error(self):
        """Test read_report raises FileNotFoundError for missing files"""
        with self.assertRaises(FileNotFoundError):
            self.reader.read_report("/nonexistent/file.html", self.config)


if __name__ == "__main__":
    unittest.main()
