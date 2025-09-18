import os
import tempfile
import unittest
from unittest.mock import Mock

from common.config import Config
from report_readers.base_reader import BaseReportReader
from report_readers.google_sheets_reader import GoogleSheetsReportReader
from report_readers.html_reader import HtmlReportReader
from report_readers.report_reader_factory import ReportReaderFactory
from report_readers.sarif_reader import SarifReportReader


class MockReader(BaseReportReader):
    """Mock reader for testing purposes"""

    def __init__(self, can_handle_result=True, format_name="Mock"):
        self.can_handle_result = can_handle_result
        self.format_name = format_name
        self.read_report_called = False

    def can_handle(self, file_path: str, config: Config) -> bool:
        return self.can_handle_result

    def read_report(self, file_path: str, config: Config) -> list:
        self.read_report_called = True
        return [Mock(id="test1")]


class TestReportReaderFactory(unittest.TestCase):
    """Test cases for ReportReaderFactory"""

    def setUp(self):
        """Set up test environment"""
        self.factory = ReportReaderFactory()
        self.config = Mock(spec=Config)
        self.config.INPUT_REPORT_FILE_PATH = "test.sarif"
        self.config.SERVICE_ACCOUNT_JSON_PATH = "service_account.json"

    def test_given_factory_initialization_when_creating_factory_then_registers_default_readers(
        self,
    ):
        """Test factory initializes with default readers"""
        self.assertIsInstance(self.factory, ReportReaderFactory)
        self.assertGreater(len(self.factory._readers), 0)

        # Check that default readers are registered
        reader_classes = [reader_class.__name__ for reader_class in self.factory._readers]
        self.assertIn("SarifReportReader", reader_classes)
        self.assertIn("GoogleSheetsReportReader", reader_classes)
        self.assertIn("HtmlReportReader", reader_classes)

    def test_given_sarif_file_when_getting_reader_then_returns_sarif_reader(self):
        """Test that SARIF reader has highest priority"""
        # Create a mock SARIF file
        with tempfile.NamedTemporaryFile(suffix=".sarif", delete=False) as temp_file:
            temp_file.write(b'{"version": "2.1.0", "runs": [{"results": []}]}')
            temp_file.flush()

            try:
                reader = self.factory.get_reader(temp_file.name, self.config)
                self.assertIsInstance(reader, SarifReportReader)
            finally:
                os.unlink(temp_file.name)

    def test_given_html_file_when_getting_reader_then_returns_html_reader(self):
        """Test HTML reader selection for HTML files"""
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as temp_file:
            temp_file.write(b'<html><pre><a id="def1">Test</a></pre></html>')
            temp_file.flush()

            try:
                reader = self.factory.get_reader(temp_file.name, self.config)
                self.assertIsInstance(reader, HtmlReportReader)
            finally:
                os.unlink(temp_file.name)

    def test_given_google_sheets_url_when_getting_reader_then_returns_google_sheets_reader(self):
        """Test Google Sheets reader selection for Google Sheets URLs"""
        sheets_url = "https://docs.google.com/spreadsheets/d/test123/edit"
        reader = self.factory.get_reader(sheets_url, self.config)
        self.assertIsInstance(reader, GoogleSheetsReportReader)

    def test_given_unsupported_file_when_getting_reader_then_raises_value_error(self):
        """Test ValueError when no reader can handle the file"""
        with tempfile.NamedTemporaryFile(suffix=".unknown", delete=False) as temp_file:
            temp_file.write(b"unknown format")
            temp_file.flush()

            try:
                with self.assertRaises(ValueError) as context:
                    self.factory.get_reader(temp_file.name, self.config)

                self.assertIn("No suitable reader found", str(context.exception))
                self.assertIn(temp_file.name, str(context.exception))
            finally:
                os.unlink(temp_file.name)

    def test_given_multiple_mock_readers_when_getting_reader_then_returns_first_capable_reader(
        self,
    ):
        """Test that readers are tried in priority order"""

        # Create mock reader classes with specific behaviors
        class MockReader1(BaseReportReader):
            def can_handle(self, file_path: str, config: Config) -> bool:
                return False

            def read_report(self, file_path: str, config: Config) -> list:
                return []

        class MockReader2(BaseReportReader):
            def can_handle(self, file_path: str, config: Config) -> bool:
                return True

            def read_report(self, file_path: str, config: Config) -> list:
                return []

        class MockReader3(BaseReportReader):
            def can_handle(self, file_path: str, config: Config) -> bool:
                return True

            def read_report(self, file_path: str, config: Config) -> list:
                return []

        # Create factory with only mock readers
        test_factory = ReportReaderFactory()
        test_factory._readers = [MockReader1, MockReader2, MockReader3]

        reader = test_factory.get_reader("test.file", self.config)

        # Should return the second reader (first one that can handle)
        self.assertIsInstance(reader, MockReader2)

    def test_given_empty_file_path_when_getting_reader_then_raises_value_error(self):
        """Test behavior with empty file path"""
        with self.assertRaises(ValueError):
            self.factory.get_reader("", self.config)

    def test_given_none_config_when_getting_reader_then_raises_value_error(self):
        """Test behavior with None config"""
        with self.assertRaises(ValueError):
            self.factory.get_reader("test.file", None)


if __name__ == "__main__":
    unittest.main()
