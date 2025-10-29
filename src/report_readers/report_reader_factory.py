import logging
from typing import List, Type

from common.config import Config
from report_readers.base_reader import BaseReportReader
from report_readers.excel_reader import ExcelReportReader
from report_readers.google_sheets_reader import GoogleSheetsReportReader
from report_readers.html_reader import HtmlReportReader
from report_readers.sarif_reader import SarifReportReader

logger = logging.getLogger(__name__)


class ReportReaderFactory:
    """
    Factory class for creating appropriate report readers.
    """

    def __init__(self):
        # Register all available readers in order of preference
        self._readers: List[Type[BaseReportReader]] = [
            SarifReportReader,
            GoogleSheetsReportReader,
            ExcelReportReader,
            HtmlReportReader,
        ]

    def get_reader(self, file_path: str, config: Config) -> BaseReportReader:
        """
        Get the appropriate reader for the given file/path.

        Args:
            file_path: Path or URL to the report file
            config: Configuration object

        Returns:
            BaseReportReader: Appropriate reader instance

        Raises:
            ValueError: If no suitable reader is found
        """
        for reader_class in self._readers:
            reader = reader_class()
            if reader.can_handle(file_path, config):
                return reader

        # If no reader can handle the file, raise an error
        raise ValueError(f"No suitable reader found for file: {file_path}")
