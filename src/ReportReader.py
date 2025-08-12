import logging
from typing import List

from report_readers.report_reader_factory import ReportReaderFactory
from common.config import Config
from common.constants import REGEX_PATTERNS
from dto.Issue import Issue

logger = logging.getLogger(__name__)

# Global factory instance
_report_reader_factory = ReportReaderFactory()

def read_sast_report(config: Config) -> List[Issue]:
    """
    Read a SAST report using the appropriate reader based on automatic file type detection.
    
    Supported formats (in priority order):
    - SARIF: Static Analysis Results Interchange Format (.sarif, .json files)
    - Google Sheets: Web-based spreadsheets (https://docs.google.com/spreadsheets/...)
    - HTML: Local HTML reports (.html, .htm files)
    
    Args:
        config: Configuration object containing INPUT_REPORT_FILE_PATH and other settings
        
    Returns:
        List[Issue]: List of issues parsed from the report
        
    Raises:
        ValueError: If no suitable reader is found for the file type
    """
    logger.info(f"Reading report from: {config.INPUT_REPORT_FILE_PATH}")
    
    try:
        reader = _report_reader_factory.get_reader(config.INPUT_REPORT_FILE_PATH, config)
        return reader.read_report(config.INPUT_REPORT_FILE_PATH, config)
    except ValueError as e:
        logger.error(f"Failed to read report: {e}")
        raise
