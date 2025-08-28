"""
Report readers package for SAST-AI workflow.

This package provides specific readers for different types of SAST report formats.

Supported formats:
- SARIF (Static Analysis Results Interchange Format)
- HTML reports
- Google Sheets

Usage:
    from ReportReader import read_sast_report

    issues = read_sast_report(config)
"""

from .base_reader import BaseReportReader
from .google_sheets_reader import GoogleSheetsReportReader
from .html_reader import HtmlReportReader
from .report_reader_factory import ReportReaderFactory
from .sarif_reader import SarifReportReader

__all__ = [
    "BaseReportReader",
    "SarifReportReader",
    "HtmlReportReader",
    "GoogleSheetsReportReader",
    "ReportReaderFactory",
]
