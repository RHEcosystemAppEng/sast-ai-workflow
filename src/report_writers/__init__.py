"""
Report Writers Module

This module provides writers for different output formats in the SAST AI Workflow system.
It implements a dual output strategy using the Builder pattern:
- Always generates Excel analytics for internal use
- Conditionally generates clean SARIF when input is SARIF format

Main Classes:
- ReportBuilder: Builder for constructing reports step by step

Main Functions:
- write_analysis_results: Main entry point (backward compatible)
- write_to_excel_file: Direct Excel writing function (for backward compatibility)
"""

from .excel_report_writer import write_to_excel_file
from .report_builder import ReportBuilder, write_analysis_results

__all__ = ["write_analysis_results", "ReportBuilder", "write_to_excel_file"]
