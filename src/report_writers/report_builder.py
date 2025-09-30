import copy
import json
import logging
import os
from typing import List

from common.config import Config
from dto.EvaluationSummary import EvaluationSummary

from .excel_report_writer import write_to_excel_file
from .sarif_report_writer import generate_sarif_report_with_ai_analysis

logger = logging.getLogger(__name__)


class ReportBuilder:
    """
    Builder for generating analysis reports with dual output support.

    Provides a fluent interface for building different report formats
    based on input type and configuration.
    """

    def __init__(self, data: List, evaluation_summary: EvaluationSummary, config: Config):
        self.data = data
        self.evaluation_summary = evaluation_summary
        self.config = config

    def _get_sarif_report_path(self) -> str:
        """Generate SARIF report path from Excel output path."""
        if not self.config.OUTPUT_FILE_PATH:
            raise ValueError("OUTPUT_FILE_PATH is not set")

        base_path = os.path.splitext(self.config.OUTPUT_FILE_PATH)[0]
        return f"{base_path}.sarif"

    def build_excel_report(self) -> "ReportBuilder":
        """Build Excel report (always generated)."""
        logger.info(f"Building Excel report: {self.config.OUTPUT_FILE_PATH}")
        write_to_excel_file(self.data, self.evaluation_summary, self.config)

        return self

    def build_sarif_if_applicable(self) -> "ReportBuilder":
        """Build SARIF report if input was SARIF format and output extension is SARIF/JSON."""
        if not self._is_sarif_input():
            logger.info("Skipping SARIF generation - input is not SARIF format")
            return self

        # Generate SARIF report using derived path from Excel output
        sarif_path = self._get_sarif_report_path()
        logger.info(f"Building SARIF with AI analysis: {sarif_path}")

        # Create a config copy with the SARIF output path
        sarif_config = copy.deepcopy(self.config)
        sarif_config.OUTPUT_FILE_PATH = sarif_path

        generate_sarif_report_with_ai_analysis(self.data, sarif_config)

        return self

    def build(self) -> None:
        """Execute the complete build process."""
        logger.info("Starting report generation process")

        # Always build Excel report
        self.build_excel_report()

        # Conditionally build SARIF
        self.build_sarif_if_applicable()

        logger.info("Report generation completed successfully")

    # Private helper methods
    def _is_sarif_input(self) -> bool:
        """Check if input is SARIF format."""
        input_path = self.config.INPUT_REPORT_FILE_PATH

        # Note: SARIF SAST report URLs are downloaded as local files by Tekton task before
        # AI workflow execution, so we only expect local file paths here
        if not input_path or (isinstance(input_path, str) and ("://" in input_path)):
            return False

        file_ext = os.path.splitext(input_path.lower())[1]

        if file_ext == ".sarif":
            return True
        elif file_ext == ".json":
            # Quick SARIF content check
            try:
                with open(input_path, "r", encoding="utf-8") as f:
                    data = json.load(f)
                return isinstance(data, dict) and "runs" in data and "version" in data
            except Exception:
                return False

        return False

    def _update_config(self) -> Config:
        """Create config copy with updated output path for analytics."""
        analytics_path = self._get_analytics_path()
        updated_config = copy.deepcopy(self.config)
        updated_config.OUTPUT_FILE_PATH = analytics_path
        return updated_config

    def _get_analytics_path(self) -> str:
        """Derive analytics path from primary output path."""
        base_path = os.path.splitext(self.config.OUTPUT_FILE_PATH)[0]
        return f"{base_path}_analytics.xlsx"


# Public API function (maintains backward compatibility)
def write_analysis_results(
    data: List, evaluation_summary: EvaluationSummary, config: Config
) -> None:
    """
    Write analysis results using the Builder pattern.

    This function provides the same interface as before but uses the Builder
    pattern internally for better organization and maintainability.
    """
    builder = ReportBuilder(data, evaluation_summary, config)
    builder.build()
