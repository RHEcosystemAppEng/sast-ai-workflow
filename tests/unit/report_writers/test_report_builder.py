"""
Unit tests for the report_builder module's core functionality.
"""

import unittest
from unittest.mock import Mock, mock_open, patch

from common.config import Config
from dto.EvaluationSummary import EvaluationSummary
from report_writers.report_builder import ReportBuilder, write_analysis_results
from tests.nat_tests.test_utils import TestUtils


def create_mock_config(
    input_path="/workspace/input/report.sarif",
    output_path="/workspace/output/enhanced_report.sarif",
):
    """Helper function to create a properly configured mock Config object."""
    mock_config = Mock(spec=Config)
    mock_config.INPUT_REPORT_FILE_PATH = input_path
    mock_config.OUTPUT_FILE_PATH = output_path
    mock_config.PROJECT_VERSION = "2.0.0"
    mock_config.RUN_WITH_CRITIQUE = False
    mock_config.SHOW_FINAL_JUDGE_CONTEXT = False
    mock_config.AGGREGATE_RESULTS_G_SHEET = None
    mock_config.HUMAN_VERIFIED_FILE_PATH = None
    return mock_config


class TestReportBuilder(unittest.TestCase):
    """Test cases for the ReportBuilder class."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_config = Mock(spec=Config)
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/report.sarif"
        self.mock_config.OUTPUT_FILE_PATH = "/workspace/output/enhanced_report.sarif"

        # Create sample data
        self.sample_issues = TestUtils.create_sample_issues(count=2)
        self.sample_data = []
        for issue in self.sample_issues:
            summary_info = Mock()
            summary_info.llm_response.investigation_result = "FALSE_POSITIVE"
            summary_info.llm_response.short_justifications = "Test justification"
            summary_info.llm_response.recommendations = ["Fix this issue"]
            self.sample_data.append((issue, summary_info))

        self.evaluation_summary = Mock(spec=EvaluationSummary)
        self.evaluation_summary.tp = 1
        self.evaluation_summary.fp = 1

        self.builder = ReportBuilder(self.sample_data, self.evaluation_summary, self.mock_config)

    def test_given_sample_data_when_initializing_then_creates_correct_attributes(self):
        """Test that ReportBuilder initializes with correct attributes."""
        self.assertEqual(self.builder.data, self.sample_data)
        self.assertEqual(self.builder.evaluation_summary, self.evaluation_summary)
        self.assertEqual(self.builder.config, self.mock_config)

    @patch("report_writers.report_builder.write_to_excel_file")
    def test_given_non_sarif_input_when_building_excel_then_uses_original_config(
        self, mock_excel_writer
    ):
        """Test that Excel report uses original config when input is not SARIF."""
        # Setup non-SARIF input
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/report.html"

        # Execute
        result = self.builder.build_excel_report()

        # Verify
        self.assertEqual(result, self.builder)  # Returns self for chaining
        mock_excel_writer.assert_called_once_with(
            self.sample_data, self.evaluation_summary, self.mock_config  # Original config used
        )

    @patch("report_writers.report_builder.write_to_excel_file")
    def test_given_sarif_input_when_building_excel_then_uses_analytics_config(
        self, mock_excel_writer
    ):
        """Test that Excel report uses analytics config when input is SARIF."""
        # Setup SARIF input
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/report.sarif"
        self.mock_config.OUTPUT_FILE_PATH = "/workspace/output/enhanced_report.sarif"

        # Execute
        result = self.builder.build_excel_report()

        # Verify
        self.assertEqual(result, self.builder)  # Returns self for chaining
        mock_excel_writer.assert_called_once()

        # Check that original config was used (Excel writer gets original config)
        call_args = mock_excel_writer.call_args[0]
        used_config = call_args[2]
        self.assertEqual(used_config.OUTPUT_FILE_PATH, "/workspace/output/enhanced_report.sarif")

    @patch("report_writers.report_builder.generate_sarif_report_with_ai_analysis")
    def test_given_sarif_input_when_building_sarif_then_generates_report(self, mock_sarif_writer):
        """Test that SARIF report is generated when input is SARIF format."""
        # Setup SARIF input
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/report.sarif"

        # Execute
        result = self.builder.build_sarif_if_applicable()

        # Verify
        self.assertEqual(result, self.builder)  # Returns self for chaining
        # SARIF writer is called with data and a modified config (not evaluation_summary)
        mock_sarif_writer.assert_called_once()
        call_args = mock_sarif_writer.call_args[0]
        self.assertEqual(len(call_args), 2)  # data and config only
        self.assertEqual(call_args[0], self.sample_data)

    @patch("report_writers.report_builder.generate_sarif_report_with_ai_analysis")
    def test_given_non_sarif_input_when_building_sarif_then_skips_generation(
        self, mock_sarif_writer
    ):
        """Test that SARIF report is skipped when input is not SARIF format."""
        # Setup non-SARIF input
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/report.html"

        # Execute
        result = self.builder.build_sarif_if_applicable()

        # Verify
        self.assertEqual(result, self.builder)  # Returns self for chaining
        mock_sarif_writer.assert_not_called()

    @patch("report_writers.report_builder.write_to_excel_file")
    @patch("report_writers.report_builder.generate_sarif_report_with_ai_analysis")
    def test_given_sarif_input_when_building_then_generates_both_reports(
        self, mock_sarif_writer, mock_excel_writer
    ):
        """Test that build() generates both Excel and SARIF reports for SARIF input."""
        # Setup SARIF input
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/report.sarif"

        # Execute
        self.builder.build()

        # Verify both writers were called
        mock_excel_writer.assert_called_once()
        mock_sarif_writer.assert_called_once()

    @patch("report_writers.report_builder.write_to_excel_file")
    @patch("report_writers.report_builder.generate_sarif_report_with_ai_analysis")
    def test_given_non_sarif_input_when_building_then_generates_only_excel(
        self, mock_sarif_writer, mock_excel_writer
    ):
        """Test that build() generates only Excel report for non-SARIF input."""
        # Setup non-SARIF input
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/report.html"

        # Execute
        self.builder.build()

        # Verify only Excel writer was called
        mock_excel_writer.assert_called_once()
        mock_sarif_writer.assert_not_called()

    def test_given_sarif_extension_when_checking_then_returns_true(self):
        """Test that .sarif extension is detected as SARIF input."""
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/report.sarif"
        self.assertTrue(self.builder._is_sarif_input())

    def test_given_non_sarif_extension_when_checking_then_returns_false(self):
        """Test that non-SARIF extensions are detected correctly."""
        test_cases = [
            "/workspace/input/report.html",
            "/workspace/input/report.xlsx",
            "/workspace/input/report.txt",
            "https://docs.google.com/spreadsheets/d/123/edit",
        ]

        for input_path in test_cases:
            with self.subTest(input_path=input_path):
                self.mock_config.INPUT_REPORT_FILE_PATH = input_path
                self.assertFalse(self.builder._is_sarif_input())

    def test_given_url_input_when_checking_then_returns_false(self):
        """Test that URLs are not considered SARIF input."""
        self.mock_config.INPUT_REPORT_FILE_PATH = "https://example.com/report.sarif"
        self.assertFalse(self.builder._is_sarif_input())

    def test_given_none_path_when_checking_then_returns_false(self):
        """Test that None path returns False."""
        self.mock_config.INPUT_REPORT_FILE_PATH = None
        self.assertFalse(self.builder._is_sarif_input())

    def test_given_empty_path_when_checking_then_returns_false(self):
        """Test that empty path returns False."""
        self.mock_config.INPUT_REPORT_FILE_PATH = ""
        self.assertFalse(self.builder._is_sarif_input())

    @patch("builtins.open", new_callable=mock_open, read_data='{"version": "2.1.0", "runs": []}')
    def test_given_json_with_sarif_content_when_checking_then_returns_true(self, mock_file):
        """Test that .json file with SARIF content is detected as SARIF."""
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/report.json"
        self.assertTrue(self.builder._is_sarif_input())
        mock_file.assert_called_once_with("/workspace/input/report.json", "r", encoding="utf-8")

    @patch("builtins.open", new_callable=mock_open, read_data='{"results": [], "metadata": {}}')
    def test_given_json_with_non_sarif_content_when_checking_then_returns_false(self, mock_file):
        """Test that .json file with non-SARIF content is not detected as SARIF."""
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/report.json"
        self.assertFalse(self.builder._is_sarif_input())

    @patch("builtins.open", side_effect=FileNotFoundError())
    def test_given_missing_json_file_when_checking_then_returns_false(self, mock_file):
        """Test that missing .json file returns False."""
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/missing.json"
        self.assertFalse(self.builder._is_sarif_input())

    @patch("builtins.open", new_callable=mock_open, read_data="invalid json")
    def test_given_json_with_invalid_content_when_checking_then_returns_false(self, mock_file):
        """Test that .json file with invalid JSON returns False."""
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/invalid.json"
        self.assertFalse(self.builder._is_sarif_input())

    def test_given_output_path_when_getting_analytics_then_derives_correct_path(self):
        """Test that analytics path is derived correctly from output path."""
        test_cases = [
            ("/workspace/output/report.sarif", "/workspace/output/report_analytics.xlsx"),
            ("/workspace/output/enhanced.json", "/workspace/output/enhanced_analytics.xlsx"),
            ("/workspace/output/results", "/workspace/output/results_analytics.xlsx"),
        ]

        for output_path, expected_analytics_path in test_cases:
            with self.subTest(output_path=output_path):
                self.mock_config.OUTPUT_FILE_PATH = output_path
                actual_analytics_path = self.builder._get_analytics_path()
                self.assertEqual(actual_analytics_path, expected_analytics_path)

    def test_given_config_when_updating_then_creates_copy_with_analytics_path(self):
        """Test that _update_config creates a config copy with analytics path."""
        self.mock_config.OUTPUT_FILE_PATH = "/workspace/output/report.sarif"

        updated_config = self.builder._update_config()

        # Verify it's a different object
        self.assertIsNot(updated_config, self.mock_config)

        # Verify the analytics path is set
        self.assertEqual(updated_config.OUTPUT_FILE_PATH, "/workspace/output/report_analytics.xlsx")


class TestWriteAnalysisResultsFunction(unittest.TestCase):
    """Test cases for the write_analysis_results public API function."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_config = Mock(spec=Config)
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/report.sarif"
        self.mock_config.OUTPUT_FILE_PATH = "/workspace/output/enhanced_report.sarif"

        # Create sample data
        self.sample_issues = TestUtils.create_sample_issues(count=1)
        self.sample_data = []
        for issue in self.sample_issues:
            summary_info = Mock()
            summary_info.llm_response.investigation_result = "FALSE_POSITIVE"
            summary_info.llm_response.short_justifications = "Test justification"
            summary_info.llm_response.recommendations = ["Fix this issue"]
            self.sample_data.append((issue, summary_info))

        self.evaluation_summary = Mock(spec=EvaluationSummary)

    @patch("report_writers.report_builder.ReportBuilder")
    def test_given_analysis_data_when_writing_results_then_creates_builder_and_builds(
        self, mock_builder_class
    ):
        """Test that write_analysis_results creates ReportBuilder and calls build()."""
        mock_builder_instance = Mock()
        mock_builder_class.return_value = mock_builder_instance

        # Execute
        write_analysis_results(self.sample_data, self.evaluation_summary, self.mock_config)

        # Verify
        mock_builder_class.assert_called_once_with(
            self.sample_data, self.evaluation_summary, self.mock_config
        )
        mock_builder_instance.build.assert_called_once()


class TestReportBuilderErrorHandling(unittest.TestCase):
    """Test cases for ReportBuilder error handling scenarios."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_config = Mock(spec=Config)
        self.mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/report.sarif"
        self.mock_config.OUTPUT_FILE_PATH = "/workspace/output/enhanced_report.sarif"

        self.sample_data = []
        self.evaluation_summary = Mock(spec=EvaluationSummary)
        self.builder = ReportBuilder(self.sample_data, self.evaluation_summary, self.mock_config)

    @patch(
        "report_writers.report_builder.write_to_excel_file",
        side_effect=Exception("Excel write failed"),
    )
    def test_given_excel_writer_exception_when_building_then_raises_exception(
        self, mock_excel_writer
    ):
        """Test that Excel writer exceptions are raised and not caught."""
        with self.assertRaises(Exception) as cm:
            self.builder.build_excel_report()
        self.assertEqual(str(cm.exception), "Excel write failed")
        mock_excel_writer.assert_called_once()

    @patch(
        "report_writers.report_builder.generate_sarif_report_with_ai_analysis",
        side_effect=Exception("SARIF write failed"),
    )
    def test_given_sarif_writer_exception_when_building_then_raises_exception(
        self, mock_sarif_writer
    ):
        """Test that SARIF writer exceptions are raised and not caught."""
        self.builder._is_sarif_input = Mock(return_value=True)  # Force SARIF input
        with self.assertRaises(Exception) as cm:
            self.builder.build_sarif_if_applicable()
        self.assertEqual(str(cm.exception), "SARIF write failed")
        mock_sarif_writer.assert_called_once()

    @patch(
        "report_writers.report_builder.write_to_excel_file", side_effect=Exception("Excel failed")
    )
    @patch(
        "report_writers.report_builder.generate_sarif_report_with_ai_analysis",
        side_effect=Exception("SARIF failed"),
    )
    def test_given_multiple_writer_exceptions_when_building_then_raises_first_exception(
        self, mock_sarif_writer, mock_excel_writer
    ):
        """Test that build() raises the first exception encountered (Excel)."""
        self.builder._is_sarif_input = Mock(return_value=True)  # Force SARIF input
        with self.assertRaises(Exception) as cm:
            self.builder.build()
        self.assertEqual(str(cm.exception), "Excel failed")
        mock_excel_writer.assert_called_once()
        # SARIF writer should not be called since Excel failed first
        mock_sarif_writer.assert_not_called()


if __name__ == "__main__":
    unittest.main()
