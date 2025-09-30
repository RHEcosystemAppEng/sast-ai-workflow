"""
Integration tests for the report writers module.

These tests verify the end-to-end functionality of the dual output system,
including the interaction between different components and real file operations.
"""

import json
import os
import shutil
import tempfile
import unittest
from unittest.mock import Mock, patch

from common.config import Config
from dto.EvaluationSummary import EvaluationSummary
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from dto.SummaryInfo import SummaryInfo
from report_writers import write_analysis_results
from tests.nat_tests.test_utils import TestUtils


class TestReportWritersIntegration(unittest.TestCase):
    """Integration tests for the complete report writers system."""

    def setUp(self):
        """Set up test fixtures with temporary directories."""
        # Create temporary directories for input and output
        self.temp_dir = tempfile.mkdtemp()
        self.input_dir = os.path.join(self.temp_dir, "input")
        self.output_dir = os.path.join(self.temp_dir, "output")
        os.makedirs(self.input_dir, exist_ok=True)
        os.makedirs(self.output_dir, exist_ok=True)

        # Create sample SARIF input file
        self.sarif_input_path = os.path.join(self.input_dir, "original.sarif")
        self.original_sarif = {
            "version": "2.1.0",
            "runs": [
                {
                    "tool": {"driver": {"name": "original-scanner", "version": "1.0.0"}},
                    "results": [
                        {
                            "ruleId": "buffer-overflow",
                            "message": {"text": "Potential buffer overflow"},
                            "locations": [
                                {
                                    "physicalLocation": {
                                        "artifactLocation": {"uri": "src/main.c"},
                                        "region": {"startLine": 42},
                                    }
                                }
                            ],
                        },
                        {
                            "ruleId": "use-after-free",
                            "message": {"text": "Use after free detected"},
                            "locations": [
                                {
                                    "physicalLocation": {
                                        "artifactLocation": {"uri": "src/utils.c"},
                                        "region": {"startLine": 15},
                                    }
                                }
                            ],
                        },
                    ],
                }
            ],
        }

        with open(self.sarif_input_path, "w") as f:
            json.dump(self.original_sarif, f, indent=2)

        # Create sample HTML input file
        self.html_input_path = os.path.join(self.input_dir, "report.html")
        with open(self.html_input_path, "w") as f:
            f.write("<html><body><h1>SAST Report</h1></body></html>")

        # Setup config with all required attributes
        self.mock_config = Mock(spec=Config)
        self.mock_config.PROJECT_VERSION = "2.0.0"
        self.mock_config.RUN_WITH_CRITIQUE = False
        self.mock_config.SHOW_FINAL_JUDGE_CONTEXT = False
        self.mock_config.AGGREGATE_RESULTS_G_SHEET = None
        self.mock_config.HUMAN_VERIFIED_FILE_PATH = None

        # Create sample analysis data
        self.sample_issues = TestUtils.create_sample_issues(count=2)
        self.sample_data = []
        for i, issue in enumerate(self.sample_issues):
            summary_info = Mock(spec=SummaryInfo)
            summary_info.llm_response = Mock(spec=AnalysisResponse)
            summary_info.llm_response.investigation_result = (
                CVEValidationStatus.FALSE_POSITIVE.value
                if i == 0
                else CVEValidationStatus.TRUE_POSITIVE.value
            )
            summary_info.llm_response.short_justifications = f"AI analysis result {i+1}"
            summary_info.llm_response.justifications = [f"Detailed justification {i+1}"]
            summary_info.llm_response.recommendations = [f"Recommendation {i+1}"]
            summary_info.llm_response.is_true_positive.return_value = i == 1
            # Add missing attributes for Excel writer
            summary_info.metrics = {"answer_relevancy": 0.8, "context_precision": 0.9}
            summary_info.critique = f"Critique {i+1}"
            summary_info.context = f"Context {i+1}"
            self.sample_data.append((issue, summary_info))

        self.evaluation_summary = Mock(spec=EvaluationSummary)
        self.evaluation_summary.tp = 1
        self.evaluation_summary.fp = 1
        self.evaluation_summary.tn = 0
        self.evaluation_summary.fn = 0
        self.evaluation_summary.accuracy = 0.5
        self.evaluation_summary.precision = 0.5
        self.evaluation_summary.recall = 1.0
        self.evaluation_summary.f1_score = 0.67
        self.evaluation_summary.actual_true_positives = ["issue1"]
        self.evaluation_summary.actual_false_positives = ["issue2"]
        self.evaluation_summary.actual_true_negatives = []
        self.evaluation_summary.actual_false_negatives = []
        self.evaluation_summary.predicted_true_positives = ["issue1"]
        self.evaluation_summary.predicted_false_positives = ["issue2"]
        self.evaluation_summary.predicted_true_negatives = []
        self.evaluation_summary.predicted_false_negatives = []

    def tearDown(self):
        """Clean up temporary directories."""
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    @patch("report_writers.report_builder.write_to_excel_file")
    def test_given_sarif_input_when_writing_results_then_creates_both_reports(
        self, mock_excel_writer
    ):
        """Integration test: SARIF input should generate both SARIF and Excel reports."""
        # Setup paths
        sarif_output_path = os.path.join(self.output_dir, "enhanced.sarif")
        self.mock_config.INPUT_REPORT_FILE_PATH = self.sarif_input_path
        self.mock_config.OUTPUT_FILE_PATH = sarif_output_path

        # Execute
        write_analysis_results(self.sample_data, self.evaluation_summary, self.mock_config)

        # Verify SARIF file was created
        self.assertTrue(os.path.exists(sarif_output_path))

        # Verify SARIF content
        with open(sarif_output_path, "r") as f:
            enhanced_sarif = json.load(f)

        # Check that AI analysis was added
        results = enhanced_sarif["runs"][0]["results"]
        self.assertEqual(len(results), 2)

        # Verify suppressions were added
        for result in results:
            self.assertIn("suppressions", result)
            self.assertEqual(len(result["suppressions"]), 1)
            suppression = result["suppressions"][0]
            self.assertEqual(suppression["kind"], "external")
            self.assertIn(suppression["status"], ["accepted", "rejected"])

        # Verify tool info was updated
        driver = enhanced_sarif["runs"][0]["tool"]["driver"]
        self.assertEqual(driver["name"], "sast-ai")
        self.assertEqual(driver["organization"], "Red Hat Ecosystem AppEng")
        self.assertIn("originalTool", driver["properties"])

        # Verify Excel writer was called with original config
        mock_excel_writer.assert_called_once()
        call_args = mock_excel_writer.call_args[0]
        used_config = call_args[2]
        self.assertEqual(used_config.OUTPUT_FILE_PATH, sarif_output_path)

    @patch("report_writers.report_builder.write_to_excel_file")
    def test_given_html_input_when_writing_results_then_creates_only_excel(self, mock_excel_writer):
        """Integration test: HTML input should generate only Excel report."""
        # Setup paths
        excel_output_path = os.path.join(self.output_dir, "report.xlsx")
        self.mock_config.INPUT_REPORT_FILE_PATH = self.html_input_path
        self.mock_config.OUTPUT_FILE_PATH = excel_output_path

        # Execute
        write_analysis_results(self.sample_data, self.evaluation_summary, self.mock_config)

        # Verify no SARIF file was created
        sarif_files = [f for f in os.listdir(self.output_dir) if f.endswith(".sarif")]
        self.assertEqual(len(sarif_files), 0)

        # Verify Excel writer was called with original config
        mock_excel_writer.assert_called_once()
        call_args = mock_excel_writer.call_args[0]
        used_config = call_args[2]
        self.assertEqual(used_config.OUTPUT_FILE_PATH, excel_output_path)

    def test_given_sarif_input_when_enhancing_then_preserves_structure_and_adds_ai(self):
        """Integration test: SARIF enhancement preserves structure and adds AI data."""
        # Setup paths
        sarif_output_path = os.path.join(self.output_dir, "enhanced.sarif")
        self.mock_config.INPUT_REPORT_FILE_PATH = self.sarif_input_path
        self.mock_config.OUTPUT_FILE_PATH = sarif_output_path

        # Mock Excel writer to focus on SARIF functionality
        with patch("report_writers.excel_report_writer.write_to_excel_file"):
            # Execute
            write_analysis_results(self.sample_data, self.evaluation_summary, self.mock_config)

        # Load and verify enhanced SARIF
        with open(sarif_output_path, "r") as f:
            enhanced_sarif = json.load(f)

        # Verify original structure is preserved
        self.assertEqual(enhanced_sarif["version"], "2.1.0")
        self.assertEqual(len(enhanced_sarif["runs"]), 1)

        run = enhanced_sarif["runs"][0]
        self.assertEqual(len(run["results"]), 2)

        # Verify original result data is preserved
        result1 = run["results"][0]
        self.assertEqual(result1["ruleId"], "buffer-overflow")
        self.assertEqual(result1["message"]["text"], "Potential buffer overflow")
        self.assertEqual(len(result1["locations"]), 1)

        result2 = run["results"][1]
        self.assertEqual(result2["ruleId"], "use-after-free")
        self.assertEqual(result2["message"]["text"], "Use after free detected")

        # Verify AI enhancements were added
        for i, result in enumerate(run["results"]):
            # Check suppressions
            self.assertIn("suppressions", result)
            suppression = result["suppressions"][0]
            self.assertEqual(suppression["kind"], "external")
            expected_status = "accepted" if i == 0 else "rejected"  # First is FP, second is TP
            self.assertEqual(suppression["status"], expected_status)
            self.assertEqual(suppression["justification"], f"AI analysis result {i+1}")

            # Check AI analysis properties
            self.assertIn("properties", result)
            ai_analysis = result["properties"]["aiAnalysis"]
            expected_result = (
                CVEValidationStatus.FALSE_POSITIVE.value
                if i == 0
                else CVEValidationStatus.TRUE_POSITIVE.value
            )
            self.assertEqual(ai_analysis["investigation_result"], expected_result)
            self.assertEqual(ai_analysis["recommendations"], [f"Recommendation {i+1}"])

    def test_given_mismatched_sarif_and_analysis_counts_when_writing_results_then_raises_error(
        self,
    ):
        """Integration test: Mismatched SARIF and analysis counts should raise ValueError."""
        # Setup paths
        sarif_output_path = os.path.join(self.output_dir, "enhanced.sarif")
        self.mock_config.INPUT_REPORT_FILE_PATH = self.sarif_input_path
        self.mock_config.OUTPUT_FILE_PATH = sarif_output_path

        # Create analysis data with different count than SARIF results (2 vs 3)
        extra_issue = TestUtils.create_sample_issues(count=1)[0]
        summary_info = Mock(spec=SummaryInfo)
        summary_info.llm_response = Mock(spec=AnalysisResponse)
        summary_info.llm_response.investigation_result = CVEValidationStatus.TRUE_POSITIVE.value
        summary_info.llm_response.short_justifications = "Extra analysis"
        summary_info.llm_response.recommendations = ["Extra recommendation"]
        summary_info.llm_response.is_true_positive.return_value = True
        summary_info.metrics = {"answer_relevancy": 0.8, "context_precision": 0.9}
        summary_info.critique = "Extra critique"
        summary_info.context = "Extra context"

        # Add extra analysis data (3 analysis items vs 2 SARIF results)
        mismatched_data = self.sample_data + [(extra_issue, summary_info)]

        # Mock Excel writer to focus on SARIF functionality
        with patch("report_writers.excel_report_writer.write_to_excel_file"):
            # Should raise ValueError due to index mismatch
            with self.assertRaises(ValueError) as cm:
                write_analysis_results(mismatched_data, self.evaluation_summary, self.mock_config)

            self.assertIn("Index mismatch error", str(cm.exception))
            self.assertIn("SARIF has 2 results but analysis data has 3 items", str(cm.exception))


if __name__ == "__main__":
    unittest.main()
