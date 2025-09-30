"""
Unit tests for the sarif_report_writer module's core functionality.
"""

import json
import os
import tempfile
import unittest
from unittest.mock import Mock, mock_open, patch

from common.config import Config
from dto.EvaluationSummary import EvaluationSummary
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from dto.SummaryInfo import SummaryInfo
from report_writers.sarif_report_writer import (
    _add_suppression,
    _inject_analysis_results,
    _update_tool_info,
    generate_sarif_report_with_ai_analysis,
)
from tests.nat_tests.test_utils import TestUtils


class SarifTestBase(unittest.TestCase):
    """Base class for SARIF-related tests with shared fixtures."""

    @classmethod
    def create_sample_sarif_data(cls):
        """Create standard SARIF data for testing."""
        return {
            "version": "2.1.0",
            "runs": [
                {
                    "tool": {"driver": {"name": "original-tool", "version": "1.0.0"}},
                    "results": [
                        {"ruleId": "rule1", "message": {"text": "First issue"}, "locations": []},
                        {"ruleId": "rule2", "message": {"text": "Second issue"}, "locations": []},
                    ],
                }
            ],
        }

    @classmethod
    def create_sample_analysis_data(cls, count=2):
        """Create standard analysis data for testing."""
        sample_issues = TestUtils.create_sample_issues(count=count)
        analysis_data = []
        for i, issue in enumerate(sample_issues):
            summary_info = Mock(spec=SummaryInfo)
            summary_info.llm_response = Mock(spec=AnalysisResponse)
            summary_info.llm_response.investigation_result = (
                CVEValidationStatus.FALSE_POSITIVE.value
                if i == 0
                else CVEValidationStatus.TRUE_POSITIVE.value
            )
            summary_info.llm_response.short_justifications = f"Test justification {i+1}"
            summary_info.llm_response.recommendations = [f"Test recommendation {i+1}"]
            summary_info.llm_response.is_true_positive.return_value = (
                i == 1
            )  # Second issue is true positive
            analysis_data.append((issue, summary_info))
        return analysis_data

    @classmethod
    def create_mock_config(cls, project_version="2.0.0"):
        """Create standard mock config for testing."""
        mock_config = Mock(spec=Config)
        mock_config.INPUT_REPORT_FILE_PATH = "/workspace/input/original.sarif"
        mock_config.OUTPUT_FILE_PATH = "/workspace/output/enhanced.sarif"
        mock_config.PROJECT_VERSION = project_version
        return mock_config


class TestGenerateSarifReportWithAiAnalysis(SarifTestBase):
    """Test cases for the main SARIF generation function."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_config = self.create_mock_config()
        self.sample_data = self.create_sample_analysis_data()
        self.evaluation_summary = Mock(spec=EvaluationSummary)
        self.original_sarif = self.create_sample_sarif_data()

    @patch("os.makedirs")
    @patch("builtins.open", new_callable=mock_open)
    @patch("report_writers.sarif_report_writer._inject_analysis_results")
    def test_given_analysis_data_when_generating_sarif_then_loads_and_saves_enhanced(
        self, mock_inject, mock_file, mock_makedirs
    ):
        """Test that the function loads original SARIF, enhances it, and saves the result."""
        # Setup mocks
        mock_file.return_value.read.return_value = json.dumps(self.original_sarif)
        enhanced_sarif = {"enhanced": True}
        mock_inject.return_value = enhanced_sarif

        # Execute
        generate_sarif_report_with_ai_analysis(self.sample_data, self.mock_config)

        # Verify file operations
        mock_file.assert_any_call(self.mock_config.INPUT_REPORT_FILE_PATH, "r", encoding="utf-8")
        mock_file.assert_any_call(self.mock_config.OUTPUT_FILE_PATH, "w", encoding="utf-8")

        # Verify directory creation
        mock_makedirs.assert_called_once_with(
            os.path.dirname(self.mock_config.OUTPUT_FILE_PATH), exist_ok=True
        )

        # Verify enhancement was called
        mock_inject.assert_called_once_with(self.original_sarif, self.sample_data, self.mock_config)

        # Verify JSON was written
        mock_file.return_value.__enter__.return_value.write.assert_called()
        # Verify that json.dump was called (indirectly through the write operation)
        self.assertTrue(mock_file.return_value.__enter__.return_value.write.called)

    @patch("os.makedirs")
    @patch("builtins.open", side_effect=FileNotFoundError("Input file not found"))
    def test_given_missing_input_file_when_generating_sarif_then_raises_file_not_found(
        self, mock_file, mock_makedirs
    ):
        """Test that missing input file raises appropriate exception."""
        with self.assertRaises(FileNotFoundError):
            generate_sarif_report_with_ai_analysis(self.sample_data, self.mock_config)

    @patch("os.makedirs", side_effect=PermissionError("Cannot create directory"))
    @patch("builtins.open", new_callable=mock_open)
    def test_given_directory_creation_failure_when_generating_sarif_then_raises_permission_error(
        self, mock_file, mock_makedirs
    ):
        """Test that directory creation failure raises appropriate exception."""
        mock_file.return_value.read.return_value = json.dumps(self.original_sarif)

        with self.assertRaises(PermissionError):
            generate_sarif_report_with_ai_analysis(self.sample_data, self.mock_config)

    @patch("os.makedirs")
    @patch("builtins.open", new_callable=mock_open, read_data="invalid json content")
    def test_given_corrupted_sarif_input_when_generating_sarif_then_raises_json_decode_error(
        self, mock_file, mock_makedirs
    ):
        """Test that corrupted SARIF input raises appropriate exception."""
        with self.assertRaises(json.JSONDecodeError):
            generate_sarif_report_with_ai_analysis(self.sample_data, self.mock_config)


class TestInjectAnalysisResults(SarifTestBase):
    """Test cases for the _inject_analysis_results function."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_config = self.create_mock_config()
        self.sarif_data = self.create_sample_sarif_data()

        # Create sample analysis data
        self.sample_issues = TestUtils.create_sample_issues(count=2)
        self.analysis_data = []
        for i, issue in enumerate(self.sample_issues):
            summary_info = Mock(spec=SummaryInfo)
            summary_info.llm_response = Mock(spec=AnalysisResponse)
            summary_info.llm_response.investigation_result = (
                CVEValidationStatus.FALSE_POSITIVE.value
                if i == 0
                else CVEValidationStatus.TRUE_POSITIVE.value
            )
            summary_info.llm_response.short_justifications = f"Test justification {i+1}"
            summary_info.llm_response.recommendations = [f"Test recommendation {i+1}"]
            summary_info.llm_response.is_true_positive.return_value = (
                i == 1
            )  # Second issue is true positive
            self.analysis_data.append((issue, summary_info))

    @patch("report_writers.sarif_report_writer._update_tool_info")
    @patch("report_writers.sarif_report_writer._add_suppression")
    def test_given_analysis_data_when_injecting_then_updates_tool_and_adds_suppressions(
        self, mock_add_suppression, mock_update_tool
    ):
        """Test that analysis injection updates tool info and adds suppressions to all results."""
        # Execute
        result = _inject_analysis_results(self.sarif_data, self.analysis_data, self.mock_config)

        # Verify tool info was updated
        mock_update_tool.assert_called_once_with(self.sarif_data, self.mock_config)

        # Verify suppressions were added for each result
        self.assertEqual(mock_add_suppression.call_count, 2)

        # Verify the calls were made with correct parameters
        for i in range(2):
            call_args = mock_add_suppression.call_args_list[i]
            self.assertEqual(
                call_args[0][0], self.sarif_data["runs"][0]["results"][i]
            )  # SARIF result
            self.assertEqual(call_args[0][1], self.analysis_data[i][0])  # Issue
            self.assertEqual(call_args[0][2], self.analysis_data[i][1])  # SummaryInfo

        # Verify the original SARIF data is returned (modified in place)
        self.assertEqual(result, self.sarif_data)

    def test_given_empty_analysis_data_with_sarif_results_when_injecting_then_raises_error(self):
        """Test that empty analysis data with SARIF results raises ValueError."""
        # Should raise ValueError due to index mismatch (2 SARIF results, 0 analysis data)
        with self.assertRaises(ValueError) as cm:
            _inject_analysis_results(self.sarif_data, [], self.mock_config)

        self.assertIn("Index mismatch error", str(cm.exception))
        self.assertIn("SARIF has 2 results but analysis data has 0 items", str(cm.exception))

    def test_given_more_sarif_results_than_analysis_data_when_injecting_then_raises_error(self):
        """Test that more SARIF results than analysis data raises ValueError."""
        # More results than analysis data
        extra_result = {"ruleId": "rule3", "message": {"text": "Third issue"}, "locations": []}
        self.sarif_data["runs"][0]["results"].append(extra_result)

        # Should raise ValueError due to index mismatch
        with self.assertRaises(ValueError) as cm:
            _inject_analysis_results(self.sarif_data, self.analysis_data, self.mock_config)

        self.assertIn("Index mismatch error", str(cm.exception))
        self.assertIn("SARIF has 3 results but analysis data has 2 items", str(cm.exception))

    def test_given_more_analysis_data_than_sarif_results_when_injecting_then_raises_error(self):
        """Test that more analysis data than SARIF results raises ValueError."""
        # More analysis data than SARIF results - remove one SARIF result
        self.sarif_data["runs"][0]["results"].pop()

        # Should raise ValueError due to index mismatch
        with self.assertRaises(ValueError) as cm:
            _inject_analysis_results(self.sarif_data, self.analysis_data, self.mock_config)

        self.assertIn("Index mismatch error", str(cm.exception))
        self.assertIn("SARIF has 1 results but analysis data has 2 items", str(cm.exception))

    def test_given_sarif_with_no_runs_when_injecting_then_handles_gracefully(self):
        """Test that SARIF data with no runs is handled gracefully."""
        sarif_no_runs = {"version": "2.1.0", "runs": []}

        result = _inject_analysis_results(sarif_no_runs, self.analysis_data, self.mock_config)
        self.assertEqual(result, sarif_no_runs)

    def test_given_sarif_run_with_no_results_and_analysis_data_when_injecting_then_raises_error(
        self,
    ):
        """Test that SARIF run with no results but analysis data raises ValueError."""
        sarif_no_results = {
            "version": "2.1.0",
            "runs": [{"tool": {"driver": {"name": "test"}}, "results": []}],
        }

        # Should raise ValueError due to index mismatch (0 SARIF results, 2 analysis data)
        with self.assertRaises(ValueError) as cm:
            _inject_analysis_results(sarif_no_results, self.analysis_data, self.mock_config)

        self.assertIn("Index mismatch error", str(cm.exception))
        self.assertIn("SARIF has 0 results but analysis data has 2 items", str(cm.exception))

    def test_given_sarif_run_with_no_results_and_no_analysis_data_when_injecting_then_succeeds(
        self,
    ):
        """Test that SARIF run with no results and no analysis data succeeds."""
        sarif_no_results = {
            "version": "2.1.0",
            "runs": [{"tool": {"driver": {"name": "test"}}, "results": []}],
        }

        # Should succeed when both lists are empty (0 SARIF results, 0 analysis data)
        result = _inject_analysis_results(sarif_no_results, [], self.mock_config)
        self.assertEqual(result, sarif_no_results)


class TestUpdateToolInfo(SarifTestBase):
    """Test cases for the _update_tool_info function."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_config = self.create_mock_config()
        self.sarif_data = self.create_sample_sarif_data()
        # Add informationUri for tool info tests
        self.sarif_data["runs"][0]["tool"]["driver"]["informationUri"] = "https://original-tool.com"

    def test_given_sarif_data_when_updating_tool_then_updates_driver_with_ai_info(self):
        """Test that tool driver is updated with AI workflow information."""
        _update_tool_info(self.sarif_data, self.mock_config)

        driver = self.sarif_data["runs"][0]["tool"]["driver"]

        # Verify main fields are updated
        self.assertEqual(driver["name"], "sast-ai")
        self.assertEqual(driver["version"], "unknown")  # Default when no env var set
        self.assertEqual(
            driver["informationUri"], "https://github.com/RHEcosystemAppEng/sast-ai-workflow"
        )
        self.assertEqual(driver["organization"], "Red Hat Ecosystem AppEng")
        self.assertEqual(driver["product"], "SAST AI Workflow")

        # Verify properties contain original tool info
        properties = driver["properties"]
        self.assertEqual(properties["originalTool"], "original-tool")
        self.assertEqual(properties["originalVersion"], "1.0.0")
        self.assertEqual(properties["workflowEngine"], "sast-ai-workflow")
        self.assertEqual(properties["analysisType"], "enhanced-static-analysis")

    def test_given_missing_original_tool_info_when_updating_tool_then_handles_gracefully(self):
        """Test that missing original tool info is handled gracefully."""
        # Remove original tool info
        self.sarif_data["runs"][0]["tool"]["driver"] = {}

        _update_tool_info(self.sarif_data, self.mock_config)

        driver = self.sarif_data["runs"][0]["tool"]["driver"]
        properties = driver["properties"]

        # Should use 'unknown' for missing original info
        self.assertEqual(properties["originalTool"], "unknown")
        self.assertEqual(properties["originalVersion"], "unknown")

    def test_given_missing_project_version_when_updating_tool_then_uses_default(self):
        """Test that missing PROJECT_VERSION uses default."""
        del self.mock_config.PROJECT_VERSION

        _update_tool_info(self.sarif_data, self.mock_config)

        driver = self.sarif_data["runs"][0]["tool"]["driver"]
        self.assertEqual(driver["version"], "unknown")  # Default value when no env var

    def test_given_multiple_runs_when_updating_tool_then_updates_all_runs(self):
        """Test that all runs are updated when multiple runs exist."""
        # Add second run
        second_run = {"tool": {"driver": {"name": "second-tool", "version": "2.0.0"}}}
        self.sarif_data["runs"].append(second_run)

        _update_tool_info(self.sarif_data, self.mock_config)

        # Verify both runs were updated
        for run in self.sarif_data["runs"]:
            driver = run["tool"]["driver"]
            self.assertEqual(driver["name"], "sast-ai")
            self.assertEqual(driver["organization"], "Red Hat Ecosystem AppEng")

    def test_given_missing_tool_structure_when_updating_tool_then_handles_gracefully(self):
        """Test that missing tool structure is handled gracefully."""
        # Remove tool structure
        self.sarif_data["runs"][0] = {}

        # Should not raise exception
        _update_tool_info(self.sarif_data, self.mock_config)


class TestAddSuppression(SarifTestBase):
    """Test cases for the _add_suppression function."""

    def setUp(self):
        """Set up test fixtures."""
        self.sarif_result = {
            "ruleId": "test-rule",
            "message": {"text": "Test issue"},
            "locations": [],
        }

        self.issue = TestUtils.create_sample_issue("test-issue-1", "BUFFER_OVERFLOW")

        self.summary_info = Mock(spec=SummaryInfo)
        self.summary_info.llm_response = Mock(spec=AnalysisResponse)

    def test_given_false_positive_when_adding_suppression_then_adds_accepted(self):
        """Test that false positive issues get 'accepted' suppression status."""
        # Setup false positive
        self.summary_info.llm_response.is_true_positive.return_value = False
        self.summary_info.llm_response.short_justifications = "This is a false positive"
        self.summary_info.llm_response.investigation_result = "FALSE_POSITIVE"
        self.summary_info.llm_response.recommendations = ["No action needed"]

        # Execute
        _add_suppression(self.sarif_result, self.issue, self.summary_info)

        # Verify suppression was added
        self.assertIn("suppressions", self.sarif_result)
        suppressions = self.sarif_result["suppressions"]
        self.assertEqual(len(suppressions), 1)

        suppression = suppressions[0]
        self.assertEqual(suppression["kind"], "external")
        self.assertEqual(suppression["status"], "accepted")
        self.assertEqual(suppression["justification"], "This is a false positive")

    def test_given_true_positive_when_adding_suppression_then_adds_rejected(self):
        """Test that true positive issues get 'rejected' suppression status."""
        # Setup true positive
        self.summary_info.llm_response.is_true_positive.return_value = True
        self.summary_info.llm_response.short_justifications = "This is a true positive"
        self.summary_info.llm_response.investigation_result = "TRUE_POSITIVE"
        self.summary_info.llm_response.recommendations = ["Fix this vulnerability"]

        # Execute
        _add_suppression(self.sarif_result, self.issue, self.summary_info)

        # Verify suppression was added
        suppressions = self.sarif_result["suppressions"]
        suppression = suppressions[0]
        self.assertEqual(suppression["status"], "rejected")
        self.assertEqual(suppression["justification"], "This is a true positive")

    def test_given_analysis_response_when_adding_suppression_then_adds_ai_properties(self):
        """Test that AI analysis properties are added to the result."""

        # Setup analysis response
        self.summary_info.llm_response.is_true_positive.return_value = False
        self.summary_info.llm_response.short_justifications = "Test justification"
        self.summary_info.llm_response.investigation_result = "FALSE_POSITIVE"
        self.summary_info.llm_response.recommendations = ["Test recommendation"]

        # Execute
        _add_suppression(self.sarif_result, self.issue, self.summary_info)

        # Verify AI analysis properties were added
        self.assertIn("properties", self.sarif_result)
        ai_analysis = self.sarif_result["properties"]["aiAnalysis"]

        self.assertEqual(ai_analysis["investigation_result"], "FALSE_POSITIVE")
        self.assertEqual(ai_analysis["recommendations"], ["Test recommendation"])

    def test_given_existing_suppressions_when_adding_then_appends_to_list(self):
        """Test that suppression is appended to existing suppressions list."""
        # Add existing suppression
        self.sarif_result["suppressions"] = [
            {"kind": "inSource", "status": "accepted", "justification": "Existing suppression"}
        ]

        # Setup analysis response
        self.summary_info.llm_response.is_true_positive.return_value = True
        self.summary_info.llm_response.short_justifications = "New AI analysis"
        self.summary_info.llm_response.investigation_result = "TRUE_POSITIVE"
        self.summary_info.llm_response.recommendations = ["Fix it"]

        # Execute
        _add_suppression(self.sarif_result, self.issue, self.summary_info)

        # Verify both suppressions exist
        suppressions = self.sarif_result["suppressions"]
        self.assertEqual(len(suppressions), 2)

        # Verify existing suppression is preserved
        self.assertEqual(suppressions[0]["kind"], "inSource")
        self.assertEqual(suppressions[0]["justification"], "Existing suppression")

        # Verify new suppression was added
        self.assertEqual(suppressions[1]["kind"], "external")
        self.assertEqual(suppressions[1]["status"], "rejected")
        self.assertEqual(suppressions[1]["justification"], "New AI analysis")

    def test_given_existing_properties_when_adding_suppression_then_appends_ai(self):
        """Test that AI analysis is appended to existing properties."""
        # Add existing properties
        self.sarif_result["properties"] = {"existingProperty": "existing value"}

        # Setup analysis response
        self.summary_info.llm_response.is_true_positive.return_value = False
        self.summary_info.llm_response.short_justifications = "AI analysis"
        self.summary_info.llm_response.investigation_result = "FALSE_POSITIVE"
        self.summary_info.llm_response.recommendations = ["No action"]

        # Execute
        _add_suppression(self.sarif_result, self.issue, self.summary_info)

        # Verify both properties exist
        properties = self.sarif_result["properties"]
        self.assertEqual(properties["existingProperty"], "existing value")
        self.assertIn("aiAnalysis", properties)

    def test_given_empty_justification_when_adding_suppression_then_handles_gracefully(self):
        """Test that empty justification is handled gracefully."""
        # Setup with empty justification
        self.summary_info.llm_response.is_true_positive.return_value = False
        self.summary_info.llm_response.short_justifications = ""
        self.summary_info.llm_response.investigation_result = "FALSE_POSITIVE"
        self.summary_info.llm_response.recommendations = []

        # Execute
        _add_suppression(self.sarif_result, self.issue, self.summary_info)

        # Verify suppression was still added
        suppressions = self.sarif_result["suppressions"]
        self.assertEqual(len(suppressions), 1)
        self.assertEqual(suppressions[0]["justification"], "")

    def test_given_none_recommendations_when_adding_suppression_then_handles_gracefully(self):
        """Test that None recommendations are handled gracefully."""
        # Setup with None recommendations
        self.summary_info.llm_response.is_true_positive.return_value = False
        self.summary_info.llm_response.short_justifications = "Test"
        self.summary_info.llm_response.investigation_result = "FALSE_POSITIVE"
        self.summary_info.llm_response.recommendations = None

        # Execute
        _add_suppression(self.sarif_result, self.issue, self.summary_info)

        # Verify AI analysis properties handle None recommendations
        ai_analysis = self.sarif_result["properties"]["aiAnalysis"]
        self.assertIsNone(ai_analysis["recommendations"])


class TestSarifReportWriterIntegration(unittest.TestCase):
    """Integration tests for SARIF report writer with real file operations."""

    def setUp(self):
        """Set up test fixtures with temporary files."""
        self.temp_dir = tempfile.mkdtemp()
        self.mock_config = Mock(spec=Config)
        self.mock_config.PROJECT_VERSION = "2.0.0"

        # Create sample data
        self.sample_issues = TestUtils.create_sample_issues(count=1)
        self.sample_data = []
        for issue in self.sample_issues:
            summary_info = Mock(spec=SummaryInfo)
            summary_info.llm_response = Mock(spec=AnalysisResponse)
            summary_info.llm_response.investigation_result = (
                CVEValidationStatus.FALSE_POSITIVE.value
            )
            summary_info.llm_response.short_justifications = "Test justification"
            summary_info.llm_response.recommendations = ["Test recommendation"]
            summary_info.llm_response.is_true_positive.return_value = False
            self.sample_data.append((issue, summary_info))

        self.evaluation_summary = Mock(spec=EvaluationSummary)

    def tearDown(self):
        """Clean up temporary files."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_given_corrupted_sarif_file_when_generating_sarif_then_raises_json_decode_error(self):
        """Integration test: Corrupted SARIF file should raise JSONDecodeError."""
        # Create corrupted SARIF file
        corrupted_sarif_path = os.path.join(self.temp_dir, "corrupted.sarif")
        with open(corrupted_sarif_path, "w") as f:
            f.write("invalid json content")

        # Setup paths
        output_path = os.path.join(self.temp_dir, "enhanced.sarif")
        self.mock_config.INPUT_REPORT_FILE_PATH = corrupted_sarif_path
        self.mock_config.OUTPUT_FILE_PATH = output_path

        # Should raise exception due to invalid JSON
        with self.assertRaises(json.JSONDecodeError):
            generate_sarif_report_with_ai_analysis(self.sample_data, self.mock_config)

    def test_given_missing_sarif_file_when_generating_sarif_then_raises_file_not_found_error(self):
        """Integration test: Missing SARIF file should raise FileNotFoundError."""
        # Setup paths with non-existent input file
        missing_input_path = os.path.join(self.temp_dir, "missing.sarif")
        output_path = os.path.join(self.temp_dir, "enhanced.sarif")

        self.mock_config.INPUT_REPORT_FILE_PATH = missing_input_path
        self.mock_config.OUTPUT_FILE_PATH = output_path

        # Should raise FileNotFoundError when trying to read missing file
        with self.assertRaises(FileNotFoundError):
            generate_sarif_report_with_ai_analysis(self.sample_data, self.mock_config)


if __name__ == "__main__":
    unittest.main()
