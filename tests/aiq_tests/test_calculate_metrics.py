import unittest
from unittest.mock import Mock, patch

from sast_agent_workflow.tools.calculate_metrics import calculate_metrics, CalculateMetricsConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from common.config import Config
from aiq.builder.builder import Builder
from tests.aiq_tests.test_utils import TestUtils


class TestCalculateMetricsCore(unittest.IsolatedAsyncioTestCase):

    def setUp(self):
        self.sample_issues = TestUtils.create_sample_issues()
        self.calculate_metrics_config = CalculateMetricsConfig()
        self.builder = Mock(spec=Builder)

    def _create_mock_config(self, calculate_metrics=True, use_critique_as_final=False):
        mock_config = Mock(spec=Config)
        mock_config.CALCULATE_METRICS = calculate_metrics
        mock_config.USE_CRITIQUE_AS_FINAL_RESULTS = use_critique_as_final
        return mock_config

    async def test__aiq_tests__calculate_metrics_disabled_preserves_empty_metrics(self):
        # preparation
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_metrics=False)
        
        # testing
        with self.assertLogs('sast_agent_workflow.tools.calculate_metrics', level='INFO') as log:
            result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        self.assertEqual(result_tracker.metrics, {})
        self.assertEqual(len(result_tracker.issues), 2)
        self.assertTrue(any("CALCULATE_METRICS is disabled" in record.message for record in log.records))

    async def test__aiq_tests__no_config_preserves_empty_metrics(self):
        # preparation
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = None
        
        # testing
        with self.assertLogs('sast_agent_workflow.tools.calculate_metrics', level='WARNING') as log:
            result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        self.assertEqual(result_tracker.metrics, {})
        self.assertTrue(any("No config found" in record.message for record in log.records))

    async def test__aiq_tests__no_completed_issues_returns_error(self):
        # preparation
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_metrics=True)
        for per_issue_data in tracker.issues.values():
            per_issue_data.analysis_response.is_final = "FALSE"
        
        # testing
        result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        self.assertIn("error", result_tracker.metrics)
        self.assertEqual(result_tracker.metrics["error"], "No completed issues found")
        self.assertEqual(len(result_tracker.metrics), 1)

    async def test__aiq_tests__mixed_issues_processes_only_final(self):
        # preparation
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_metrics=True)
        
        issue_ids = list(tracker.issues.keys())
        tracker.issues[issue_ids[0]].analysis_response.is_final = "TRUE"
        tracker.issues[issue_ids[0]].analysis_response.investigation_result = CVEValidationStatus.TRUE_POSITIVE.value
        tracker.issues[issue_ids[1]].analysis_response.is_final = "FALSE"
        
        # testing
        with patch('sast_agent_workflow.tools.calculate_metrics.get_human_verified_results') as mock_get_ground_truth:
            mock_get_ground_truth.return_value = None
            result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        metrics = result_tracker.metrics
        self.assertEqual(metrics["total_issues"], 1)
        self.assertEqual(metrics["predicted_true_positives_count"], 1)
        self.assertEqual(metrics["predicted_false_positives_count"], 0)

    @patch('sast_agent_workflow.tools.calculate_metrics.get_human_verified_results')
    async def test__aiq_tests__no_ground_truth_calculates_basic_metrics(self, mock_get_ground_truth):
        # preparation
        mock_get_ground_truth.return_value = None
        
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_metrics=True)
        
        issue_ids = list(tracker.issues.keys())
        tracker.issues[issue_ids[0]].analysis_response.investigation_result = CVEValidationStatus.TRUE_POSITIVE.value
        tracker.issues[issue_ids[0]].analysis_response.is_final = "TRUE"
        tracker.issues[issue_ids[1]].analysis_response.investigation_result = CVEValidationStatus.FALSE_POSITIVE.value
        tracker.issues[issue_ids[1]].analysis_response.is_final = "TRUE"
        
        # testing
        result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        expected_metrics = {
            "total_issues": 2,
            "predicted_true_positives_count": 1,
            "predicted_false_positives_count": 1,
            "has_ground_truth": False,
            "accuracy": None,
            "precision": None,
            "recall": None,
            "f1_score": None,
            "confusion_matrix": None
        }
        self.assertEqual(result_tracker.metrics, expected_metrics)
        mock_get_ground_truth.assert_called_once_with(tracker.config)

    @patch('sast_agent_workflow.tools.calculate_metrics.get_human_verified_results')
    async def test__aiq_tests__with_ground_truth_calculates_full_metrics(self, mock_get_ground_truth):
        # preparation
        mock_ground_truth = {
            "def1": "yes",
            "def2": "no"
        }
        mock_get_ground_truth.return_value = mock_ground_truth
        
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_metrics=True)
        
        issue_ids = list(tracker.issues.keys())
        tracker.issues[issue_ids[0]].analysis_response.investigation_result = CVEValidationStatus.FALSE_POSITIVE.value
        tracker.issues[issue_ids[0]].analysis_response.is_final = "TRUE"
        tracker.issues[issue_ids[1]].analysis_response.investigation_result = CVEValidationStatus.FALSE_POSITIVE.value
        tracker.issues[issue_ids[1]].analysis_response.is_final = "TRUE"
        
        # testing
        result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        metrics = result_tracker.metrics
        self.assertEqual(metrics["total_issues"], 2)
        self.assertEqual(metrics["has_ground_truth"], True)
        self.assertEqual(metrics["actual_true_positives_count"], 1)
        self.assertEqual(metrics["actual_false_positives_count"], 1)
        
        self.assertIsInstance(metrics["accuracy"], float)
        self.assertIsInstance(metrics["precision"], float)
        self.assertIsInstance(metrics["recall"], float)
        self.assertIsInstance(metrics["f1_score"], float)
        
        cm = metrics["confusion_matrix"]
        self.assertIn("true_positives", cm)
        self.assertIn("true_negatives", cm)
        self.assertIn("false_positives", cm)
        self.assertIn("false_negatives", cm)
        for key, value in cm.items():
            self.assertGreaterEqual(int(value), 0)

    @patch('sast_agent_workflow.tools.calculate_metrics.get_human_verified_results')
    async def test__aiq_tests__perfect_predictions_calculates_correct_metrics(self, mock_get_ground_truth):
        # preparation
        mock_ground_truth = {
            "def1": "no",
            "def2": "no"
        }
        mock_get_ground_truth.return_value = mock_ground_truth
        
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_metrics=True)
        
        for per_issue_data in tracker.issues.values():
            per_issue_data.analysis_response.investigation_result = CVEValidationStatus.TRUE_POSITIVE.value
            per_issue_data.analysis_response.is_final = "TRUE"
        
        # testing
        result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        metrics = result_tracker.metrics
        self.assertEqual(metrics["predicted_true_positives_count"], 2)
        self.assertEqual(metrics["predicted_false_positives_count"], 0)
        self.assertEqual(metrics["actual_true_positives_count"], 2)
        self.assertEqual(metrics["actual_false_positives_count"], 0)
        self.assertEqual(metrics["accuracy"], 1.0)

    @patch('sast_agent_workflow.tools.calculate_metrics.get_human_verified_results')
    async def test__aiq_tests__calculation_failure_captures_error(self, mock_get_ground_truth):
        # preparation
        mock_get_ground_truth.side_effect = Exception("Ground truth loading failed")
        
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_metrics=True)
        
        for per_issue_data in tracker.issues.values():
            per_issue_data.analysis_response.is_final = "TRUE"
        
        # testing
        result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        self.assertIn("error", result_tracker.metrics)
        self.assertIn("Ground truth loading failed", result_tracker.metrics["error"])
        self.assertIn("Unexpected error", result_tracker.metrics["error"])
        self.assertEqual(len(result_tracker.issues), 2)
        self.assertEqual(tracker.iteration_count, result_tracker.iteration_count)

    async def test__aiq_tests__empty_tracker_returns_error(self):
        # preparation
        tracker = SASTWorkflowTracker(issues={})
        tracker.config = self._create_mock_config(calculate_metrics=True)
        
        # testing
        result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        self.assertEqual(result_tracker.metrics, {"error": "No completed issues found"})

    async def test__aiq_tests__preserves_all_other_data_unchanged(self):
        # preparation
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_metrics=True)
        
        import copy
        original_iteration_count = tracker.iteration_count
        original_issues = copy.deepcopy(tracker.issues)
        original_config = tracker.config
        
        list(tracker.issues.values())[0].analysis_response.is_final = "TRUE"
        
        # testing
        with patch('sast_agent_workflow.tools.calculate_metrics.get_human_verified_results') as mock_get_ground_truth:
            mock_get_ground_truth.return_value = None
            result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        self.assertEqual(result_tracker.iteration_count, original_iteration_count)
        self.assertEqual(len(result_tracker.issues), len(original_issues))
        self.assertEqual(result_tracker.config, original_config)
        
        for issue_id, per_issue_data in result_tracker.issues.items():
            self.assertIsInstance(per_issue_data.issue, Issue)
            self.assertIsInstance(per_issue_data.analysis_response, AnalysisResponse)
            self.assertIsInstance(per_issue_data.source_code, dict)
            self.assertEqual(per_issue_data.issue.id, original_issues[issue_id].issue.id)

    async def test__aiq_tests__critique_enabled_uses_correct_config(self):
        # preparation
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_metrics=True, use_critique_as_final=True)
        
        issue_ids = list(tracker.issues.keys())
        per_issue = tracker.issues[issue_ids[0]]
        per_issue.analysis_response.investigation_result = CVEValidationStatus.TRUE_POSITIVE.value
        per_issue.analysis_response.is_final = "TRUE"
        
        # testing
        with patch('sast_agent_workflow.tools.calculate_metrics.get_human_verified_results') as mock_get_ground_truth:
            with patch('sast_agent_workflow.tools.calculate_metrics.EvaluationSummary') as mock_eval_summary:
                mock_get_ground_truth.return_value = None
                mock_eval_instance = Mock()
                mock_eval_instance.predicted_summary = [(issue_ids[0], per_issue.analysis_response, 0)]
                mock_eval_instance.predicted_true_positives = {issue_ids[0]}
                mock_eval_instance.predicted_false_positives = set()
                mock_eval_instance.ground_truth = None
                mock_eval_summary.return_value = mock_eval_instance
                
                result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
                
                mock_eval_summary.assert_called_once()
                call_args = mock_eval_summary.call_args
                passed_config = call_args[0][1]
                self.assertTrue(passed_config.USE_CRITIQUE_AS_FINAL_RESULTS)
        
        # assertion
        self.assertIn("total_issues", result_tracker.metrics)
        self.assertIn("has_ground_truth", result_tracker.metrics)


if __name__ == '__main__':
    unittest.main()