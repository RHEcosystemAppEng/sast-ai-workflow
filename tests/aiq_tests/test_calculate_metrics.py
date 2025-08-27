import unittest
from unittest.mock import Mock, patch

from sast_agent_workflow.tools.calculate_metrics import calculate_metrics, CalculateMetricsConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus
from common.config import Config
from aiq.builder.builder import Builder
from tests.aiq_tests.test_utils import TestUtils


class TestCalculateMetricsCore(unittest.IsolatedAsyncioTestCase):

    def setUp(self):
        self.sample_issues = TestUtils.create_sample_issues()
        self.calculate_metrics_config = CalculateMetricsConfig()
        self.builder = Mock(spec=Builder)

    def _create_mock_config(self, calculate_ragas_metrics=True, use_critique_as_final=False, write_results_include_non_final=True):
        mock_config = Mock(spec=Config)
        mock_config.CALCULATE_RAGAS_METRICS = calculate_ragas_metrics
        mock_config.USE_CRITIQUE_AS_FINAL_RESULTS = use_critique_as_final
        mock_config.WRITE_RESULTS_INCLUDE_NON_FINAL = write_results_include_non_final
        return mock_config

    async def test__aiq_tests__ragas_metrics_disabled_still_calculates(self):
        # preparation
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_ragas_metrics=False)
        
        # Make at least one issue final so metrics can be calculated
        issue_ids = list(tracker.issues.keys())
        tracker.issues[issue_ids[0]].analysis_response.is_final = FinalStatus.TRUE.value
        tracker.issues[issue_ids[0]].analysis_response.investigation_result = CVEValidationStatus.FALSE_POSITIVE.value
        
        # testing
        with patch('sast_agent_workflow.tools.calculate_metrics.get_human_verified_results') as mock_get_ground_truth:
            mock_get_ground_truth.return_value = None
            result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion - metrics should still be calculated even when CALCULATE_RAGAS_METRICS is False
        metrics = result_tracker.metrics
        self.assertEqual(metrics["total_issues"], 2)
        self.assertEqual(metrics["predicted_false_positives_count"], 1)
        self.assertEqual(metrics["has_ground_truth"], False)
        self.assertIsNone(metrics["confusion_matrix"])

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

    async def test__aiq_tests__no_completed_issues_when_include_non_final_is_false_returns_error(self):
        # preparation
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_ragas_metrics=True, write_results_include_non_final=False)
        for per_issue_data in tracker.issues.values():
            per_issue_data.analysis_response.is_final = FinalStatus.FALSE.value
        
        # testing
        result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        self.assertIn("error", result_tracker.metrics)
        self.assertEqual(result_tracker.metrics["error"], "No completed issues found")
        self.assertEqual(len(result_tracker.metrics), 1)

    async def test__aiq_tests__mixed_issues_when_include_non_final_is_false_processes_only_final(self):
        # preparation
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_ragas_metrics=True, write_results_include_non_final=False)
        
        issue_ids = list(tracker.issues.keys())
        tracker.issues[issue_ids[0]].analysis_response.is_final = FinalStatus.TRUE.value
        tracker.issues[issue_ids[0]].analysis_response.investigation_result = CVEValidationStatus.TRUE_POSITIVE.value
        tracker.issues[issue_ids[1]].analysis_response.is_final = FinalStatus.FALSE.value
        tracker.issues[issue_ids[1]].analysis_response.investigation_result = CVEValidationStatus.TRUE_POSITIVE.value
        
        # testing
        with patch('sast_agent_workflow.tools.calculate_metrics.get_human_verified_results') as mock_get_ground_truth:
            mock_get_ground_truth.return_value = None
            result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        metrics = result_tracker.metrics
        self.assertEqual(metrics["total_issues"], 1)
        self.assertEqual(metrics["predicted_true_positives_count"], 1)
        self.assertEqual(metrics["predicted_false_positives_count"], 0)
        # Verify lists are also present
        self.assertIsInstance(metrics["predicted_true_positives"], set)
        self.assertIsInstance(metrics["predicted_false_positives"], set)

    @patch('sast_agent_workflow.tools.calculate_metrics.get_human_verified_results')
    async def test__aiq_tests__no_ground_truth_calculates_basic_metrics(self, mock_get_ground_truth):
        # preparation
        mock_get_ground_truth.return_value = None

        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_ragas_metrics=True)
        
        issue_ids = list(tracker.issues.keys())
        tracker.issues[issue_ids[0]].analysis_response.investigation_result = CVEValidationStatus.TRUE_POSITIVE.value
        tracker.issues[issue_ids[0]].analysis_response.is_final = FinalStatus.TRUE.value
        tracker.issues[issue_ids[1]].analysis_response.investigation_result = CVEValidationStatus.FALSE_POSITIVE.value
        tracker.issues[issue_ids[1]].analysis_response.is_final = FinalStatus.TRUE.value
        
        # testing
        result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        # Check key metrics are present with correct values
        metrics = result_tracker.metrics
        self.assertEqual(metrics["total_issues"], 2)
        self.assertEqual(metrics["predicted_true_positives_count"], 1)
        self.assertEqual(metrics["predicted_false_positives_count"], 1)
        self.assertEqual(metrics["has_ground_truth"], False)
        self.assertIsNone(metrics["confusion_matrix"])
        # Verify lists are present
        self.assertIsInstance(metrics["predicted_true_positives"], set)
        self.assertIsInstance(metrics["predicted_false_positives"], set)
        # Verify dynamic metrics have None values (no ground truth available)
        self.assertIsNone(metrics["accuracy"])
        self.assertIsNone(metrics["precision"])
        self.assertIsNone(metrics["recall"])
        self.assertIsNone(metrics["f1_score"])
        # This assertion was removed since we now check individual metrics above
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
        tracker.config = self._create_mock_config(calculate_ragas_metrics=True)
        
        issue_ids = list(tracker.issues.keys())
        tracker.issues[issue_ids[0]].analysis_response.investigation_result = CVEValidationStatus.FALSE_POSITIVE.value
        tracker.issues[issue_ids[0]].analysis_response.is_final = FinalStatus.TRUE.value
        tracker.issues[issue_ids[1]].analysis_response.investigation_result = CVEValidationStatus.FALSE_POSITIVE.value
        tracker.issues[issue_ids[1]].analysis_response.is_final = FinalStatus.TRUE.value
        
        # testing
        result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        metrics = result_tracker.metrics
        self.assertEqual(metrics["total_issues"], 2)
        self.assertEqual(metrics["has_ground_truth"], True)
        self.assertEqual(metrics["actual_true_positives_count"], 1)
        self.assertEqual(metrics["actual_false_positives_count"], 1)
        # Verify sets are also present (EvaluationSummary returns sets)
        self.assertIsInstance(metrics["actual_true_positives"], set)
        self.assertIsInstance(metrics["actual_false_positives"], set)
        
        # Verify specific metric values based on ground truth scenario
        self.assertEqual(metrics["accuracy"], 0.5)                    # (1+0)/(1+0+1+0) = 0.5
        self.assertEqual(metrics["precision"], 0.5)                   # 1/(1+1) = 0.5
        self.assertEqual(metrics["recall"], 1.0)                      # 1/(1+0) = 1.0
        self.assertAlmostEqual(metrics["f1_score"], 0.6666666666666666)  # 2*(0.5*1.0)/(0.5+1.0) = 0.667
        
        cm = metrics["confusion_matrix"]
        self.assertEqual(cm["true_positives"], 1)   # AI correctly identified true positive
        self.assertEqual(cm["true_negatives"], 0)   # AI correctly identified false positive
        self.assertEqual(cm["false_positives"], 1)  # AI incorrectly marked as positive
        self.assertEqual(cm["false_negatives"], 0)  # AI incorrectly marked as negative

    @patch('sast_agent_workflow.tools.calculate_metrics.get_human_verified_results')
    async def test__aiq_tests__perfect_predictions_calculates_correct_metrics(self, mock_get_ground_truth):
        # preparation
        mock_ground_truth = {
            "def1": "no",
            "def2": "no"
        }
        mock_get_ground_truth.return_value = mock_ground_truth
        
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_ragas_metrics=True)
        
        for per_issue_data in tracker.issues.values():
            per_issue_data.analysis_response.investigation_result = CVEValidationStatus.TRUE_POSITIVE.value
            per_issue_data.analysis_response.is_final = FinalStatus.TRUE.value
        
        # testing
        result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        metrics = result_tracker.metrics
        self.assertEqual(metrics["predicted_true_positives_count"], 2)
        self.assertEqual(metrics["predicted_false_positives_count"], 0)
        self.assertEqual(metrics["actual_true_positives_count"], 2)
        self.assertEqual(metrics["actual_false_positives_count"], 0)
        self.assertEqual(metrics["accuracy"], 1.0)
        # Verify sets are also present (EvaluationSummary returns sets)
        self.assertIsInstance(metrics["predicted_true_positives"], set)
        self.assertIsInstance(metrics["actual_true_positives"], set)

    @patch('sast_agent_workflow.tools.calculate_metrics.get_human_verified_results')
    async def test__aiq_tests__calculation_failure_captures_error(self, mock_get_ground_truth):
        # preparation
        mock_get_ground_truth.side_effect = Exception("Ground truth loading failed")
        
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_ragas_metrics=True)
        
        for per_issue_data in tracker.issues.values():
            per_issue_data.analysis_response.is_final = FinalStatus.TRUE.value
        
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
        tracker.config = self._create_mock_config(calculate_ragas_metrics=True)
        
        # testing
        result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, tracker)
        
        # assertion
        self.assertEqual(result_tracker.metrics, {"error": "No completed issues found"})

    async def test__aiq_tests__preserves_all_other_data_unchanged(self):
        # preparation
        tracker = TestUtils.create_sample_tracker(self.sample_issues)
        tracker.config = self._create_mock_config(calculate_ragas_metrics=True)
        
        import copy
        original_iteration_count = tracker.iteration_count
        original_issues = copy.deepcopy(tracker.issues)
        original_config = tracker.config
        
        list(tracker.issues.values())[0].analysis_response.is_final = FinalStatus.TRUE.value
        
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
        tracker.config = self._create_mock_config(calculate_ragas_metrics=True, use_critique_as_final=True)
        
        issue_ids = list(tracker.issues.keys())
        per_issue = tracker.issues[issue_ids[0]]
        per_issue.analysis_response.investigation_result = CVEValidationStatus.TRUE_POSITIVE.value
        per_issue.analysis_response.is_final = FinalStatus.TRUE.value
        
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