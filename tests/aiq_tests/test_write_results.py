"""
Unit tests for the write_results tool's core function.
"""

import unittest
from unittest.mock import Mock, patch

from sast_agent_workflow.tools.write_results import write_results, WriteResultsConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from common.config import Config
from aiq.builder.builder import Builder
from tests.aiq_tests.test_utils import TestUtils


class TestWriteResultsCore(unittest.IsolatedAsyncioTestCase):

    def setUp(self):
        self.mock_config = Mock(spec=Config)
        self.mock_config.WRITE_RESULTS = True
        self.write_results_config = WriteResultsConfig()
        self.builder = Mock(spec=Builder)

    async def test__aiq_tests__final_issues_writes_to_excel_successfully(self):
        # preparation
        issues = [
            TestUtils.create_sample_issue(issue_id="final_issue_1", issue_type="BUFFER_OVERFLOW"),
            TestUtils.create_sample_issue(issue_id="final_issue_2", issue_type="USE_AFTER_FREE")
        ]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final="TRUE",
            justifications=["Issue is a true positive", "Buffer overflow confirmed"],
            short_justifications="True positive"
        )
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        tracker.metrics = {"total_issues": 2, "confusion_matrix": {"true_positives": 1, "true_negatives": 1, "false_positives": 0, "false_negatives": 0}}
        
        with patch('sast_agent_workflow.tools.write_results.convert_tracker_to_summary_data') as mock_convert, \
             patch('sast_agent_workflow.tools.write_results.write_to_excel_file') as mock_excel_writer, \
             patch('sast_agent_workflow.tools.write_results.EvaluationSummary') as mock_eval_summary:
            
            mock_summary_data = [
                (issues[0], Mock()), 
                (issues[1], Mock())
            ]
            mock_convert.return_value = mock_summary_data
            mock_eval_summary_instance = Mock()
            mock_eval_summary.return_value = mock_eval_summary_instance
            
            # testing
            result_tracker = await TestUtils.run_single_fn(write_results, self.write_results_config, self.builder, tracker)
            
            # assertion
            self.assertIsInstance(result_tracker, SASTWorkflowTracker)
            
            # Verify conversion called with include_non_final=False by default
            mock_convert.assert_called_once_with(tracker, include_non_final=False)
            
            # Verify Excel writer called with correct parameters - using metrics-based evaluation summary
            mock_excel_writer.assert_called_once()
            call_args = mock_excel_writer.call_args[0]
            self.assertEqual(call_args[0], mock_summary_data)  # summary_data
            self.assertEqual(call_args[2], self.mock_config)  # config
            # Verify evaluation_summary was created from metrics (not the mock)
            eval_summary = call_args[1]
            self.assertEqual(eval_summary.tp, 1)
            self.assertEqual(eval_summary.tn, 1)
            self.assertEqual(eval_summary.fp, 0)
            self.assertEqual(eval_summary.fn, 0)

    async def test__aiq_tests__preserves_all_tracker_data_unchanged(self):
        # preparation
        issues = [
            TestUtils.create_sample_issue(issue_id="test_issue", issue_type="BUFFER_OVERFLOW")
        ]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final="TRUE",
            justifications=["Test justification"],
            short_justifications="Test summary"
        )
        
        original_tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        original_tracker.metrics = {"test_metric": "test_value"}
        original_tracker.iteration_count = 5
        
        with patch('sast_agent_workflow.tools.write_results.convert_tracker_to_summary_data'), \
             patch('sast_agent_workflow.tools.write_results.write_to_excel_file'), \
             patch('sast_agent_workflow.tools.write_results.EvaluationSummary'):
            
            # testing
            result_tracker = await TestUtils.run_single_fn(write_results, self.write_results_config, self.builder, original_tracker)
            
            # assertion - verify terminal node behavior (no state changes)
            self.assertEqual(result_tracker.issues, original_tracker.issues)
            self.assertEqual(result_tracker.config, original_tracker.config)
            self.assertEqual(result_tracker.metrics, original_tracker.metrics)
            self.assertEqual(result_tracker.iteration_count, original_tracker.iteration_count)

    async def test__aiq_tests__write_results_disabled_skips_writing(self):
        # preparation
        self.mock_config.WRITE_RESULTS = False
        
        issues = [
            TestUtils.create_sample_issue(issue_id="test_issue", issue_type="BUFFER_OVERFLOW")
        ]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final="TRUE",
            justifications=["Test justification"],
            short_justifications="Test summary"
        )
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        
        with patch('sast_agent_workflow.tools.write_results.convert_tracker_to_summary_data') as mock_convert, \
             patch('sast_agent_workflow.tools.write_results.write_to_excel_file') as mock_excel_writer:
            
            # testing
            result_tracker = await TestUtils.run_single_fn(write_results, self.write_results_config, self.builder, tracker)
            
            # assertion
            self.assertIsInstance(result_tracker, SASTWorkflowTracker)
            
            # Verify no writing operations were called
            mock_convert.assert_not_called()
            mock_excel_writer.assert_not_called()

    async def test__aiq_tests__no_config_skips_writing_gracefully(self):
        # preparation
        issues = [
            TestUtils.create_sample_issue(issue_id="test_issue", issue_type="BUFFER_OVERFLOW")
        ]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final="TRUE",
            justifications=["Test justification"],
            short_justifications="Test summary"
        )
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        tracker.config = None  # Explicitly set to None after creation
        
        with patch('sast_agent_workflow.tools.write_results.convert_tracker_to_summary_data') as mock_convert, \
             patch('sast_agent_workflow.tools.write_results.write_to_excel_file') as mock_excel_writer:
            
            # testing
            result_tracker = await TestUtils.run_single_fn(write_results, self.write_results_config, self.builder, tracker)
            
            # assertion
            self.assertIsInstance(result_tracker, SASTWorkflowTracker)
            
            # Verify no writing operations were called
            mock_convert.assert_not_called()
            mock_excel_writer.assert_not_called()

    async def test__aiq_tests__empty_tracker_handles_gracefully(self):
        # preparation
        empty_tracker = SASTWorkflowTracker(config=self.mock_config, issues={})
        
        with patch('sast_agent_workflow.tools.write_results.convert_tracker_to_summary_data') as mock_convert, \
             patch('sast_agent_workflow.tools.write_results.write_to_excel_file') as mock_excel_writer:
            
            mock_convert.return_value = []  # Empty summary data
            
            # testing
            result_tracker = await TestUtils.run_single_fn(write_results, self.write_results_config, self.builder, empty_tracker)
            
            # assertion
            self.assertIsInstance(result_tracker, SASTWorkflowTracker)
            self.assertEqual(len(result_tracker.issues), 0)
            self.assertEqual(result_tracker.config, self.mock_config)
            
            # Verify conversion was called but no issues to write
            mock_convert.assert_called_once_with(empty_tracker, include_non_final=False)
            
            # Excel writer should still be called but with empty data
            mock_excel_writer.assert_called_once()

    async def test__aiq_tests__no_completed_issues_handles_appropriately(self):
        # preparation
        issues = [
            TestUtils.create_sample_issue(issue_id="non_final_issue_1", issue_type="BUFFER_OVERFLOW"),
            TestUtils.create_sample_issue(issue_id="non_final_issue_2", issue_type="USE_AFTER_FREE")
        ]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final="FALSE",  # All issues are non-final
            justifications=["Under investigation"],
            short_justifications="Pending"
        )
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        
        with patch('sast_agent_workflow.tools.write_results.convert_tracker_to_summary_data') as mock_convert, \
             patch('sast_agent_workflow.tools.write_results.write_to_excel_file') as mock_excel_writer:
            
            mock_convert.return_value = []  # No final issues to convert
            
            # testing
            result_tracker = await TestUtils.run_single_fn(write_results, self.write_results_config, self.builder, tracker)
            
            # assertion
            self.assertIsInstance(result_tracker, SASTWorkflowTracker)
            
            # Verify conversion was called with include_non_final=False
            mock_convert.assert_called_once_with(tracker, include_non_final=False)
            
            # Excel writer should still be called with empty data
            mock_excel_writer.assert_called_once()

    async def test__aiq_tests__excel_writer_failure_handles_gracefully(self):
        # preparation
        issues = [
            TestUtils.create_sample_issue(issue_id="test_issue", issue_type="BUFFER_OVERFLOW")
        ]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final="TRUE",
            justifications=["Test justification"],
            short_justifications="Test summary"
        )
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        
        with patch('sast_agent_workflow.tools.write_results.convert_tracker_to_summary_data') as mock_convert, \
             patch('sast_agent_workflow.tools.write_results.write_to_excel_file') as mock_excel_writer, \
             patch('sast_agent_workflow.tools.write_results.EvaluationSummary'):
            
            mock_convert.return_value = [(issues[0], Mock())]
            mock_excel_writer.side_effect = Exception("Excel writing failed")
            
            # testing
            result_tracker = await TestUtils.run_single_fn(write_results, self.write_results_config, self.builder, tracker)
            
            # assertion
            self.assertIsInstance(result_tracker, SASTWorkflowTracker)
            
            # Verify tracker returned unchanged despite error
            self.assertEqual(result_tracker.issues, tracker.issues)
            self.assertEqual(result_tracker.config, tracker.config)


if __name__ == '__main__':
    unittest.main() 