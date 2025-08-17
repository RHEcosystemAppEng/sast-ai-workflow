"""
Unit tests for the summarize_justifications tool's core function.
"""

import unittest
from unittest.mock import Mock, patch

from sast_agent_workflow.tools.summarize_justifications import summarize_justifications, SummarizeJustificationsConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from common.config import Config
from aiq.builder.builder import Builder
from tests.aiq_tests.test_utils import TestUtils


class TestSummarizeJustificationsCore(unittest.IsolatedAsyncioTestCase):

    def setUp(self):
        self.mock_config = Mock(spec=Config)
        self.summarize_justifications_config = SummarizeJustificationsConfig(llm_name="test_llm")
        self.builder = Mock(spec=Builder)

    async def test__summarize_justifications__final_issues_with_detailed_justifications_populates_short_justifications(self):
        # preparation
        issues = [
            TestUtils.create_sample_issue(issue_id="final_issue_1", issue_type="BUFFER_OVERFLOW"),
            TestUtils.create_sample_issue(issue_id="final_issue_2", issue_type="USE_AFTER_FREE")
        ]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final="TRUE",
            justifications=["Detailed justification 1", "Detailed justification 2"],
            short_justifications=""
        )
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        
        with patch('sast_agent_workflow.tools.summarize_justifications.IssueAnalysisService') as mock_service_class:
            mock_service = Mock()
            mock_service_class.return_value = mock_service
            
            mock_summary_response = Mock()
            mock_summary_response.short_justifications = "Summarized: Buffer overflow detected"
            mock_service.summarize_justification.return_value = mock_summary_response
            
            # testing
            result_tracker = await TestUtils.run_single_fn(summarize_justifications, self.summarize_justifications_config, self.builder, tracker)
            
            # assertion
            self.assertIsInstance(result_tracker, SASTWorkflowTracker)
            
            for issue_id in ["final_issue_1", "final_issue_2"]:
                self.assertEqual(result_tracker.issues[issue_id].analysis_response.short_justifications, 
                               "Summarized: Buffer overflow detected")
                self.assertEqual(result_tracker.issues[issue_id].analysis_response.justifications,
                               ["Detailed justification 1", "Detailed justification 2"])
            
            self.assertEqual(mock_service.summarize_justification.call_count, 2)

    async def test__summarize_justifications__final_issues_with_existing_short_justifications_preserves_summaries(self):
        # preparation
        issues = [
            TestUtils.create_sample_issue(issue_id="already_processed", issue_type="BUFFER_OVERFLOW")
        ]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues,
            is_final="TRUE", 
            justifications=["Detailed justification"],
            short_justifications="Already summarized content"
        )
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        
        with patch('sast_agent_workflow.tools.summarize_justifications.IssueAnalysisService') as mock_service_class:
            mock_service = Mock()
            mock_service_class.return_value = mock_service
            
            # testing
            result_tracker = await TestUtils.run_single_fn(summarize_justifications, self.summarize_justifications_config, self.builder, tracker)
            
            # assertion
            self.assertEqual(result_tracker.issues["already_processed"].analysis_response.short_justifications,
                           "Already summarized content")
            mock_service.summarize_justification.assert_not_called()

    async def test__summarize_justifications__mixed_issue_states_processes_only_eligible_issues(self):
        # preparation
        issues = [
            TestUtils.create_sample_issue(issue_id="eligible_issue", issue_type="BUFFER_OVERFLOW"),
            TestUtils.create_sample_issue(issue_id="non_final_issue", issue_type="USE_AFTER_FREE"),
            TestUtils.create_sample_issue(issue_id="already_processed_issue", issue_type="MEMORY_LEAK"),
            TestUtils.create_sample_issue(issue_id="no_analysis_response", issue_type="NULL_POINTER")
        ]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues,
            is_final="TRUE",
            justifications=["Test justification"],
            short_justifications=""
        )
        
        per_issue_data["non_final_issue"].analysis_response.is_final = "FALSE"
        per_issue_data["already_processed_issue"].analysis_response.short_justifications = "Already done"
        per_issue_data["no_analysis_response"].analysis_response = None
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        
        with patch('sast_agent_workflow.tools.summarize_justifications.IssueAnalysisService') as mock_service_class:
            mock_service = Mock()
            mock_service_class.return_value = mock_service
            
            mock_summary_response = Mock()
            mock_summary_response.short_justifications = "New summary for eligible issue"
            mock_service.summarize_justification.return_value = mock_summary_response
            
            # testing
            result_tracker = await TestUtils.run_single_fn(summarize_justifications, self.summarize_justifications_config, self.builder, tracker)
            
            # assertion
            self.assertEqual(result_tracker.issues["eligible_issue"].analysis_response.short_justifications,
                           "New summary for eligible issue")
            self.assertEqual(result_tracker.issues["non_final_issue"].analysis_response.short_justifications,
                           "New summary for eligible issue")
            self.assertEqual(result_tracker.issues["already_processed_issue"].analysis_response.short_justifications, "Already done")
            self.assertIsNone(result_tracker.issues["no_analysis_response"].analysis_response)
            self.assertEqual(mock_service.summarize_justification.call_count, 2)

    async def test__summarize_justifications__empty_tracker_handles_gracefully(self):
        # preparation
        empty_tracker = SASTWorkflowTracker(config=self.mock_config, issues={})
        
        # testing
        result_tracker = await TestUtils.run_single_fn(summarize_justifications, self.summarize_justifications_config, self.builder, empty_tracker)
        
        # assertion
        self.assertIsInstance(result_tracker, SASTWorkflowTracker)
        self.assertEqual(len(result_tracker.issues), 0)
        self.assertEqual(result_tracker.config, self.mock_config)

    async def test__summarize_justifications__summarization_error_handles_gracefully(self):
        # preparation
        issues = [
            TestUtils.create_sample_issue(issue_id="error_issue", issue_type="BUFFER_OVERFLOW")
        ]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues,
            is_final="TRUE",
            justifications=["Original justification that should be preserved"],
            short_justifications=""
        )
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        
        with patch('sast_agent_workflow.tools.summarize_justifications.IssueAnalysisService') as mock_service_class:
            mock_service = Mock()
            mock_service_class.return_value = mock_service
            mock_service.summarize_justification.side_effect = Exception("Summarization failed")
            
            # testing
            result_tracker = await TestUtils.run_single_fn(summarize_justifications, self.summarize_justifications_config, self.builder, tracker)
            
            # assertion
            self.assertIsInstance(result_tracker, SASTWorkflowTracker)
            self.assertEqual(result_tracker.issues["error_issue"].analysis_response.short_justifications, "")
            self.assertEqual(result_tracker.issues["error_issue"].analysis_response.justifications,
                           ["Original justification that should be preserved"])


if __name__ == '__main__':
    unittest.main()