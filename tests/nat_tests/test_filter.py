"""
Unit tests for the filter tool's core function.
"""

import unittest
from unittest.mock import Mock, patch

from common.constants import KNOWN_NON_ISSUES_SHORT_JUSTIFICATION
from dto.LLMResponse import FinalStatus
from sast_agent_workflow.tools.filter import filter, FilterConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import CVEValidationStatus
from dto.ResponseStructures import KnownFalsePositive
from common.config import Config
from nat.builder.builder import Builder
from tests.nat_tests.test_utils import TestUtils


class TestFilterCore(unittest.IsolatedAsyncioTestCase):
    """BDD-style test cases for the filter tool core function."""

    def setUp(self):
        """Set up test fixtures."""
        self.filter_config = FilterConfig()
        self.builder = Mock(spec=Builder)
        
        # Create clean tracker for filter testing using test utils
        self.sample_issues = TestUtils.create_sample_issues(count=2)
        self.test_tracker = TestUtils.create_clean_filter_tracker(issues=self.sample_issues)

    @patch('sast_agent_workflow.tools.filter.LLMService')
    @patch('sast_agent_workflow.tools.filter.create_known_issue_retriever')
    @patch('sast_agent_workflow.tools.filter.is_known_false_positive')
    @patch('sast_agent_workflow.tools.filter.convert_similar_issues_to_examples_context_string')
    async def test_given_tracker_with_mixed_issues_when_filter_executed_then_populates_similar_issues_and_marks_known_false_positives(
        self, mock_convert, mock_is_fp, mock_create_retriever, mock_llm_service
    ):
        """Given a tracker with mixed issues, when filter is executed, then it populates similar issues and marks known false positives."""
        # preparation
        
        similar_issues = [
            KnownFalsePositive(
                error_trace="Similar buffer overflow",
                reason_of_false_positive="False positive due to buffer overflow",
                issue_type="OVERRUN",
                issue_cwe="CWE-120"
            )
        ]
        
        mock_retriever = Mock()
        mock_retriever.get_relevant_known_issues.return_value = similar_issues
        mock_create_retriever.return_value = mock_retriever
        mock_convert.return_value = "Example 1: Similar buffer overflow..."
        
        def mock_fp_side_effect(issue, similar_list, llm_service):
            if issue.id == self.sample_issues[0].id:
                return True, ["Similar buffer overflow"]
            return False, []
        
        mock_is_fp.side_effect = mock_fp_side_effect
        
        # testing
        result_tracker = await TestUtils.run_single_fn(
            filter, self.filter_config, self.builder, self.test_tracker
        )
        
        # assertion
        self.assertIsInstance(result_tracker, SASTWorkflowTracker)
        self.assertEqual(len(result_tracker.issues), 2)
        self.assertEqual(result_tracker.iteration_count, 0)
        self.assertEqual(result_tracker.config, self.test_tracker.config)
        
        for issue_data in result_tracker.issues.values():
            self.assertIsNotNone(issue_data.similar_known_issues)
            self.assertEqual(issue_data.similar_known_issues, "Example 1: Similar buffer overflow...")
        
        first_issue_data = result_tracker.issues[self.sample_issues[0].id]
        self.assertIsNotNone(first_issue_data.analysis_response)
        self.assertEqual(first_issue_data.analysis_response.is_final, FinalStatus.TRUE.value)
        self.assertEqual(first_issue_data.analysis_response.investigation_result, 
                        CVEValidationStatus.NON_ISSUE.value)
        self.assertIn(KNOWN_NON_ISSUES_SHORT_JUSTIFICATION, first_issue_data.analysis_response.short_justifications)
        
        second_issue_data = result_tracker.issues[self.sample_issues[1].id]
        self.assertIsNotNone(second_issue_data.analysis_response)
        self.assertEqual(second_issue_data.analysis_response.is_final, FinalStatus.FALSE.value)
        self.assertEqual(second_issue_data.analysis_response.investigation_result, 
                        CVEValidationStatus.ISSUE.value)

    async def test_given_tracker_with_disabled_filter_config_when_filter_executed_then_no_changes_made(self):
        """Given a tracker with filter disabled in config, when filter is executed, then no changes are made."""
        # preparation
        if self.test_tracker.config:
            self.test_tracker.config.USE_KNOWN_FALSE_POSITIVE_FILE = False
        else:
            assert False, "Test tracker has no config"
        
        # testing
        result_tracker = await TestUtils.run_single_fn(
            filter, self.filter_config, self.builder, self.test_tracker
        )
        
        # assertion
        self.assertEqual(result_tracker.iteration_count, 0)
        self.assertEqual(len(result_tracker.issues), 2)
        
        # Verify no changes made - similar_known_issues remain empty, analysis_response remain unchanged
        for issue_data in result_tracker.issues.values():
            self.assertEqual(issue_data.similar_known_issues, "")
            self.assertIsNotNone(issue_data.analysis_response)
            self.assertEqual(issue_data.analysis_response.is_final, FinalStatus.FALSE.value)
            self.assertEqual(issue_data.analysis_response.investigation_result, CVEValidationStatus.ISSUE.value)

    async def test_given_tracker_with_no_config_when_filter_executed_then_validation_error_raised(self):
        """Given a tracker with no config, when filter is executed, then ValidationError is raised."""
        # preparation
        self.test_tracker.config = None
        
        # testing & assertion
        with self.assertRaises(Exception):
            await TestUtils.run_single_fn(
                filter, self.filter_config, self.builder, self.test_tracker
            )


if __name__ == '__main__':
    unittest.main()