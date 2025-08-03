"""
Unit tests for the summarize_justifications tool's core function.
"""

import unittest
from unittest.mock import Mock, patch

from common.constants import FALSE, NOT_A_FALSE_POSITIVE
from sast_agent_workflow.tools.summarize_justifications import summarize_justifications, SummarizeJustificationsConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from common.config import Config
from aiq.builder.builder import Builder
from sast_agent_workflow.tests.test_utils import TestUtils


class TestSummarizeJustificationsCore(unittest.IsolatedAsyncioTestCase):
    """Test cases for the summarize_justifications core function (_summarize_justifications_fn)."""

    def setUp(self):
        """Set up test fixtures."""
        self.sample_issues = TestUtils.create_sample_issues()
        self.mock_config = Mock(spec=Config)
        self.summarize_justifications_config = SummarizeJustificationsConfig()
        self.builder = Mock(spec=Builder)
        
        # Create a sample tracker with issues
        self.sample_tracker = TestUtils.create_sample_tracker(self.sample_issues)

    async def test_summarize_justifications_fn_basic_state_change(self):
        """Basic test for _summarize_justifications_fn execution - verifies SASTWorkflowTracker state changes.
           
           Expected state changes for Summarize_Justifications tool:
           - short_justifications field should be populated for non-final and final issues
           - Original justifications field should remain intact
           - All other analysis response fields should remain unchanged
           - Iteration count should remain unchanged
           
           TODO: Add comprehensive tests after tool implementation including:
           - Justification preservation tests (original justifications remain)
           - short_justifications field updated in analysis response
           - Error handling tests (LLM summarization failures)
           - Edge case tests (empty justifications, etc.)
        """
        # TODO: Mock the actual LLM summarization service dependencies when implemented
        
        summarize_result = await TestUtils.run_single_fn(summarize_justifications, self.summarize_justifications_config, self.builder, self.sample_tracker)
        
        # Verify the result is still a SASTWorkflowTracker
        self.assertIsInstance(summarize_result, SASTWorkflowTracker)
        
        # Verify basic tracker properties remain intact
        self.assertEqual(len(summarize_result.issues), 2)
        self.assertEqual(summarize_result.iteration_count, 0)  # Should not change in summarize_justifications
        self.assertEqual(summarize_result.config, self.sample_tracker.config)
        
        # Verify issues structure is preserved
        for issue_id, per_issue_data in summarize_result.issues.items():
            self.assertIsNotNone(per_issue_data.issue)
            self.assertIsInstance(per_issue_data.issue, Issue)
            self.assertIsNotNone(per_issue_data.analysis_response)
            self.assertIsInstance(per_issue_data.analysis_response, AnalysisResponse)
            
            # TODO: Add specific assertions for summarize_justifications tool state changes when implemented:
            # - Verify short_justifications field is populated for final issues
            # - Verify both final and non-final issues are processed by LLM
            # - Verify original justifications field remains intact
            # - Verify all other analysis response fields remain unchanged


if __name__ == '__main__':
    unittest.main() 