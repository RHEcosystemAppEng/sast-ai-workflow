"""
Unit tests for the evaluate_analysis tool's core function.
"""

import unittest
from unittest.mock import Mock, patch

from common.constants import FALSE, NOT_A_FALSE_POSITIVE
from sast_agent_workflow.tools.evaluate_analysis import evaluate_analysis, EvaluateAnalysisConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from common.config import Config
from aiq.builder.builder import Builder
from sast_agent_workflow.tests.test_utils import TestUtils


class TestEvaluateAnalysisCore(unittest.IsolatedAsyncioTestCase):
    """Test cases for the evaluate_analysis core function (_evaluate_analysis_fn)."""

    def setUp(self):
        """Set up test fixtures."""
        self.sample_issues = TestUtils.create_sample_issues()
        self.mock_config = Mock(spec=Config)
        self.evaluate_analysis_config = EvaluateAnalysisConfig()
        self.builder = Mock(spec=Builder)
        
        # Create a sample tracker with issues
        self.sample_tracker = TestUtils.create_sample_tracker(self.sample_issues)

    async def test_evaluate_analysis_fn_basic_state_change(self):
        """Basic test for _evaluate_analysis_fn execution - verifies SASTWorkflowTracker state changes.
           
           Expected state changes for Evaluate_Analysis tool:
           - is_final flag should be updated for processed issues
           - evaluation field should be populated with LLM evaluation results
           - For non-final decisions: recommendations and instructions should be populated
           - Only processes issues where analysis_response.is_final is False
           - Iteration count should remain unchanged
           
           TODO: Add comprehensive tests after tool implementation including:
           - LLM evaluation service integration tests
           - Final vs non-final decision logic tests
           - Recommendations and instructions generation tests
           - Non-final issue filtering tests
           - Edge case tests (empty issues, etc.)
        """
        # TODO: Mock the actual LLM evaluation service dependencies when implemented
        
        evaluate_result = await TestUtils.run_single_fn(evaluate_analysis, self.evaluate_analysis_config, self.builder, self.sample_tracker)
        
        # Verify the result is still a SASTWorkflowTracker
        self.assertIsInstance(evaluate_result, SASTWorkflowTracker)
        
        # Verify basic tracker properties remain intact
        self.assertEqual(len(evaluate_result.issues), 2)
        self.assertEqual(evaluate_result.iteration_count, 0)  # Should not change in evaluate_analysis
        self.assertEqual(evaluate_result.config, self.sample_tracker.config)
        
        # Verify issues structure is preserved
        for issue_id, per_issue_data in evaluate_result.issues.items():
            self.assertIsNotNone(per_issue_data.issue)
            self.assertIsInstance(per_issue_data.issue, Issue)
            self.assertIsNotNone(per_issue_data.analysis_response)
            self.assertIsInstance(per_issue_data.analysis_response, AnalysisResponse)
            
            # TODO: Add specific assertions for evaluate_analysis tool state changes when implemented:
            # - Verify evaluation field is populated with LLM evaluation
            # - Verify is_final flag is updated based on evaluation decision
            # - Verify recommendations and instructions are populated for non-final decisions


if __name__ == '__main__':
    unittest.main() 