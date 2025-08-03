"""
Unit tests for the judge_llm_analysis tool's core function.
"""

import unittest
from unittest.mock import Mock, patch

from common.constants import FALSE, NOT_A_FALSE_POSITIVE
from sast_agent_workflow.tools.judge_llm_analysis import judge_llm_analysis, JudgeLLMAnalysisConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from common.config import Config
from aiq.builder.builder import Builder
from sast_agent_workflow.tests.test_utils import TestUtils


class TestJudgeLLMAnalysisCore(unittest.IsolatedAsyncioTestCase):
    """Test cases for the judge_llm_analysis core function (_judge_llm_analysis_fn)."""

    def setUp(self):
        """Set up test fixtures."""
        self.sample_issues = TestUtils.create_sample_issues()
        self.mock_config = Mock(spec=Config)
        self.judge_llm_analysis_config = JudgeLLMAnalysisConfig()
        self.builder = Mock(spec=Builder)
        
        # Create a sample tracker with issues
        self.sample_tracker = TestUtils.create_sample_tracker(self.sample_issues)

    async def test_judge_llm_analysis_fn_basic_state_change(self):
        """Basic test for _judge_llm_analysis_fn execution - verifies SASTWorkflowTracker state changes.
           
           Expected state changes for Judge_LLM_Analysis tool:
           - analysis_response fields should be updated (investigation_result, justifications, is_final, prompt)
           - Only non-final issues should be processed
           - Full analysis context should be built from source_code and similar_known_issues
           - Global iteration_count should be incremented
           
           TODO: Add comprehensive tests after tool implementation including:
           - LLM service integration tests
           - Context building tests (source_code + similar_known_issues)
           - Non-final vs final issue processing
           - Analysis response field updates
           - Edge case tests (empty issues, etc.)
        """
        # TODO: Mock the actual LLM service dependencies when implemented
        
        judge_result = await TestUtils.run_single_fn(judge_llm_analysis, self.judge_llm_analysis_config, self.builder, self.sample_tracker)
        
        # Verify the result is still a SASTWorkflowTracker
        self.assertIsInstance(judge_result, SASTWorkflowTracker)
        
        # Verify iteration count was incremented (key state change for this tool)
        self.assertEqual(judge_result.iteration_count, 1)
        
        # Verify basic tracker properties
        self.assertEqual(len(judge_result.issues), 2)
        self.assertEqual(judge_result.config, self.sample_tracker.config)
        
        # Verify issues structure is preserved
        for issue_id, per_issue_data in judge_result.issues.items():
            self.assertIsNotNone(per_issue_data.issue)
            self.assertIsInstance(per_issue_data.issue, Issue)
            self.assertIsNotNone(per_issue_data.analysis_response)
            self.assertIsInstance(per_issue_data.analysis_response, AnalysisResponse)
            
            # TODO: Add specific assertions for judge_llm_analysis tool state changes when implemented:
            # - Verify analysis_response fields are populated (investigation_result, justifications, prompt)
            # - Verify only non-final issues are processed by LLM
            # - Verify context building from source_code and similar_known_issues


if __name__ == '__main__':
    unittest.main() 