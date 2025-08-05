"""
Unit tests for the judge_llm_analysis tool's core function.
"""

import unittest
from unittest.mock import Mock

from sast_agent_workflow.tools.judge_llm_analysis import judge_llm_analysis, JudgeLLMAnalysisConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from common.config import Config
from aiq.builder.builder import Builder
from tests.aiq_tests.test_utils import TestUtils


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

    async def test_given_sample_tracker_when_judge_llm_analysis_executed_then_updates_analysis_response_and_increments_iteration(self):
        """Given a sample tracker, when judge_llm_analysis is executed, then it updates analysis response and increments iteration.
           
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
        # preparation
        # TODO: Mock the actual LLM service dependencies when implemented
        
        # testing
        result_tracker = await TestUtils.run_single_fn(judge_llm_analysis, self.judge_llm_analysis_config, self.builder, self.sample_tracker)
        
        # assertion
        self.assertIsInstance(result_tracker, SASTWorkflowTracker)
        
        self.assertEqual(result_tracker.iteration_count, 1)
        
        self.assertEqual(len(result_tracker.issues), 2)
        self.assertEqual(result_tracker.config, self.sample_tracker.config)
        
        for per_issue_data in result_tracker.issues.values():
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