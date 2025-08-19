"""
Unit tests for the judge_llm_analysis tool's core function.
"""

import unittest
from unittest.mock import Mock, patch

from sast_agent_workflow.tools.judge_llm_analysis import judge_llm_analysis, JudgeLLMAnalysisConfig
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from dto.ResponseStructures import EvaluationResponse
from common.config import Config
from aiq.builder.builder import Builder
from common.constants import TRUE, FALSE
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

    @patch('sast_agent_workflow.tools.judge_llm_analysis.LLMService')
    async def test_given_sample_tracker_when_judge_llm_analysis_executed_then_processes_non_final_issues(self, mock_llm_service_class):
        """Test that only non-final issues are processed by LLM analysis."""
        
        # Preparation: mock LLM service and its response
        mock_llm_service = Mock()
        mock_analysis_response = AnalysisResponse(
            investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
            is_final=TRUE,
            prompt="test prompt",
            justifications=["test justification"]
        )
        mock_evaluation_response = EvaluationResponse(
            critique_result=CVEValidationStatus.FALSE_POSITIVE.value,
            justifications=["test evaluation"]
        )
        mock_llm_service.investigate_issue.return_value = (mock_analysis_response, mock_evaluation_response)
        mock_llm_service_class.return_value = mock_llm_service
        
        # Set up tracker with mixed final/non-final issues
        self.sample_tracker.config = self.mock_config
        issue_ids = list(self.sample_tracker.issues.keys())
        
        # Make first issue final, second non-final
        self.sample_tracker.issues[issue_ids[0]].analysis_response = AnalysisResponse(
            investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
            is_final=TRUE
        )
        self.sample_tracker.issues[issue_ids[1]].analysis_response = AnalysisResponse(
            investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
            is_final=FALSE
        )
        
        # Add source code and similar issues to test context building
        self.sample_tracker.issues[issue_ids[1]].source_code = {
            "test.c": ["int main() { return 0; }"]
        }
        self.sample_tracker.issues[issue_ids[1]].similar_known_issues = "Similar issue context"
        
        # Execution
        result_tracker = await TestUtils.run_single_fn(judge_llm_analysis, self.judge_llm_analysis_config, self.builder, self.sample_tracker)
        
        # Verification
        # LLM service should be initialized
        mock_llm_service_class.assert_called_once_with(self.mock_config)
        
        # Only one issue should be analyzed (the non-final one)
        mock_llm_service.investigate_issue.assert_called_once()
        
        # Verify context structure and content
        call_args = mock_llm_service.investigate_issue.call_args
        context = call_args[0][0]  # First argument
        self.assertIn("*** Examples ***", context)
        self.assertIn("Similar issue context", context)
        self.assertIn("*** Source Code Context ***", context)
        self.assertIn("code of test.c file:", context)
        self.assertIn("int main() { return 0; }", context)
        
        # Analysis response should be updated for the non-final issue
        self.assertEqual(
            result_tracker.issues[issue_ids[1]].analysis_response.investigation_result,
            CVEValidationStatus.FALSE_POSITIVE.value
        )
        self.assertEqual(result_tracker.issues[issue_ids[1]].analysis_response.is_final, TRUE)
        
        # Final issue should remain unchanged
        self.assertEqual(
            result_tracker.issues[issue_ids[0]].analysis_response.investigation_result,
            CVEValidationStatus.TRUE_POSITIVE.value
        )
        
        # Iteration count should be incremented
        self.assertEqual(result_tracker.iteration_count, 1)

    async def test_given_tracker_without_config_when_judge_llm_analysis_executed_then_raises_error(self):
        """Test that tracker without config completes without LLM analysis."""
        
        # Preparation: tracker without config
        self.sample_tracker.config = None
        
        # Execution and verification
        result_tracker = await TestUtils.run_single_fn(judge_llm_analysis, self.judge_llm_analysis_config, self.builder, self.sample_tracker)
        
        # Iteration count should still be incremented
        self.assertEqual(result_tracker.iteration_count, 1)

    async def test_given_none_tracker_when_judge_llm_analysis_executed_then_raises_value_error(self):
        """Test that None tracker raises ValueError."""
        
        # Execution and verification
        with self.assertRaises(ValueError) as context:
            await TestUtils.run_single_fn(judge_llm_analysis, self.judge_llm_analysis_config, self.builder, None)
        
        self.assertEqual(str(context.exception), "Tracker must not be None")

    @patch('sast_agent_workflow.tools.judge_llm_analysis.LLMService')
    async def test_given_llm_service_init_failure_when_judge_llm_analysis_executed_then_raises_runtime_error(self, mock_llm_service_class):
        """Test that LLM service initialization failure raises RuntimeError."""
        
        # Preparation: mock LLM service to raise exception
        mock_llm_service_class.side_effect = Exception("LLM init failed")
        self.sample_tracker.config = self.mock_config
        
        # Execution and verification
        with self.assertRaises(RuntimeError) as context:
            await TestUtils.run_single_fn(judge_llm_analysis, self.judge_llm_analysis_config, self.builder, self.sample_tracker)
        
        self.assertIn("LLM service initialization failed", str(context.exception))

    @patch('sast_agent_workflow.tools.judge_llm_analysis.LLMService')
    async def test_given_llm_analysis_failure_when_judge_llm_analysis_executed_then_continues_processing(self, mock_llm_service_class):
        """Test that LLM analysis failure for one issue does not stop processing of remaining issues."""
        
        # Preparation: mock LLM service to fail on first call, succeed on second
        mock_llm_service = Mock()
        mock_llm_service.investigate_issue.side_effect = [
            Exception("Analysis failed"),
            (AnalysisResponse(investigation_result=CVEValidationStatus.FALSE_POSITIVE.value, is_final=TRUE), None)
        ]
        mock_llm_service_class.return_value = mock_llm_service
        
        self.sample_tracker.config = self.mock_config
        issue_ids = list(self.sample_tracker.issues.keys())
        
        # Make both issues non-final
        for issue_id in issue_ids[:2]:
            self.sample_tracker.issues[issue_id].analysis_response = AnalysisResponse(
                investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=FALSE
            )
        
        # Execution
        result_tracker = await TestUtils.run_single_fn(judge_llm_analysis, self.judge_llm_analysis_config, self.builder, self.sample_tracker)
        
        # Verification
        # Both issues should have been attempted
        self.assertEqual(mock_llm_service.investigate_issue.call_count, 2)
        
        # Second issue should have been updated despite first failure
        self.assertEqual(
            result_tracker.issues[issue_ids[1]].analysis_response.investigation_result,
            CVEValidationStatus.FALSE_POSITIVE.value
        )
        
        # Iteration count should still be incremented
        self.assertEqual(result_tracker.iteration_count, 1)

    async def test_given_tracker_with_invalid_issue_data_when_judge_llm_analysis_executed_then_skips_invalid_issues(self):
        """Test that invalid issue data is skipped gracefully."""
        
        # Preparation: add invalid issue data
        self.sample_tracker.config = self.mock_config
        self.sample_tracker.issues["invalid_issue"] = "not_a_per_issue_data_object"
        
        # Execution
        result_tracker = await TestUtils.run_single_fn(judge_llm_analysis, self.judge_llm_analysis_config, self.builder, self.sample_tracker)
        
        # Verification
        self.assertEqual(result_tracker.iteration_count, 1)
        
        # Invalid issue should remain unchanged
        self.assertEqual(result_tracker.issues["invalid_issue"], "not_a_per_issue_data_object")


if __name__ == "__main__":
    unittest.main()