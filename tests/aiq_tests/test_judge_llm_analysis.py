"""
Unit tests for the judge_llm_analysis tool's core function.
"""

import unittest
from unittest.mock import Mock, patch, ANY

from sast_agent_workflow.tools.judge_llm_analysis import judge_llm_analysis, JudgeLLMAnalysisConfig
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus
from dto.ResponseStructures import EvaluationResponse, JudgeLLMResponse
from common.config import Config
from aiq.builder.builder import Builder
from tests.aiq_tests.test_utils import TestUtils
from Utils.validation_utils import ValidationError
from langchain_core.prompts import ChatPromptTemplate


class TestJudgeLLMAnalysisCore(unittest.IsolatedAsyncioTestCase):
    """Test cases for the judge_llm_analysis core function (_judge_llm_analysis_fn)."""

    def setUp(self):
        """Set up test fixtures."""
        self.sample_issues = TestUtils.create_sample_issues()
        self.mock_config = Mock(spec=Config)
        self.judge_llm_analysis_config = JudgeLLMAnalysisConfig(llm_name="test_llm")
        self.builder = Mock(spec=Builder)
        
        # Mock the LLM from builder
        self.mock_llm = Mock()
        self.builder.get_llm.return_value = self.mock_llm
        
        # Create a sample tracker with issues
        self.sample_tracker = TestUtils.create_sample_tracker(self.sample_issues)

    @patch('sast_agent_workflow.tools.judge_llm_analysis.IssueAnalysisService')
    @patch('sast_agent_workflow.tools.judge_llm_analysis.VectorStoreService')
    async def test_processes_non_final_issues(self, mock_vector_service_class, mock_issue_analysis_service_class):
        """Test that only non-final issues are processed by LLM analysis."""
        
        # Preparation: mock services and their responses
        mock_vector_service = Mock()
        mock_vector_service_class.return_value = mock_vector_service
        
        mock_issue_analysis_service = Mock()
        mock_llm_response = JudgeLLMResponse(
            investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
            justifications=["test justification"]
        )
        mock_issue_analysis_service.analyze_issue_core_only.return_value = ("test prompt", mock_llm_response)
        mock_issue_analysis_service_class.return_value = mock_issue_analysis_service
        
        # Set up tracker with mixed final/non-final issues
        self.sample_tracker.config = self.mock_config
        issue_ids = list(self.sample_tracker.issues.keys())
        
        # Make first issue final, second non-final
        self.sample_tracker.issues[issue_ids[0]].analysis_response = AnalysisResponse(
            investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
            is_final=FinalStatus.TRUE.value
        )
        self.sample_tracker.issues[issue_ids[1]].analysis_response = AnalysisResponse(
            investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
            is_final=FinalStatus.FALSE.value
        )
        
        # Add source code and similar issues to test context building
        self.sample_tracker.issues[issue_ids[1]].source_code = {
            "test.c": ["int main() { return 0; }"]
        }
        self.sample_tracker.issues[issue_ids[1]].similar_known_issues = "Similar issue context"
        
        # Execution
        result_tracker = await TestUtils.run_single_fn(judge_llm_analysis, self.judge_llm_analysis_config, self.builder, self.sample_tracker)
        
        # Verification
        # Services should be initialized correctly
        mock_vector_service_class.assert_called_once()
        mock_issue_analysis_service_class.assert_called_once_with(self.mock_config, mock_vector_service)
        
        # Builder should get the LLM
        self.builder.get_llm.assert_called_once_with("test_llm", wrapper_type=ANY)
        
        # Only one issue should be analyzed (the non-final one)
        mock_issue_analysis_service.analyze_issue_core_only.assert_called_once()
        
        # Verify analyze_issue_core_only was called with correct parameters
        call_args = mock_issue_analysis_service.analyze_issue_core_only.call_args
        self.assertEqual(call_args[1]['issue'], self.sample_tracker.issues[issue_ids[1]].issue)
        self.assertEqual(call_args[1]['main_llm'], self.mock_llm)
        
        # Verify context structure and content
        context = call_args[1]['context']  # context argument
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
        # Should be FALSE since no recommendations/full workflow done
        self.assertEqual(result_tracker.issues[issue_ids[1]].analysis_response.is_final, FinalStatus.FALSE.value)
        # Check that basic fields are populated
        self.assertEqual(result_tracker.issues[issue_ids[1]].analysis_response.prompt, "test prompt")
        self.assertEqual(result_tracker.issues[issue_ids[1]].analysis_response.justifications, ["test justification"])
        # Check that workflow fields are empty
        self.assertEqual(result_tracker.issues[issue_ids[1]].analysis_response.recommendations, [])
        self.assertEqual(result_tracker.issues[issue_ids[1]].analysis_response.instructions, [])
        self.assertEqual(result_tracker.issues[issue_ids[1]].analysis_response.short_justifications, "")
        
        # Final issue should remain unchanged
        self.assertEqual(
            result_tracker.issues[issue_ids[0]].analysis_response.investigation_result,
            CVEValidationStatus.TRUE_POSITIVE.value
        )
        
        # Iteration count should be incremented
        self.assertEqual(result_tracker.iteration_count, 1)

    async def test_raises_error_when_no_config(self):
        """Test that tracker without config raises ValidationError."""
        
        # Preparation: tracker without config
        self.sample_tracker.config = None
        
        # Execution and verification
        with self.assertRaises(ValidationError) as context:
            await TestUtils.run_single_fn(judge_llm_analysis, self.judge_llm_analysis_config, self.builder, self.sample_tracker)
        
        self.assertIn("No config found in tracker", str(context.exception))

    async def test_raises_error_when_tracker_none(self):
        """Test that None tracker raises ValueError."""
        
        # Execution and verification
        with self.assertRaises(ValueError) as context:
            await TestUtils.run_single_fn(judge_llm_analysis, self.judge_llm_analysis_config, self.builder, None)
        
        self.assertEqual(str(context.exception), "Tracker must not be None")

    @patch('sast_agent_workflow.tools.judge_llm_analysis.IssueAnalysisService')
    @patch('sast_agent_workflow.tools.judge_llm_analysis.VectorStoreService')
    async def test_raises_error_when_service_init_fails(self, mock_vector_service_class, mock_issue_analysis_service_class):
        """Test that Issue Analysis service initialization failure raises exception."""
        
        # Preparation: mock IssueAnalysisService to raise exception
        mock_vector_service_class.return_value = Mock()
        mock_issue_analysis_service_class.side_effect = Exception("Service init failed")
        self.sample_tracker.config = self.mock_config
        
        # Execution and verification
        with self.assertRaises(Exception) as context:
            await TestUtils.run_single_fn(judge_llm_analysis, self.judge_llm_analysis_config, self.builder, self.sample_tracker)
        
        self.assertIn("Service init failed", str(context.exception))

    @patch('sast_agent_workflow.tools.judge_llm_analysis.IssueAnalysisService')
    @patch('sast_agent_workflow.tools.judge_llm_analysis.VectorStoreService')
    async def test_continues_processing_on_analysis_failure(self, mock_vector_service_class, mock_issue_analysis_service_class):
        """Test that LLM analysis failure for one issue does not stop processing of remaining issues."""
        
        # Preparation: mock services
        mock_vector_service = Mock()
        mock_vector_service_class.return_value = mock_vector_service
        
        mock_issue_analysis_service = Mock()
        # Mock analyze_issue_core_only to fail first, succeed second
        mock_llm_response = JudgeLLMResponse(
            investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
            justifications=["test justification"]
        )
        mock_issue_analysis_service.analyze_issue_core_only.side_effect = [
            Exception("Analysis failed"),
            ("test prompt", mock_llm_response)
        ]
        mock_issue_analysis_service_class.return_value = mock_issue_analysis_service
        
        self.sample_tracker.config = self.mock_config
        issue_ids = list(self.sample_tracker.issues.keys())
        
        # Make both issues non-final
        for issue_id in issue_ids[:2]:
            self.sample_tracker.issues[issue_id].analysis_response = AnalysisResponse(
                investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=FinalStatus.FALSE.value
            )
        
        # Execution
        result_tracker = await TestUtils.run_single_fn(judge_llm_analysis, self.judge_llm_analysis_config, self.builder, self.sample_tracker)
        
        # Verification
        # Both issues should have been attempted
        self.assertEqual(mock_issue_analysis_service.analyze_issue_core_only.call_count, 2)
        
        # Second issue should have been updated despite first failure
        self.assertEqual(
            result_tracker.issues[issue_ids[1]].analysis_response.investigation_result,
            CVEValidationStatus.FALSE_POSITIVE.value
        )
        
        # Iteration count should still be incremented
        self.assertEqual(result_tracker.iteration_count, 1)

    @patch('sast_agent_workflow.tools.judge_llm_analysis.IssueAnalysisService')
    @patch('sast_agent_workflow.tools.judge_llm_analysis.VectorStoreService')
    async def test_skips_invalid_issue_data(self, mock_vector_service_class, mock_issue_analysis_service_class):
        """Test that invalid issue data is skipped gracefully."""
        
        # Preparation: add invalid issue data
        mock_vector_service = Mock()
        mock_vector_service_class.return_value = mock_vector_service
        mock_issue_analysis_service_class.return_value = Mock()
        
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