"""
Unit tests for the evaluate_analysis tool's core function.
"""

import unittest
from unittest.mock import Mock, patch

from common.constants import DEFAULT_FIELD_VALUE
from Utils.validation_utils import ValidationError
from sast_agent_workflow.tools.evaluate_analysis import evaluate_analysis, EvaluateAnalysisConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.LLMResponse import FinalStatus
from dto.ResponseStructures import RecommendationsResponse, InstructionResponse
from common.config import Config
from aiq.builder.builder import Builder
from tests.aiq_tests.test_utils import TestUtils


class TestEvaluateAnalysisCore(unittest.IsolatedAsyncioTestCase):
    """Test cases for the evaluate_analysis core function (_evaluate_analysis_fn)."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_config = Mock(spec=Config)
        self.mock_config.MAX_ANALYSIS_ITERATIONS = 3
        self.evaluate_analysis_config = EvaluateAnalysisConfig(llm_name="test_llm")
        self.builder = Mock(spec=Builder)

    async def test__evaluate_analysis__non_final_issues_calls_recommend_and_updates_response(self):
        # Preparation
        issues = [
            TestUtils.create_sample_issue(issue_id="non_final_issue_1", issue_type="BUFFER_OVERFLOW"),
            TestUtils.create_sample_issue(issue_id="non_final_issue_2", issue_type="USE_AFTER_FREE")
        ]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.FALSE.value,
            justifications=["Initial analysis justification"],
            instructions=[]
        )
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        
        # Mock dependencies
        mock_llm = Mock()
        self.builder.get_llm.return_value = mock_llm
        
        with patch('sast_agent_workflow.tools.evaluate_analysis.VectorStoreService') as mock_vector_service_class, \
             patch('sast_agent_workflow.tools.evaluate_analysis.IssueAnalysisService') as mock_analysis_service_class:
            
            mock_vector_service = Mock()
            mock_vector_service_class.return_value = mock_vector_service
            
            mock_analysis_service = Mock()
            mock_analysis_service_class.return_value = mock_analysis_service
            
            # Mock recommendation response
            mock_instruction = InstructionResponse(
                expression_name="test_function",
                referring_source_code_path="/path/to/file.c",
                recommendation="Examine this function"
            )
            
            mock_recommendations_response = RecommendationsResponse(
                is_final=FinalStatus.TRUE.value,
                justifications=["LLM evaluation justification"],
                recommendations=["Fix the buffer overflow"],
                instructions=[mock_instruction]
            )
            mock_analysis_service.recommend.return_value = mock_recommendations_response
            
            # Testing
            result_tracker = await TestUtils.run_single_fn(evaluate_analysis, self.evaluate_analysis_config, self.builder, tracker)
            
            # Assertions
            self.assertIsInstance(result_tracker, SASTWorkflowTracker)
            self.assertEqual(len(result_tracker.issues), 2)
            
            # Verify recommend was called for both issues
            self.assertEqual(mock_analysis_service.recommend.call_count, 2)
            
            # Verify analysis responses were updated
            for issue_id in ["non_final_issue_1", "non_final_issue_2"]:
                analysis_response = result_tracker.issues[issue_id].analysis_response
                self.assertEqual(analysis_response.is_final, FinalStatus.TRUE.value)
                self.assertEqual(analysis_response.recommendations, mock_recommendations_response.recommendations)
                self.assertEqual(analysis_response.evaluation, mock_recommendations_response.justifications)
                self.assertEqual(len(analysis_response.instructions), 1)

    async def test__evaluate_analysis__final_issues_skips_processing(self):
        # Preparation
        issues = [
            TestUtils.create_sample_issue(issue_id="final_issue", issue_type="BUFFER_OVERFLOW")
        ]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.TRUE.value  # Already final
        )
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        
        # Mock dependencies
        mock_llm = Mock()
        self.builder.get_llm.return_value = mock_llm
        
        with patch('sast_agent_workflow.tools.evaluate_analysis.VectorStoreService') as mock_vector_service_class, \
             patch('sast_agent_workflow.tools.evaluate_analysis.IssueAnalysisService') as mock_analysis_service_class:
            
            mock_vector_service = Mock()
            mock_vector_service_class.return_value = mock_vector_service
            
            mock_analysis_service = Mock()
            mock_analysis_service_class.return_value = mock_analysis_service
            
            # Testing
            result_tracker = await TestUtils.run_single_fn(evaluate_analysis, self.evaluate_analysis_config, self.builder, tracker)
            
            # Assertions
            self.assertIsInstance(result_tracker, SASTWorkflowTracker)
            
            # Verify recommend was not called for final issues
            mock_analysis_service.recommend.assert_not_called()
            
            # Verify analysis response remains unchanged
            analysis_response = result_tracker.issues["final_issue"].analysis_response
            self.assertEqual(analysis_response.is_final, FinalStatus.TRUE.value)

    async def test__evaluate_analysis__mixed_issues_processes_only_non_final(self):
        # Preparation
        issues = [
            TestUtils.create_sample_issue(issue_id="final_issue", issue_type="BUFFER_OVERFLOW"),
            TestUtils.create_sample_issue(issue_id="non_final_issue", issue_type="USE_AFTER_FREE"),
            TestUtils.create_sample_issue(issue_id="no_analysis_issue", issue_type="MEMORY_LEAK")
        ]
        
        # Create mixed per issue data
        per_issue_data = {}
        per_issue_data["final_issue"] = PerIssueData(
            issue=issues[0],
            analysis_response=TestUtils.create_sample_analysis_response(
                is_final=FinalStatus.TRUE.value,
                justifications=["Final issue justification"]
            )
        )
        per_issue_data["non_final_issue"] = PerIssueData(
            issue=issues[1],
            analysis_response=TestUtils.create_sample_analysis_response(
                is_final=FinalStatus.FALSE.value,
                justifications=["Non-final issue justification"]
            )
        )
        per_issue_data["no_analysis_issue"] = PerIssueData(
            issue=issues[2],
            analysis_response=None  # No analysis response
        )
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        
        # Mock dependencies
        mock_llm = Mock()
        self.builder.get_llm.return_value = mock_llm
        
        with patch('sast_agent_workflow.tools.evaluate_analysis.VectorStoreService') as mock_vector_service_class, \
             patch('sast_agent_workflow.tools.evaluate_analysis.IssueAnalysisService') as mock_analysis_service_class:
            
            mock_vector_service = Mock()
            mock_vector_service_class.return_value = mock_vector_service
            
            mock_analysis_service = Mock()
            mock_analysis_service_class.return_value = mock_analysis_service
            
            mock_recommendations_response = RecommendationsResponse(
                is_final=FinalStatus.TRUE.value,
                justifications=["Evaluation complete"],
                recommendations=["Fix identified"],
                instructions=[]
            )
            mock_analysis_service.recommend.return_value = mock_recommendations_response
            
            # Testing
            result_tracker = await TestUtils.run_single_fn(evaluate_analysis, self.evaluate_analysis_config, self.builder, tracker)
            
            # Assertions
            self.assertIsInstance(result_tracker, SASTWorkflowTracker)
            
            # Verify recommend was called only once (for non-final issue)
            self.assertEqual(mock_analysis_service.recommend.call_count, 1)
            
            # Verify final issue remains unchanged
            self.assertEqual(result_tracker.issues["final_issue"].analysis_response.is_final, FinalStatus.TRUE.value)
            
            # Verify non-final issue was processed
            self.assertEqual(result_tracker.issues["non_final_issue"].analysis_response.is_final, FinalStatus.TRUE.value)
            
            # Verify issue with no analysis response remains unchanged
            self.assertIsNone(result_tracker.issues["no_analysis_issue"].analysis_response)

    async def test__evaluate_analysis__max_iterations_reached_skips_processing(self):
        # Preparation
        issues = [TestUtils.create_sample_issue(issue_id="test_issue", issue_type="BUFFER_OVERFLOW")]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.FALSE.value
        )
        
        # Set iteration count to max
        tracker = TestUtils.create_sample_tracker(
            issues_dict=per_issue_data, 
            config=self.mock_config,
            iteration_count=3  # Equals MAX_ANALYSIS_ITERATIONS
        )
        
        # Mock dependencies
        mock_llm = Mock()
        self.builder.get_llm.return_value = mock_llm
        
        with patch('sast_agent_workflow.tools.evaluate_analysis.VectorStoreService') as mock_vector_service_class, \
             patch('sast_agent_workflow.tools.evaluate_analysis.IssueAnalysisService') as mock_analysis_service_class:
            
            mock_vector_service = Mock()
            mock_vector_service_class.return_value = mock_vector_service
            
            mock_analysis_service = Mock()
            mock_analysis_service_class.return_value = mock_analysis_service
            
            # Testing
            result_tracker = await TestUtils.run_single_fn(evaluate_analysis, self.evaluate_analysis_config, self.builder, tracker)
            
            # Assertions
            self.assertIsInstance(result_tracker, SASTWorkflowTracker)
            
            # Verify recommend was not called due to max iterations
            mock_analysis_service.recommend.assert_not_called()
            
            # Verify tracker remains unchanged
            self.assertEqual(result_tracker.issues["test_issue"].analysis_response.is_final, FinalStatus.FALSE.value)

    async def test__evaluate_analysis__no_config_raises_validation_error(self):
        # Preparation
        issues = [TestUtils.create_sample_issue(issue_id="test_issue", issue_type="BUFFER_OVERFLOW")]
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(issues)
        
        tracker = TestUtils.create_sample_tracker(
            issues_dict=per_issue_data
        )
        tracker.config = None # No config
        
        # Testing & Assertion
        with self.assertRaises(ValidationError):
            await TestUtils.run_single_fn(evaluate_analysis, self.evaluate_analysis_config, self.builder, tracker)

    async def test__evaluate_analysis__empty_issues_raises_validation_error(self):
        # Preparation
        empty_tracker = SASTWorkflowTracker(config=self.mock_config, issues={})
        
        # Testing & Assertion
        with self.assertRaises(ValidationError):
            await TestUtils.run_single_fn(evaluate_analysis, self.evaluate_analysis_config, self.builder, empty_tracker)

    async def test__evaluate_analysis__llm_service_error_handles_gracefully(self):
        # Preparation
        issues = [TestUtils.create_sample_issue(issue_id="error_issue", issue_type="BUFFER_OVERFLOW")]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.FALSE.value,
            justifications=[DEFAULT_FIELD_VALUE]
            )
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        
        # Mock dependencies
        mock_llm = Mock()
        self.builder.get_llm.return_value = mock_llm
        
        with patch('sast_agent_workflow.tools.evaluate_analysis.VectorStoreService') as mock_vector_service_class, \
             patch('sast_agent_workflow.tools.evaluate_analysis.IssueAnalysisService') as mock_analysis_service_class:
            
            mock_vector_service = Mock()
            mock_vector_service_class.return_value = mock_vector_service
            
            mock_analysis_service = Mock()
            mock_analysis_service_class.return_value = mock_analysis_service
            
            # Mock recommend to raise an exception
            mock_analysis_service.recommend.side_effect = Exception("LLM service error")
            
            # Testing
            result_tracker = await TestUtils.run_single_fn(evaluate_analysis, self.evaluate_analysis_config, self.builder, tracker)
            
            # Assertions
            self.assertIsInstance(result_tracker, SASTWorkflowTracker)
            
            # Verify original analysis response is preserved
            analysis_response = result_tracker.issues["error_issue"].analysis_response
            self.assertEqual(analysis_response.justifications, [DEFAULT_FIELD_VALUE])
            self.assertEqual(analysis_response.is_final, FinalStatus.FALSE.value)

    async def test__evaluate_analysis__non_final_becomes_final_updates_correctly(self):
        # Preparation
        issues = [TestUtils.create_sample_issue(issue_id="becoming_final", issue_type="BUFFER_OVERFLOW")]
        
        per_issue_data = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.FALSE.value,
            justifications=["Initial analysis"],
            instructions=[]
        )
        
        tracker = TestUtils.create_sample_tracker(issues_dict=per_issue_data, config=self.mock_config)
        
        # Mock dependencies
        mock_llm = Mock()
        self.builder.get_llm.return_value = mock_llm
        
        with patch('sast_agent_workflow.tools.evaluate_analysis.VectorStoreService') as mock_vector_service_class, \
             patch('sast_agent_workflow.tools.evaluate_analysis.IssueAnalysisService') as mock_analysis_service_class:
            
            mock_vector_service = Mock()
            mock_vector_service_class.return_value = mock_vector_service
            
            mock_analysis_service = Mock()
            mock_analysis_service_class.return_value = mock_analysis_service
            
            # Mock recommendation response that makes it final
            justifications = "Analysis is now complete"
            recommendations = "Nothing to recommend"
            mock_recommendations_response = RecommendationsResponse(
                is_final=FinalStatus.TRUE.value,  # Becomes final
                justifications=[justifications],
                recommendations=[recommendations],
                instructions=[]  # No further instructions needed
            )
            mock_analysis_service.recommend.return_value = mock_recommendations_response
            
            # Testing
            result_tracker = await TestUtils.run_single_fn(evaluate_analysis, self.evaluate_analysis_config, self.builder, tracker)
            
            # Assertions
            analysis_response = result_tracker.issues["becoming_final"].analysis_response
            
            # Verify the issue is now final
            self.assertEqual(analysis_response.is_final, FinalStatus.TRUE.value)
            self.assertEqual(analysis_response.evaluation, [justifications])
            self.assertEqual(analysis_response.recommendations, [recommendations])
            self.assertEqual(len(analysis_response.instructions), 0)


if __name__ == '__main__':
    unittest.main() 