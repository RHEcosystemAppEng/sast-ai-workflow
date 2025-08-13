"""
Integration tests for summarize_justifications workflow node.
"""

import unittest
from unittest.mock import Mock

from sast_agent_workflow.tools.summarize_justifications import summarize_justifications, SummarizeJustificationsConfig
from services.issue_analysis_service import IssueAnalysisService
from services.vector_store_service import VectorStoreService
from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from dto.ResponseStructures import JustificationsSummary
from common.config import Config
from aiq.builder.builder import Builder
from tests.aiq_tests.test_utils import TestUtils


class TestSummarizeJustificationsIntegration(unittest.IsolatedAsyncioTestCase):

    def setUp(self):
        self.mock_config = Mock(spec=Config)
        self.mock_config.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT = "Summarize the following justifications"
        self.mock_config.JUSTIFICATION_SUMMARY_HUMAN_PROMPT = "Create a short summary: {actual_prompt} {examples_str} {response}"
        
        self.summarize_justifications_config = SummarizeJustificationsConfig(llm_name="test_llm")
        self.builder = Mock(spec=Builder)
        self.mock_llm = Mock()
        self.builder.get_llm.return_value = self.mock_llm

    async def test__integration__summarize_justifications_node_produces_same_content_as_direct_service_call(self):
        # preparation
        issue = TestUtils.create_sample_issue(issue_id="integration_test_issue", issue_type="BUFFER_OVERFLOW")
        
        analysis_response = TestUtils.create_sample_analysis_response(
            is_final="TRUE",
            justifications=["This is a buffer overflow vulnerability", "The bounds check is missing"],
            short_justifications="",
            prompt="Test analysis prompt for integration"
        )
        
        per_issue_data = PerIssueData(
            issue=issue,
            source_code={"test.c": "int main() { return 0; }"},
            similar_known_issues="",
            analysis_response=analysis_response
        )
        
        tracker = SASTWorkflowTracker(
            config=self.mock_config,
            issues={"integration_test_issue": per_issue_data}
        )
        
        expected_summary_content = "Buffer overflow detected with missing bounds check"
        mock_summary_response = JustificationsSummary(short_justifications=expected_summary_content)
        
        # testing
        with unittest.mock.patch('sast_agent_workflow.tools.summarize_justifications.IssueAnalysisService') as mock_service_class:
            mock_service = Mock()
            mock_service_class.return_value = mock_service
            mock_service.summarize_justification.return_value = mock_summary_response
            
            node_result_tracker = await TestUtils.run_single_fn(
                summarize_justifications, 
                self.summarize_justifications_config, 
                self.builder, 
                tracker
            )
            
            node_call_args = mock_service.summarize_justification.call_args
        
        vector_service = VectorStoreService()
        direct_issue_service = IssueAnalysisService(self.mock_config, vector_service)
        
        with unittest.mock.patch.object(direct_issue_service, 'summarize_justification', return_value=mock_summary_response) as mock_direct_call:
            direct_service_result = direct_issue_service.summarize_justification(
                actual_prompt=analysis_response.prompt,
                response=analysis_response,
                issue_id=issue.id,
                main_llm=self.mock_llm
            )
            
            direct_call_args = mock_direct_call.call_args
        
        # assertion
        node_result_content = node_result_tracker.issues["integration_test_issue"].analysis_response.short_justifications
        direct_result_content = direct_service_result.short_justifications
        
        self.assertEqual(node_result_content, direct_result_content)
        self.assertEqual(node_result_content, expected_summary_content)
        self.assertEqual(node_call_args[1]['actual_prompt'], direct_call_args[1]['actual_prompt'])
        self.assertEqual(node_call_args[1]['issue_id'], direct_call_args[1]['issue_id'])
        
        node_response_justifications = node_call_args[1]['response'].justifications
        direct_response_justifications = direct_call_args[1]['response'].justifications
        self.assertEqual(node_response_justifications, direct_response_justifications)
        
        self.assertEqual(
            node_result_tracker.issues["integration_test_issue"].analysis_response.justifications,
            ["This is a buffer overflow vulnerability", "The bounds check is missing"]
        )
        self.assertEqual(
            node_result_tracker.issues["integration_test_issue"].analysis_response.prompt,
            "Test analysis prompt for integration"
        )

    async def test__integration__multiple_issues_with_different_content_produce_consistent_results(self):
        # preparation
        issues_data = [
            {
                "issue_id": "issue_1",
                "justifications": ["Memory leak detected", "Resource not freed"],
                "expected_summary": "Memory leak with resource management issue"
            },
            {
                "issue_id": "issue_2", 
                "justifications": ["SQL injection vulnerability", "Input validation missing"],
                "expected_summary": "SQL injection due to missing input validation"
            }
        ]
        
        tracker_issues = {}
        direct_service_calls = []
        
        for issue_data in issues_data:
            issue = TestUtils.create_sample_issue(issue_id=issue_data["issue_id"], issue_type="SECURITY")
            
            analysis_response = TestUtils.create_sample_analysis_response(
                is_final="TRUE",
                justifications=issue_data["justifications"],
                short_justifications="",
                prompt=f"Analysis prompt for {issue_data['issue_id']}"
            )
            
            per_issue_data = PerIssueData(
                issue=issue,
                source_code={f"{issue_data['issue_id']}.c": "code"},
                similar_known_issues="",
                analysis_response=analysis_response
            )
            
            tracker_issues[issue_data["issue_id"]] = per_issue_data
            direct_service_calls.append((analysis_response, issue, issue_data["expected_summary"]))
        
        tracker = SASTWorkflowTracker(
            config=self.mock_config,
            issues=tracker_issues
        )
        
        call_count = 0
        def mock_summarize_side_effect(*args, **kwargs):
            nonlocal call_count
            result = JustificationsSummary(short_justifications=issues_data[call_count]["expected_summary"])
            call_count += 1
            return result
        
        # testing
        with unittest.mock.patch('sast_agent_workflow.tools.summarize_justifications.IssueAnalysisService') as mock_service_class:
            mock_service = Mock()
            mock_service_class.return_value = mock_service
            mock_service.summarize_justification.side_effect = mock_summarize_side_effect
            
            node_result_tracker = await TestUtils.run_single_fn(
                summarize_justifications,
                self.summarize_justifications_config,
                self.builder, 
                tracker
            )
        
        vector_service = VectorStoreService()
        direct_issue_service = IssueAnalysisService(self.mock_config, vector_service)
        
        direct_results = []
        for i, (analysis_response, issue, expected_summary) in enumerate(direct_service_calls):
            with unittest.mock.patch.object(direct_issue_service, 'summarize_justification') as mock_direct:
                mock_direct.return_value = JustificationsSummary(short_justifications=expected_summary)
                
                direct_result = direct_issue_service.summarize_justification(
                    actual_prompt=analysis_response.prompt,
                    response=analysis_response,
                    issue_id=issue.id,
                    main_llm=self.mock_llm
                )
                direct_results.append((issue.id, direct_result.short_justifications))
        
        # assertion
        for issue_id, expected_direct_result in direct_results:
            node_result = node_result_tracker.issues[issue_id].analysis_response.short_justifications
            self.assertEqual(node_result, expected_direct_result)
        
        self.assertEqual(mock_service.summarize_justification.call_count, len(issues_data))
        
        for issue_data in issues_data:
            issue_id = issue_data["issue_id"]
            expected_summary = issue_data["expected_summary"]
            actual_summary = node_result_tracker.issues[issue_id].analysis_response.short_justifications
            
            self.assertEqual(actual_summary, expected_summary)
            self.assertEqual(
                node_result_tracker.issues[issue_id].analysis_response.justifications,
                issue_data["justifications"]
            )


if __name__ == '__main__':
    unittest.main()