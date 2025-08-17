"""
Integration tests comparing the filter tool and capture_known_issues function.

This module tests that the tool filter from sast_agent_workflow.tools.filter
produces the same filtering results as the existing capture_known_issues function
from stage.filter_known_issues that is used in the run_script.

The goal is to verify that both approaches:
1. Mark the same issues as known false positives
2. Return the same similar known issues for the same input issues
"""

import unittest
from unittest.mock import Mock, patch

from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus
from dto.ResponseStructures import KnownFalsePositive
from common.config import Config
from aiq.builder.builder import Builder

from sast_agent_workflow.tools.filter import filter, FilterConfig
from stage.filter_known_issues import capture_known_issues
from tests.aiq_tests.test_utils import TestUtils


class TestFilterToolIntegration(unittest.IsolatedAsyncioTestCase):
    """Integration tests comparing filter tool with capture_known_issues function."""

    def setUp(self):
        """Set up test fixtures for integration testing."""
        # preparation
        self.filter_config = FilterConfig()
        self.builder = Mock(spec=Builder)
        
        # Create test issues
        self.test_issues = [
            Issue(
                id="issue1",
                issue_type="OVERRUN",
                issue_label="Buffer Overflow",
                issue_cve="CWE-119",
                issue_cve_link="https://cwe.mitre.org/data/definitions/119.html",
                trace="char buffer[10]; strcpy(buffer, user_input);"
            ),
            Issue(
                id="issue2", 
                issue_type="NULL_RETURNS",
                issue_label="Null Pointer Dereference", 
                issue_cve="CWE-476",
                issue_cve_link="https://cwe.mitre.org/data/definitions/476.html",
                trace="int *ptr = malloc(sizeof(int)); *ptr = 42;"
            ),
            Issue(
                id="issue3",
                issue_type="VARARGS",
                issue_label="Format String Vulnerability",
                issue_cve="CWE-134", 
                issue_cve_link="https://cwe.mitre.org/data/definitions/134.html",
                trace="printf(user_input);"
            )
        ]
        
        # Create mock config
        self.mock_config = Mock(spec=Config)
        self.mock_config.USE_KNOWN_FALSE_POSITIVE_FILE = True
        self.mock_config.KNOWN_FALSE_POSITIVE_FILE_PATH = "/test/known_fps.txt"
        self.mock_config.SIMILARITY_ERROR_THRESHOLD = 3
        
        # Create tracker for filter tool
        self.tracker = TestUtils.create_clean_filter_tracker(
            issues=self.test_issues,
            config=self.mock_config
        )
        
        # Create mock similar known issues for different scenarios
        # Scenario 1: issue1 - exact match, will be known FP
        self.mock_similar_issues_issue1 = [
            KnownFalsePositive(
                error_trace="char buffer[10]; strcpy(buffer, user_input);",
                reason_of_false_positive="Safe input validation prevents overflow",
                issue_type="OVERRUN",
                issue_cwe="CWE-119"
            )
        ]
        
        # Scenario 2: issue2 - similar but not exact match, not FP
        self.mock_similar_issues_issue2 = [
            KnownFalsePositive(
                error_trace="int *ptr = malloc(size); if(ptr) *ptr = value;",
                reason_of_false_positive="Null check prevents dereference",
                issue_type="NULL_RETURNS",
                issue_cwe="CWE-476"
            )
        ]
        
        # Scenario 3: issue3 - no similar issues
        self.mock_similar_issues_issue3 = []

    @patch('sast_agent_workflow.tools.filter.LLMService')
    @patch('sast_agent_workflow.tools.filter.create_known_issue_retriever') 
    @patch('stage.filter_known_issues.create_known_issue_retriever')
    @patch('stage.filter_known_issues.read_known_errors_file')
    async def test_given_same_issues_when_both_methods_applied_then_same_known_false_positives_identified(
        self, mock_read_file, mock_create_retriever_run_script, mock_create_retriever_tool, mock_llm_service_tool
    ):
        """Given the same issues, when both filter methods are applied, then the same known false positives are identified."""
        # preparation
        
        # Setup mock LLM service for filter tool
        mock_llm_instance = Mock()
        mock_llm_service_tool.return_value = mock_llm_instance
        
        # Setup mock retriever for filter tool - return different issues based on issue type
        def get_similar_issues(trace, issue_type):
            if issue_type == "OVERRUN":
                return self.mock_similar_issues_issue1
            elif issue_type == "NULL_RETURNS":
                return self.mock_similar_issues_issue2
            else:  # VARARGS
                return self.mock_similar_issues_issue3
        mock_retriever_tool = Mock()
        mock_retriever_tool.get_relevant_known_issues.side_effect = get_similar_issues
        mock_create_retriever_tool.return_value = mock_retriever_tool
        
        # Setup mock retriever for capture_known_issues - same logic
        mock_retriever_run_script = Mock()
        mock_retriever_run_script.get_relevant_known_issues.side_effect = get_similar_issues
        mock_create_retriever_run_script.return_value = mock_retriever_run_script
        
        # Setup mock LLM service for capture_known_issues
        mock_llm_service_run_script = Mock()
        mock_read_file.return_value = ["test known issues"]
        
        # Mock filter responses - only issue1 is known FP (exact match), issue2 and issue3 are not
        def mock_filter_response(issue, similar_issues_context):
            mock_response = Mock()
            if issue.id == "issue1":  # exact match scenario
                mock_response.result = "yes"
                mock_response.equal_error_trace = ["char buffer[10]; strcpy(buffer, user_input);"]
            else:  # issue2 (similar but not exact) and issue3 (no similar issues)
                mock_response.result = "no" 
                mock_response.equal_error_trace = []
            return mock_response
            
        mock_llm_service_run_script.filter_known_error = mock_filter_response
        
        # Mock is_known_false_positive for filter tool - only issue1 is FP
        def mock_is_fp(issue, similar_list, llm_service):
            if issue.id == "issue1":  # exact match scenario
                return True, ["char buffer[10]; strcpy(buffer, user_input);"]
            return False, []
            
        # Mock convert_similar_issues_to_examples_context_string
        def mock_convert_context(similar_list):
            if not similar_list:
                return ""
            return f"Example context with {len(similar_list)} similar issues"
            
        # testing
        with patch('sast_agent_workflow.tools.filter.is_known_false_positive', side_effect=mock_is_fp), \
             patch('sast_agent_workflow.tools.filter.convert_similar_issues_to_examples_context_string', side_effect=mock_convert_context), \
             patch('stage.filter_known_issues.convert_similar_issues_to_examples_context_string', side_effect=mock_convert_context):
            
            # Run filter tool
            filter_result = await TestUtils.run_single_fn(
                filter, self.filter_config, self.builder, self.tracker
            )
            
            # Run capture_known_issues
            run_script_known_fps, run_script_similar_contexts = capture_known_issues(
                mock_llm_service_run_script, self.test_issues, self.mock_config
            )
        
        # assertion
        
        # Extract known FPs from filter tool result
        filter_known_fps = set()
        for issue_id, issue_data in filter_result.issues.items():
            if (issue_data.analysis_response and 
                issue_data.analysis_response.is_final == FinalStatus.TRUE.value and 
                issue_data.analysis_response.investigation_result == CVEValidationStatus.FALSE_POSITIVE.value):
                filter_known_fps.add(issue_id)
        
        # Extract known FPs from run_script method
        run_script_known_fp_set = set(run_script_known_fps.keys())
        
        # Verify same issues identified as known FPs - only issue1 should be FP
        self.assertEqual(filter_known_fps, run_script_known_fp_set)
        self.assertEqual(filter_known_fps, {"issue1"})
        
        # Issue2: should have similar issues but not be FP (similar but not exact match)
        self.assertNotIn("issue2", filter_known_fps)
        self.assertNotIn("issue2", run_script_known_fp_set)
        self.assertIn("issue2", run_script_similar_contexts)
        self.assertNotEqual(run_script_similar_contexts["issue2"], "")
        self.assertNotEqual(filter_result.issues["issue2"].similar_known_issues, "")
        
        # Issue3: should have no similar issues and not be FP
        self.assertNotIn("issue3", filter_known_fps)
        self.assertNotIn("issue3", run_script_known_fp_set)
        self.assertEqual(filter_result.issues["issue3"].similar_known_issues, "")
        self.assertEqual(run_script_similar_contexts["issue3"], "")
        
        # Verify content of similar known issues matches between methods
        for issue_id in [issue.id for issue in self.test_issues]:
            self.assertIn(issue_id, run_script_similar_contexts)
            filter_similar = filter_result.issues[issue_id].similar_known_issues
            run_script_similar = run_script_similar_contexts[issue_id]
            # Both methods should produce the same similar issues content
            self.assertEqual(filter_similar, run_script_similar, 
                           f"Similar issues content mismatch for {issue_id}")


if __name__ == '__main__':
    unittest.main()