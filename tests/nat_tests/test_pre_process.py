"""
Unit tests for the pre_process tool's core function.
"""

import unittest
from unittest.mock import Mock, patch

from dto.LLMResponse import FinalStatus
from sast_agent_workflow.tools.pre_process import pre_process, PreProcessConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from common.config import Config
from handlers.protocols import RepoHandlerProtocol
from nat.builder.builder import Builder
from tests.nat_tests.test_utils import TestUtils

class TestPreProcessCore(unittest.IsolatedAsyncioTestCase):
    """Test cases for the pre_process core function (_pre_process_fn)."""

    def setUp(self):
        # Create sample issues for testing
        self.sample_issues = TestUtils.create_sample_issues()
        self.mock_config = Mock(spec=Config)
        self.mock_repo_handler = Mock(spec=RepoHandlerProtocol)
        self.pre_process_config = PreProcessConfig()
        self.builder = Mock(spec=Builder)
    
    @patch('sast_agent_workflow.tools.pre_process.repo_handler_factory')
    @patch('sast_agent_workflow.tools.pre_process.read_sast_report')
    @patch('sast_agent_workflow.tools.pre_process.Config')
    async def test_given_valid_input_when_pre_process_executed_then_returns_structured_workflow_tracker(self, mock_config_class, mock_read_sast_report, mock_repo_handler_factory):
        """Given valid input, when pre_process is executed, then it returns a properly structured SASTWorkflowTracker."""
        # preparation
        mock_config_class.return_value = self.mock_config
        mock_read_sast_report.return_value = self.sample_issues
        mock_repo_handler_factory.return_value = self.mock_repo_handler
        
        # testing
        result_tracker = await TestUtils.run_single_fn(pre_process, self.pre_process_config, self.builder, {})
        
        # assertion
        self.assertIsInstance(result_tracker, SASTWorkflowTracker)
        
        self.assertEqual(len(result_tracker.issues), 2)
        self.assertEqual(result_tracker.iteration_count, 0)
        self.assertEqual(result_tracker.config, self.mock_config)
        
        for per_issue_data in result_tracker.issues.values():
            self.assertIsInstance(per_issue_data, PerIssueData)
            self.assertIsInstance(per_issue_data.issue, Issue)
            self.assertEqual(per_issue_data.source_code, {})
            self.assertEqual(per_issue_data.similar_known_issues, "")
            self.assertIsInstance(per_issue_data.analysis_response, AnalysisResponse)
            
            analysis_resp = per_issue_data.analysis_response
            self.assertEqual(analysis_resp.investigation_result, CVEValidationStatus.ISSUE.value)
            self.assertEqual(analysis_resp.is_final, FinalStatus.FALSE.value)
        
        mock_config_class.assert_called_once()
        mock_read_sast_report.assert_called_once_with(self.mock_config)
        mock_repo_handler_factory.assert_called_once_with(self.mock_config)

    @patch('sast_agent_workflow.tools.pre_process.repo_handler_factory')
    @patch('sast_agent_workflow.tools.pre_process.read_sast_report')
    @patch('sast_agent_workflow.tools.pre_process.Config')
    async def test_given_empty_issue_list_when_pre_process_executed_then_returns_empty_workflow_tracker(self, mock_config_class, mock_read_sast_report, mock_repo_handler_factory):
        """Given an empty issue list, when pre_process is executed, then it returns an empty SASTWorkflowTracker."""
        # preparation
        mock_config_class.return_value = self.mock_config
        mock_read_sast_report.return_value = []
        mock_repo_handler_factory.return_value = self.mock_repo_handler
        
        # testing
        result_tracker = await TestUtils.run_single_fn(pre_process, self.pre_process_config, self.builder, {})
            
        # assertion
        self.assertIsInstance(result_tracker, SASTWorkflowTracker)
        self.assertEqual(len(result_tracker.issues), 0)
        self.assertEqual(result_tracker.iteration_count, 0)
        self.assertEqual(result_tracker.config, self.mock_config)

    @patch('sast_agent_workflow.tools.pre_process.repo_handler_factory')
    @patch('sast_agent_workflow.tools.pre_process.read_sast_report')
    @patch('sast_agent_workflow.tools.pre_process.Config')
    async def test_given_sast_report_read_failure_when_pre_process_executed_then_raises_exception(self, mock_config_class, mock_read_sast_report, mock_repo_handler_factory):
        """Given SAST report read failure, when pre_process is executed, then it raises an exception."""
        # preparation
        mock_config_class.return_value = self.mock_config
        mock_read_sast_report.side_effect = Exception("Failed to read SAST report")
        mock_repo_handler_factory.return_value = self.mock_repo_handler
                    
        # testing & assertion
        with self.assertRaises(Exception) as context:
            await TestUtils.run_single_fn(pre_process, self.pre_process_config, self.builder, {})
            
            self.assertIn("Failed to read SAST report", str(context.exception))

    @patch('sast_agent_workflow.tools.pre_process.repo_handler_factory')
    @patch('sast_agent_workflow.tools.pre_process.read_sast_report')
    @patch('sast_agent_workflow.tools.pre_process.Config')
    async def test_given_repo_handler_factory_failure_when_pre_process_executed_then_raises_exception(self, mock_config_class, mock_read_sast_report, mock_repo_handler_factory):
        """Given repo handler initialization failure, when pre_process is executed, then it raises an exception."""
        # preparation
        mock_config_class.return_value = self.mock_config
        mock_read_sast_report.return_value = self.sample_issues
        mock_repo_handler_factory.side_effect = Exception("Failed to initialize repo handler")
        
        # testing & assertion
        with self.assertRaises(Exception) as context:
            await TestUtils.run_single_fn(pre_process, self.pre_process_config, self.builder, {})
        
        self.assertIn("Failed to initialize repo handler", str(context.exception))


if __name__ == '__main__':
    unittest.main() 