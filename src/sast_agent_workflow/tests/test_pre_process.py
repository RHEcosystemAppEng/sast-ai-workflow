"""
Unit tests for the pre_process tool's core function.
"""

import unittest
from unittest.mock import Mock, patch, AsyncMock
import asyncio

from common.constants import FALSE, NOT_A_FALSE_POSITIVE
from sast_agent_workflow.tools.pre_process import pre_process, PreProcessConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from common.config import Config
from handlers.protocols import RepoHandlerProtocol
from aiq.builder.builder import Builder
from sast_agent_workflow.tests.test_utils import TestUtils

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
    async def test_pre_process_fn_successful_execution(self, mock_config_class, mock_read_sast_report, mock_repo_handler_factory):
        """Test _pre_process_fn execution with valid input.
           Verify that the result is a SASTWorkflowTracker and that the issues are properly structured.
        """
        # Setup mocks
        mock_config_class.return_value = self.mock_config
        mock_read_sast_report.return_value = self.sample_issues
        mock_repo_handler_factory.return_value = self.mock_repo_handler
        
        pre_process_result = await TestUtils.run_single_fn(pre_process, self.pre_process_config, self.builder, {})
        
        # Verify the result is a SASTWorkflowTracker
        self.assertIsInstance(pre_process_result, SASTWorkflowTracker)
        
        # Verify tracker properties
        self.assertEqual(len(pre_process_result.issues), 2)
        self.assertEqual(pre_process_result.iteration_count, 0)
        self.assertEqual(pre_process_result.config, self.mock_config)
        
        # Verify issues are properly structured
        for _, per_issue_data in pre_process_result.issues.items():
            self.assertIsInstance(per_issue_data, PerIssueData)
            self.assertIsInstance(per_issue_data.issue, Issue)
            self.assertEqual(per_issue_data.source_code, {})
            self.assertEqual(per_issue_data.similar_known_issues, "")
            self.assertIsInstance(per_issue_data.analysis_response, AnalysisResponse)
            
            # Verify default analysis response
            analysis_resp = per_issue_data.analysis_response
            self.assertEqual(analysis_resp.investigation_result, NOT_A_FALSE_POSITIVE)
            self.assertEqual(analysis_resp.is_final, FALSE)
        
        # Verify external dependencies were called
        mock_config_class.assert_called_once()
        mock_read_sast_report.assert_called_once_with(self.mock_config)
        mock_repo_handler_factory.assert_called_once_with(self.mock_config)

    @patch('sast_agent_workflow.tools.pre_process.repo_handler_factory')
    @patch('sast_agent_workflow.tools.pre_process.read_sast_report')
    @patch('sast_agent_workflow.tools.pre_process.Config')
    async def test_pre_process_fn_empty_issues(self, mock_config_class, mock_read_sast_report, mock_repo_handler_factory):
        """Test _pre_process_fn with empty issue list."""
        # Setup mocks
        mock_config_class.return_value = self.mock_config
        mock_read_sast_report.return_value = []  # Empty issue list
        mock_repo_handler_factory.return_value = self.mock_repo_handler
        
        pre_process_result = await TestUtils.run_single_fn(pre_process, self.pre_process_config, self.builder, {})
            
        # Verify the result
        self.assertIsInstance(pre_process_result, SASTWorkflowTracker)
        self.assertEqual(len(pre_process_result.issues), 0)
        self.assertEqual(pre_process_result.iteration_count, 0)
        self.assertEqual(pre_process_result.config, self.mock_config)

    @patch('sast_agent_workflow.tools.pre_process.repo_handler_factory')
    @patch('sast_agent_workflow.tools.pre_process.read_sast_report')
    @patch('sast_agent_workflow.tools.pre_process.Config')
    async def test_pre_process_fn_read_sast_report_error(self, mock_config_class, mock_read_sast_report, mock_repo_handler_factory):
        """Test _pre_process_fn when read_sast_report raises an exception."""
        # Setup mocks
        mock_config_class.return_value = self.mock_config
        mock_read_sast_report.side_effect = Exception("Failed to read SAST report")
        mock_repo_handler_factory.return_value = self.mock_repo_handler
                    
        # Should raise the exception
        with self.assertRaises(Exception) as context:
            await TestUtils.run_single_fn(pre_process, self.pre_process_config, self.builder, {})
            
            self.assertIn("Failed to read SAST report", str(context.exception))

    @patch('sast_agent_workflow.tools.pre_process.repo_handler_factory')
    @patch('sast_agent_workflow.tools.pre_process.read_sast_report')
    @patch('sast_agent_workflow.tools.pre_process.Config')
    async def test_pre_process_fn_repo_handler_error(self, mock_config_class, mock_read_sast_report, mock_repo_handler_factory):
        """Test _pre_process_fn when repo_handler_factory raises an exception."""
        # Setup mocks
        mock_config_class.return_value = self.mock_config
        mock_read_sast_report.return_value = self.sample_issues
        mock_repo_handler_factory.side_effect = Exception("Failed to initialize repo handler")
        
        # Should raise the exception
        with self.assertRaises(Exception) as context:
            await TestUtils.run_single_fn(pre_process, self.pre_process_config, self.builder, {})
        
        self.assertIn("Failed to initialize repo handler", str(context.exception))


if __name__ == '__main__':
    unittest.main() 