"""
Unit tests for the data_fetcher tool's core function.
"""

import unittest
from unittest.mock import Mock

from sast_agent_workflow.tools.data_fetcher import data_fetcher, DataFetcherConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from common.config import Config
from aiq.builder.builder import Builder
from tests.aiq_tests.test_utils import TestUtils


class TestDataFetcherCore(unittest.IsolatedAsyncioTestCase):
    """Test cases for the data_fetcher core function (_data_fetcher_fn)."""

    def setUp(self):
        """Set up test fixtures."""
        self.sample_issues = TestUtils.create_sample_issues()
        self.mock_config = Mock(spec=Config)
        self.data_fetcher_config = DataFetcherConfig()
        self.builder = Mock(spec=Builder)
        
        # Create a sample tracker with issues
        self.sample_tracker = TestUtils.create_sample_tracker(self.sample_issues)

    async def test_given_sample_tracker_when_data_fetcher_executed_then_preserves_tracker_structure_and_populates_source_code(self):
        """Given a sample tracker, when data_fetcher is executed, then it preserves tracker structure and populates source_code.
           
           Expected state changes for Data_Fetcher tool:
           - source_code field should be populated with relevant code snippets
           - For initial analysis: fetch based on report data
           - For subsequent loops: fetch based on instructions field
           - If no new data found with instructions: set is_final to True
           - Iteration count should remain unchanged
           
           TODO: Add comprehensive tests after tool implementation including:
           - Repository handler integration tests
           - Initial vs subsequent analysis scenarios
           - Instruction-based data fetching tests
           - Verification step tests (no new data found)
           - Edge case tests (empty issues, invalid instructions, etc.)
        """
        # preparation
        # TODO: Mock the actual repo handler dependencies when implemented
        
        # testing
        result_tracker = await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, self.sample_tracker)
        
        # assertion
        self.assertIsInstance(result_tracker, SASTWorkflowTracker)
        
        self.assertEqual(len(result_tracker.issues), 2)
        self.assertEqual(result_tracker.iteration_count, 0)
        self.assertEqual(result_tracker.config, self.sample_tracker.config)
        
        for per_issue_data in result_tracker.issues.values():
            self.assertIsNotNone(per_issue_data.issue)
            self.assertIsInstance(per_issue_data.issue, Issue)
            self.assertIsInstance(per_issue_data.analysis_response, AnalysisResponse)
            
            # TODO: Add specific assertions for data_fetcher tool state changes when implemented:
            # - Verify source_code is populated with relevant code
            # - Verify appropriate handling of instructions-based fetching
            # - Verify verification step logic (is_final flag when no new data)


if __name__ == '__main__':
    unittest.main()