"""
Unit tests for the write_results tool's core function.
"""

import unittest
from unittest.mock import Mock

from sast_agent_workflow.tools.write_results import write_results, WriteResultsConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from common.config import Config
from aiq.builder.builder import Builder
from tests.aiq_tests.test_utils import TestUtils


class TestWriteResultsCore(unittest.IsolatedAsyncioTestCase):
    """Test cases for the write_results core function (_write_results_fn)."""

    def setUp(self):
        """Set up test fixtures."""
        self.sample_issues = TestUtils.create_sample_issues()
        self.mock_config = Mock(spec=Config)
        self.write_results_config = WriteResultsConfig()
        self.builder = Mock(spec=Builder)
        
        # Create a sample tracker with issues
        self.sample_tracker = TestUtils.create_sample_tracker(self.sample_issues)

    async def test_given_sample_tracker_when_write_results_executed_then_preserves_all_data_unchanged_and_calls_external_services(self):
        """Given a sample tracker, when write_results is executed, then it preserves all data unchanged,
           and calls external services as configured for output.
           
           Expected state changes for Write_Results tool:
           - This is a terminal node that produces final output files
           - Should write results to destinations specified in config (Google Sheet, CSV, etc.)
           - All tracker fields should remain unchanged (read-only operation)
           - Iteration count should remain unchanged
           - Function focuses on output generation rather than state modification
           
           TODO: Add comprehensive tests after tool implementation including:
           - Output file generation tests (CSV, Excel, Google Sheets)
           - Config-based destination handling tests
           - Error handling tests (file write failures, network issues)
        """
        # preparation
        # TODO: Mock the actual output writing dependencies when implemented
        
        # testing
        result_tracker = await TestUtils.run_single_fn(write_results, self.write_results_config, self.builder, self.sample_tracker)
        
        # assertion
        self.assertIsInstance(result_tracker, SASTWorkflowTracker)
        
        self.assertEqual(len(result_tracker.issues), 2)
        self.assertEqual(result_tracker.iteration_count, 0)
        self.assertEqual(result_tracker.config, self.sample_tracker.config)
        self.assertEqual(result_tracker.metrics, self.sample_tracker.metrics)
        
        for issue_id, per_issue_data in result_tracker.issues.items():
            self.assertIsNotNone(per_issue_data.issue)
            self.assertIsInstance(per_issue_data.issue, Issue)
            self.assertIsNotNone(per_issue_data.analysis_response)
            self.assertIsInstance(per_issue_data.analysis_response, AnalysisResponse)
            
            original_data = self.sample_tracker.issues[issue_id]
            self.assertEqual(per_issue_data.issue.id, original_data.issue.id)
            self.assertEqual(per_issue_data.source_code, original_data.source_code)
            self.assertEqual(per_issue_data.similar_known_issues, original_data.similar_known_issues)
        
        # TODO: Add specific assertions for write_results tool functionality when implemented:
        # - Verify relevant functions called with correct parameters to write to the results
        # - Verify no state modifications occur (read-only terminal operation)


if __name__ == '__main__':
    unittest.main() 