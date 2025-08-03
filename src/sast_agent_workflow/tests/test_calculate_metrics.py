"""
Unit tests for the calculate_metrics tool's core function.
"""

import unittest
from unittest.mock import Mock, patch

from common.constants import FALSE, NOT_A_FALSE_POSITIVE
from sast_agent_workflow.tools.calculate_metrics import calculate_metrics, CalculateMetricsConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from common.config import Config
from aiq.builder.builder import Builder
from sast_agent_workflow.tests.test_utils import TestUtils


class TestCalculateMetricsCore(unittest.IsolatedAsyncioTestCase):
    """Test cases for the calculate_metrics core function (_calculate_metrics_fn)."""

    def setUp(self):
        """Set up test fixtures."""
        self.sample_issues = TestUtils.create_sample_issues()
        self.mock_config = Mock(spec=Config)
        self.calculate_metrics_config = CalculateMetricsConfig()
        self.builder = Mock(spec=Builder)
        
        # Create a sample tracker with issues
        self.sample_tracker = TestUtils.create_sample_tracker(self.sample_issues)

    async def test_calculate_metrics_fn_basic_state_change(self):
        """Basic test for _calculate_metrics_fn execution - verifies SASTWorkflowTracker state changes.
           
           Expected state changes for Calculate_Metrics tool:
           - global metrics dictionary should be populated with ragas metrics
           - Only runs if metrics calculation is enabled in config
           - All other tracker fields should remain unchanged
           - Iteration count should remain unchanged
           
           TODO: Add comprehensive tests after tool implementation including:
           - ragas metrics calculation logic tests
           - Config-based enable/disable tests
           - Error handling tests (calculation failures)
        """
        # TODO: Mock the actual metrics calculation dependencies when implemented
        
        calculate_result = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, self.sample_tracker)
        
        # Verify the result is still a SASTWorkflowTracker
        self.assertIsInstance(calculate_result, SASTWorkflowTracker)
        
        # Verify basic tracker properties remain intact
        self.assertEqual(len(calculate_result.issues), 2)
        self.assertEqual(calculate_result.iteration_count, 0)  # Should not change in calculate_metrics
        self.assertEqual(calculate_result.config, self.sample_tracker.config)
        
        # Verify issues structure is preserved
        for issue_id, per_issue_data in calculate_result.issues.items():
            self.assertIsNotNone(per_issue_data.issue)
            self.assertIsInstance(per_issue_data.issue, Issue)
            self.assertIsNotNone(per_issue_data.analysis_response)
            self.assertIsInstance(per_issue_data.analysis_response, AnalysisResponse)
        
        # Verify metrics structure exists (key state change for this tool)
        self.assertIsInstance(calculate_result.metrics, dict)
        
        # TODO: Add specific assertions for calculate_metrics tool state changes when implemented:
        # - Verify metrics dictionary is populated with calculated statistics
        # - Verify metrics only calculated when enabled in config


if __name__ == '__main__':
    unittest.main() 