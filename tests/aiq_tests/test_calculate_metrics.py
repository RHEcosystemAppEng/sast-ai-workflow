"""
Unit tests for the calculate_metrics tool's core function.
"""

import unittest
from unittest.mock import Mock

from sast_agent_workflow.tools.calculate_metrics import calculate_metrics, CalculateMetricsConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from common.config import Config
from aiq.builder.builder import Builder
from tests.aiq_tests.test_utils import TestUtils


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

    async def test_given_sample_tracker_when_calculate_metrics_executed_then_preserves_tracker_structure_and_updates_metrics(self):
        """Given a sample tracker, when calculate_metrics is executed, then it preserves tracker structure and updates metrics.
            
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
        # preparation
        # TODO: Mock the actual metrics calculation dependencies when implemented
        
        # testing
        result_tracker = await TestUtils.run_single_fn(calculate_metrics, self.calculate_metrics_config, self.builder, self.sample_tracker)
        
        # assertion
        self.assertIsInstance(result_tracker, SASTWorkflowTracker)
        
        self.assertEqual(len(result_tracker.issues), 2)
        self.assertEqual(result_tracker.iteration_count, 0)
        self.assertEqual(result_tracker.config, self.sample_tracker.config)
        
        for per_issue_data in result_tracker.issues.values():
            self.assertIsNotNone(per_issue_data.issue)
            self.assertIsInstance(per_issue_data.issue, Issue)
            self.assertIsNotNone(per_issue_data.analysis_response)
            self.assertIsInstance(per_issue_data.analysis_response, AnalysisResponse)
        
        self.assertIsInstance(result_tracker.metrics, dict)
        
        # TODO: Add specific assertions for calculate_metrics tool state changes when implemented:
        # - Verify metrics dictionary is populated with calculated statistics
        # - Verify metrics only calculated when enabled in config


if __name__ == '__main__':
    unittest.main() 