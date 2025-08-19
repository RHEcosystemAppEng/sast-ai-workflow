"""
Tests for SAST Agent Workflow Graph Builder.

This module tests the graph structure, conditional routing logic,
and overall workflow compilation using BDD-style given__when__then naming.
"""

import pytest
from unittest.mock import Mock, AsyncMock

from sast_agent_workflow.graph_builder import (
    should_continue_analysis,
    build_sast_workflow_graph,
)
from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.LLMResponse import FinalStatus, CVEValidationStatus, AnalysisResponse
from common.config import Config
from tests.aiq_tests.test_utils import TestUtils
from Utils.workflow_utils import WorkflowNode


class TestShouldContinueAnalysis:
    """Test the conditional edge logic for workflow routing."""
    
    def test__should_continue_analysis__issues_needing_second_analysis_under_limit_returns_data_fetcher(self):
        # Preparation
        issues = TestUtils.create_sample_issues(count=2)
        issues_dict = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.FALSE.value,  # Non-final issues
            instructions=[{"action": "analyze_deeper"}]  # Issues need second analysis
        )
        config = Mock(spec=Config)
        config.MAX_ANALYSIS_ITERATIONS = 3
        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=config,
            iteration_count=1  # Under limit
        )
        
        # Testing
        result = should_continue_analysis(tracker)
        
        # Assertion
        assert result == WorkflowNode.DATA_FETCHER.value
    
    def test__should_continue_analysis__no_issues_needing_second_analysis_returns_summarize_justifications(self):
        # Preparation
        issues = TestUtils.create_sample_issues(count=2)
        issues_dict = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.TRUE.value  # All final issues - no second analysis needed
        )
        config = Mock(spec=Config)
        config.MAX_ANALYSIS_ITERATIONS = 3
        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=config,
            iteration_count=1
        )
        
        # Testing
        result = should_continue_analysis(tracker)
        
        # Assertion
        assert result == WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value
    
    def test__should_continue_analysis__issues_needing_second_analysis_at_limit_returns_summarize_justifications(self):
        # Preparation
        issues = TestUtils.create_sample_issues(count=2)
        issues_dict = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.FALSE.value,  # Non-final issues
            instructions=[{"action": "analyze_deeper"}]  # Issues need second analysis
        )
        config = Mock(spec=Config)
        config.MAX_ANALYSIS_ITERATIONS = 2
        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=config,
            iteration_count=2  # At limit
        )
        
        # Testing
        result = should_continue_analysis(tracker)
        
        # Assertion
        assert result == WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value
    
    def test__should_continue_analysis__issues_needing_second_analysis_over_limit_returns_summarize_justifications(self):
        # Preparation
        issues = TestUtils.create_sample_issues(count=2)
        issues_dict = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.FALSE.value,
            instructions=[{"action": "analyze_deeper"}]  # Issues need second analysis
        )
        config = Mock(spec=Config)
        config.MAX_ANALYSIS_ITERATIONS = 2
        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=config,
            iteration_count=3  # Over limit
        )
        
        # Testing
        result = should_continue_analysis(tracker)
        
        # Assertion
        assert result == WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value
    
    def test__should_continue_analysis__config_missing_max_iterations_uses_default_returns_data_fetcher(self):
        # Preparation
        issues = TestUtils.create_sample_issues(count=1)
        issues_dict = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.FALSE.value,
            instructions=[{"action": "analyze_deeper"}]  # Issues need second analysis
        )
        config = Mock(spec=Config)
        # Don't set MAX_ANALYSIS_ITERATIONS attribute
        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=config,
            iteration_count=1  # Under default limit of 2
        )
        
        # Testing
        result = should_continue_analysis(tracker)
        
        # Assertion
        assert result == WorkflowNode.DATA_FETCHER.value
    
    def test__should_continue_analysis__config_missing_max_iterations_at_default_limit_returns_summarize_justifications(self):
        # Preparation
        issues = TestUtils.create_sample_issues(count=1)
        issues_dict = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.FALSE.value,
            instructions=[{"action": "analyze_deeper"}]  # Issues need second analysis
        )
        config = Mock(spec=Config)
        # Don't set MAX_ANALYSIS_ITERATIONS attribute
        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=config,
            iteration_count=2  # At limit of default (2)
        )
        
        # Testing
        result = should_continue_analysis(tracker)
        
        # Assertion
        assert result == WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value

    def test__should_continue_analysis__iteration_count_missing_sets_to_1_and_returns_data_fetcher(self):
        # Preparation
        issues = TestUtils.create_sample_issues(count=1)
        issues_dict = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.FALSE.value,
            instructions=[{"action": "analyze_deeper"}]  # Issues need second analysis
        )
        config = Mock(spec=Config)
        config.MAX_ANALYSIS_ITERATIONS = 2
        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=config
            )
        tracker.iteration_count = None
        
        # Testing
        result = should_continue_analysis(tracker)
        
        # Assertion
        assert result == WorkflowNode.DATA_FETCHER.value

    def test__should_continue_analysis__iteration_count_missing_sets_to_1_and_returns_summarize_justifications(self):
        # Preparation
        issues = TestUtils.create_sample_issues(count=1)
        issues_dict = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.FALSE.value,
            instructions=[{"action": "analyze_deeper"}]  # Issues need second analysis
        )
        config = Mock(spec=Config)
        config.MAX_ANALYSIS_ITERATIONS = 1
        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=config
            )
        tracker.iteration_count = None
        
        # Testing
        result = should_continue_analysis(tracker)
        
        # Assertion
        assert result == WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value
    
    def test__should_continue_analysis__non_final_issues_without_instructions_returns_summarize_justifications(self):
        # Preparation
        issues = TestUtils.create_sample_issues(count=2)
        issues_dict = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_final=FinalStatus.FALSE.value,
            instructions=[]  # No instructions = no second analysis needed
        )
        config = Mock(spec=Config)
        config.MAX_ANALYSIS_ITERATIONS = 3
        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=config,
            iteration_count=1
        )
        
        # Testing
        result = should_continue_analysis(tracker)
        
        # Assertion
        assert result == WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value
    
    def test__should_continue_analysis__mixed_issues_some_needing_second_analysis_returns_data_fetcher(self):
        # Preparation
        issues = TestUtils.create_sample_issues(count=3)
        # Create mixed issues: some final, some non-final, some needing second analysis
        issues_dict = {}
        for i, issue in enumerate(issues):
            if i == 0:
                # Final issue - no second analysis needed
                is_final = FinalStatus.TRUE.value
                instructions = []
            elif i == 1:
                # Non-final but needs second analysis
                is_final = FinalStatus.FALSE.value
                instructions = [{"action": "analyze_deeper"}]
            else:
                # Non-final but no instructions - no second analysis needed
                is_final = FinalStatus.FALSE.value
                instructions = []
            
            issues_dict[issue.id] = PerIssueData(
                issue=issue,
                analysis_response=AnalysisResponse(
                    investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                    is_final=is_final,
                    instructions=instructions
                )
            )
        config = Mock(spec=Config)
        config.MAX_ANALYSIS_ITERATIONS = 3
        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=config,
            iteration_count=1
        )
        
        # Testing
        result = should_continue_analysis(tracker)
        
        # Assertion
        assert result == WorkflowNode.DATA_FETCHER.value
    
    def test__should_continue_analysis__empty_issues_dict_returns_summarize_justifications(self):
        # Preparation
        config = Mock(spec=Config)
        config.MAX_ANALYSIS_ITERATIONS = 3
        tracker = SASTWorkflowTracker(
            issues={},  # Empty issues
            config=config,
            iteration_count=1
        )
        
        # Testing
        result = should_continue_analysis(tracker)
        
        # Assertion
        assert result == WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value
    
    def test__should_continue_analysis__issues_without_analysis_response_returns_summarize_justifications(self):
        # Preparation
        issues = TestUtils.create_sample_issues(count=2)
        issues_dict = {}
        for issue in issues:
            issues_dict[issue.id] = PerIssueData(
                issue=issue,
                analysis_response=None  # No analysis response
            )
        config = Mock(spec=Config)
        config.MAX_ANALYSIS_ITERATIONS = 3
        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=config,
            iteration_count=1
        )
        
        # Testing
        result = should_continue_analysis(tracker)
        
        # Assertion
        assert result == WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value
    
    def test__should_continue_analysis__non_final_false_positive_issues_returns_summarize_justifications(self):
        # Preparation - false positive issues don't need second analysis even if non-final
        issues = TestUtils.create_sample_issues(count=2)
        issues_dict = TestUtils.create_sample_per_issue_data_dict(
            issues, 
            is_false_positive=CVEValidationStatus.FALSE_POSITIVE.value,  # False positive
            is_final=FinalStatus.FALSE.value,
            instructions=[{"action": "analyze_deeper"}]  # Has instructions but is false positive
        )
        config = Mock(spec=Config)
        config.MAX_ANALYSIS_ITERATIONS = 3
        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=config,
            iteration_count=1
        )
        
        # Testing
        result = should_continue_analysis(tracker)
        
        # Assertion
        assert result == WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value


class TestBuildSastWorkflowGraph:
    """Test the graph building functionality."""
    
    @pytest.fixture
    def mock_nodes(self):
        """Create mock node functions for testing."""
        return {
            'pre_process_node': AsyncMock(),
            'filter_node': AsyncMock(),
            'data_fetcher_node': AsyncMock(),
            'judge_llm_analysis_node': AsyncMock(),
            'evaluate_analysis_node': AsyncMock(),
            'summarize_justifications_node': AsyncMock(),
            'calculate_metrics_node': AsyncMock(),
            'write_results_node': AsyncMock()
        }
    
    def test__build_sast_workflow_graph__mock_node_functions_compiles_successfully(self, mock_nodes):
        # Preparation
        # mock_nodes fixture provides all required node functions
        
        # Testing
        graph = build_sast_workflow_graph(**mock_nodes)
        
        # Assertion
        assert graph is not None
        assert hasattr(graph, 'invoke') or hasattr(graph, 'ainvoke')
