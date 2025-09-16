"""
Tests for SAST Agent Workflow Graph Builder.

This module tests the graph structure, conditional routing logic,
and overall workflow compilation using BDD-style given__when__then naming.
"""

from unittest import mock
from common.constants import GRAPH_BUILDER_VERIFY_GRAPH_STRUCTURE_LOG
import pytest
from unittest.mock import Mock, AsyncMock

from sast_agent_workflow.graph_builder import (
    should_continue_analysis,
    build_sast_workflow_graph,
    verify_graph_structure,
)
from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.LLMResponse import FinalStatus, CVEValidationStatus, AnalysisResponse
from common.config import Config
from tests.nat_tests.test_utils import TestUtils
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

        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=None,
            iteration_count=1  # Under default limit of 2
        )
        
        # Testing
        with mock.patch('common.config.Config.validate_configurations'):
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
        
        tracker = SASTWorkflowTracker(
            issues=issues_dict,
            config=None,
            iteration_count=2  # At limit of default (2)
        )
        
        # Testing
        with mock.patch('common.config.Config.validate_configurations'):
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


class TestGraphBuilder:
    """Test all graph building functionality including compilation and verification."""
    
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
    
    def test__build_sast_workflow_graph__contains_all_expected_nodes(self, mock_nodes):
        # Preparation
        expected_nodes = [
            WorkflowNode.PRE_PROCESS.value,
            WorkflowNode.FILTER.value,
            WorkflowNode.DATA_FETCHER.value,
            WorkflowNode.JUDGE_LLM_ANALYSIS.value,
            WorkflowNode.EVALUATE_ANALYSIS.value,
            WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value,
            WorkflowNode.CALCULATE_METRICS.value,
            WorkflowNode.WRITE_RESULTS.value
        ]
        
        # Testing
        graph = build_sast_workflow_graph(**mock_nodes)
        
        # Assertion
        graph_structure = graph.get_graph()
        actual_nodes = list(graph_structure.nodes.keys()) if hasattr(graph_structure, 'nodes') else []
        
        for expected_node in expected_nodes:
            assert expected_node in actual_nodes, f"Expected node '{expected_node}' not found in graph"
    
    def test__build_sast_workflow_graph__has_conditional_edge_from_evaluate_analysis(self, mock_nodes):
        # Preparation
        # mock_nodes fixture provides all required node functions
        
        # Testing
        graph = build_sast_workflow_graph(**mock_nodes)
        
        # Assertion
        graph_structure = graph.get_graph()


        # Check that conditional edges exist from evaluate_analysis node
        graph_edges = graph_structure.edges
        assert any(edge for edge in graph_edges if edge.source == WorkflowNode.EVALUATE_ANALYSIS.value and edge.conditional == True)

    def test__build_sast_workflow_graph__has_linear_edges_between_sequential_nodes(self, mock_nodes):
        # Preparation
        from Utils.workflow_utils import get_linear_edges
        expected_linear_edges = set(get_linear_edges())
        
        # Testing
        graph = build_sast_workflow_graph(**mock_nodes)
        
        # Assertion
        graph_structure = graph.get_graph()
        
        # Verify linear edges exist
        actual_edges = graph_structure.edges
        assert len(actual_edges) > 0, "Graph should have edges between nodes"
        
        # Verify that expected linear edges exist
        actual_edges_tuples = set((edge.source, edge.target) for edge in actual_edges if not edge.conditional)
        assert expected_linear_edges == actual_edges_tuples, f"Expected edges {expected_linear_edges} and actual edges {actual_edges_tuples} are not the same"

    def test__build_sast_workflow_graph__missing_node_function_raises_error(self):
        # Preparation
        incomplete_nodes = {
            'pre_process_node': AsyncMock(),
            'filter_node': AsyncMock(),
            # Missing other required nodes
        }

        # Testing & Assertion
        with pytest.raises(TypeError):
            build_sast_workflow_graph(**incomplete_nodes)
    
    def test__build_sast_workflow_graph__none_node_function_raises_error(self):
        # Preparation
        nodes_with_none = {
            'pre_process_node': AsyncMock(),
            'filter_node': None,  # None node function
            'data_fetcher_node': AsyncMock(),
            'judge_llm_analysis_node': AsyncMock(),
            'evaluate_analysis_node': AsyncMock(),
            'summarize_justifications_node': AsyncMock(),
            'calculate_metrics_node': AsyncMock(),
            'write_results_node': AsyncMock()
        }
        
        # Testing & Assertion
        with pytest.raises(RuntimeError):
            build_sast_workflow_graph(**nodes_with_none)
    
    def test__verify_graph_structure__valid_graph_passes_verification(self, mock_nodes, caplog):
        # Preparation
        import logging
        graph = build_sast_workflow_graph(**mock_nodes)
        
        # Testing
        with caplog.at_level(logging.WARNING):
            verify_graph_structure(graph)

        # Assertion
        # Should not log any warnings for a valid graph
        warning_message = GRAPH_BUILDER_VERIFY_GRAPH_STRUCTURE_LOG.split("{e}")
        assert warning_message[0] not in caplog.text
        assert warning_message[1] not in caplog.text
    
    def test__verify_graph_structure__none_graph_raises_runtime_error(self):
        # Preparation
        graph = None
        
        # Testing & Assertion
        with pytest.raises(RuntimeError, match="Graph compilation failed - graph is None"):
            verify_graph_structure(graph)
    
    def test__verify_graph_structure__graph_with_missing_nodes_logs_warning(self, mock_nodes, caplog):
        # Preparation
        import logging
        # Create a graph and then mock it to appear missing nodes
        graph = build_sast_workflow_graph(**mock_nodes)
        
        # Mock the graph structure to simulate missing nodes
        mock_graph_structure = Mock()
        mock_graph_structure.nodes = Mock()
        mock_graph_structure.nodes.keys.return_value = [
            WorkflowNode.PRE_PROCESS.value,
            # Missing other expected nodes
        ]
        
        with mock.patch.object(graph, 'get_graph', return_value=mock_graph_structure):
            # Testing
            with caplog.at_level(logging.WARNING):
                verify_graph_structure(graph)

            # Assertion
            warning_message = GRAPH_BUILDER_VERIFY_GRAPH_STRUCTURE_LOG.split("{e}")
            assert warning_message[0] in caplog.text
            assert warning_message[1] in caplog.text
    
    def test__verify_graph_structure__graph_structure_access_error_logs_warning_but_succeeds(self, mock_nodes, caplog):
        # Preparation
        import logging
        graph = build_sast_workflow_graph(**mock_nodes)
        
        # Mock get_graph to raise an exception
        with mock.patch.object(graph, 'get_graph', side_effect=Exception("Graph access error")):
            with caplog.at_level(logging.WARNING):
                verify_graph_structure(graph)

            # Assertion
            warning_message = GRAPH_BUILDER_VERIFY_GRAPH_STRUCTURE_LOG.split("{e}")
            assert warning_message[0] in caplog.text
            assert warning_message[1] in caplog.text