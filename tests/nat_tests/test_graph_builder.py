"""
Tests for SAST Agent Workflow Graph Builder.

This module tests the graph structure and overall workflow compilation
using BDD-style given__when__then naming.
"""

from unittest import mock
from unittest.mock import AsyncMock, Mock

import pytest

from common.constants import GRAPH_BUILDER_VERIFY_GRAPH_STRUCTURE_LOG
from sast_agent_workflow.graph_builder import (
    build_sast_workflow_graph,
    verify_graph_structure,
)
from Utils.workflow_utils import WorkflowNode


class TestGraphBuilder:
    """Test all graph building functionality including compilation and verification."""

    @pytest.fixture
    def mock_nodes(self):
        """Create mock node functions for testing."""
        return {
            "pre_process_node": AsyncMock(),
            "filter_node": AsyncMock(),
            "investigate_node": AsyncMock(),
            "summarize_justifications_node": AsyncMock(),
            "calculate_metrics_node": AsyncMock(),
            "write_results_node": AsyncMock(),
        }

    def test__build_sast_workflow_graph__mock_node_functions_compiles_successfully(
        self, mock_nodes
    ):
        graph = build_sast_workflow_graph(**mock_nodes)

        assert graph is not None
        assert hasattr(graph, "invoke") or hasattr(graph, "ainvoke")

    def test__build_sast_workflow_graph__contains_all_expected_nodes(self, mock_nodes):
        expected_nodes = [
            WorkflowNode.PRE_PROCESS.value,
            WorkflowNode.FILTER.value,
            WorkflowNode.INVESTIGATE.value,
            WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value,
            WorkflowNode.CALCULATE_METRICS.value,
            WorkflowNode.WRITE_RESULTS.value,
        ]

        graph = build_sast_workflow_graph(**mock_nodes)

        graph_structure = graph.get_graph()
        actual_nodes = (
            list(graph_structure.nodes.keys()) if hasattr(graph_structure, "nodes") else []
        )

        for expected_node in expected_nodes:
            assert (
                expected_node in actual_nodes
            ), f"Expected node '{expected_node}' not found in graph"

    def test__build_sast_workflow_graph__has_no_conditional_edges(self, mock_nodes):
        graph = build_sast_workflow_graph(**mock_nodes)

        graph_structure = graph.get_graph()
        graph_edges = graph_structure.edges

        conditional_edges = [edge for edge in graph_edges if edge.conditional]
        assert (
            len(conditional_edges) == 0
        ), f"Expected no conditional edges, found {conditional_edges}"

    def test__build_sast_workflow_graph__has_linear_edges_between_sequential_nodes(
        self, mock_nodes
    ):
        from Utils.workflow_utils import get_linear_edges

        expected_linear_edges = set(get_linear_edges())

        graph = build_sast_workflow_graph(**mock_nodes)

        graph_structure = graph.get_graph()

        actual_edges = graph_structure.edges
        assert len(actual_edges) > 0, "Graph should have edges between nodes"

        actual_edges_tuples = set(
            (edge.source, edge.target) for edge in actual_edges if not edge.conditional
        )
        assert expected_linear_edges == actual_edges_tuples, (
            f"Expected edges {expected_linear_edges} and "
            f"actual edges {actual_edges_tuples} are not the same"
        )

    def test__build_sast_workflow_graph__missing_node_function_raises_error(self):
        incomplete_nodes = {
            "pre_process_node": AsyncMock(),
            "filter_node": AsyncMock(),
        }

        with pytest.raises(TypeError):
            build_sast_workflow_graph(**incomplete_nodes)

    def test__build_sast_workflow_graph__none_node_function_raises_error(self):
        nodes_with_none = {
            "pre_process_node": AsyncMock(),
            "filter_node": None,
            "investigate_node": AsyncMock(),
            "summarize_justifications_node": AsyncMock(),
            "calculate_metrics_node": AsyncMock(),
            "write_results_node": AsyncMock(),
        }

        with pytest.raises(RuntimeError):
            build_sast_workflow_graph(**nodes_with_none)

    def test__verify_graph_structure__valid_graph_passes_verification(self, mock_nodes, caplog):
        import logging

        graph = build_sast_workflow_graph(**mock_nodes)

        with caplog.at_level(logging.WARNING):
            verify_graph_structure(graph)

        warning_message = GRAPH_BUILDER_VERIFY_GRAPH_STRUCTURE_LOG.split("{e}")
        assert warning_message[0] not in caplog.text
        assert warning_message[1] not in caplog.text

    def test__verify_graph_structure__none_graph_raises_runtime_error(self):
        graph = None

        with pytest.raises(RuntimeError, match="Graph compilation failed - graph is None"):
            verify_graph_structure(graph)

    def test__verify_graph_structure__graph_with_missing_nodes_logs_warning(
        self, mock_nodes, caplog
    ):
        import logging

        graph = build_sast_workflow_graph(**mock_nodes)

        mock_graph_structure = Mock()
        mock_graph_structure.nodes = Mock()
        mock_graph_structure.nodes.keys.return_value = [
            WorkflowNode.PRE_PROCESS.value,
        ]

        with mock.patch.object(graph, "get_graph", return_value=mock_graph_structure):
            with caplog.at_level(logging.WARNING):
                verify_graph_structure(graph)

            warning_message = GRAPH_BUILDER_VERIFY_GRAPH_STRUCTURE_LOG.split("{e}")
            assert warning_message[0] in caplog.text
            assert warning_message[1] in caplog.text

    def test__verify_graph_structure__graph_structure_access_error_logs_warning_but_succeeds(
        self, mock_nodes, caplog
    ):
        import logging

        graph = build_sast_workflow_graph(**mock_nodes)

        with mock.patch.object(graph, "get_graph", side_effect=Exception("Graph access error")):
            with caplog.at_level(logging.WARNING):
                verify_graph_structure(graph)

            warning_message = GRAPH_BUILDER_VERIFY_GRAPH_STRUCTURE_LOG.split("{e}")
            assert warning_message[0] in caplog.text
            assert warning_message[1] in caplog.text
