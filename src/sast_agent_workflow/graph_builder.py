"""
Graph Builder for SAST Agent Workflow.

This module contains the logic for building the LangGraph workflow structure,
"""

import logging
from typing import Callable

from langgraph.graph import StateGraph
from langgraph.graph.state import CompiledStateGraph

from common.constants import GRAPH_BUILDER_VERIFY_GRAPH_STRUCTURE_LOG
from dto.SASTWorkflowModels import SASTWorkflowTracker
from Utils.workflow_utils import (
    WorkflowNode,
    get_linear_edges,
)

logger = logging.getLogger(__name__)


def build_sast_workflow_graph(
    pre_process_node: Callable,
    filter_node: Callable,
    investigate_node: Callable,
    summarize_justifications_node: Callable,
    calculate_metrics_node: Callable,
    write_results_node: Callable,
) -> CompiledStateGraph:
    """
    Build the complete SAST workflow graph with all nodes and edges.

    Args:
        *_node: Callable functions for each workflow node

    Returns:
        Compiled LangGraph CompiledStateGraph ready for execution
    """
    logger.info("Building SAST workflow graph...")

    graph_builder = StateGraph(SASTWorkflowTracker)

    graph_builder.add_node(WorkflowNode.PRE_PROCESS.value, pre_process_node)
    graph_builder.add_node(WorkflowNode.FILTER.value, filter_node)
    graph_builder.add_node(WorkflowNode.INVESTIGATE.value, investigate_node)
    graph_builder.add_node(
        WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value, summarize_justifications_node
    )
    graph_builder.add_node(WorkflowNode.CALCULATE_METRICS.value, calculate_metrics_node)
    graph_builder.add_node(WorkflowNode.WRITE_RESULTS.value, write_results_node)

    for source, target in get_linear_edges():
        graph_builder.add_edge(source, target)

    graph = graph_builder.compile()
    logger.info("SAST workflow graph compiled successfully")

    return graph


def verify_graph_structure(graph: CompiledStateGraph | None) -> None:
    """
    Verify that the compiled graph has the expected structure and nodes.

    Args:
        graph: The compiled LangGraph to verify

    Raises:
        RuntimeError: If verification fails
    """
    if graph is None:
        raise RuntimeError("Graph compilation failed - graph is None")

    expected_nodes = WorkflowNode.get_all_node_names()

    try:
        # Get graph structure for verification
        drawable_graph = graph.get_graph()
        actual_nodes = list(drawable_graph.nodes.keys()) if hasattr(drawable_graph, "nodes") else []

        logger.debug(f"Graph nodes detected: {actual_nodes}")

        # Check if all expected nodes are present
        missing_nodes = [node for node in expected_nodes if node not in actual_nodes]
        if missing_nodes:
            raise RuntimeError(f"Missing nodes in graph: {missing_nodes}")
        else:
            logger.info("All expected workflow nodes are present in the graph")

    except Exception as e:
        logger.warning(GRAPH_BUILDER_VERIFY_GRAPH_STRUCTURE_LOG.format(e=e))
