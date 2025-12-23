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
from Utils.workflow_utils import WorkflowNode

logger = logging.getLogger(__name__)


# NOTE: should_continue_analysis removed - no longer needed with agent-based investigation
# The agent handles iteration logic internally


def build_sast_workflow_graph(
    pre_process_node: Callable,
    filter_node: Callable,
    investigate_issue_node: Callable,
    summarize_justifications_node: Callable,
    calculate_metrics_node: Callable,
    write_results_node: Callable,
) -> CompiledStateGraph:
    """
    Build the complete SAST workflow graph with agent-based investigation.

    Graph structure:
        PRE_PROCESS → FILTER → INVESTIGATE_ISSUE → SUMMARIZE → CALCULATE_METRICS → WRITE_RESULTS

    Args:
        pre_process_node: Initialize workflow tracker
        filter_node: Filter known false positives
        investigate_issue_node: Agent-based investigation (replaces 3-iteration loop)
        summarize_justifications_node: Summarize analysis results
        calculate_metrics_node: Calculate performance metrics
        write_results_node: Write final results

    Returns:
        Compiled LangGraph CompiledStateGraph ready for execution
    """
    logger.info("Building SAST workflow graph with agent-based investigation...")

    from langgraph.graph import END, START

    # Build the LangGraph workflow
    graph_builder = StateGraph(SASTWorkflowTracker)

    # Add all nodes
    graph_builder.add_node(WorkflowNode.PRE_PROCESS.value, pre_process_node)
    graph_builder.add_node(WorkflowNode.FILTER.value, filter_node)
    graph_builder.add_node(WorkflowNode.INVESTIGATE_ISSUE.value, investigate_issue_node)
    graph_builder.add_node(
        WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value, summarize_justifications_node
    )
    graph_builder.add_node(WorkflowNode.CALCULATE_METRICS.value, calculate_metrics_node)
    graph_builder.add_node(WorkflowNode.WRITE_RESULTS.value, write_results_node)

    # Connect nodes in linear sequence
    graph_builder.add_edge(START, WorkflowNode.PRE_PROCESS.value)
    graph_builder.add_edge(WorkflowNode.PRE_PROCESS.value, WorkflowNode.FILTER.value)
    graph_builder.add_edge(WorkflowNode.FILTER.value, WorkflowNode.INVESTIGATE_ISSUE.value)
    graph_builder.add_edge(
        WorkflowNode.INVESTIGATE_ISSUE.value, WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value
    )
    graph_builder.add_edge(
        WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value, WorkflowNode.CALCULATE_METRICS.value
    )
    graph_builder.add_edge(WorkflowNode.CALCULATE_METRICS.value, WorkflowNode.WRITE_RESULTS.value)
    graph_builder.add_edge(WorkflowNode.WRITE_RESULTS.value, END)

    # Compile and return the graph
    graph = graph_builder.compile()
    logger.info("SAST workflow graph with agent-based investigation compiled successfully")

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
