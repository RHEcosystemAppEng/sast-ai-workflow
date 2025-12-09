"""
Graph Builder for SAST Agent Workflow.

This module contains the logic for building the LangGraph workflow structure,
"""

import logging
from typing import Callable

from langgraph.graph import StateGraph
from langgraph.graph.state import CompiledStateGraph

from common.config import Config
from common.constants import (
    CONDITIONAL_EDGE_LOG,
    GRAPH_BUILDER_CONFIG_NOT_FOUND_LOG,
    GRAPH_BUILDER_VERIFY_GRAPH_STRUCTURE_LOG,
)
from dto.SASTWorkflowModels import SASTWorkflowTracker
from Utils.workflow_utils import (
    WorkflowNode,
    count_issues_needing_second_analysis,
    get_linear_edges,
)

logger = logging.getLogger(__name__)


def should_continue_analysis(tracker: SASTWorkflowTracker) -> str:
    """
    Conditional function to determine if analysis should continue or proceed to final steps.

    Args:
        tracker: The current state of the workflow

    Returns:
        WorkflowNode.DATA_FETCHER to loop back for re-analysis,
        or WorkflowNode.SUMMARIZE_JUSTIFICATIONS to proceed to final steps
    """
    try:
        if not hasattr(tracker, "iteration_count") or not isinstance(tracker.iteration_count, int):
            logger.warning("iteration_count not found in tracker, setting to 1")
            tracker.iteration_count = 1

        issues_needing_second_analysis_count = count_issues_needing_second_analysis(tracker.issues)

        if not hasattr(tracker, "config") or tracker.config is None:
            logger.warning(GRAPH_BUILDER_CONFIG_NOT_FOUND_LOG)
            tracker.config = Config()

        if (
            issues_needing_second_analysis_count > 0
            and tracker.iteration_count < tracker.config.MAX_ANALYSIS_ITERATIONS
        ):
            logger.info(
                CONDITIONAL_EDGE_LOG.format(
                    action="Continuing analysis loop",
                    issues_needing_second_analysis_count=issues_needing_second_analysis_count,
                    iteration_count=tracker.iteration_count,
                    max_analysis_iterations=tracker.config.MAX_ANALYSIS_ITERATIONS,
                )
            )
            return WorkflowNode.DATA_FETCHER.value
        else:
            logger.info(
                CONDITIONAL_EDGE_LOG.format(
                    action="Proceeding to final steps",
                    issues_needing_second_analysis_count=issues_needing_second_analysis_count,
                    iteration_count=tracker.iteration_count,
                    max_analysis_iterations=tracker.config.MAX_ANALYSIS_ITERATIONS,
                )
            )
            return WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value

    except Exception as e:
        logger.error(
            f"Error in should_continue_analysis: {e}. Proceeding to final steps as fallback."
        )
        return WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value


def build_sast_workflow_graph(
    pre_process_node: Callable,
    filter_node: Callable,
    data_fetcher_node: Callable,
    judge_llm_analysis_node: Callable,
    evaluate_analysis_node: Callable,
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

    # Build the LangGraph workflow
    graph_builder = StateGraph(SASTWorkflowTracker)

    # Add all nodes using workflow constants
    graph_builder.add_node(WorkflowNode.PRE_PROCESS.value, pre_process_node)
    graph_builder.add_node(WorkflowNode.FILTER.value, filter_node)
    graph_builder.add_node(WorkflowNode.DATA_FETCHER.value, data_fetcher_node)
    graph_builder.add_node(WorkflowNode.JUDGE_LLM_ANALYSIS.value, judge_llm_analysis_node)
    graph_builder.add_node(WorkflowNode.EVALUATE_ANALYSIS.value, evaluate_analysis_node)
    graph_builder.add_node(
        WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value, summarize_justifications_node
    )
    graph_builder.add_node(WorkflowNode.CALCULATE_METRICS.value, calculate_metrics_node)
    graph_builder.add_node(WorkflowNode.WRITE_RESULTS.value, write_results_node)

    # Connect nodes using defined graph structure
    for source, target in get_linear_edges():
        graph_builder.add_edge(source, target)

    # Add conditional edge from evaluate_analysis with explicit path_map
    graph_builder.add_conditional_edges(
        WorkflowNode.EVALUATE_ANALYSIS.value,
        should_continue_analysis,
        path_map={
            WorkflowNode.DATA_FETCHER.value: WorkflowNode.DATA_FETCHER.value,
            WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value: (
                WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value
            ),
        },
    )

    # Compile and return the graph
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
