"""
Workflow Builder - Assemble SAST workflow graph without NAT.

This module builds the complete workflow graph using plain LangGraph.
"""

import logging

from langgraph.graph import END, StateGraph
from langgraph.graph.state import CompiledStateGraph

from common.config import Config
from dto.SASTWorkflowModels import SASTWorkflowTracker
from sast_agent_workflow.research_agent.core import (
    create_llm,
    create_embedding_llm,
    create_investigation_tools,
)
from sast_agent_workflow.research_agent.nodes import (
    create_pre_process_node,
    create_filter_node,
    create_investigate_node,
    create_summarize_node,
    create_metrics_node,
    create_write_results_node,
)

logger = logging.getLogger(__name__)


def build_workflow(config: Config) -> CompiledStateGraph:
    """
    Build the complete SAST workflow graph without NAT.
    
    Workflow structure:
        PRE_PROCESS → FILTER → INVESTIGATE → SUMMARIZE → CALCULATE_METRICS → WRITE_RESULTS → END
    
    Args:
        config: Configuration instance (existing Config class)
    
    Returns:
        Compiled LangGraph ready for execution
    """
    logger.info("Building SAST workflow (no NAT)...")
    
    # Initialize dependencies
    logger.info("Initializing LLM and tools...")
    llm = create_llm(config)
    embedding_llm = create_embedding_llm(config)
    tools = create_investigation_tools(config)
    
    logger.info(f"Created {len(tools)} investigation tools")
    
    # Create node functions
    logger.info("Creating workflow nodes...")
    pre_process = create_pre_process_node(config)
    filter_node = create_filter_node(config, embedding_llm)
    investigate = create_investigate_node(config, llm, tools)
    summarize = create_summarize_node(config, llm)
    metrics = create_metrics_node(config)
    write_results = create_write_results_node(config)
    
    # Build graph
    logger.info("Assembling workflow graph...")
    graph = StateGraph(SASTWorkflowTracker)
    
    # Add nodes
    graph.add_node("pre_process", pre_process)
    graph.add_node("filter", filter_node)
    graph.add_node("investigate", investigate)
    graph.add_node("summarize", summarize)
    graph.add_node("calculate_metrics", metrics)
    graph.add_node("write_results", write_results)
    
    # Define linear flow
    graph.set_entry_point("pre_process")
    graph.add_edge("pre_process", "filter")
    graph.add_edge("filter", "investigate")
    graph.add_edge("investigate", "summarize")
    graph.add_edge("summarize", "calculate_metrics")
    graph.add_edge("calculate_metrics", "write_results")
    graph.add_edge("write_results", END)
    
    # Compile
    compiled = graph.compile()
    
    logger.info("Workflow graph compiled successfully")
    logger.info("Graph structure: PRE_PROCESS → FILTER → INVESTIGATE → SUMMARIZE → CALCULATE_METRICS → WRITE_RESULTS → END")
    
    return compiled

