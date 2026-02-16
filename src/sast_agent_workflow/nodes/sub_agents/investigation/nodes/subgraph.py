"""
Investigation Subgraph - Multi-stage investigation with feedback loop.

Structure:
    RESEARCH (ReAct Agent) → ANALYSIS → EVALUATION → [loop back or finish]
"""

import logging
from typing import List

from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.tools import BaseTool
from langgraph.graph import END, StateGraph

from common.config import Config

from .analysis import create_analysis_node
from .circuit_breaker import create_circuit_breaker_node
from .evaluation import create_evaluation_node
from .research import create_research_node
from .router import should_continue
from .schemas import InvestigationState

logger = logging.getLogger(__name__)

# Export for external use
__all__ = [
    "InvestigationState",
    "build_investigation_subgraph",
]


def build_investigation_subgraph(
    llm: BaseChatModel, tools: List[BaseTool], config: Config
) -> StateGraph:
    """
    Build investigation subgraph with research → analysis → evaluation loop.

    Args:
        llm: Language model
        tools: Investigation tools
        config: Configuration

    Returns:
        Compiled investigation subgraph
    """
    logger.info("Building investigation subgraph (research → analysis → evaluation)")

    # Create nodes
    research = create_research_node(llm, tools)
    analyze = create_analysis_node(llm, config)
    evaluate = create_evaluation_node(llm, config)
    circuit_breaker = create_circuit_breaker_node()

    # Build graph
    graph = StateGraph(InvestigationState)

    # Add nodes
    graph.add_node("research", research)
    graph.add_node("analyze", analyze)
    graph.add_node("evaluate", evaluate)
    graph.add_node("circuit_breaker", circuit_breaker)

    # Add increment node to update iteration counter
    def increment_iteration(state: InvestigationState) -> InvestigationState:
        return {**state, "iteration": state["iteration"] + 1}

    graph.add_node("increment", increment_iteration)

    # Define edges
    graph.set_entry_point("research")
    graph.add_edge("research", "analyze")
    graph.add_edge("analyze", "evaluate")

    # Conditional edge from evaluate
    graph.add_conditional_edges(
        "evaluate",
        should_continue,
        {
            "research": "increment",  # Go to increment before research
            "reanalyze": "increment",  # Go to increment before re-analysis
            "circuit_breaker": "circuit_breaker",  # Route to circuit breaker node first
            "end": END,  # Investigation complete
        },
    )

    # Conditional edge from increment: route based on needs_reanalysis flag
    def route_after_increment(state: InvestigationState) -> str:
        return "analyze" if state.get("needs_reanalysis", False) else "research"

    graph.add_conditional_edges(
        "increment",
        route_after_increment,
        {
            "research": "research",
            "analyze": "analyze",
        },
    )

    # Edge from circuit_breaker to END (after setting stop_reason and verdict)
    graph.add_edge("circuit_breaker", END)

    compiled = graph.compile()

    logger.info("Investigation subgraph compiled successfully")
    return compiled
