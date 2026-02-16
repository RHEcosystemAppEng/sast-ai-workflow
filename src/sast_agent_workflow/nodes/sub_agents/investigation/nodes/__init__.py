"""Investigation module - Multi-stage SAST vulnerability investigation.

This module provides the investigation subgraph and its components:
- orchestrator: Main investigate node that runs the subgraph for each issue
- subgraph: Builds the research → analysis → evaluation loop
- research: ReAct agent for gathering code evidence
- analysis: LLM-based verdict decision
- evaluation: Quality critique and loop control
- circuit_breaker: Graceful termination on safety limits
- router: Conditional edge logic
- schemas: State and output schemas
"""

from .analysis import create_analysis_node
from .circuit_breaker import create_circuit_breaker_node
from .evaluation import create_evaluation_node
from .orchestrator import create_investigate_node
from .research import create_research_node
from .router import should_continue
from .schemas import AnalysisResultOutput, InvestigationState
from .subgraph import build_investigation_subgraph

__all__ = [
    "create_investigate_node",
    "build_investigation_subgraph",
    "create_analysis_node",
    "create_circuit_breaker_node",
    "create_evaluation_node",
    "create_research_node",
    "should_continue",
    "AnalysisResultOutput",
    "InvestigationState",
]
