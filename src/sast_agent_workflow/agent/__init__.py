"""
Agent-based investigation framework for SAST triage.

This package implements a ReAct-style agent with NAT integration for adaptive
SAST investigation. The agent uses LangGraph for orchestration and dynamically
determines investigation depth based on security evaluation feedback.

Importing this module registers all SAST investigation tools and the agent with NAT.
"""

# Import tools for NAT registration (side effects)
from . import tools  # noqa: F401
from .agent_graph import create_agent_graph
from .agent_state import (
    AgentMemory,
    AnalysisState,
    Claim,
    ErrorState,
    EvaluatorReport,
    Evidence,
    InvestigationContext,
    SASTAgentState,
    ToolError,
    Unknown,
)

# Import agent registration
from .register_agent import (  # noqa: F401
    SASTInvestigationAgentConfig,
    sast_investigation_agent,
)

# ProjectContext will be added in Phase 2
# initialize_project_context will be added in Phase 2


__all__ = [
    # NAT registrations
    "sast_investigation_agent",
    "SASTInvestigationAgentConfig",
    # State models
    "SASTAgentState",
    "ToolError",
    "InvestigationContext",
    "AnalysisState",
    "ErrorState",
    "AgentMemory",
    "Claim",
    "Evidence",
    "Unknown",
    "EvaluatorReport",
    # Graph builder
    "create_agent_graph",
]
