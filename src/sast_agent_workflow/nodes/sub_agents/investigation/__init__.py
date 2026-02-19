"""Investigation package for autonomous SAST vulnerability analysis.

This package provides tools, prompts, nodes, and observability components
for the research agent investigation workflow.

Main entry points:
- create_investigate_node: builds the orchestrator node that runs
  the investigation subgraph per-issue with Langfuse tracing
- create_investigation_tools: builds the tool set (fetch_code,
  read_file, search_codebase) used by the research agent
"""

from .orchestrator import create_investigate_node
from .subgraph import build_investigation_subgraph
from .tools import create_investigation_tools

__all__ = [
    "create_investigate_node",
    "build_investigation_subgraph",
    "create_investigation_tools",
]
