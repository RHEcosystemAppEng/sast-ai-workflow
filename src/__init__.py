"""
SAST AI Workflow - NeMo Agent for Static Application Security Testing

This package contains the complete SAST AI workflow system including:
- SAST agent workflow implementation using LangGraph
- Data models and configuration management
- File handlers and utilities
- Integration with NAT
"""

# Import agent module to trigger NAT registration
from sast_agent_workflow import agent  # noqa: F401
