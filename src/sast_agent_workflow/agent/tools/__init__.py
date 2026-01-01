"""
Agent tools for SAST investigation - NAT Integration.

This package contains NAT-registered LangChain StructuredTools:
- fetch_code: Retrieve source code by file path or symbol name
- evaluator: Gate for verification (ADR-0002)

Importing this module registers all tools with NAT via @register_function decorators.
"""

from .evaluator import (  # noqa: F401
    EvaluatorToolConfig,
    register_evaluator,
)

# Import NAT-registered tools (side effects - decorator registration)
from .fetch_code import (  # noqa: F401
    FetchCodeToolConfig,
    register_fetch_code_tool,
)

__all__ = [
    # Tool registration functions
    "register_fetch_code_tool",
    "register_evaluator",
    # Tool configs
    "FetchCodeToolConfig",
    "EvaluatorToolConfig",
]
