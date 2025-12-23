"""
Agent tools for SAST investigation - NAT Integration.

This package contains NAT-registered LangChain StructuredTools:
- fetch_code: Retrieve source code by file path or symbol name
- analyze_issue: Perform security analysis on gathered context
- comprehensive_evaluation: Combined process and logic audit

Importing this module registers all tools with NAT via @register_function decorators.
"""

from .analyze_issue import (  # noqa: F401
    AnalyzeIssueToolConfig,
    register_analyze_issue_tool,
)
from .comprehensive_evaluation import (  # noqa: F401
    ComprehensiveEvaluationToolConfig,
    register_comprehensive_evaluation_tool,
)

# Import NAT-registered tools (side effects - decorator registration)
from .fetch_code import (  # noqa: F401
    FetchCodeToolConfig,
    register_fetch_code_tool,
)

__all__ = [
    # Tool registration functions
    "register_fetch_code_tool",
    "register_analyze_issue_tool",
    "register_comprehensive_evaluation_tool",
    # Tool configs
    "FetchCodeToolConfig",
    "AnalyzeIssueToolConfig",
    "ComprehensiveEvaluationToolConfig",
]
