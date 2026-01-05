"""
Agent tools for SAST investigation - NAT Integration.

This package contains NAT-registered LangChain StructuredTools:
- fetch_code: Retrieve source code by file path or symbol name
- evaluator: Gate for verification (ADR-0002)
- list_files: List files in a directory
- read_file: Read file contents
- search_codebase: Search for patterns in codebase

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

from .code_exploration import (  # noqa: F401
    ListFilesToolConfig,
    ReadFileToolConfig,
    SearchCodebaseToolConfig,
    register_list_files_tool,
    register_read_file_tool,
    register_search_codebase_tool,
)

__all__ = [
    # Tool registration functions
    "register_fetch_code_tool",
    "register_evaluator",
    "register_list_files_tool",
    "register_read_file_tool",
    "register_search_codebase_tool",
    # Tool configs
    "FetchCodeToolConfig",
    "EvaluatorToolConfig",
    "ListFilesToolConfig",
    "ReadFileToolConfig",
    "SearchCodebaseToolConfig",
]
