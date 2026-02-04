"""Core infrastructure for SAST workflow without NAT.

This module provides:
- LLM factory for creating language model instances
- Investigation tools for code analysis
- Prompt templates for workflow nodes
- Constants and exceptions
"""

# Constants
from .constants import (
    CODE_GATHERING_TOOLS,
    DEFAULT_CONTEXT_LINES,
    DEFAULT_MAX_SEARCH_RESULTS,
    DEFAULT_MAX_TOKENS,
    ERROR_MSG_TRUNCATE_CHARS,
    FAILED_MSG_PREVIEW_CHARS,
    MAX_FUNCTION_SCAN_LINES,
    MAX_MODEL_CALLS,
    MAX_NO_PROGRESS_STREAK,
    MAX_REJECTION_STREAK,
    MAX_TOOL_RESULT_CHARS,
    RECURSION_WARNING_THRESHOLD,
    RESEARCH_AGENT_RECURSION_LIMIT,
)

# Exceptions
from .exceptions import (
    AnalysisError,
    CircuitBreakerTriggered,
    CodeExtractionError,
    ConfigurationError,
    EvaluationError,
    InvestigationError,
    LLMError,
    ResearchAgentError,
    SearchError,
    ToolExecutionError,
)

# LLM Factory
from .llm import create_embedding_llm, create_llm

# Prompts
from .prompts import (
    build_analysis_prompt,
    build_code_bank,
    build_evaluation_prompt,
    build_research_instructions,
)

# Tools
from .tools import FetchCodeInput, SearchCodebaseInput, create_investigation_tools

__all__ = [
    # Constants
    "CODE_GATHERING_TOOLS",
    "DEFAULT_CONTEXT_LINES",
    "DEFAULT_MAX_SEARCH_RESULTS",
    "DEFAULT_MAX_TOKENS",
    "ERROR_MSG_TRUNCATE_CHARS",
    "FAILED_MSG_PREVIEW_CHARS",
    "MAX_FUNCTION_SCAN_LINES",
    "MAX_MODEL_CALLS",
    "MAX_NO_PROGRESS_STREAK",
    "MAX_REJECTION_STREAK",
    "MAX_TOOL_RESULT_CHARS",
    "RECURSION_WARNING_THRESHOLD",
    "RESEARCH_AGENT_RECURSION_LIMIT",
    # Exceptions
    "AnalysisError",
    "CircuitBreakerTriggered",
    "CodeExtractionError",
    "ConfigurationError",
    "EvaluationError",
    "InvestigationError",
    "LLMError",
    "ResearchAgentError",
    "SearchError",
    "ToolExecutionError",
    # LLM Factory
    "create_llm",
    "create_embedding_llm",
    # Tools
    "create_investigation_tools",
    "FetchCodeInput",
    "SearchCodebaseInput",
    # Prompts
    "build_analysis_prompt",
    "build_code_bank",
    "build_evaluation_prompt",
    "build_research_instructions",
]
