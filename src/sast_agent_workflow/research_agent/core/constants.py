"""Constants for the research agent workflow.

Centralizes magic numbers and configuration values to improve maintainability.
"""

# =============================================================================
# Tool Result Truncation
# =============================================================================

# Maximum characters to keep in tool result summaries
# Full code is stored in prompt CODE BANK, so tool results are truncated
MAX_TOOL_RESULT_CHARS = 200

# Truncation limits for error messages and failed fetch summaries
ERROR_MSG_TRUNCATE_CHARS = 200  # Error message in tool middleware
FAILED_FETCH_SUMMARY_CHARS = 150  # Summary in failed_fetches list (enough for full file paths)
FAILED_MSG_PREVIEW_CHARS = 100  # Failed message preview to model

# =============================================================================
# Circuit Breaker Thresholds
# =============================================================================

MAX_REJECTION_STREAK = 3  # Stop after N consecutive evaluation rejections
MAX_NO_PROGRESS_STREAK = 2  # Stop after N iterations without gathering new code

# =============================================================================
# Agent Configuration
# =============================================================================

# Recursion limit for research agent (per-iteration tool calls)
RESEARCH_AGENT_RECURSION_LIMIT = 30

# Default max tokens for LLM responses
DEFAULT_MAX_TOKENS = 4096

# =============================================================================
# Tool Defaults
# =============================================================================

# Default context lines around functions in fetch_code
DEFAULT_CONTEXT_LINES = 10

# Maximum results for search operations
DEFAULT_MAX_SEARCH_RESULTS = 20

# Maximum lines to scan when extracting a function without clear end marker
MAX_FUNCTION_SCAN_LINES = 50

# =============================================================================
# Code Gathering Tools
# =============================================================================

# Tools that gather code from the repository
# Note: Tool names match langchain_community tools:
# - read_file (ReadFileTool)
# - list_directory (ListDirectoryTool)
# - file_search (FileSearchTool)
# - fetch_code, search_codebase are custom tools
# Note: brave_search is NOT included as it searches the web, not the repo
CODE_GATHERING_TOOLS = frozenset({
    'fetch_code',
    'read_file',
    'search_codebase',
    'list_directory',
    'file_search',
})
