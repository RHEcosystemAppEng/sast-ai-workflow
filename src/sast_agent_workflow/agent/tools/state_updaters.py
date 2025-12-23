"""
Tool state updaters - Strategy pattern for tool-specific state management.

This module defines the protocol for updating SASTAgentState after tool execution,
and provides concrete implementations for each tool. This separates state update
logic from the generic tool wrapper, improving testability and maintainability.

Each state updater is a small, focused class that knows how to update the agent
state based on a specific tool's results.
"""

import json
import logging
from datetime import datetime
from typing import Any, Dict, Protocol

from ..agent_state import (
    AnalysisResponse,
    ComprehensiveEvaluationResponse,
    SASTAgentState,
    ToolError,
)

logger = logging.getLogger(__name__)


class ToolStateUpdater(Protocol):
    """
    Protocol for tool-specific state updates.

    Implementations handle parsing tool results and updating the appropriate
    parts of SASTAgentState. This enables the generic tool wrapper to remain
    tool-agnostic while still properly updating state.
    """

    def update_state(self, state: SASTAgentState, tool_args: Dict[str, Any], result: str) -> None:
        """
        Update agent state with tool execution results.

        Args:
            state: Agent state to update (modified in-place)
            tool_args: Arguments passed to the tool
            result: String result returned by the tool
        """
        ...


class FetchCodeStateUpdater:
    """State updater for fetch_code tool."""

    def update_state(self, state: SASTAgentState, tool_args: Dict[str, Any], result: str) -> None:
        """
        Update state with fetched code.

        Stores code in fetched_files, adds identifier to found_symbols,
        and resets error recovery counter on success.
        """
        identifier = tool_args.get("identifier", "unknown")

        # Store fetched code
        state.context.fetched_files[identifier] = [result]
        # Add to found_symbols with deduplication (List instead of Set for JSON serialization)
        if identifier not in state.context.found_symbols:
            state.context.found_symbols.append(identifier)

        # Reset error state on success
        state.error_state.error_recovery_attempts = 0
        state.error_state.last_error = None

        logger.debug(f"[{state.issue_id}] Updated state: fetched {identifier}")


class AnalyzeIssueStateUpdater:
    """State updater for analyze_issue tool."""

    def update_state(self, state: SASTAgentState, tool_args: Dict[str, Any], result: str) -> None:
        """
        Update state with analysis results.

        Parses JSON result and stores as AnalysisResponse in analysis state.
        """
        try:
            analysis_dict = json.loads(result)
            state.analysis.investigation_result = AnalysisResponse(**analysis_dict)

            # Reset error state on success
            state.error_state.error_recovery_attempts = 0
            state.error_state.last_error = None

            logger.debug(
                f"[{state.issue_id}] Updated state: analysis result = "
                f"{state.analysis.investigation_result.investigation_result}"
            )
        except (json.JSONDecodeError, Exception) as e:
            logger.error(f"[{state.issue_id}] Failed to parse analysis result: {e}")
            # Record error so agent can see it and make recovery decisions
            error = ToolError(
                tool_name="analyze_issue",
                error_message=f"Failed to parse result: {str(e)}",
                attempted_args=tool_args,
                timestamp=datetime.now().isoformat(),
            )
            state.error_state.errors.append(error)
            state.error_state.last_error = error
            state.error_state.error_recovery_attempts += 1


class ComprehensiveEvaluationStateUpdater:
    """State updater for comprehensive_evaluation tool."""

    def update_state(self, state: SASTAgentState, tool_args: Dict[str, Any], result: str) -> None:
        """
        Update state with evaluation results.

        Parses JSON result, stores as ComprehensiveEvaluationResponse,
        sets is_final flag, and sets verdict if investigation is complete.
        """
        try:
            eval_dict = json.loads(result)
            state.analysis.evaluation_result = ComprehensiveEvaluationResponse(**eval_dict)

            # Set is_final based on evaluation
            state.is_final = eval_dict.get("is_final") == "TRUE"

            # Set verdict if final and we have investigation result
            if state.is_final and state.analysis.investigation_result:
                state.analysis.verdict = state.analysis.investigation_result.investigation_result

            # Reset error state on success
            state.error_state.error_recovery_attempts = 0
            state.error_state.last_error = None

            logger.debug(
                f"[{state.issue_id}] Updated state: is_final={state.is_final}, "
                f"exploration_gaps={len(state.analysis.evaluation_result.exploration_gaps)}"
            )
        except (json.JSONDecodeError, Exception) as e:
            logger.error(f"[{state.issue_id}] Failed to parse evaluation result: {e}")
            # Record error so agent can see it and make recovery decisions
            error = ToolError(
                tool_name="comprehensive_evaluation",
                error_message=f"Failed to parse result: {str(e)}",
                attempted_args=tool_args,
                timestamp=datetime.now().isoformat(),
            )
            state.error_state.errors.append(error)
            state.error_state.last_error = error
            state.error_state.error_recovery_attempts += 1


class SearchCodebaseStateUpdater:
    """
    State updater for search_codebase tool (Phase 2).

    This updater will be implemented when search_codebase tool is added.
    """

    def update_state(self, state: SASTAgentState, tool_args: Dict[str, Any], result: str) -> None:
        """
        Update state with search results.

        Parses search results (file paths and matching code snippets)
        and adds them to fetched_files.
        """
        try:
            # Expected format: {"file_path": ["snippet1", "snippet2"], ...}
            search_results = json.loads(result)

            for file_path, snippets in search_results.items():
                # Merge with existing snippets if file was already fetched
                if file_path in state.context.fetched_files:
                    state.context.fetched_files[file_path].extend(snippets)
                else:
                    state.context.fetched_files[file_path] = snippets

                # Add to found_symbols with deduplication
                if file_path not in state.context.found_symbols:
                    state.context.found_symbols.append(file_path)

            # Reset error state on success
            state.error_state.error_recovery_attempts = 0
            state.error_state.last_error = None

            logger.debug(
                f"[{state.issue_id}] Updated state: search found " f"{len(search_results)} files"
            )
        except (json.JSONDecodeError, Exception) as e:
            logger.error(f"[{state.issue_id}] Failed to parse search results: {e}")
            # Record error so agent can see it and make recovery decisions
            error = ToolError(
                tool_name="search_codebase",
                error_message=f"Failed to parse result: {str(e)}",
                attempted_args=tool_args,
                timestamp=datetime.now().isoformat(),
            )
            state.error_state.errors.append(error)
            state.error_state.last_error = error
            state.error_state.error_recovery_attempts += 1


class ListFilesStateUpdater:
    """
    State updater for list_files tool (Phase 2).

    This updater will be implemented when list_files tool is added.
    """

    def update_state(self, state: SASTAgentState, tool_args: Dict[str, Any], result: str) -> None:
        """
        Update state with file listing.

        Stores discovered file paths in project context for reference.
        """
        try:
            # Expected format: {"files": ["path1", "path2", ...]}
            file_list_dict = json.loads(result)
            discovered_files = file_list_dict.get("files", [])

            # Note: SASTAgentState doesn't currently have a discovered_files field
            # This would need to be added to ProjectContext when implementing
            # For now, just log
            # Reset error state on success
            state.error_state.error_recovery_attempts = 0
            state.error_state.last_error = None

            logger.debug(
                f"[{state.issue_id}] Discovered {len(discovered_files)} files "
                f"(state update pending ProjectContext extension)"
            )
        except (json.JSONDecodeError, Exception) as e:
            logger.error(f"[{state.issue_id}] Failed to parse file list: {e}")
            # Record error so agent can see it and make recovery decisions
            error = ToolError(
                tool_name="list_files",
                error_message=f"Failed to parse result: {str(e)}",
                attempted_args=tool_args,
                timestamp=datetime.now().isoformat(),
            )
            state.error_state.errors.append(error)
            state.error_state.last_error = error
            state.error_state.error_recovery_attempts += 1


# Registry mapping tool names to their state updaters
STATE_UPDATER_REGISTRY: Dict[str, ToolStateUpdater] = {
    "fetch_code": FetchCodeStateUpdater(),
    "analyze_issue": AnalyzeIssueStateUpdater(),
    "comprehensive_evaluation": ComprehensiveEvaluationStateUpdater(),
    # Phase 2 tools (ready for when tools are implemented)
    "search_codebase": SearchCodebaseStateUpdater(),
    "list_files": ListFilesStateUpdater(),
}


def get_state_updater(tool_name: str) -> ToolStateUpdater:
    """
    Get the state updater for a given tool.

    Args:
        tool_name: Name of the tool

    Returns:
        ToolStateUpdater instance for the tool

    Raises:
        KeyError: If no state updater exists for the tool
    """
    if tool_name not in STATE_UPDATER_REGISTRY:
        raise KeyError(
            f"No state updater found for tool '{tool_name}'. "
            f"Available tools: {list(STATE_UPDATER_REGISTRY.keys())}"
        )

    return STATE_UPDATER_REGISTRY[tool_name]
