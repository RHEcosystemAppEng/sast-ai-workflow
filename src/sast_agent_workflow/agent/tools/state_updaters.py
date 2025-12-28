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
from hashlib import sha1
from typing import Any, Dict, Protocol

from ..agent_state import EvaluatorReport, SASTAgentState, ToolError, Unknown

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

        Stores code in fetched_files, adds identifier to found_symbols ONLY on success,
        and resets error recovery counter on success.

        The tool returns error messages starting with "Error:" for failed fetches.
        Only successful fetches should update found_symbols to prevent duplicate tracking.
        """
        identifier = tool_args.get("identifier", "unknown")

        # Check if result is an error message
        is_error = result.startswith("Error:")

        if not is_error:
            # Success: Store fetched code and update found_symbols
            state.context.fetched_files[identifier] = [result]
            state.context.found_symbols.add(identifier)

            # Reset error state on success
            state.error_state.error_recovery_attempts = 0
            state.error_state.last_error = None

            logger.debug(f"[{state.issue_id}] Updated state: fetched {identifier}")
        else:
            # Error: Store error message but DON'T add to found_symbols
            # This allows the agent to see the error and try a different approach
            state.context.fetched_files[identifier] = [result]

            logger.debug(
                f"[{state.issue_id}] Fetch failed for {identifier}, "
                f"not adding to found_symbols"
            )


class AnalyzeIssueStateUpdater:
    """State updater for analyze_issue tool."""

    def update_state(self, state: SASTAgentState, tool_args: Dict[str, Any], result: str) -> None:
        """
        Update state with analysis results.

        Parses evaluator JSON result, stores EvaluatorReport, merges blocking gaps
        into unknowns, and finalizes only when verification_passed==true.
        """
        try:
            report_dict = json.loads(result)
            report = EvaluatorReport(**report_dict)

            state.guard_attempt_count += 1
            state.analysis.last_evaluator_report = report
            state.analysis.evaluator_reports.append(report)

            # Merge evaluator gaps into unknowns (dedupe by question)
            existing_questions = {u.question for u in state.analysis.unknowns}
            for gap in report.blocking_gaps:
                if gap in existing_questions:
                    continue
                gap_id = sha1(gap.encode("utf-8")).hexdigest()[:10]
                state.analysis.unknowns.append(
                    Unknown(
                        unknown_id=f"gap:{gap_id}",
                        question=gap,
                        priority=100,
                        blocking=True,
                    )
                )

            if report.verification_passed:
                state.is_final = True
                state.stop_reason = "verified"
                state.guard_rejection_streak = 0
                if report.verdict:
                    state.analysis.verdict = report.verdict
                if report.justifications:
                    state.analysis.final_justifications = report.justifications
            else:
                state.is_final = False
                state.guard_rejection_streak += 1

            # Reset error state on success
            state.error_state.error_recovery_attempts = 0
            state.error_state.last_error = None

            logger.debug(
                f"[{state.issue_id}] Updated state from evaluator: "
                f"verification_passed={report.verification_passed}, "
                f"is_final={state.is_final}, "
                f"blocking_gaps={len(report.blocking_gaps)}"
            )
        except (json.JSONDecodeError, Exception) as e:
            logger.error(f"[{state.issue_id}] Failed to parse evaluator result: {e}")
            # Record error so agent can see it and make recovery decisions
            error = ToolError(
                tool_name="evaluator",
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
    State updater for list_files tool (core parsing only).

    Expected result format (placeholder): {"files": ["path1", "path2", ...]}
    """

    def update_state(self, state: SASTAgentState, tool_args: Dict[str, Any], result: str) -> None:
        try:
            _ = json.loads(result)
            # We don't currently store discovered files in state; this is a no-op parser.
            state.error_state.error_recovery_attempts = 0
            state.error_state.last_error = None
        except (json.JSONDecodeError, Exception) as e:
            logger.error(f"[{state.issue_id}] Failed to parse list_files result: {e}")
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
    "evaluator": EvaluatorStateUpdater(),
    # Future tools (core parsing only; tool implementations happen elsewhere)
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
