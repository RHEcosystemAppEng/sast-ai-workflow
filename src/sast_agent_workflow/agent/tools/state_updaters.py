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

from ..agent_state import EvaluatorReport, SASTAgentState, ToolError

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

        Stores code in fetched_files, adds expression_name to found_symbols ONLY on success,
        and resets error recovery counter on success.

        The tool returns error messages starting with "Error:" for failed fetches.
        On error, creates a ToolError and stores in error_state
        to allow retries with different parameters.
        """
        expression_name = tool_args.get("expression_name", "unknown")

        # Check if result is an error message
        is_error = result.startswith("Error:")

        if not is_error:
            # Success: Store fetched code and update found_symbols
            state.context.fetched_files[expression_name] = [result]
            state.context.found_symbols.append(expression_name)

            # Reset error state on success
            state.error_state.error_recovery_attempts = 0
            state.error_state.last_error = None

            logger.debug(f"[{state.issue_id}] Updated state: fetched {expression_name}")
        else:
            tool_error = ToolError(
                tool_name="fetch_code",
                error_message=result,
                attempted_args=tool_args,
                timestamp=datetime.utcnow().isoformat(),
            )

            state.error_state.errors.append(tool_error)
            state.error_state.last_error = tool_error
            state.error_state.error_recovery_attempts += 1

            logger.debug(
                f"[{state.issue_id}] Fetch failed for {expression_name}, "
                f"stored in error_state (retries allowed with different params)"
            )


class EvaluatorToolStateUpdater:
    """State updater for evaluator."""

    def update_state(self, state: SASTAgentState, tool_args: Dict[str, Any], result: str) -> None:
        """
        Update state with evaluation results.

        Parses JSON result, stores as EvaluatorReport,
        sets is_final flag, and sets verdict if investigation is complete.
        """
        try:
            eval_dict = json.loads(result)
            evaluator_report = EvaluatorReport(**eval_dict)
            state.analysis.last_evaluator_report = evaluator_report

            # Set is_final based on verification_passed
            state.is_final = evaluator_report.verification_passed

            # Set verdict if final
            if state.is_final and evaluator_report.verdict:
                state.analysis.verdict = evaluator_report.verdict
                logger.info(
                    f"[{state.issue_id}] Evaluator verified: verdict={evaluator_report.verdict}"
                )
            elif not state.is_final:
                logger.info(
                    f"[{state.issue_id}] Evaluator rejected: "
                    f"{len(evaluator_report.blocking_gaps)} blocking gaps, "
                    f"{len(evaluator_report.required_next_fetches)} required fetches"
                )

            logger.debug(
                f"[{state.issue_id}] Updated state: is_final={state.is_final}, "
                f"verification_passed={evaluator_report.verification_passed}"
            )
        except (json.JSONDecodeError, Exception) as e:
            logger.error(
                f"[{state.issue_id}] Failed to parse evaluation result: {result} \nError: {e}"
            )
            # Don't raise - let the tool wrapper handle this as an error


class SearchCodebaseStateUpdater:
    """State updater for search_codebase tool."""

    def update_state(self, state: SASTAgentState, tool_args: Dict[str, Any], result: str) -> None:
        """
        Update state with search results.

        Search results are added to fetched_files as formatted strings
        (already formatted by the tool with file paths and line numbers).
        """
        pattern = tool_args.get("pattern", "search")

        logger.info(
            f"[{state.issue_id}] SearchCodebaseStateUpdater called: "
            f"pattern='{pattern}', result_len={len(result)}"
        )

        # Check if search was successful (not an error message)
        is_error = result.startswith("Error:") or result.startswith("No matches found")

        if not is_error:
            # Store the formatted search results
            # Use pattern as identifier since results span multiple files
            identifier = f"search_results_{pattern[:50]}"  # Truncate pattern for identifier
            state.context.fetched_files[identifier] = [result]
            state.context.found_symbols.append(identifier)

            # Reset error state on success
            state.error_state.error_recovery_attempts = 0
            state.error_state.last_error = None

            logger.info(
                f"[{state.issue_id}] Search results stored: identifier='{identifier}', "
                f"total_fetched_files={len(state.context.fetched_files)}"
            )
        else:
            # Log but don't treat as error (no matches is a valid result)
            logger.info(f"[{state.issue_id}] Search completed: {result[:100]}")


class ListFilesStateUpdater:
    """State updater for list_files tool."""

    def update_state(self, state: SASTAgentState, tool_args: Dict[str, Any], result: str) -> None:
        """
        Update state with file listing.

        File listings are stored in fetched_files for agent reference.
        These help the agent understand directory structure and find related files.
        """
        directory = tool_args.get("directory", ".")

        # Check if listing was successful
        is_error = result.startswith("Error:")

        if not is_error:
            # Store the formatted directory listing
            identifier = f"directory_list_{directory.replace('/', '_')}"
            state.context.fetched_files[identifier] = [result]
            state.context.found_symbols.append(identifier)

            logger.debug(f"[{state.issue_id}] Directory listed: {directory}")
        else:
            logger.debug(f"[{state.issue_id}] List files error: {result[:100]}")


class ReadFileStateUpdater:
    """State updater for read_file tool."""

    def update_state(self, state: SASTAgentState, tool_args: Dict[str, Any], result: str) -> None:
        """
        Update state with file contents.

        File contents are stored in fetched_files similar to fetch_code tool.
        """
        file_path = tool_args.get("file_path", "unknown")

        # Check if read was successful
        is_error = result.startswith("Error:")

        if not is_error:
            # Store the file contents
            state.context.fetched_files[file_path] = [result]
            state.context.found_symbols.append(file_path)

            # Reset error state on success
            state.error_state.error_recovery_attempts = 0
            state.error_state.last_error = None

            logger.debug(f"[{state.issue_id}] File read: {file_path}")
        else:
            tool_error = ToolError(
                tool_name="read_file",
                error_message=result,
                attempted_args=tool_args,
                timestamp=datetime.utcnow().isoformat(),
            )

            state.error_state.errors.append(tool_error)
            state.error_state.last_error = tool_error
            state.error_state.error_recovery_attempts += 1

            logger.debug(f"[{state.issue_id}] Read file error: {result[:100]}")


# Registry mapping tool names to their state updaters
STATE_UPDATER_REGISTRY: Dict[str, ToolStateUpdater] = {
    "fetch_code": FetchCodeStateUpdater(),
    "evaluator": EvaluatorToolStateUpdater(),
    "search_codebase": SearchCodebaseStateUpdater(),
    "list_files": ListFilesStateUpdater(),
    "read_file": ReadFileStateUpdater(),
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
