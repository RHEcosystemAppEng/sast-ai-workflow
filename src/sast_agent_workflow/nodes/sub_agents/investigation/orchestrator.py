"""
Investigate Node - Multi-stage investigation with research-analysis-evaluation loop.

This uses a subgraph with three nodes:
1. RESEARCH: ReAct agent gathers code
2. ANALYSIS: LLM makes verdict decision
3. EVALUATION: LLM critiques analysis and decides if more research needed
"""

import asyncio
import logging
from typing import Any, Dict, List, Optional

from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.tools import BaseTool

from common.config import Config
from dto.SASTWorkflowModels import SASTWorkflowTracker
from handlers.repo_handler_factory import repo_handler_factory

from .constants import INVESTIGATION_SUBGRAPH_RECURSION_LIMIT
from .nodes.schemas import InvestigationState
from .observability.ground_truth_loader import load_ground_truth_verdicts
from .observability.langfuse_integration import (
    add_langfuse_scores,
    build_langfuse_metadata_for_investigation,
    issue_langfuse_context,
    langfuse_score_client_context,
)
from .subgraph import build_investigation_subgraph

logger = logging.getLogger(__name__)


def create_investigate_node(config: Config, llm: BaseChatModel, tools: List[BaseTool]):
    """
    Create investigate node using create_react_agent.

    Args:
        config: Configuration instance
        llm: Language model for agent
        tools: List of investigation tools

    Returns:
        Node function that investigates issues
    """
    # Create investigation subgraph (research → analysis → evaluation)
    investigation_subgraph = build_investigation_subgraph(llm, tools, config)

    logger.info("Created investigation subgraph (research → analysis → evaluation)")

    async def investigate(state: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Investigate each non-final issue using the research-analysis-evaluation subgraph."""
        logger.info("Investigating issues with ReAct agent...")
        issues_to_investigate = _get_issues_to_investigate(state)
        logger.info(f"Investigating {len(issues_to_investigate)}/{len(state.issues)} issues")

        max_concurrent = config.MAX_CONCURRENT_INVESTIGATIONS or 1
        logger.info(f"Using {max_concurrent} concurrent investigation worker(s)")

        ground_truth_verdicts = _load_ground_truth_safe(config)

        await _investigate_issues(
            issues_to_investigate,
            investigation_subgraph,
            ground_truth_verdicts,
            config,
            max_concurrent,
        )

        logger.info("Investigation complete")
        return state

    return investigate


async def _investigate_issues(
    issues_to_investigate: Dict[str, Any],
    investigation_subgraph: Any,
    ground_truth_verdicts: Optional[dict],
    config: Any,
    max_concurrent: int = 1,
) -> None:
    """
    Investigate issues with controlled concurrency.

    When max_concurrent=1, processes sequentially (original behavior).
    When max_concurrent>1, processes in parallel with semaphore control.

    Args:
        issues_to_investigate: Dict of issue_id -> PerIssueData
        investigation_subgraph: Compiled investigation graph
        ground_truth_verdicts: Optional ground truth for scoring
        config: Configuration object
        max_concurrent: Maximum number of concurrent investigations (default: 1)
    """
    semaphore = asyncio.Semaphore(max_concurrent)
    processing_mode = "sequential" if max_concurrent == 1 else "parallel"

    async def _investigate_with_resources(issue_id: str, per_issue: Any) -> None:
        """Investigate single issue with concurrency control and isolated resources."""
        async with semaphore:
            logger.info(f"[{processing_mode}] Investigating {issue_id}...")
            # Create isolated repo_handler per task to prevent clang index race conditions
            repo_handler = repo_handler_factory(config)

            await _investigate_single_issue(
                issue_id,
                per_issue,
                investigation_subgraph,
                repo_handler,
                ground_truth_verdicts,
                config,
            )
            logger.info(f"[{processing_mode}] Completed {issue_id}")

    # Create all investigation tasks
    tasks = [
        _investigate_with_resources(issue_id, per_issue)
        for issue_id, per_issue in issues_to_investigate.items()
    ]

    # Execute all tasks, capturing exceptions to prevent one failure from stopping others
    logger.info(f"[{processing_mode}] Starting {len(tasks)} investigation task(s)...")
    results = await asyncio.gather(*tasks, return_exceptions=True)

    # Log any exceptions that occurred
    failed_count = sum(1 for r in results if isinstance(r, Exception))
    if failed_count > 0:
        logger.warning(f"[{processing_mode}] {failed_count}/{len(tasks)} investigation(s) failed")
        for i, result in enumerate(results):
            if isinstance(result, Exception):
                issue_id = list(issues_to_investigate.keys())[i]
                logger.error(f"[{processing_mode}] Failed {issue_id}: {result}", exc_info=result)
    else:
        logger.info(f"[{processing_mode}] All {len(tasks)} investigation(s) completed successfully")


async def _investigate_single_issue(
    issue_id: str,
    per_issue: Any,
    investigation_subgraph: Any,
    repo_handler: Any,
    ground_truth_verdicts: Optional[dict],
    config: Any,
) -> None:
    """
    Investigate a single issue through the research-analysis-evaluation subgraph.

    Encapsulates core investigation logic for one issue, making it reusable
    for both sequential and parallel processing.

    Args:
        issue_id: Unique identifier for the issue
        per_issue: PerIssueData object containing issue details
        investigation_subgraph: Compiled investigation graph
        repo_handler: Repository handler (isolated per task to avoid race conditions)
        ground_truth_verdicts: Optional ground truth data for evaluation
        config: Configuration object
    """
    # Langfuse is optional - context managers handle None gracefully
    with langfuse_score_client_context() as langfuse_score_client:
        with issue_langfuse_context(issue_id, config) as (
            langfuse_handler,
            issue_session_id,
            issue_trace_id,
        ):
            try:
                logger.debug(f"[{issue_id}] Extracting initial code context")
                code_context, fetched_initial, gathered_initial = (
                    _extract_initial_code_and_build_context(repo_handler, per_issue, issue_id)
                )

                logger.debug(f"[{issue_id}] Building initial investigation state")
                initial_state = _build_initial_investigation_state(
                    issue_id,
                    per_issue,
                    code_context,
                    gathered_initial,
                    fetched_initial,
                    config,
                )

                subgraph_config = _build_subgraph_config(
                    issue_id, config, langfuse_handler, issue_session_id, per_issue
                )

                logger.debug(f"[{issue_id}] Running investigation subgraph")
                result = await investigation_subgraph.ainvoke(initial_state, config=subgraph_config)

                logger.debug(f"[{issue_id}] Updating tracker with results")
                _update_tracker_from_result(per_issue, result, issue_id)

                # Only add scores if Langfuse is configured and available
                if langfuse_handler and langfuse_score_client and issue_trace_id:
                    logger.debug(f"[{issue_id}] Adding Langfuse scores")
                    add_langfuse_scores(
                        langfuse_score_client,
                        issue_trace_id,
                        issue_id,
                        result,
                        ground_truth_verdicts,
                    )

            except Exception as e:
                logger.error(f"[{issue_id}] Investigation failed: {e}", exc_info=True)
                if per_issue.analysis_response:
                    per_issue.analysis_response.investigation_result = "NEEDS REVIEW"
                    per_issue.analysis_response.justifications = [f"Investigation failed: {e}"]
                    per_issue.analysis_response.is_final = "TRUE"
                else:
                    logger.warning(f"[{issue_id}] No analysis_response to update with failure")


def _get_issues_to_investigate(state: SASTWorkflowTracker) -> Dict[str, Any]:
    """Return dict of issue_id -> per_issue for issues not yet final."""
    return {
        issue_id: per_issue
        for issue_id, per_issue in state.issues.items()
        if not per_issue.analysis_response or per_issue.analysis_response.is_final != "TRUE"
    }


def _load_ground_truth_safe(config: Config) -> Optional[dict]:
    """Load ground truth verdicts; return None on failure."""
    try:
        verdicts = load_ground_truth_verdicts(config)
        if verdicts:
            logger.info(f"Loaded ground truth for {len(verdicts)} issues for Langfuse scoring")
        return verdicts
    except Exception as e:
        logger.warning(f"Could not load ground truth for scoring: {e}")
        return None


def _extract_initial_code_and_build_context(repo_handler, per_issue, issue_id: str):
    """Extract initial code from trace and build code_context, fetched_initial, gathered_initial."""
    initial_code = repo_handler.get_source_code_blocks_from_error_trace(per_issue.issue.trace)
    file_count = len(initial_code) if initial_code else 0
    logger.debug(f"[{issue_id}] Extracted {file_count} files from trace")

    code_context = "\n\n".join(
        [f"=== {fp} ===\n{code}" for fp, code in (initial_code or {}).items()]
    )
    if not code_context:
        code_context = ""
        logger.debug(f"[{issue_id}] No code extracted from trace - will rely on agent tool calls")

    fetched_files_initial = {}
    gathered_code_initial = ""
    if initial_code:
        fetched_files_initial["fetch_code_from_error_trace"] = []
        for file_path, code in initial_code.items():
            formatted_block = f"=== {file_path} (from error trace) ===\n{code}"
            fetched_files_initial["fetch_code_from_error_trace"].append(formatted_block)
            gathered_code_initial += f"\n\n{formatted_block}\n"
        logger.info(
            f"[{issue_id}] Pre-populated with {len(initial_code)} files from error trace "
            f"({len(gathered_code_initial)} chars)"
        )
    else:
        logger.info(f"[{issue_id}] No code extracted from trace - starting with empty context")

    return (code_context, fetched_files_initial, gathered_code_initial)


def _build_initial_investigation_state(
    issue_id: str,
    per_issue: Any,
    code_context: str,
    gathered_code_initial: str,
    fetched_files_initial: dict,
    config: Config,
) -> InvestigationState:
    """Build initial InvestigationState for one issue."""
    issue_description = f"""**Issue ID:** {issue_id}
**Type:** {per_issue.issue.issue_type}
**Label:** {per_issue.issue.issue_label}
**CWE:** {per_issue.issue.issue_cwe}

**Trace:**
{per_issue.issue.trace}"""
    return {
        "issue_id": issue_id,
        "issue_description": issue_description,
        "initial_code": code_context,
        "research_messages": [],
        "gathered_code": gathered_code_initial,
        "fetched_files": fetched_files_initial,
        "tool_call_history": [],
        "analysis": "",
        "analysis_prompt": "",
        "proposed_verdict": "",
        "justifications": [],
        "confidence": 0.0,
        "evaluation_result": "",
        "evaluation_feedback": "",
        "required_information": [],
        "evaluation_rejection_streak": 0,
        "no_progress_streak": 0,
        "previous_code_length": 0,
        "stop_reason": None,
        "iteration": 1,
        "max_iterations": config.MAX_ANALYSIS_ITERATIONS or 4,
        "is_complete": False,
        "needs_reanalysis": False,
        "reanalysis_count": 0,
        "total_tool_calls": 0,
    }


def _build_subgraph_config(
    issue_id: str,
    config: Config,
    langfuse_handler: Optional[Any],
    issue_session_id: Optional[str],
    per_issue: Any,
) -> dict:
    """Build invoke config for investigation subgraph, with optional Langfuse metadata."""
    subgraph_config = {
        "recursion_limit": INVESTIGATION_SUBGRAPH_RECURSION_LIMIT,
        "runName": f"investigate_{config.PROJECT_NAME}_{issue_id}",
    }
    if langfuse_handler and issue_session_id:
        subgraph_config["callbacks"] = [langfuse_handler]
        subgraph_config["metadata"] = build_langfuse_metadata_for_investigation(
            config, issue_session_id, issue_id, per_issue.issue.issue_type
        )
    return subgraph_config


def _update_tracker_from_result(per_issue: Any, result: dict, issue_id: str) -> None:
    """Update per_issue.analysis_response from subgraph result and log."""
    verdict = result["proposed_verdict"].replace("_", " ")
    justifications = result["justifications"]
    analysis_prompt = result.get("analysis_prompt", "")
    iterations = result["iteration"]
    reanalysis_count = result.get("reanalysis_count", 0)
    total_tool_calls = result.get("total_tool_calls", 0)
    stop_reason = result.get("stop_reason")
    required_information = result.get("required_information", [])

    if per_issue.analysis_response:
        per_issue.analysis_response.investigation_result = verdict
        per_issue.analysis_response.is_final = "TRUE"
        per_issue.analysis_response.justifications = justifications
        per_issue.analysis_response.prompt = analysis_prompt
        per_issue.analysis_response.agent_confidence = result["confidence"]
        per_issue.analysis_response.recommendations = required_information

    # Store investigation subgraph quality metrics in PerIssueData for confidence scoring
    per_issue.investigation_tool_call_count = total_tool_calls
    per_issue.investigation_reanalysis_count = reanalysis_count
    per_issue.investigation_stop_reason = stop_reason

    # Transfer investigation fetched_files (Dict[str, List[str]]) → PerIssueData (List[str])
    # Each entry represents a code block fetched by a research tool
    investigation_fetched = result.get("fetched_files", {})
    fetched_list = []
    for code_blocks in investigation_fetched.values():
        fetched_list.extend(code_blocks)
    per_issue.fetched_files = fetched_list

    # Store gathered_code so build_analysis_context can populate the Context column
    per_issue.gathered_code = result.get("gathered_code", "")

    # Transfer explored symbols from successful tool calls to PerIssueData
    # Each successful code-gathering call represents a unique code artifact explored
    tool_history = result.get("tool_call_history", [])
    for entry in tool_history:
        if entry.startswith("✓"):
            per_issue.found_symbols.add(entry)

    logger.info(
        f"{issue_id}: {verdict} (confidence: {result['confidence']}, "
        f"iterations: {iterations}, reanalysis: {reanalysis_count}, "
        f"tool_calls: {total_tool_calls}, stop_reason: {stop_reason}, "
        f"fetched_files: {len(fetched_list)}, symbols: {len(per_issue.found_symbols)})"
    )
