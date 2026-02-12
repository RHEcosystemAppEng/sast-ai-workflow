"""
Investigate Node - Multi-stage investigation with research-analysis-evaluation loop.

This uses a subgraph with three nodes:
1. RESEARCH: ReAct agent gathers code
2. ANALYSIS: LLM makes verdict decision
3. EVALUATION: LLM critiques analysis and decides if more research needed
"""

import logging
from typing import Any, Dict, List, Optional

from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.tools import BaseTool

from common.config import Config
from dto.SASTWorkflowModels import SASTWorkflowTracker
from handlers.repo_handler_factory import repo_handler_factory

from ...observability.ground_truth_loader import load_ground_truth_verdicts
from ...observability.langfuse_integration import (
    add_langfuse_scores,
    build_langfuse_metadata_for_investigation,
    issue_langfuse_context,
    langfuse_score_client_context,
)
from .schemas import InvestigationState
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
        repo_handler = repo_handler_factory(config)
        issues_to_investigate = _get_issues_to_investigate(state)
        logger.info("Investigating %s/%s issues", len(issues_to_investigate), len(state.issues))

        ground_truth_verdicts = _load_ground_truth_safe(config)
        with langfuse_score_client_context() as langfuse_score_client:
            for issue_id, per_issue in issues_to_investigate.items():
                logger.info("Investigating %s...", issue_id)
                with issue_langfuse_context(issue_id, config) as (
                    langfuse_handler,
                    issue_session_id,
                    issue_trace_id,
                ):
                    try:
                        code_context, fetched_initial, gathered_initial = (
                            _extract_initial_code_and_build_context(
                                repo_handler, per_issue, issue_id
                            )
                        )
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
                        logger.info("Running investigation subgraph for %s...", issue_id)
                        result = await investigation_subgraph.ainvoke(
                            initial_state, config=subgraph_config
                        )
                        _update_tracker_from_result(per_issue, result, issue_id)
                        if langfuse_handler and langfuse_score_client and issue_trace_id:
                            add_langfuse_scores(
                                langfuse_score_client,
                                issue_trace_id,
                                issue_id,
                                result,
                                ground_truth_verdicts,
                            )
                    except Exception as e:
                        logger.error("Investigation failed for %s: %s", issue_id, e, exc_info=True)
                        if per_issue.analysis_response:
                            per_issue.analysis_response.investigation_result = "NEEDS REVIEW"
                            per_issue.analysis_response.justifications = [
                                "Investigation failed: %s" % str(e)
                            ]
                            per_issue.analysis_response.is_final = "TRUE"
        logger.info("Investigation complete")
        return state

    return investigate


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
            logger.info("Loaded ground truth for %s issues for Langfuse scoring", len(verdicts))
        return verdicts
    except Exception as e:
        logger.warning("Could not load ground truth for scoring: %s", e)
        return None


def _extract_initial_code_and_build_context(repo_handler, per_issue, issue_id: str):
    """Extract initial code from trace and build code_context, fetched_initial, gathered_initial."""
    initial_code = repo_handler.get_source_code_blocks_from_error_trace(per_issue.issue.trace)
    file_count = len(initial_code) if initial_code else 0
    logger.debug("[%s] Extracted %s files from trace", issue_id, file_count)

    code_context = "\n\n".join(
        ["=== %s ===\n%s" % (fp, code) for fp, code in (initial_code or {}).items()]
    )
    if not code_context:
        code_context = (
            "(No code could be automatically extracted from the trace. "
            "The agent will need to fetch code based on the trace information above.)"
        )
        logger.debug("[%s] No code extracted from trace - will rely on agent tool calls", issue_id)

    fetched_files_initial = {}
    gathered_code_initial = ""
    if initial_code:
        fetched_files_initial["fetch_code_from_error_trace"] = []
        for file_path, code in initial_code.items():
            formatted_block = "=== %s (from error trace) ===\n%s" % (file_path, code)
            fetched_files_initial["fetch_code_from_error_trace"].append(formatted_block)
            gathered_code_initial += "\n\n%s\n" % formatted_block
        logger.info(
            "[%s] Pre-populated with %s files from error trace (%s chars)",
            issue_id,
            len(initial_code),
            len(gathered_code_initial),
        )
    else:
        logger.info("[%s] No code extracted from trace - starting with empty context", issue_id)

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
        "confidence": "",
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
        "recursion_limit": 100,
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
    if per_issue.analysis_response:
        per_issue.analysis_response.investigation_result = verdict
        per_issue.analysis_response.is_final = "TRUE"
        per_issue.analysis_response.justifications = justifications
        per_issue.analysis_response.prompt = analysis_prompt
    logger.info(
        f"{issue_id}: {verdict} (confidence: {result['confidence']}, iterations: {iterations})"
    )
