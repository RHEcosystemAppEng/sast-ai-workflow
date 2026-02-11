"""
Investigate Node - Multi-stage investigation with research-analysis-evaluation loop.

This uses a subgraph with three nodes:
1. RESEARCH: ReAct agent gathers code
2. ANALYSIS: LLM makes verdict decision
3. EVALUATION: LLM critiques analysis and decides if more research needed
"""

import logging
import os
from typing import List, Optional

from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.messages import ToolMessage
from langchain_core.tools import BaseTool

from common.config import Config
from dto.SASTWorkflowModels import SASTWorkflowTracker
from handlers.repo_handler_factory import repo_handler_factory

from ...observability.ground_truth_loader import load_ground_truth_verdicts
from .schemas import InvestigationState
from .subgraph import build_investigation_subgraph

logger = logging.getLogger(__name__)

# Langfuse integration (optional)
try:
    from langfuse.langchain import CallbackHandler as LangfuseCallbackHandler

    LANGFUSE_AVAILABLE = True
except ImportError:
    LANGFUSE_AVAILABLE = False
    logger.debug("Langfuse not available - tracing disabled")


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
        """
        Investigate each non-final issue using ReAct agent.

        Args:
            state: Tracker with issues to investigate

        Returns:
            Tracker with investigation results
        """
        logger.info("Investigating issues with ReAct agent...")

        # Initialize repo handler for initial code extraction
        repo_handler = repo_handler_factory(config)

        # Count issues to investigate
        issues_to_investigate = {
            issue_id: per_issue
            for issue_id, per_issue in state.issues.items()
            if not per_issue.analysis_response or per_issue.analysis_response.is_final != "TRUE"
        }

        logger.info(f"Investigating {len(issues_to_investigate)}/{len(state.issues)} issues")

        # Load ground truth for verdict scoring (if available)
        ground_truth_verdicts = None
        try:
            ground_truth_verdicts = load_ground_truth_verdicts(config)
            if ground_truth_verdicts:
                logger.info(
                    f"Loaded ground truth for {len(ground_truth_verdicts)} issues "
                    "for Langfuse scoring"
                )
        except Exception as e:
            logger.warning(f"Could not load ground truth for scoring: {e}")

        # Create Langfuse client once for all score operations
        langfuse_score_client = None
        if LANGFUSE_AVAILABLE and os.getenv("LANGFUSE_PUBLIC_KEY"):
            try:
                from langfuse import Langfuse

                langfuse_score_client = Langfuse(
                    public_key=os.getenv("LANGFUSE_PUBLIC_KEY"),
                    secret_key=os.getenv("LANGFUSE_SECRET_KEY"),
                    host=os.getenv("LANGFUSE_HOST", "http://localhost:3000"),
                )
                logger.info("Langfuse score client initialized for workflow")
            except Exception as e:
                logger.warning(f"Failed to initialize Langfuse score client: {e}")

        for issue_id, per_issue in issues_to_investigate.items():
            logger.info(f"Investigating {issue_id}...")

            # Setup per-issue Langfuse tracing with unique trace ID
            langfuse_handler: Optional[LangfuseCallbackHandler] = None
            issue_session_id: Optional[str] = None
            issue_trace_id: Optional[str] = None

            if LANGFUSE_AVAILABLE and os.getenv("LANGFUSE_PUBLIC_KEY"):
                try:
                    import uuid

                    # Create unique session ID per issue for clean trace separation
                    issue_session_id = f"{config.PROJECT_NAME}_{config.TEST_RUN_ID}_{issue_id}"

                    # Generate unique trace_id per run (32 lowercase hex chars)
                    issue_trace_id = uuid.uuid4().hex

                    # Create CallbackHandler with trace_context to set custom trace ID
                    langfuse_handler = LangfuseCallbackHandler(
                        trace_context={"trace_id": issue_trace_id}
                    )

                    # Log the trace URL for easy access
                    langfuse_host = os.getenv("LANGFUSE_HOST", "http://localhost:3000")
                    trace_url = f"{langfuse_host}/trace/{issue_trace_id}"
                    logger.info(f"[{issue_id}] Langfuse tracing enabled")
                    logger.info(f"[{issue_id}]   trace_id: {issue_trace_id}")
                    logger.info(f"[{issue_id}]   session: {issue_session_id}")
                    logger.info(f"[{issue_id}]   URL: {trace_url}")
                except Exception as e:
                    logger.warning(f"[{issue_id}] Failed to initialize Langfuse: {e}")

            try:
                # Get initial code from SAST trace
                logger.debug(f"[{issue_id}] Attempting to extract code from error trace...")
                initial_code = repo_handler.get_source_code_blocks_from_error_trace(
                    per_issue.issue.trace
                )

                file_count = len(initial_code) if initial_code else 0
                logger.debug(f"[{issue_id}] Extracted {file_count} files from trace")

                # Format initial code for display
                code_context = "\n\n".join(
                    [
                        f"=== {file_path} ===\n{code}"
                        for file_path, code in (initial_code or {}).items()
                    ]
                )

                if not code_context:
                    code_context = (
                        "(No code could be automatically extracted from the trace. "
                        "The agent will need to fetch code based on the trace "
                        "information above.)"
                    )
                    logger.debug(
                        f"[{issue_id}] No code extracted from trace - will rely on agent tool calls"
                    )

                # Pre-populate fetched_files and gathered_code with initial trace code
                fetched_files_initial = {}
                gathered_code_initial = ""

                if initial_code:
                    fetched_files_initial["fetch_code_from_error_trace"] = []

                    for file_path, code in initial_code.items():
                        formatted_block = f"=== {file_path} (from error trace) ===\n{code}"
                        fetched_files_initial["fetch_code_from_error_trace"].append(formatted_block)
                        gathered_code_initial += f"\n\n{formatted_block}\n"

                    logger.info(
                        f"[{issue_id}] Pre-populated with {len(initial_code)} files "
                        f"from error trace ({len(gathered_code_initial)} chars)"
                    )
                else:
                    logger.info(
                        f"[{issue_id}] No code extracted from trace - starting with empty context"
                    )

                # Create issue description
                issue_description = f"""**Issue ID:** {issue_id}
**Type:** {per_issue.issue.issue_type}
**Label:** {per_issue.issue.issue_label}
**CWE:** {per_issue.issue.issue_cwe}

**Trace:**
{per_issue.issue.trace}"""

                # Create initial investigation state with pre-populated context
                initial_state: InvestigationState = {
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

                # Prepare config with per-issue Langfuse tracing
                subgraph_config = {
                    "recursion_limit": 100,
                    "runName": f"investigate_{config.PROJECT_NAME}_{issue_id}",
                }

                # Add per-issue Langfuse tracing if available
                if langfuse_handler and issue_session_id:
                    subgraph_config["callbacks"] = [langfuse_handler]
                    subgraph_config["metadata"] = {
                        "langfuse_session_id": issue_session_id,
                        "langfuse_user_id": config.PROJECT_NAME,
                        "langfuse_tags": [
                            f"project:{config.PROJECT_NAME}",
                            f"test_run:{config.TEST_RUN_ID}",
                            f"model:{config.LLM_MODEL_NAME}",
                            f"issue:{issue_id}",
                            f"issue_type:{per_issue.issue.issue_type}",
                            "stage:investigate",
                            "method:multi_stage",
                        ],
                        "langfuse_release": config.PROJECT_VERSION,
                        "project_name": config.PROJECT_NAME,
                        "issue_id": issue_id,
                        "issue_type": per_issue.issue.issue_type,
                        "test_run_id": config.TEST_RUN_ID,
                    }

                # Run investigation subgraph
                logger.info(f"Running investigation subgraph for {issue_id}...")
                result = await investigation_subgraph.ainvoke(initial_state, config=subgraph_config)

                # Extract results
                verdict = result["proposed_verdict"].replace("_", " ")
                justifications = result["justifications"]
                analysis_prompt = result.get("analysis_prompt", "")
                iterations = result["iteration"]

                # Update tracker
                if per_issue.analysis_response:
                    per_issue.analysis_response.investigation_result = verdict
                    per_issue.analysis_response.is_final = "TRUE"
                    per_issue.analysis_response.justifications = justifications[:5]
                    per_issue.analysis_response.prompt = analysis_prompt

                logger.info(
                    f"{issue_id}: {verdict} "
                    f"(confidence: {result['confidence']}, iterations: {iterations})"
                )

                # Add Langfuse scores
                if langfuse_handler and langfuse_score_client and issue_trace_id:
                    _add_langfuse_scores(
                        langfuse_score_client,
                        issue_trace_id,
                        issue_id,
                        result,
                        ground_truth_verdicts,
                    )

            except Exception as e:
                logger.error(f"Investigation failed for {issue_id}: {e}", exc_info=True)

                # Mark as NEEDS_REVIEW on error
                if per_issue.analysis_response:
                    per_issue.analysis_response.investigation_result = "NEEDS REVIEW"
                    per_issue.analysis_response.justifications = [f"Investigation failed: {str(e)}"]
                    per_issue.analysis_response.is_final = "TRUE"

            finally:
                # Flush the Langfuse handler after each issue
                if langfuse_handler:
                    try:
                        logger.info(f"[{issue_id}] Flushing Langfuse trace...")
                        langfuse_handler.client.flush()
                        logger.info(f"[{issue_id}] Langfuse trace flushed successfully")
                    except Exception as flush_error:
                        logger.warning(
                            f"[{issue_id}] Failed to flush Langfuse handler: {flush_error}"
                        )

        logger.info("Investigation complete")
        logger.info("All Langfuse traces flushed")

        # Flush score client to ensure all scores are sent
        if langfuse_score_client:
            try:
                logger.info("Flushing Langfuse score client...")
                langfuse_score_client.flush()
                logger.info("Langfuse score client flushed successfully")
            except Exception as e:
                logger.warning(f"Failed to flush Langfuse score client: {e}")

        return state

    return investigate


def _add_langfuse_scores(
    langfuse_score_client,
    issue_trace_id: str,
    issue_id: str,
    result: dict,
    ground_truth_verdicts: dict | None,
):
    """Add Langfuse scores for an investigation result."""
    try:
        # Score 1: Count tool calls from research_messages
        tool_call_count = sum(
            1 for msg in result.get("research_messages", []) if isinstance(msg, ToolMessage)
        )

        logger.info(f"[{issue_id}] Adding Langfuse scores: tool_call_count={tool_call_count}")

        langfuse_score_client.create_score(
            trace_id=issue_trace_id,
            name="tool_call_count",
            value=float(tool_call_count),
            data_type="NUMERIC",
            comment="Total tool executions during investigation",
        )

        # Score 2: Investigation verdict
        investigation_verdict = result["proposed_verdict"]
        langfuse_score_client.create_score(
            trace_id=issue_trace_id,
            name="verdict",
            value=investigation_verdict,
            data_type="CATEGORICAL",
            comment=f"Investigation verdict: {investigation_verdict}",
        )

        # Score 3: Ground truth verdict (if available)
        if ground_truth_verdicts and issue_id in ground_truth_verdicts:
            ground_truth_verdict = ground_truth_verdicts[issue_id]

            langfuse_score_client.create_score(
                trace_id=issue_trace_id,
                name="ground_truth_verdict",
                value=ground_truth_verdict,
                data_type="CATEGORICAL",
                comment="Human-verified ground truth verdict",
            )

            # Score 4: Verdict correctness comparison
            is_correct = investigation_verdict == ground_truth_verdict

            logger.info(
                f"[{issue_id}] Verdict comparison: "
                f"investigation={investigation_verdict}, "
                f"ground_truth={ground_truth_verdict}, "
                f"correct={is_correct}"
            )

            langfuse_score_client.create_score(
                trace_id=issue_trace_id,
                name="verdict_correct",
                value=1 if is_correct else 0,
                data_type="BOOLEAN",
                comment=f"Verdict matches ground truth: {ground_truth_verdict}",
            )
        else:
            if ground_truth_verdicts is None:
                logger.debug(
                    f"[{issue_id}] No ground truth available - skipping verdict correctness score"
                )
            else:
                logger.debug(
                    f"[{issue_id}] No ground truth for this issue - "
                    "skipping verdict correctness score"
                )

    except Exception as score_error:
        logger.warning(f"[{issue_id}] Failed to add Langfuse scores: {score_error}")
