"""Langfuse integration for tracing and scoring.

Provides optional Langfuse client/handlers and context managers for
workflow-level and per-issue tracing and score client flush.
"""

import logging
import os
import uuid
from contextlib import contextmanager
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple

from langchain_core.messages import ToolMessage

logger = logging.getLogger(__name__)

# Optional Langfuse dependency
try:
    from langfuse.langchain import CallbackHandler as LangfuseCallbackHandler

    LANGFUSE_AVAILABLE = True
except ImportError:
    LANGFUSE_AVAILABLE = False
    LangfuseCallbackHandler = None  # type: ignore[misc, assignment]
    logger.debug("Langfuse not available - tracing disabled")


@dataclass(frozen=True)
class LangfuseMetadata:
    """Langfuse trace metadata. Required fields shared; optional vary by context."""

    session_id: str
    user_id: str
    tags: Tuple[str, ...]
    project_name: str
    test_run_id: str
    project_version: Optional[str] = None
    llm_model: Optional[str] = None
    langfuse_release: Optional[str] = None
    issue_id: Optional[str] = None
    issue_type: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        """Return metadata as dict for LangChain config (langfuse_* and custom keys)."""
        out: Dict[str, Any] = {
            "langfuse_session_id": self.session_id,
            "langfuse_user_id": self.user_id,
            "langfuse_tags": list(self.tags),
            "project_name": self.project_name,
            "test_run_id": self.test_run_id,
        }
        if self.project_version is not None:
            out["project_version"] = self.project_version
        if self.llm_model is not None:
            out["llm_model"] = self.llm_model
        if self.langfuse_release is not None:
            out["langfuse_release"] = self.langfuse_release
        if self.issue_id is not None:
            out["issue_id"] = self.issue_id
        if self.issue_type is not None:
            out["issue_type"] = self.issue_type
        return out


def _workflow_tags(config: Any) -> List[str]:
    """Tags for workflow-level trace."""
    return [
        f"project:{config.PROJECT_NAME}",
        f"version:{config.PROJECT_VERSION}",
        f"test_run:{config.TEST_RUN_ID}",
        f"model:{config.LLM_MODEL_NAME}",
        "workflow:sast_pipeline",
    ]


def _investigation_tags(config: Any, issue_id: str, issue_type: str) -> List[str]:
    """Tags for per-issue investigation trace."""
    return [
        f"project:{config.PROJECT_NAME}",
        f"test_run:{config.TEST_RUN_ID}",
        f"model:{config.LLM_MODEL_NAME}",
        f"issue:{issue_id}",
        f"issue_type:{issue_type}",
        "stage:investigate",
        "method:multi_stage",
    ]


@contextmanager
def langfuse_score_client_context():
    """Create Langfuse score client; flush on exit. Yields client or None."""
    client = create_langfuse_score_client_safe()
    try:
        yield client
    finally:
        if client:
            try:
                logger.info("Flushing Langfuse score client...")
                client.flush()
                logger.info("Langfuse score client flushed successfully")
            except Exception as e:
                logger.warning("Failed to flush Langfuse score client: %s", e)


@contextmanager
def issue_langfuse_context(issue_id: str, config: Any):
    """Setup per-issue Langfuse handler; flush on exit. Yields (handler, session_id, trace_id)."""
    handler, session_id, trace_id = setup_issue_langfuse(issue_id, config)
    try:
        yield (handler, session_id, trace_id)
    finally:
        flush_issue_langfuse(handler, issue_id)


@contextmanager
def workflow_langfuse_context():
    """Create Langfuse workflow handler (single trace); flush on exit. Yields handler or None."""
    handler = create_langfuse_workflow_handler()
    try:
        yield handler
    finally:
        if handler:
            try:
                logger.info("Flushing Langfuse workflow trace...")
                handler.client.flush()
                logger.info("Langfuse workflow trace flushed")
            except Exception as e:
                logger.warning("Failed to flush Langfuse workflow handler: %s", e)


def create_langfuse_workflow_handler() -> Optional[Any]:
    """Create Langfuse callback for workflow (single trace). None if per-issue or unavailable."""
    if not LANGFUSE_AVAILABLE or LangfuseCallbackHandler is None:
        return None
    trace_per_issue = os.getenv("LANGFUSE_TRACE_PER_ISSUE", "false").lower() == "true"
    if trace_per_issue:
        logger.info("Langfuse per-issue tracing enabled (separate traces for each issue)")
        return None
    if not os.getenv("LANGFUSE_PUBLIC_KEY"):
        return None
    try:
        handler = LangfuseCallbackHandler()
        logger.info("Langfuse workflow tracing enabled (single trace for all issues)")
        return handler
    except Exception as e:
        logger.warning("Failed to initialize Langfuse: %s", e)
        return None


def build_langfuse_metadata_for_workflow(config: Any, session_id: str) -> Dict[str, Any]:
    """Build Langfuse metadata dict for workflow-level tracing (single trace)."""
    meta = LangfuseMetadata(
        session_id=session_id,
        user_id=config.PROJECT_NAME,
        tags=tuple(_workflow_tags(config)),
        project_name=config.PROJECT_NAME,
        test_run_id=config.TEST_RUN_ID,
        project_version=config.PROJECT_VERSION,
        llm_model=config.LLM_MODEL_NAME,
    )
    return meta.to_dict()


def build_langfuse_metadata_for_investigation(
    config: Any, session_id: str, issue_id: str, issue_type: str
) -> Dict[str, Any]:
    """Build Langfuse metadata dict for per-issue investigation subgraph."""
    meta = LangfuseMetadata(
        session_id=session_id,
        user_id=config.PROJECT_NAME,
        tags=tuple(_investigation_tags(config, issue_id, issue_type)),
        project_name=config.PROJECT_NAME,
        test_run_id=config.TEST_RUN_ID,
        langfuse_release=config.PROJECT_VERSION,
        issue_id=issue_id,
        issue_type=issue_type,
    )
    return meta.to_dict()


def create_langfuse_score_client_safe() -> Optional[Any]:
    """Create Langfuse client for scoring; return None if unavailable or on error."""
    if not LANGFUSE_AVAILABLE or not os.getenv("LANGFUSE_PUBLIC_KEY"):
        return None
    try:
        from langfuse import Langfuse

        client = Langfuse(
            public_key=os.getenv("LANGFUSE_PUBLIC_KEY"),
            secret_key=os.getenv("LANGFUSE_SECRET_KEY"),
            host=os.getenv("LANGFUSE_HOST"),
        )
        logger.info("Langfuse score client initialized for workflow")
        return client
    except Exception as e:
        logger.warning("Failed to initialize Langfuse score client: %s", e)
        return None


def setup_issue_langfuse(
    issue_id: str, config: Any
) -> Tuple[Optional[Any], Optional[str], Optional[str]]:
    """Create per-issue Langfuse handler. Returns (handler, session_id, trace_id) or all None."""
    if (
        not LANGFUSE_AVAILABLE
        or not os.getenv("LANGFUSE_PUBLIC_KEY")
        or LangfuseCallbackHandler is None
    ):
        return (None, None, None)
    try:
        issue_session_id = "%s_%s_%s" % (config.PROJECT_NAME, config.TEST_RUN_ID, issue_id)
        issue_trace_id = uuid.uuid4().hex
        handler = LangfuseCallbackHandler(trace_context={"trace_id": issue_trace_id})
        logger.info("[%s] Langfuse tracing enabled", issue_id)
        logger.info("[%s]   trace_id: %s", issue_id, issue_trace_id)
        logger.info("[%s]   session: %s", issue_id, issue_session_id)
        return (handler, issue_session_id, issue_trace_id)
    except Exception as e:
        logger.warning("[%s] Failed to initialize Langfuse: %s", issue_id, e)
        return (None, None, None)


def flush_issue_langfuse(langfuse_handler: Optional[Any], issue_id: str) -> None:
    """Flush Langfuse handler for the issue if present."""
    if not langfuse_handler:
        return
    try:
        logger.info("[%s] Flushing Langfuse trace...", issue_id)
        langfuse_handler.client.flush()
        logger.info("[%s] Langfuse trace flushed successfully", issue_id)
    except Exception as flush_error:
        logger.warning("[%s] Failed to flush Langfuse handler: %s", issue_id, flush_error)


def add_langfuse_scores(
    langfuse_score_client: Any,
    issue_trace_id: str,
    issue_id: str,
    result: dict,
    ground_truth_verdicts: Optional[dict],
) -> None:
    """Add Langfuse scores for an investigation result."""
    try:
        tool_call_count = sum(
            1 for msg in result.get("research_messages", []) if isinstance(msg, ToolMessage)
        )
        logger.info("[%s] Adding Langfuse scores: tool_call_count=%s", issue_id, tool_call_count)

        langfuse_score_client.create_score(
            trace_id=issue_trace_id,
            name="tool_call_count",
            value=float(tool_call_count),
            data_type="NUMERIC",
            comment="Total tool executions during investigation",
        )

        investigation_verdict = result["proposed_verdict"]
        langfuse_score_client.create_score(
            trace_id=issue_trace_id,
            name="verdict",
            value=investigation_verdict,
            data_type="CATEGORICAL",
            comment="Investigation verdict: %s" % investigation_verdict,
        )

        if ground_truth_verdicts and issue_id in ground_truth_verdicts:
            ground_truth_verdict = ground_truth_verdicts[issue_id]
            langfuse_score_client.create_score(
                trace_id=issue_trace_id,
                name="ground_truth_verdict",
                value=ground_truth_verdict,
                data_type="CATEGORICAL",
                comment="Human-verified ground truth verdict",
            )
            is_correct = investigation_verdict == ground_truth_verdict
            logger.info(
                "[%s] Verdict comparison: investigation=%s, ground_truth=%s, correct=%s",
                issue_id,
                investigation_verdict,
                ground_truth_verdict,
                is_correct,
            )
            langfuse_score_client.create_score(
                trace_id=issue_trace_id,
                name="verdict_correct",
                value=1 if is_correct else 0,
                data_type="BOOLEAN",
                comment="Verdict matches ground truth: %s" % ground_truth_verdict,
            )
        else:
            if ground_truth_verdicts is None:
                logger.debug(
                    "[%s] No ground truth available - skipping verdict correctness score", issue_id
                )
            else:
                logger.debug(
                    "[%s] No ground truth for this issue - skipping verdict correctness score",
                    issue_id,
                )
    except Exception as score_error:
        logger.warning("[%s] Failed to add Langfuse scores: %s", issue_id, score_error)
