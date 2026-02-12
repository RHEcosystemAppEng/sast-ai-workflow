"""Circuit breaker node for graceful investigation termination."""

import logging

from ...core import MAX_NO_PROGRESS_STREAK, MAX_REJECTION_STREAK
from .schemas import InvestigationState

logger = logging.getLogger(__name__)


def create_circuit_breaker_node():
    """
    Create circuit breaker node.

    This node handles graceful termination when safety limits are reached:
    - Max iterations exceeded
    - Evaluation rejection streak (evaluator keeps rejecting)
    - No progress detected (no new code gathered)
    """

    def circuit_breaker(state: InvestigationState) -> InvestigationState:
        """Handle circuit breaker termination with proper state updates."""
        issue_id = state["issue_id"]
        logger.warning(f"[{issue_id}] Circuit breaker triggered")

        # Determine stop reason
        if state["iteration"] >= state["max_iterations"]:
            stop_reason = "max_iterations"
            logger.warning(f"[{issue_id}] Max iterations ({state['max_iterations']}) reached")
        elif state.get("evaluation_rejection_streak", 0) >= MAX_REJECTION_STREAK:
            stop_reason = "evaluation_rejection_streak"
            logger.warning(
                f"[{issue_id}] Evaluator rejected {state.get('evaluation_rejection_streak')} "
                f"consecutive times"
            )
        elif state.get("no_progress_streak", 0) >= MAX_NO_PROGRESS_STREAK:
            stop_reason = "no_progress_detected"
            logger.warning(
                f"[{issue_id}] No progress detected for {state.get('no_progress_streak')} "
                f"consecutive iterations"
            )
        else:
            stop_reason = "circuit_breaker_unknown"
            logger.warning(f"[{issue_id}] Circuit breaker triggered (unknown reason)")

        logger.warning(f"[{issue_id}] Stop reason: {stop_reason}")

        # Force NEEDS_REVIEW if no verdict or already NEEDS_REVIEW
        verdict = state.get("proposed_verdict")
        logger.debug(f"[{issue_id}] Original verdict: {verdict}")
        logger.debug(f"[{issue_id}] Setting verdict to NEEDS_REVIEW due to {stop_reason}")

        return {
            **state,
            "is_complete": True,
            "stop_reason": stop_reason,
            "proposed_verdict": "NEEDS_REVIEW",
            "evaluation_feedback": f"Investigation terminated by circuit breaker: {stop_reason}",
        }

    return circuit_breaker
