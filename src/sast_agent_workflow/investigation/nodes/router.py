"""Router logic for investigation subgraph."""

import logging
from typing import Literal

from ..constants import MAX_NO_PROGRESS_STREAK, MAX_REJECTION_STREAK
from .schemas import InvestigationState

logger = logging.getLogger(__name__)


def should_continue(
    state: InvestigationState,
) -> Literal["research", "reanalyze", "circuit_breaker", "end"]:
    """
    Determine if investigation should continue, trigger circuit breaker, or end.

    Returns:
        - "end": Investigation complete (is_complete=True)
        - "circuit_breaker": Safety limit reached (max iterations, rejection streak, no progress)
        - "reanalyze": Evaluator disagrees but has all evidence - retry analysis with feedback
        - "research": Continue with next iteration (gather more code)
    """
    issue_id = state.get("issue_id", "unknown")

    # First check if investigation is complete

    # Check circuit breaker conditions
    iteration = state["iteration"]
    max_iterations = state["max_iterations"]
    rejection_streak = state.get("evaluation_rejection_streak", 0)
    no_progress_streak = state.get("no_progress_streak", 0)

    # Circuit breaker condition 1: Max iterations exceeded
    if state["is_complete"]:
        logger.info(f"[{issue_id}] Investigation complete - routing to END")
        return "end"

    if iteration >= max_iterations:
        logger.warning(
            f"[{issue_id}] Circuit breaker: max_iterations reached "
            f"({iteration}/{max_iterations})"
        )
        return "circuit_breaker"

    # Circuit breaker condition 2: Evaluation rejection streak (evaluator keeps rejecting)
    if rejection_streak >= MAX_REJECTION_STREAK:
        logger.warning(
            f"[{issue_id}] Circuit breaker: evaluation_rejection_streak "
            f"({rejection_streak} consecutive rejections)"
        )
        return "circuit_breaker"

    # Circuit breaker condition 3: No progress detected (no new code gathered)
    if no_progress_streak >= MAX_NO_PROGRESS_STREAK:
        logger.warning(
            f"[{issue_id}] Circuit breaker: no_progress_streak "
            f"({no_progress_streak} iterations without new code)"
        )
        return "circuit_breaker"

    # Check if reanalysis is needed (evaluator disagrees but has all evidence)
    if state.get("needs_reanalysis", False):
        logger.info(f"[{issue_id}] Evaluator disagrees - routing to REANALYZE with feedback")
        return "reanalyze"

    # Continue research
    logger.info(
        f"[{issue_id}] Continuing research (iteration {iteration}/{max_iterations}, "
        f"rejection_streak={rejection_streak}, no_progress_streak={no_progress_streak})"
    )
    return "research"
