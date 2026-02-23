"""
Tests for circuit breaker node in investigation/nodes/circuit_breaker.py.

Covers: stop_reason classification, forced verdict, is_complete flag,
evaluation_feedback message, and priority ordering of stop conditions.
"""

from sast_agent_workflow.nodes.sub_agents.investigation.nodes.circuit_breaker import (
    create_circuit_breaker_node,
)

_CB_MOD = "sast_agent_workflow.nodes.sub_agents.investigation.nodes.circuit_breaker"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_state(**overrides) -> dict:
    """Build a minimal InvestigationState dict with sensible defaults."""
    base = {
        "issue_id": "test-issue",
        "issue_cwe": "CWE-120",
        "issue_description": "desc",
        "initial_code": "",
        "research_messages": [],
        "gathered_code": "",
        "fetched_files": {},
        "tool_call_history": [],
        "analysis": "",
        "analysis_prompt": "",
        "proposed_verdict": "FALSE_POSITIVE",
        "justifications": ["reason"],
        "confidence": "HIGH",
        "evaluation_result": "",
        "evaluation_feedback": "",
        "required_information": [],
        "evaluation_rejection_streak": 0,
        "no_progress_streak": 0,
        "previous_code_length": 0,
        "stop_reason": None,
        "iteration": 1,
        "max_iterations": 4,
        "is_complete": False,
        "needs_reanalysis": False,
        "reanalysis_count": 0,
    }
    base.update(overrides)
    return base


# ---------------------------------------------------------------------------
# stop_reason classification
# ---------------------------------------------------------------------------


class TestStopReason:
    """Tests for stop_reason values set by circuit breaker."""

    def test__sets_max_iterations_when_iteration_exceeds_limit(self):
        """stop_reason should be 'max_iterations' when iteration >= max_iterations."""
        cb = create_circuit_breaker_node()
        state = _make_state(iteration=4, max_iterations=4)

        result = cb(state)

        assert result["stop_reason"] == "max_iterations"

    def test__sets_evaluation_rejection_streak(self):
        """stop_reason should be 'evaluation_rejection_streak' when streak >= threshold."""
        cb = create_circuit_breaker_node()
        state = _make_state(iteration=2, max_iterations=4, evaluation_rejection_streak=3)

        result = cb(state)

        assert result["stop_reason"] == "evaluation_rejection_streak"

    def test__sets_no_progress_detected(self):
        """stop_reason should be 'no_progress_detected' when no_progress_streak >= threshold."""
        cb = create_circuit_breaker_node()
        state = _make_state(iteration=2, max_iterations=4, no_progress_streak=2)

        result = cb(state)

        assert result["stop_reason"] == "no_progress_detected"

    def test__sets_unknown_when_no_condition_matches(self):
        """stop_reason should be 'circuit_breaker_unknown' as fallback."""
        cb = create_circuit_breaker_node()
        state = _make_state(
            iteration=1,
            max_iterations=4,
            evaluation_rejection_streak=0,
            no_progress_streak=0,
        )

        result = cb(state)

        assert result["stop_reason"] == "circuit_breaker_unknown"

    def test__max_iterations_takes_priority_over_rejection_streak(self):
        """When multiple conditions match, max_iterations should win."""
        cb = create_circuit_breaker_node()
        state = _make_state(iteration=4, max_iterations=4, evaluation_rejection_streak=3)

        result = cb(state)

        assert result["stop_reason"] == "max_iterations"

    def test__rejection_streak_takes_priority_over_no_progress(self):
        """When rejection streak and no progress both match, rejection streak wins."""
        cb = create_circuit_breaker_node()
        state = _make_state(
            iteration=2,
            max_iterations=4,
            evaluation_rejection_streak=3,
            no_progress_streak=2,
        )

        result = cb(state)

        assert result["stop_reason"] == "evaluation_rejection_streak"


# ---------------------------------------------------------------------------
# State updates on circuit breaker
# ---------------------------------------------------------------------------


class TestCircuitBreakerStateUpdates:
    """Tests for state fields set by circuit breaker node."""

    def test__sets_is_complete_to_true(self):
        """Circuit breaker should always set is_complete=True."""
        cb = create_circuit_breaker_node()

        result = cb(_make_state(iteration=4, max_iterations=4))

        assert result["is_complete"] is True

    def test__forces_verdict_to_needs_review(self):
        """Circuit breaker should override proposed_verdict to NEEDS_REVIEW."""
        cb = create_circuit_breaker_node()
        state = _make_state(iteration=4, max_iterations=4, proposed_verdict="FALSE_POSITIVE")

        result = cb(state)

        assert result["proposed_verdict"] == "NEEDS_REVIEW"

    def test__overrides_true_positive_verdict(self):
        """Even TRUE_POSITIVE verdict should be overridden to NEEDS_REVIEW."""
        cb = create_circuit_breaker_node()
        state = _make_state(iteration=4, max_iterations=4, proposed_verdict="TRUE_POSITIVE")

        result = cb(state)

        assert result["proposed_verdict"] == "NEEDS_REVIEW"

    def test__sets_evaluation_feedback_with_stop_reason(self):
        """evaluation_feedback should mention the stop reason."""
        cb = create_circuit_breaker_node()

        result = cb(_make_state(iteration=4, max_iterations=4))

        assert "max_iterations" in result["evaluation_feedback"]
        assert "circuit breaker" in result["evaluation_feedback"].lower()

    def test__preserves_other_state_fields(self):
        """Fields not modified by circuit breaker should be preserved."""
        cb = create_circuit_breaker_node()
        state = _make_state(
            iteration=4,
            max_iterations=4,
            justifications=["reason1", "reason2"],
            confidence="HIGH",
            reanalysis_count=2,
        )

        result = cb(state)

        assert result["justifications"] == ["reason1", "reason2"]
        assert result["confidence"] == "HIGH"
        assert result["reanalysis_count"] == 2
        assert result["issue_id"] == "test-issue"
