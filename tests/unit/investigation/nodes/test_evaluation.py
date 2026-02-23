"""
Tests for evaluation node in investigation/nodes/evaluation.py.

Covers:
- reanalysis_count tracking — incremented when evaluator disagrees
  with analysis (needs_reanalysis=True), left unchanged otherwise.
- Error handling — evaluation failure terminates investigation early.
"""

from unittest.mock import MagicMock, Mock, patch

import pytest

from sast_agent_workflow.nodes.sub_agents.investigation.nodes.evaluation import (
    create_evaluation_node,
)

_MOD = "sast_agent_workflow.nodes.sub_agents.investigation.nodes.evaluation"


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def mock_config():
    config = Mock()
    config.structured_output_max_retries = 3
    return config


@pytest.fixture
def mock_llm():
    return MagicMock()


def _make_state(**overrides) -> dict:
    """Build a minimal InvestigationState dict with sensible defaults."""
    base = {
        "issue_id": "test-issue",
        "issue_cwe": "CWE-120",
        "issue_description": "desc",
        "initial_code": "",
        "research_messages": [],
        "gathered_code": "some code",
        "fetched_files": {},
        "tool_call_history": [],
        "analysis": "analysis text",
        "analysis_prompt": "prompt",
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


def _mock_evaluation_result(result="APPROVED", feedback="", required_information=None):
    """Create a mock EvaluationResult."""
    from sast_agent_workflow.nodes.sub_agents.investigation.nodes.schemas import (
        EvaluationResult,
    )

    return EvaluationResult(
        result=result,
        feedback=feedback,
        required_information=required_information or [],
    )


# ---------------------------------------------------------------------------
# reanalysis_count tracking
# ---------------------------------------------------------------------------


class TestReanalysisCount:
    """Tests for reanalysis_count field in evaluation node output."""

    @patch(f"{_MOD}.robust_structured_output")
    def test__increments_reanalysis_count_when_reanalysis_triggered(
        self, mock_structured, mock_llm, mock_config
    ):
        """reanalysis_count should increase by 1 when evaluator disagrees
        with analysis but has no missing evidence (needs_reanalysis=True)."""
        mock_structured.return_value = _mock_evaluation_result(
            result="NEEDS_MORE_RESEARCH",
            feedback="Disagree with verdict",
            required_information=[],
        )
        state = _make_state(reanalysis_count=0)
        evaluate = create_evaluation_node(mock_llm, mock_config)

        result = evaluate(state)

        assert result["needs_reanalysis"] is True
        assert result["reanalysis_count"] == 1

    @patch(f"{_MOD}.robust_structured_output")
    def test__accumulates_reanalysis_count_across_iterations(
        self, mock_structured, mock_llm, mock_config
    ):
        """reanalysis_count should accumulate across multiple reanalysis triggers."""
        mock_structured.return_value = _mock_evaluation_result(
            result="NEEDS_MORE_RESEARCH",
            feedback="Still disagree",
            required_information=[],
        )
        state = _make_state(reanalysis_count=2)
        evaluate = create_evaluation_node(mock_llm, mock_config)

        result = evaluate(state)

        assert result["reanalysis_count"] == 3

    @patch(f"{_MOD}.robust_structured_output")
    def test__does_not_increment_reanalysis_count_when_approved(
        self, mock_structured, mock_llm, mock_config
    ):
        """reanalysis_count should stay the same when evaluation is APPROVED."""
        mock_structured.return_value = _mock_evaluation_result(
            result="APPROVED",
            feedback="Looks good",
        )
        state = _make_state(reanalysis_count=1)
        evaluate = create_evaluation_node(mock_llm, mock_config)

        result = evaluate(state)

        assert result["reanalysis_count"] == 1

    @patch(f"{_MOD}.robust_structured_output")
    def test__does_not_increment_reanalysis_count_when_needs_more_research_with_items(
        self, mock_structured, mock_llm, mock_config
    ):
        """reanalysis_count should stay the same when evaluator requests more
        research with specific required_information (not a reanalysis)."""
        mock_structured.return_value = _mock_evaluation_result(
            result="NEEDS_MORE_RESEARCH",
            feedback="Need more code",
            required_information=["Check function foo"],
        )
        state = _make_state(reanalysis_count=1)
        evaluate = create_evaluation_node(mock_llm, mock_config)

        result = evaluate(state)

        assert result["needs_reanalysis"] is False
        assert result["reanalysis_count"] == 1

    @patch(f"{_MOD}.robust_structured_output")
    def test__preserves_reanalysis_count_on_error(self, mock_structured, mock_llm, mock_config):
        """reanalysis_count should be preserved (not incremented) when
        evaluation fails — the node returns early without touching the count."""
        mock_structured.side_effect = RuntimeError("LLM error")
        state = _make_state(reanalysis_count=2)
        evaluate = create_evaluation_node(mock_llm, mock_config)

        result = evaluate(state)

        assert result["reanalysis_count"] == 2

    @patch(f"{_MOD}.robust_structured_output")
    def test__defaults_to_zero_when_reanalysis_count_missing_from_state(
        self, mock_structured, mock_llm, mock_config
    ):
        """When reanalysis_count is absent from state, should treat as 0
        and increment to 1 on reanalysis."""
        mock_structured.return_value = _mock_evaluation_result(
            result="NEEDS_MORE_RESEARCH",
            feedback="Disagree",
            required_information=[],
        )
        state = _make_state()
        del state["reanalysis_count"]
        evaluate = create_evaluation_node(mock_llm, mock_config)

        result = evaluate(state)

        assert result["reanalysis_count"] == 1


# ---------------------------------------------------------------------------
# Error handling — early exit on evaluation failure
# ---------------------------------------------------------------------------


class TestEvaluationError:
    """Tests for evaluation node error handling."""

    @patch(f"{_MOD}.robust_structured_output")
    def test__sets_is_complete_on_error(self, mock_structured, mock_llm, mock_config):
        """Investigation should be marked complete when evaluation fails."""
        mock_structured.side_effect = RuntimeError("LLM timeout")
        evaluate = create_evaluation_node(mock_llm, mock_config)

        result = evaluate(_make_state())

        assert result["is_complete"] is True

    @patch(f"{_MOD}.robust_structured_output")
    def test__sets_stop_reason_on_error(self, mock_structured, mock_llm, mock_config):
        """stop_reason should indicate evaluation_error."""
        mock_structured.side_effect = RuntimeError("LLM timeout")
        evaluate = create_evaluation_node(mock_llm, mock_config)

        result = evaluate(_make_state())

        assert result["stop_reason"] == "evaluation_error"

    @patch(f"{_MOD}.robust_structured_output")
    def test__sets_needs_review_verdict_on_error(self, mock_structured, mock_llm, mock_config):
        """Verdict should be forced to NEEDS_REVIEW when evaluation fails."""
        mock_structured.side_effect = RuntimeError("LLM timeout")
        state = _make_state(proposed_verdict="FALSE_POSITIVE")
        evaluate = create_evaluation_node(mock_llm, mock_config)

        result = evaluate(state)

        assert result["proposed_verdict"] == "NEEDS_REVIEW"

    @patch(f"{_MOD}.robust_structured_output")
    def test__captures_error_in_feedback(self, mock_structured, mock_llm, mock_config):
        """evaluation_feedback should contain the error message."""
        mock_structured.side_effect = RuntimeError("connection reset")
        evaluate = create_evaluation_node(mock_llm, mock_config)

        result = evaluate(_make_state())

        assert "connection reset" in result["evaluation_feedback"]

    @patch(f"{_MOD}.robust_structured_output")
    def test__does_not_increment_reanalysis_count_on_error(
        self, mock_structured, mock_llm, mock_config
    ):
        """reanalysis_count should be unchanged on error (early return)."""
        mock_structured.side_effect = RuntimeError("LLM error")
        evaluate = create_evaluation_node(mock_llm, mock_config)

        result = evaluate(_make_state(reanalysis_count=3))

        assert result["reanalysis_count"] == 3
