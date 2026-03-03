"""
Tests for investigation/prompts/evaluation.py.

Covers: build_evaluation_prompt state interpolation,
justifications formatting, checklist verification, and anti-loop guidance.
"""

import pytest

from sast_agent_workflow.nodes.sub_agents.investigation.prompts.evaluation import (
    build_evaluation_prompt,
)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def base_state():
    """Minimal state dict required by build_evaluation_prompt."""
    return {
        "iteration": 2,
        "max_iterations": 5,
        "evaluation_rejection_streak": 1,
        "no_progress_streak": 0,
        "issue_description": "CWE-416: Use-after-free in cleanup()",
        "gathered_code": "free(ptr);\nptr->next = NULL;",
        "analysis": "The pointer is freed then dereferenced.",
        "proposed_verdict": "TRUE_POSITIVE",
        "justifications": [
            "free(ptr) at line 42",
            "ptr->next access at line 43",
        ],
    }


# ---------------------------------------------------------------------------
# build_evaluation_prompt
# ---------------------------------------------------------------------------


class TestBuildEvaluationPrompt:
    """Tests for build_evaluation_prompt."""

    def test__contains_issue_description(self, base_state):
        """Prompt should include the SAST finding."""
        result = build_evaluation_prompt(base_state)

        assert "CWE-416" in result
        assert "cleanup()" in result

    def test__contains_gathered_code(self, base_state):
        """Prompt should include gathered code."""
        result = build_evaluation_prompt(base_state)

        assert "free(ptr)" in result
        assert "ptr->next = NULL" in result

    def test__contains_analysis(self, base_state):
        """Prompt should include the analysis text."""
        result = build_evaluation_prompt(base_state)

        assert "The pointer is freed then dereferenced." in result

    def test__contains_proposed_verdict(self, base_state):
        """Prompt should include the proposed verdict."""
        result = build_evaluation_prompt(base_state)

        assert "TRUE_POSITIVE" in result

    def test__contains_confidence_scoring_instructions(self, base_state):
        """Prompt should include confidence scoring instructions for the evaluator."""
        result = build_evaluation_prompt(base_state)

        assert "0.0" in result
        assert "1.0" in result
        assert "confidence" in result.lower()

    def test__formats_justifications(self, base_state):
        """Each justification should appear as a bullet point."""
        result = build_evaluation_prompt(base_state)

        assert "- free(ptr) at line 42" in result
        assert "- ptr->next access at line 43" in result

    def test__empty_justifications_handled(self, base_state):
        """Empty justifications list should not cause errors."""
        base_state["justifications"] = []
        result = build_evaluation_prompt(base_state)

        assert "**JUSTIFICATIONS:**" in result

    def test__iteration_context_interpolated(self, base_state):
        """Iteration and max_iterations should appear in the prompt."""
        result = build_evaluation_prompt(base_state)

        assert "Iteration: 2 of 5" in result

    def test__rejection_streak_shown(self, base_state):
        """Consecutive rejection streak should be displayed."""
        result = build_evaluation_prompt(base_state)

        assert "Consecutive rejections: 1" in result

    def test__no_progress_streak_shown(self, base_state):
        """No-progress streak should appear in both progress and anti-loop sections."""
        base_state["no_progress_streak"] = 3
        result = build_evaluation_prompt(base_state)

        assert "Iterations without new code: 3" in result
        # Anti-loop guidance should reference the streak
        assert "3 iteration(s)" in result

    def test__contains_decision_rules(self, base_state):
        """Prompt should include APPROVE and NEEDS_MORE_RESEARCH rules."""
        result = build_evaluation_prompt(base_state)

        assert "APPROVE" in result
        assert "NEEDS_MORE_RESEARCH" in result

    def test__contains_output_options(self, base_state):
        """Prompt should include the JSON output format examples."""
        result = build_evaluation_prompt(base_state)

        assert '"result": "APPROVED"' in result
        assert '"result": "NEEDS_MORE_RESEARCH"' in result

    def test__contains_anti_loop_guidance(self, base_state):
        """Prompt should include anti-loop guidance to prevent re-fetching existing code."""
        result = build_evaluation_prompt(base_state)

        assert "ANTI-LOOP" in result

    def test__contains_confidence_in_output_examples(self, base_state):
        """Output examples should include a confidence field."""
        result = build_evaluation_prompt(base_state)

        assert '"confidence"' in result

    def test__contains_verification_checklist(self, base_state):
        """Prompt should include the evaluation checklist steps."""
        result = build_evaluation_prompt(base_state)

        assert "EVALUATION CHECKLIST" in result
        assert "Evidence Validation" in result
        assert "Data Flow Traced" in result
        assert "Security Controls Examined" in result
        assert "Confidence Appropriate" in result

    def test__contains_evaluation_guidelines(self, base_state):
        """Prompt should include verdict-specific evaluation guidelines."""
        result = build_evaluation_prompt(base_state)

        assert "FALSE_POSITIVE" in result
        assert "TRUE_POSITIVE" in result
        assert "BOTH verdicts require equal scrutiny" in result

    def test__contains_required_information_filtering_rule(self, base_state):
        """Prompt should include the required_information filtering constraint."""
        result = build_evaluation_prompt(base_state)

        assert "mentioned in justifications" in result
        assert "NOT in gathered code" in result
        assert "critical to the verdict" in result

    def test__defaults_for_optional_justifications(self):
        """Missing justifications should default gracefully."""
        state = {
            "iteration": 1,
            "max_iterations": 3,
            "issue_description": "CWE-119 overflow",
            "gathered_code": "char buf[10];",
            "analysis": "Buffer too small.",
            "proposed_verdict": "TRUE_POSITIVE",
        }
        result = build_evaluation_prompt(state)

        assert "**JUSTIFICATIONS:**" in result
        assert "APPROVED" in result
