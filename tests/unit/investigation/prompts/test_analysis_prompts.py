"""
Tests for investigation/prompts/analysis.py.

Covers: build_analysis_prompt for first analysis, reanalysis with evaluator
feedback, and state field interpolation.
"""

import pytest

from sast_agent_workflow.nodes.sub_agents.investigation.prompts.analysis import (
    build_analysis_prompt,
)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def base_state():
    """Minimal state dict required by build_analysis_prompt."""
    return {
        "issue_description": "CWE-476: Null pointer dereference in parse_input()",
        "gathered_code": "int *ptr = malloc(sizeof(int));\n*ptr = 42;",
        "analysis": None,
        "needs_reanalysis": False,
    }


# ---------------------------------------------------------------------------
# build_analysis_prompt
# ---------------------------------------------------------------------------


class TestBuildAnalysisPrompt:
    """Tests for build_analysis_prompt."""

    def test__contains_issue_description(self, base_state):
        """Prompt should include the SAST finding description."""
        result = build_analysis_prompt(base_state)

        assert "CWE-476" in result
        assert "parse_input()" in result

    def test__contains_gathered_code(self, base_state):
        """Prompt should include the gathered code section."""
        result = build_analysis_prompt(base_state)

        assert "malloc(sizeof(int))" in result

    def test__first_analysis_shows_no_previous(self, base_state):
        """First analysis (analysis=None) should say 'None - this is the first analysis.'."""
        result = build_analysis_prompt(base_state)

        assert "None - this is the first analysis." in result

    def test__includes_previous_analysis_when_present(self, base_state):
        """When prior analysis exists it should appear in the prompt."""
        base_state["analysis"] = "The pointer is checked before use."
        result = build_analysis_prompt(base_state)

        assert "The pointer is checked before use." in result

    def test__no_evaluator_feedback_on_first_pass(self, base_state):
        """First pass should NOT include the evaluator feedback section."""
        result = build_analysis_prompt(base_state)

        assert "EVALUATOR FEEDBACK" not in result

    def test__reanalysis_includes_evaluator_feedback(self, base_state):
        """Reanalysis with feedback should include the evaluator feedback block."""
        base_state["needs_reanalysis"] = True
        base_state["evaluation_feedback"] = "Missed guard at line 15."
        result = build_analysis_prompt(base_state)

        assert "EVALUATOR FEEDBACK" in result
        assert "Missed guard at line 15." in result

    def test__reanalysis_without_feedback_omits_section(self, base_state):
        """Reanalysis=True but no feedback should omit the feedback block."""
        base_state["needs_reanalysis"] = True
        base_state["evaluation_feedback"] = None
        result = build_analysis_prompt(base_state)

        assert "EVALUATOR FEEDBACK" not in result

    def test__contains_mandatory_analysis_steps(self, base_state):
        """Prompt should include the mandatory step-by-step instructions."""
        result = build_analysis_prompt(base_state)

        assert "Step 1: Identify Source and Sink" in result
        assert "Step 2: Trace ALL Code" in result
        assert "Step 3: Analyze Each Control Flow" in result
        assert "Step 4: Determine Reachability" in result

    def test__contains_required_output_section(self, base_state):
        """Prompt should end with the required output format."""
        result = build_analysis_prompt(base_state)

        assert "verdict" in result
        assert "confidence" in result
        assert "reasoning" in result
        assert "justifications" in result

    def test__defaults_gathered_code_when_missing(self):
        """When gathered_code is absent, should show '(No code gathered)'."""
        state = {
            "issue_description": "CWE-119 overflow",
            "analysis": None,
            "needs_reanalysis": False,
        }
        result = build_analysis_prompt(state)

        assert "(No code gathered)" in result
