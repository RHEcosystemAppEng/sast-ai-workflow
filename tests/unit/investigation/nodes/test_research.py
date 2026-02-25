"""
Tests for research node handlers in investigation/nodes/research.py.

Covers: total_tool_calls accumulation across _handle_research_success,
_handle_research_recursion_limit, and _handle_research_error.
"""

from unittest.mock import MagicMock

from langchain_core.messages import AIMessage, ToolMessage

from sast_agent_workflow.nodes.sub_agents.investigation.nodes.research import (
    _handle_research_error,
    _handle_research_recursion_limit,
    _handle_research_success,
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_tool_message(tool_call_id: str = "tc_1") -> ToolMessage:
    return ToolMessage(content="result", tool_call_id=tool_call_id)


def _make_messages(*tool_call_ids: str) -> list:
    """Build a message list with an AIMessage followed by ToolMessages."""
    msgs = [AIMessage(content="thinking")]
    for tc_id in tool_call_ids:
        msgs.append(_make_tool_message(tc_id))
    return msgs


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
        "max_iterations": 4,
        "is_complete": False,
        "needs_reanalysis": False,
        "reanalysis_count": 0,
        "total_tool_calls": 0,
    }
    base.update(overrides)
    return base


def _mock_agent_with_preserved_state(fetched=None, history=None, messages=None):
    """Create a mock agent whose get_state returns specified values."""
    agent = MagicMock()
    preserved = MagicMock()
    preserved.values = {
        "fetched_files": fetched or {},
        "tool_call_history": history or [],
        "messages": messages or [],
    }
    agent.get_state.return_value = preserved
    return agent


# ---------------------------------------------------------------------------
# _handle_research_success — total_tool_calls
# ---------------------------------------------------------------------------


class TestHandleResearchSuccessTotalToolCalls:
    """Tests for total_tool_calls in _handle_research_success."""

    def test__counts_tool_calls_from_first_research(self):
        """First research with 3 tool calls should set total_tool_calls=3."""
        state = _make_state(total_tool_calls=0)
        result = {
            "messages": _make_messages("tc_1", "tc_2", "tc_3"),
            "fetched_files": {},
            "tool_call_history": [],
        }

        out = _handle_research_success(state, result)

        assert out["total_tool_calls"] == 3

    def test__accumulates_across_iterations(self):
        """Second research adding 4 calls to existing 3 should yield total=7."""
        state = _make_state(total_tool_calls=3)
        result = {
            "messages": _make_messages("tc_4", "tc_5", "tc_6", "tc_7"),
            "fetched_files": {},
            "tool_call_history": [],
        }

        out = _handle_research_success(state, result)

        assert out["total_tool_calls"] == 7

    def test__zero_new_calls(self):
        """Research with no tool calls should not change total."""
        state = _make_state(total_tool_calls=2)
        result = {
            "messages": [AIMessage(content="no tools needed")],
            "fetched_files": {},
            "tool_call_history": [],
        }

        out = _handle_research_success(state, result)

        assert out["total_tool_calls"] == 2

    def test__defaults_to_zero_when_missing_from_state(self):
        """Should treat missing total_tool_calls as 0."""
        state = _make_state()
        del state["total_tool_calls"]
        result = {
            "messages": _make_messages("tc_1", "tc_2"),
            "fetched_files": {},
            "tool_call_history": [],
        }

        out = _handle_research_success(state, result)

        assert out["total_tool_calls"] == 2


# ---------------------------------------------------------------------------
# _handle_research_recursion_limit — total_tool_calls
# ---------------------------------------------------------------------------


class TestHandleResearchRecursionLimitTotalToolCalls:
    """Tests for total_tool_calls in _handle_research_recursion_limit."""

    def test__accumulates_from_checkpointer_state(self):
        """Should count ToolMessages recovered via checkpointer."""
        state = _make_state(total_tool_calls=1)
        agent = _mock_agent_with_preserved_state(
            messages=_make_messages("tc_1", "tc_2", "tc_3"),
        )

        out = _handle_research_recursion_limit(state, agent, "thread-1")

        assert out["total_tool_calls"] == 4

    def test__no_new_calls_from_checkpointer(self):
        """When checkpointer has no ToolMessages, total should not change."""
        state = _make_state(total_tool_calls=2)
        agent = _mock_agent_with_preserved_state(
            messages=[AIMessage(content="no tools")],
        )

        out = _handle_research_recursion_limit(state, agent, "thread-1")

        assert out["total_tool_calls"] == 2


# ---------------------------------------------------------------------------
# _handle_research_error — total_tool_calls
# ---------------------------------------------------------------------------


class TestHandleResearchErrorTotalToolCalls:
    """Tests for total_tool_calls in _handle_research_error."""

    def test__accumulates_from_checkpointer_on_error(self):
        """Should count ToolMessages recovered via checkpointer on error."""
        state = _make_state(total_tool_calls=1)
        agent = _mock_agent_with_preserved_state(
            messages=_make_messages("tc_1", "tc_2", "tc_3", "tc_4"),
        )

        out = _handle_research_error(state, agent, "thread-1", RuntimeError("boom"))

        assert out["total_tool_calls"] == 5

    def test__preserves_total_when_no_new_calls_on_error(self):
        """When checkpointer has no ToolMessages on error, total unchanged."""
        state = _make_state(total_tool_calls=1)
        agent = _mock_agent_with_preserved_state(messages=[])

        out = _handle_research_error(state, agent, "thread-1", RuntimeError("boom"))

        assert out["total_tool_calls"] == 1
