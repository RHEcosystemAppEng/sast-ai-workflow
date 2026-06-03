"""Tests for research turn schema and wrap_model_call bridge."""

from langchain_core.messages import AIMessage

from sast_agent_workflow.nodes.sub_agents.investigation.nodes.research_middleware import (
    _bridge_structured_turn,
    _resolve_structured_turn,
    _turn_from_research_tool_call,
)
from sast_agent_workflow.nodes.sub_agents.investigation.nodes.research_response import (
    ResearchTurnResponse,
    parse_gather_tool_call_from_content,
    parse_turn_from_content,
    validate_gather_tool_arguments,
)


class TestParseTurnFromContent:
    def test__parses_research_complete(self):
        turn = parse_turn_from_content("Reasoning: Done.\n\nRESEARCH_COMPLETE")
        assert turn is not None
        assert turn.completed is True

    def test__does_not_parse_search_codebase_as_turn(self):
        turn = parse_turn_from_content(
            "Reasoning: Check vendor.\n\n"
            "search_codebase(pattern='GetClientCANames', file_pattern='*.go')"
        )
        assert turn is None


class TestParseGatherToolCallFromContent:
    def test__parses_search_codebase(self):
        call = parse_gather_tool_call_from_content(
            "Reasoning: Check vendor.\n\n"
            "search_codebase(pattern='GetClientCANames', file_pattern='*.go')"
        )
        assert call is not None
        assert call["name"] == "search_codebase"
        assert call["args"]["pattern"] == "GetClientCANames"


class TestBridgeStructuredTurn:
    def test__passes_through_native_gather_calls(self):
        turn = ResearchTurnResponse(reasoning="Need callers", completed=False)
        native = [
            {
                "name": "search_codebase",
                "args": {"pattern": "main", "file_pattern": "*.go"},
                "id": "x",
                "type": "tool_call",
            }
        ]
        response = _bridge_structured_turn(AIMessage(content=""), turn, native)
        assert response.structured_response == turn
        assert len(response.result[0].tool_calls) == 1
        assert response.result[0].tool_calls[0]["name"] == "search_codebase"

    def test__completed_keeps_structured_response(self):
        turn = ResearchTurnResponse(reasoning="Done", completed=True)
        response = _bridge_structured_turn(AIMessage(content=""), turn, [])
        assert response.structured_response == turn
        assert response.result[0].tool_calls == []

    def test__incomplete_without_gather_keeps_turn_for_termination(self):
        turn = ResearchTurnResponse(reasoning="Waiting", completed=False)
        response = _bridge_structured_turn(AIMessage(content=""), turn, [])
        assert response.structured_response == turn
        assert response.result[0].tool_calls == []


class TestResolveStructuredTurn:
    def test__reads_turn_from_research_tool_call_on_ai_message(self):
        ai = AIMessage(
            content="",
            tool_calls=[
                {
                    "name": "ResearchTurnResponse",
                    "args": {"reasoning": "Need file", "completed": False},
                    "id": "x",
                    "type": "tool_call",
                }
            ],
        )
        turn = _turn_from_research_tool_call(ai)
        assert turn is not None
        assert turn.completed is False

    def test__prefers_structured_response_in_state(self):
        state_turn = ResearchTurnResponse(reasoning="from state", completed=True)
        ai = AIMessage(
            content="",
            tool_calls=[
                {
                    "name": "ResearchTurnResponse",
                    "args": {"reasoning": "from message", "completed": False},
                    "id": "x",
                    "type": "tool_call",
                }
            ],
        )
        turn = _resolve_structured_turn({"structured_response": state_turn}, ai)
        assert turn is not None
        assert turn.completed is True


class TestValidateGatherToolArguments:
    def test__search_codebase(self):
        args = validate_gather_tool_arguments(
            "search_codebase", {"pattern": "x", "file_pattern": "*.py"}
        )
        assert args["pattern"] == "x"
