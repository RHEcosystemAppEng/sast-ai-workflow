"""
Test agent structured output with with_structured_output().

This test validates that the agent properly uses with_structured_output()
to get validated Pydantic models for reasoning state updates.
"""

from unittest.mock import AsyncMock, MagicMock

import pytest

from dto.Issue import Issue
from sast_agent_workflow.agent.agent_node import (
    _create_tool_call_from_reasoning,
    agent_decision_node,
)
from sast_agent_workflow.agent.agent_state import (
    AnalysisState,
    Claim,
    ErrorState,
    Evidence,
    InvestigationContext,
    ReasoningStateUpdate,
    SASTAgentState,
    Unknown,
    AgentMemory,
)


@pytest.fixture
def sample_state():
    """Create a sample agent state for testing."""
    issue = Issue(
        id="TEST-001",
        issue_type="USE_AFTER_FREE",
        trace="test trace",
    )

    return SASTAgentState(
        issue_id="TEST-001",
        issue=issue,
        context=InvestigationContext(),
        analysis=AnalysisState(),
        error_state=ErrorState(),
        memory=AgentMemory(),
    )


class TestStructuredOutputReasoning:
    """Test structured output with Pydantic validation."""

    def test_create_tool_call_from_reasoning_fetch_code(self):
        """Test tool call synthesis for fetch_code."""
        reasoning = ReasoningStateUpdate(
            analysis="Need to fetch implementation",
            claims=[],
            evidence=[],
            unknowns=[
                Unknown(
                    unknown_id="u1",
                    question="What does this function do?",
                    priority=100,
                    blocking=True,
                )
            ],
            next_tool="fetch_code",
            tool_reasoning="Need to understand the implementation",
            tool_parameters={
                "referring_source_code_path": "test.c",
                "expression_name": "test_func",
                "reason": "Need implementation details",
            },
        )

        tool_call = _create_tool_call_from_reasoning(reasoning, "TEST-001")

        assert tool_call["name"] == "fetch_code"
        assert "referring_source_code_path" in tool_call["args"]
        assert "expression_name" in tool_call["args"]
        assert tool_call["args"]["referring_source_code_path"] == "test.c"
        assert tool_call["args"]["expression_name"] == "test_func"

    def test_create_tool_call_from_reasoning_evaluator(self):
        """Test tool call synthesis for evaluator."""
        reasoning = ReasoningStateUpdate(
            analysis="Ready to verify",
            claims=[
                Claim(
                    claim_id="c1",
                    text="Vulnerability exists",
                    status="supported",
                    supporting_evidence_ids=["e1"],
                )
            ],
            evidence=[
                Evidence(
                    evidence_id="e1",
                    path_or_identifier="test.c",
                    excerpt="vulnerable code",
                    start_line=10,
                    end_line=10,
                    why_it_matters="Shows vulnerability",
                )
            ],
            unknowns=[],
            next_tool="evaluator",
            tool_reasoning="All evidence gathered",
            tool_parameters=None,  # Let it build from reasoning state
        )

        tool_call = _create_tool_call_from_reasoning(reasoning, "TEST-001")

        assert tool_call["name"] == "evaluator"
        assert "analysis" in tool_call["args"]
        assert "claims" in tool_call["args"]
        assert "evidence" in tool_call["args"]
        assert tool_call["args"]["analysis"] == "Ready to verify"
        assert tool_call["args"]["proposed_verdict"] == "TRUE_POSITIVE"  # Default


class TestAgentWithStructuredOutput:
    """Test agent with with_structured_output() mocking."""

    @pytest.mark.asyncio
    async def test_agent_with_valid_structured_output(self, sample_state):
        """Test agent when with_structured_output returns valid reasoning."""
        mock_llm = MagicMock()

        # Mock with_structured_output to return valid ReasoningStateUpdate
        mock_structured_llm = AsyncMock()
        reasoning = ReasoningStateUpdate(
            analysis="Vulnerability analysis",
            claims=[
                Claim(
                    claim_id="c1",
                    text="Test claim",
                    status="tentative",
                    supporting_evidence_ids=[],
                )
            ],
            evidence=[],
            unknowns=[
                Unknown(
                    unknown_id="u1",
                    question="Need more info",
                    priority=100,
                    blocking=True,
                )
            ],
            next_tool="fetch_code",
            tool_reasoning="Need more context",
            tool_parameters={
                "referring_source_code_path": "test.c",
                "expression_name": "test_func",
                "reason": "Need implementation",
            },
        )
        mock_structured_llm.ainvoke.return_value = reasoning
        mock_llm.with_structured_output.return_value = mock_structured_llm

        # Execute agent node
        result_state = await agent_decision_node(sample_state, mock_llm)

        # Verify reasoning state was updated
        assert len(result_state.analysis.claims) == 1
        assert result_state.analysis.claims[0].claim_id == "c1"
        assert len(result_state.analysis.unknowns) == 1

        # Verify tool call was synthesized
        assert len(result_state.memory.messages) >= 2
        last_message = result_state.memory.messages[-1]
        assert hasattr(last_message, "tool_calls")
        assert len(last_message.tool_calls) == 1
        assert last_message.tool_calls[0]["name"] == "fetch_code"

    @pytest.mark.asyncio
    async def test_agent_handles_structured_output_failure(self, sample_state):
        """Test agent when with_structured_output returns None."""
        mock_llm = MagicMock()

        # Mock with_structured_output to return None (parse failure)
        mock_structured_llm = AsyncMock()
        mock_structured_llm.ainvoke.return_value = None
        mock_llm.with_structured_output.return_value = mock_structured_llm

        # Execute agent node
        result_state = await agent_decision_node(sample_state, mock_llm)

        # Should have recorded an error
        assert result_state.error_state.last_error is not None
        assert result_state.error_state.last_error.tool_name == "agent_reasoning"
        assert result_state.error_state.error_recovery_attempts == 1


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
