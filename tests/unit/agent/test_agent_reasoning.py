"""
Test agent reasoning functionality (ADR-0002 alignment).

This test verifies that the agent properly analyzes code and maintains
Claims, Evidence, and Unknowns state as required by the ADR.
"""

from unittest.mock import AsyncMock, MagicMock

import pytest

from dto.Issue import Issue
from sast_agent_workflow.agent.agent_node import (
    agent_decision_node,
    format_all_fetched_code,
    format_claims_for_prompt,
    format_evidence_for_prompt,
    format_unknowns_for_prompt,
)
from sast_agent_workflow.agent.agent_state import (
    AnalysisState,
    Claim,
    ErrorState,
    Evidence,
    InvestigationContext,
    SASTAgentState,
    Unknown,
)


@pytest.fixture
def mock_issue():
    """Create a mock SAST issue for testing."""
    return Issue(
        id="test-001",
        issue_type="SQL Injection",
        trace="user_input -> query",
    )


@pytest.fixture
def sample_state(mock_issue):
    """Create a sample agent state for testing."""
    return SASTAgentState(
        issue_id="test-001",
        issue=mock_issue,
        context=InvestigationContext(
            fetched_files={
                "app.py": [
                    "def vulnerable_func():\n    query = f'SELECT * FROM users WHERE id={user_id}'"
                ]
            },
            found_symbols=["app.py"],
        ),
        analysis=AnalysisState(
            claims=[
                Claim(
                    claim_id="c1",
                    text="User input flows to SQL query without sanitization",
                    status="tentative",
                    supporting_evidence_ids=["e1"],
                )
            ],
            evidence=[
                Evidence(
                    evidence_id="e1",
                    path_or_identifier="app.py",
                    excerpt="query = f'SELECT * FROM users WHERE id={user_id}'",
                    start_line=42,
                    end_line=42,
                    why_it_matters="Shows string interpolation of user input",
                )
            ],
            unknowns=[
                Unknown(
                    unknown_id="u1",
                    question="Is there middleware that sanitizes user_id?",
                    priority=100,
                    blocking=True,
                )
            ],
        ),
        error_state=ErrorState(),
    )


def test_format_all_fetched_code_empty():
    """Test formatting when no code is fetched."""
    result = format_all_fetched_code({})
    assert "No code fetched yet" in result


def test_format_all_fetched_code_with_content():
    """Test formatting fetched code."""
    fetched = {"app.py": ["def test():\n    pass"]}
    result = format_all_fetched_code(fetched)
    assert "=== app.py ===" in result
    assert "def test():" in result


def test_format_all_fetched_code_truncation():
    """Test that very large code is truncated."""
    large_code = "x" * 20000
    fetched = {"large.py": [large_code]}
    result = format_all_fetched_code(fetched, max_chars=15000)
    assert "truncated" in result
    assert len(result) < 20000


def test_format_claims_for_prompt_empty():
    """Test formatting when no claims exist."""
    result = format_claims_for_prompt([])
    assert "None yet" in result
    assert "create claims" in result


def test_format_claims_for_prompt_with_claims():
    """Test formatting existing claims."""
    claims = [
        Claim(
            claim_id="c1",
            text="Test claim",
            status="supported",
            supporting_evidence_ids=["e1"],
        )
    ]
    result = format_claims_for_prompt(claims)
    assert "[c1]" in result
    assert "Test claim" in result
    assert "✓" in result  # supported marker


def test_format_evidence_for_prompt_empty():
    """Test formatting when no evidence exists."""
    result = format_evidence_for_prompt([])
    assert "None yet" in result
    assert "Extract evidence" in result


def test_format_evidence_for_prompt_with_evidence():
    """Test formatting existing evidence."""
    evidence = [
        Evidence(
            evidence_id="e1",
            path_or_identifier="app.py",
            excerpt="test code",
            start_line=10,
            end_line=12,
            why_it_matters="test reason",
        )
    ]
    result = format_evidence_for_prompt(evidence)
    assert "[e1]" in result
    assert "app.py:10-12" in result
    assert "test code" in result
    assert "test reason" in result


def test_format_unknowns_for_prompt_empty():
    """Test formatting when no unknowns exist."""
    result = format_unknowns_for_prompt([])
    assert "None!" in result
    assert "evaluator" in result


def test_format_unknowns_for_prompt_with_blocking():
    """Test formatting blocking unknowns."""
    unknowns = [
        Unknown(
            unknown_id="u1",
            question="Is there sanitization?",
            priority=100,
            blocking=True,
        )
    ]
    result = format_unknowns_for_prompt(unknowns)
    assert "BLOCKING" in result
    assert "[u1]" in result
    assert "(P100)" in result
    assert "Is there sanitization?" in result


@pytest.mark.asyncio
async def test_agent_decision_node_enforces_reasoning(sample_state):
    """Test that agent decision node enforces structured output."""
    # Mock LLM that fails to produce structured output (returns None)
    mock_llm = MagicMock()
    mock_structured_llm = AsyncMock()
    mock_structured_llm.ainvoke.return_value = (
        None  # with_structured_output returns None on failure
    )
    mock_llm.with_structured_output.return_value = mock_structured_llm

    # Call agent node
    result_state = await agent_decision_node(sample_state, mock_llm)

    # Should have recorded an error
    assert result_state.error_state.last_error is not None
    assert result_state.error_state.last_error.tool_name == "agent_reasoning"
    assert "structured output" in result_state.error_state.last_error.error_message.lower()
    assert result_state.error_state.error_recovery_attempts == 1


@pytest.mark.asyncio
async def test_agent_decision_node_accepts_valid_reasoning(sample_state):
    """Test that agent decision node accepts valid structured output."""
    # Mock LLM that produces valid ReasoningStateUpdate
    from sast_agent_workflow.agent.agent_state import ReasoningStateUpdate

    mock_llm = MagicMock()
    mock_structured_llm = AsyncMock()

    reasoning = ReasoningStateUpdate(
        analysis="SQL injection vulnerability exists due to unsanitized user input",
        claims=[
            Claim(
                claim_id="c1",
                text="SQL injection vulnerability exists",
                status="supported",
                supporting_evidence_ids=["e1"],
            )
        ],
        evidence=[
            Evidence(
                evidence_id="e1",
                path_or_identifier="app.py",
                excerpt="query = f'SELECT * FROM users WHERE id={user_id}'",
                start_line=42,
                end_line=42,
                why_it_matters="Shows SQL injection via string interpolation",
            )
        ],
        unknowns=[],
        next_tool="evaluator",
        tool_reasoning="All evidence gathered, ready to verify",
        tool_parameters={"proposed_verdict": "TRUE_POSITIVE"},
    )

    mock_structured_llm.ainvoke.return_value = reasoning
    mock_llm.with_structured_output.return_value = mock_structured_llm

    # Call agent node
    result_state = await agent_decision_node(sample_state, mock_llm)

    # Should have updated reasoning state without errors
    assert result_state.error_state.error_recovery_attempts == 0
    assert result_state.error_state.last_error is None
    assert len(result_state.analysis.claims) == 1
    assert result_state.analysis.claims[0].claim_id == "c1"
    assert len(result_state.analysis.evidence) == 1
    assert len(result_state.analysis.unknowns) == 0


@pytest.mark.asyncio
async def test_agent_decision_node_handles_exception(sample_state):
    """Test that agent handles exceptions gracefully."""
    # Mock LLM that raises an exception
    mock_llm = MagicMock()
    mock_structured_llm = AsyncMock()
    mock_structured_llm.ainvoke.side_effect = ValueError("Invalid model response")
    mock_llm.with_structured_output.return_value = mock_structured_llm

    # Call agent node - exceptions in structured output are caught and become None
    result_state = await agent_decision_node(sample_state, mock_llm)

    # Should have recorded the error (structured output returned None after retries)
    assert result_state.error_state.last_error is not None
    assert result_state.error_state.last_error.tool_name == "agent_reasoning"
    assert result_state.error_state.error_recovery_attempts == 1


def test_formatting_shows_code_to_agent(sample_state):
    """Integration test: Verify agent sees fetched code."""
    # Format all context
    fetched_code = format_all_fetched_code(sample_state.context.fetched_files)
    formatted_claims = format_claims_for_prompt(sample_state.analysis.claims)
    formatted_evidence = format_evidence_for_prompt(sample_state.analysis.evidence)
    formatted_unknowns = format_unknowns_for_prompt(sample_state.analysis.unknowns)

    # Verify code is included
    assert "app.py" in fetched_code
    assert "vulnerable_func" in fetched_code

    # Verify reasoning state is included
    assert "c1" in formatted_claims
    assert "User input flows" in formatted_claims
    assert "e1" in formatted_evidence
    assert "u1" in formatted_unknowns
    assert "BLOCKING" in formatted_unknowns


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
