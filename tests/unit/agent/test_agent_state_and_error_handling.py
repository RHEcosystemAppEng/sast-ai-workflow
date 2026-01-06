"""
Unit tests for agent state management and error handling.

Tests cover:
1. Circuit breaker node (pure routing, state updates in dedicated node)
2. found_symbols as List type (JSON serialization, deduplication)
3. ToolError Pydantic model (proper type for last_error)
4. Error recording in state updaters (parse errors captured to state)
5. Consistent error reset across all state updaters
6. Lazy loading of agent prompts (lru_cache)
7. Duplicate call detection (circuit breaker for repeated tool calls)
"""

import json
from datetime import datetime

import pytest

from dto.Issue import Issue
from sast_agent_workflow.agent.agent_state import (
    AgentMemory,
    AnalysisState,
    ErrorState,
    InvestigationContext,
    SASTAgentState,
    ToolError,
)


@pytest.fixture
def base_state():
    """Create a base SASTAgentState for testing."""
    return SASTAgentState(
        issue_id="test-001",
        issue=Issue(
            id="issue-001",
            issue_type="SQL Injection",
            trace="user_input -> query -> execute",
            file_path="app/views.py",
            line_number=42,
        ),
        context=InvestigationContext(),
        analysis=AnalysisState(),
        error_state=ErrorState(),
        memory=AgentMemory(),
    )


# =============================================================================
# Issue #1 & #2: Circuit Breaker Node (Pure Routing)
# =============================================================================


class TestCircuitBreakerNode:
    """Tests for circuit_breaker_node and pure routing functions."""

    @pytest.mark.asyncio
    async def test_circuit_breaker_node_sets_is_final(self, base_state):
        """Test that circuit_breaker_node sets is_final=True."""
        from sast_agent_workflow.agent.agent_graph import circuit_breaker_node

        assert base_state.is_final is False

        result = await circuit_breaker_node(base_state)

        assert result.is_final is True

    @pytest.mark.asyncio
    async def test_circuit_breaker_node_sets_default_verdict(self, base_state):
        """Test that circuit_breaker_node sets NEEDS_HUMAN_REVIEW when no verdict exists."""
        from sast_agent_workflow.agent.agent_graph import circuit_breaker_node

        assert base_state.analysis.verdict is None

        result = await circuit_breaker_node(base_state)

        assert result.analysis.verdict == "NEEDS_HUMAN_REVIEW"

    @pytest.mark.asyncio
    async def test_circuit_breaker_node_preserves_existing_verdict(self, base_state):
        """Test that circuit_breaker_node preserves existing verdict if set."""
        from sast_agent_workflow.agent.agent_graph import circuit_breaker_node

        base_state.analysis.verdict = "TRUE_POSITIVE"

        result = await circuit_breaker_node(base_state)

        assert result.analysis.verdict == "TRUE_POSITIVE"

    def test_route_after_evaluation_is_pure_max_iterations(self, base_state):
        """Test that route_after_evaluation doesn't mutate state on max iterations."""
        from sast_agent_workflow.agent.agent_graph import route_after_evaluation

        base_state.iteration_count = 15

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        # Should return circuit_breaker, NOT mutate state
        assert result == "circuit_breaker"
        assert base_state.is_final is False  # State NOT mutated

    def test_route_after_evaluation_is_pure_duplicate_calls(self, base_state):
        """Test that route_after_evaluation doesn't mutate state on duplicate calls."""
        from sast_agent_workflow.agent.agent_graph import route_after_evaluation

        # Set up duplicate call history
        base_state.memory.tool_call_history = [
            ("fetch_code", {"identifier": "app.py"}),
            ("fetch_code", {"identifier": "app.py"}),
        ]

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        assert result == "circuit_breaker"
        assert base_state.is_final is False  # State NOT mutated

    def test_route_after_evaluation_is_pure_error_recovery(self, base_state):
        """Test that route_after_evaluation doesn't mutate state on error limit."""
        from sast_agent_workflow.agent.agent_graph import route_after_evaluation

        base_state.error_state.error_recovery_attempts = 3

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        assert result == "circuit_breaker"
        assert base_state.is_final is False  # State NOT mutated

    def test_route_after_evaluation_returns_end_when_final(self, base_state):
        """Test that route_after_evaluation returns END when is_final=True."""
        from langgraph.graph import END

        from sast_agent_workflow.agent.agent_graph import route_after_evaluation

        base_state.is_final = True

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        assert result == END

    def test_route_after_evaluation_returns_agent_when_continuing(self, base_state):
        """Test that route_after_evaluation returns 'agent' when investigation continues."""
        from sast_agent_workflow.agent.agent_graph import route_after_evaluation

        base_state.is_final = False
        base_state.iteration_count = 5

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        assert result == "agent"


# =============================================================================
# Issue #3: Set to List Change for found_symbols
# =============================================================================


class TestFoundSymbolsListType:
    """Tests for found_symbols being a List instead of Set."""

    def test_found_symbols_is_list_type(self):
        """Test that found_symbols is a List, not a Set."""
        context = InvestigationContext()

        # Should be a list for JSON serialization
        assert isinstance(context.found_symbols, list)

    def test_found_symbols_deduplication_in_fetch_code(self, base_state):
        """Test that FetchCodeStateUpdater deduplicates found_symbols."""
        from sast_agent_workflow.agent.tools.state_updaters import FetchCodeStateUpdater

        updater = FetchCodeStateUpdater()

        # Add same identifier twice
        updater.update_state(base_state, {"identifier": "app.py"}, "code1")
        updater.update_state(base_state, {"identifier": "app.py"}, "code2")

        # Should only appear once
        assert base_state.context.found_symbols.count("app.py") == 1

    def test_found_symbols_deduplication_in_search_codebase(self, base_state):
        """Test that SearchCodebaseStateUpdater deduplicates found_symbols."""
        from sast_agent_workflow.agent.tools.state_updaters import (
            SearchCodebaseStateUpdater,
        )

        updater = SearchCodebaseStateUpdater()

        # Add file via search twice
        result1 = json.dumps({"app/utils.py": ["snippet1"]})
        result2 = json.dumps({"app/utils.py": ["snippet2"]})

        updater.update_state(base_state, {}, result1)
        updater.update_state(base_state, {}, result2)

        # Should only appear once in found_symbols
        assert base_state.context.found_symbols.count("app/utils.py") == 1

    def test_found_symbols_is_json_serializable(self, base_state):
        """Test that found_symbols can be serialized to JSON."""
        base_state.context.found_symbols.append("app.py")
        base_state.context.found_symbols.append("utils.py")

        # This should not raise
        json_str = json.dumps({"found_symbols": base_state.context.found_symbols})
        parsed = json.loads(json_str)

        assert parsed["found_symbols"] == ["app.py", "utils.py"]


# =============================================================================
# Issue #4: ToolError Type for last_error
# =============================================================================


class TestToolErrorType:
    """Tests for ToolError being a proper Pydantic model."""

    def test_tool_error_is_pydantic_model(self):
        """Test that ToolError is a Pydantic BaseModel."""
        error = ToolError(
            tool_name="fetch_code",
            error_message="File not found",
            attempted_args={"identifier": "missing.py"},
            timestamp=datetime.now().isoformat(),
        )

        # Should have Pydantic attributes
        assert hasattr(error, "tool_name")
        assert hasattr(error, "error_message")
        assert hasattr(error, "attempted_args")
        assert hasattr(error, "timestamp")

    def test_tool_error_attributes_accessible(self):
        """Test that ToolError attributes are accessible via dot notation."""
        error = ToolError(
            tool_name="analyze_issue",
            error_message="Parse error",
            attempted_args={"key": "value"},
            timestamp="2024-01-01T00:00:00",
        )

        # These should work (unlike dict which uses ['key'])
        assert error.tool_name == "analyze_issue"
        assert error.error_message == "Parse error"
        assert error.attempted_args == {"key": "value"}
        assert error.timestamp == "2024-01-01T00:00:00"

    def test_error_state_last_error_accepts_tool_error(self, base_state):
        """Test that ErrorState.last_error accepts ToolError model."""
        error = ToolError(
            tool_name="test",
            error_message="test error",
            attempted_args={},
            timestamp=datetime.now().isoformat(),
        )

        base_state.error_state.last_error = error

        assert base_state.error_state.last_error.tool_name == "test"


# =============================================================================
# Issue #7: Error Recording in State Updaters
# =============================================================================


class TestErrorRecordingInStateUpdaters:
    """Tests for parse errors being recorded to state."""

    def test_analyze_issue_records_parse_error(self, base_state):
        """Test that AnalyzeIssueStateUpdater records parse errors to state."""
        from sast_agent_workflow.agent.tools.state_updaters import (
            AnalyzeIssueStateUpdater,
        )

        updater = AnalyzeIssueStateUpdater()
        tool_args = {"issue_trace": "test", "fetched_code": "test"}
        invalid_json = "not valid json {"

        updater.update_state(base_state, tool_args, invalid_json)

        # Error should be recorded
        assert len(base_state.error_state.errors) == 1
        assert base_state.error_state.last_error is not None
        assert base_state.error_state.last_error.tool_name == "analyze_issue"
        assert "Failed to parse result" in base_state.error_state.last_error.error_message
        assert base_state.error_state.error_recovery_attempts == 1

    def test_comprehensive_evaluation_records_parse_error(self, base_state):
        """Test that ComprehensiveEvaluationStateUpdater records parse errors to state."""
        from sast_agent_workflow.agent.tools.state_updaters import (
            ComprehensiveEvaluationStateUpdater,
        )

        updater = ComprehensiveEvaluationStateUpdater()
        tool_args = {"iteration_count": 1}
        invalid_json = "{{invalid}}"

        updater.update_state(base_state, tool_args, invalid_json)

        assert len(base_state.error_state.errors) == 1
        assert base_state.error_state.last_error.tool_name == "comprehensive_evaluation"
        assert base_state.error_state.error_recovery_attempts == 1

    def test_search_codebase_records_parse_error(self, base_state):
        """Test that SearchCodebaseStateUpdater records parse errors to state."""
        from sast_agent_workflow.agent.tools.state_updaters import (
            SearchCodebaseStateUpdater,
        )

        updater = SearchCodebaseStateUpdater()
        invalid_json = "not json"

        updater.update_state(base_state, {}, invalid_json)

        assert len(base_state.error_state.errors) == 1
        assert base_state.error_state.last_error.tool_name == "search_codebase"

    def test_list_files_records_parse_error(self, base_state):
        """Test that ListFilesStateUpdater records parse errors to state."""
        from sast_agent_workflow.agent.tools.state_updaters import ListFilesStateUpdater

        updater = ListFilesStateUpdater()
        invalid_json = "{broken"

        updater.update_state(base_state, {}, invalid_json)

        assert len(base_state.error_state.errors) == 1
        assert base_state.error_state.last_error.tool_name == "list_files"


# =============================================================================
# Issue #8: Consistent Error Reset Across All State Updaters
# =============================================================================


class TestConsistentErrorReset:
    """Tests for all state updaters resetting error state on success."""

    def test_fetch_code_resets_error_state(self, base_state):
        """Test that FetchCodeStateUpdater resets error state on success."""
        from sast_agent_workflow.agent.tools.state_updaters import FetchCodeStateUpdater

        base_state.error_state.error_recovery_attempts = 2
        base_state.error_state.last_error = ToolError(
            tool_name="previous",
            error_message="old error",
            attempted_args={},
            timestamp="old",
        )

        updater = FetchCodeStateUpdater()
        updater.update_state(base_state, {"identifier": "app.py"}, "code")

        assert base_state.error_state.error_recovery_attempts == 0
        assert base_state.error_state.last_error is None

    def test_analyze_issue_resets_error_state(self, base_state):
        """Test that AnalyzeIssueStateUpdater resets error state on success."""
        from sast_agent_workflow.agent.tools.state_updaters import (
            AnalyzeIssueStateUpdater,
        )

        base_state.error_state.error_recovery_attempts = 2
        base_state.error_state.last_error = ToolError(
            tool_name="previous",
            error_message="old error",
            attempted_args={},
            timestamp="old",
        )

        updater = AnalyzeIssueStateUpdater()
        valid_result = json.dumps(
            {
                "investigation_result": "FALSE_POSITIVE",
                "is_final": "FALSE",
                "justifications": ["test"],
                "recommendations": [],
            }
        )
        updater.update_state(base_state, {}, valid_result)

        assert base_state.error_state.error_recovery_attempts == 0
        assert base_state.error_state.last_error is None

    def test_comprehensive_evaluation_resets_error_state(self, base_state):
        """Test that ComprehensiveEvaluationStateUpdater resets error state on success."""
        from sast_agent_workflow.agent.tools.state_updaters import (
            ComprehensiveEvaluationStateUpdater,
        )

        base_state.error_state.error_recovery_attempts = 2
        base_state.error_state.last_error = ToolError(
            tool_name="previous",
            error_message="old error",
            attempted_args={},
            timestamp="old",
        )

        updater = ComprehensiveEvaluationStateUpdater()
        valid_result = json.dumps(
            {
                "is_final": "FALSE",
                "verdict_confidence": "low",
                "exploration_gaps": [],
                "has_exploration_gaps": False,
                "logic_gaps": [],
                "required_code": [],
                "recommendations": [],
                "justifications": [],
            }
        )
        updater.update_state(base_state, {}, valid_result)

        assert base_state.error_state.error_recovery_attempts == 0
        assert base_state.error_state.last_error is None

    def test_search_codebase_resets_error_state(self, base_state):
        """Test that SearchCodebaseStateUpdater resets error state on success."""
        from sast_agent_workflow.agent.tools.state_updaters import (
            SearchCodebaseStateUpdater,
        )

        base_state.error_state.error_recovery_attempts = 2
        base_state.error_state.last_error = ToolError(
            tool_name="previous",
            error_message="old error",
            attempted_args={},
            timestamp="old",
        )

        updater = SearchCodebaseStateUpdater()
        valid_result = json.dumps({"app/utils.py": ["snippet"]})
        updater.update_state(base_state, {}, valid_result)

        assert base_state.error_state.error_recovery_attempts == 0
        assert base_state.error_state.last_error is None

    def test_list_files_resets_error_state(self, base_state):
        """Test that ListFilesStateUpdater resets error state on success."""
        from sast_agent_workflow.agent.tools.state_updaters import ListFilesStateUpdater

        base_state.error_state.error_recovery_attempts = 2
        base_state.error_state.last_error = ToolError(
            tool_name="previous",
            error_message="old error",
            attempted_args={},
            timestamp="old",
        )

        updater = ListFilesStateUpdater()
        valid_result = json.dumps({"files": ["file1.py", "file2.py"]})
        updater.update_state(base_state, {}, valid_result)

        assert base_state.error_state.error_recovery_attempts == 0
        assert base_state.error_state.last_error is None


# =============================================================================
# Issue #9: Lazy Loading of Agent Prompts
# =============================================================================


class TestLazyLoadingPrompts:
    """Tests for lazy loading of agent prompts with lru_cache."""

    def test_get_agent_prompts_returns_tuple(self):
        """Test that get_agent_prompts returns (system_prompt, formatting_templates)."""
        from sast_agent_workflow.agent.agent_node import get_agent_prompts

        result = get_agent_prompts()

        assert isinstance(result, tuple)
        assert len(result) == 2
        system_prompt, formatting_templates = result
        assert isinstance(system_prompt, str)
        assert isinstance(formatting_templates, dict)

    def test_get_agent_prompts_is_cached(self):
        """Test that get_agent_prompts returns cached result on second call."""
        from sast_agent_workflow.agent.agent_node import get_agent_prompts

        # Clear cache first
        get_agent_prompts.cache_clear()

        result1 = get_agent_prompts()
        result2 = get_agent_prompts()

        # Should be the exact same object (cached)
        assert result1 is result2

    def test_get_agent_prompts_cache_can_be_cleared(self):
        """Test that cache can be cleared for testing purposes."""
        from sast_agent_workflow.agent.agent_node import get_agent_prompts

        # This should not raise
        get_agent_prompts.cache_clear()

        # Should still work after clearing
        result = get_agent_prompts()
        assert result is not None

    def test_formatting_templates_contains_required_keys(self):
        """Test that formatting templates contain all required keys."""
        from sast_agent_workflow.agent.agent_node import get_agent_prompts

        _, templates = get_agent_prompts()

        required_keys = [
            "project_context_template",
            "project_context_no_context",
            "error_context_template",
            "error_context_no_errors",
            "evaluation_feedback_template",
            "evaluation_feedback_no_evaluation",
            "exploration_gaps_complete",
            "logic_gaps_complete",
            "state_summary_template",
        ]

        for key in required_keys:
            assert key in templates, f"Missing required template key: {key}"

    def test_system_prompt_contains_placeholders(self):
        """Test that system prompt contains required placeholders."""
        from sast_agent_workflow.agent.agent_node import get_agent_prompts

        system_prompt, _ = get_agent_prompts()

        required_placeholders = [
            "{project_context}",
            "{state_summary}",
            "{evaluation_feedback}",
            "{error_context}",
        ]

        for placeholder in required_placeholders:
            assert placeholder in system_prompt, f"Missing placeholder: {placeholder}"


# =============================================================================
# Integration: Duplicate Detection with is_duplicate_call
# =============================================================================


class TestDuplicateCallDetection:
    """Tests for is_duplicate_call function."""

    def test_detects_duplicate_with_same_args(self, base_state):
        """Test that duplicate calls with same args are detected."""
        from sast_agent_workflow.agent.agent_graph import is_duplicate_call

        base_state.memory.tool_call_history = [
            ("fetch_code", {"identifier": "app.py", "reason": "test"}),
            ("fetch_code", {"identifier": "app.py", "reason": "test"}),
        ]

        assert is_duplicate_call(base_state) is True

    def test_no_duplicate_with_different_args(self, base_state):
        """Test that calls with different args are not duplicates."""
        from sast_agent_workflow.agent.agent_graph import is_duplicate_call

        base_state.memory.tool_call_history = [
            ("fetch_code", {"identifier": "app.py"}),
            ("fetch_code", {"identifier": "utils.py"}),
        ]

        assert is_duplicate_call(base_state) is False

    def test_no_duplicate_with_different_tools(self, base_state):
        """Test that different tools are not duplicates."""
        from sast_agent_workflow.agent.agent_graph import is_duplicate_call

        base_state.memory.tool_call_history = [
            ("fetch_code", {"identifier": "app.py"}),
            ("analyze_issue", {"identifier": "app.py"}),
        ]

        assert is_duplicate_call(base_state) is False

    def test_no_duplicate_with_single_call(self, base_state):
        """Test that single call is not a duplicate."""
        from sast_agent_workflow.agent.agent_graph import is_duplicate_call

        base_state.memory.tool_call_history = [
            ("fetch_code", {"identifier": "app.py"}),
        ]

        assert is_duplicate_call(base_state) is False

    def test_no_duplicate_with_empty_history(self, base_state):
        """Test that empty history has no duplicates."""
        from sast_agent_workflow.agent.agent_graph import is_duplicate_call

        base_state.memory.tool_call_history = []

        assert is_duplicate_call(base_state) is False
