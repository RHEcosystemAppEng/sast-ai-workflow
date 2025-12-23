"""
Unit tests for tool state updaters.

These tests verify that each state updater correctly updates SASTAgentState
based on tool results, without needing to invoke actual tools or the LLM.
"""

import json

import pytest

from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from sast_agent_workflow.agent.agent_state import (
    AgentMemory,
    AnalysisState,
    ErrorState,
    InvestigationContext,
    SASTAgentState,
)
from sast_agent_workflow.agent.tools.state_updaters import (
    AnalyzeIssueStateUpdater,
    ComprehensiveEvaluationStateUpdater,
    FetchCodeStateUpdater,
    ListFilesStateUpdater,
    SearchCodebaseStateUpdater,
    get_state_updater,
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


class TestFetchCodeStateUpdater:
    """Tests for FetchCodeStateUpdater."""

    def test_updates_fetched_files(self, base_state):
        """Test that fetched code is stored in context."""
        updater = FetchCodeStateUpdater()
        tool_args = {"identifier": "app/models.py", "reason": "check sanitization"}
        result = "=== app/models.py ===\ndef sanitize(data):\n    return data.strip()"

        updater.update_state(base_state, tool_args, result)

        assert "app/models.py" in base_state.context.fetched_files
        assert base_state.context.fetched_files["app/models.py"] == [result]

    def test_adds_to_found_symbols(self, base_state):
        """Test that identifier is added to found_symbols."""
        updater = FetchCodeStateUpdater()
        tool_args = {"identifier": "sanitize_input", "reason": "check function"}
        result = "def sanitize_input(data): ..."

        updater.update_state(base_state, tool_args, result)

        assert "sanitize_input" in base_state.context.found_symbols

    def test_resets_error_state_on_success(self, base_state):
        """Test that error recovery counter is reset on successful fetch."""
        # Set up state with previous errors
        base_state.error_state.error_recovery_attempts = 2
        base_state.error_state.last_error = {"tool": "fetch_code", "error": "previous error"}

        updater = FetchCodeStateUpdater()
        tool_args = {"identifier": "app.py", "reason": "test"}
        result = "code here"

        updater.update_state(base_state, tool_args, result)

        assert base_state.error_state.error_recovery_attempts == 0
        assert base_state.error_state.last_error is None


class TestAnalyzeIssueStateUpdater:
    """Tests for AnalyzeIssueStateUpdater."""

    def test_parses_and_stores_analysis_result(self, base_state):
        """Test that JSON result is parsed and stored as AnalysisResponse."""
        updater = AnalyzeIssueStateUpdater()
        tool_args = {"issue_trace": "...", "fetched_code": "..."}
        result = json.dumps(
            {
                "investigation_result": "FALSE_POSITIVE",
                "is_final": "FALSE",
                "justifications": ["Input is sanitized", "No SQL execution path"],
                "recommendations": ["No fix needed"],
            }
        )

        updater.update_state(base_state, tool_args, result)

        assert base_state.analysis.investigation_result is not None
        assert base_state.analysis.investigation_result.investigation_result == "FALSE_POSITIVE"
        assert len(base_state.analysis.investigation_result.justifications) == 2

    def test_handles_invalid_json_gracefully(self, base_state, caplog):
        """Test that invalid JSON doesn't crash, just logs error."""
        updater = AnalyzeIssueStateUpdater()
        tool_args = {}
        result = "invalid json {"

        updater.update_state(base_state, tool_args, result)

        # Should log error but not crash
        assert "Failed to parse analysis result" in caplog.text
        assert base_state.analysis.investigation_result is None


class TestComprehensiveEvaluationStateUpdater:
    """Tests for ComprehensiveEvaluationStateUpdater."""

    def test_parses_and_stores_evaluation_result(self, base_state):
        """Test that evaluation result is parsed and stored."""
        updater = ComprehensiveEvaluationStateUpdater()
        tool_args = {"iteration_count": 3, "analysis_verdict": "TRUE_POSITIVE"}
        result = json.dumps(
            {
                "is_final": "TRUE",
                "verdict_confidence": "high",
                "exploration_gaps": [],
                "has_exploration_gaps": False,
                "logic_gaps": [],
                "required_code": [],
                "recommendations": ["Investigation complete"],
                "justifications": ["All paths checked"],
            }
        )

        updater.update_state(base_state, tool_args, result)

        assert base_state.analysis.evaluation_result is not None
        assert base_state.analysis.evaluation_result.is_final == "TRUE"
        assert base_state.analysis.evaluation_result.verdict_confidence == "high"

    def test_sets_is_final_flag_when_true(self, base_state):
        """Test that is_final state flag is set when evaluation says TRUE."""
        updater = ComprehensiveEvaluationStateUpdater()
        result = json.dumps(
            {
                "is_final": "TRUE",
                "verdict_confidence": "medium",
                "exploration_gaps": [],
                "has_exploration_gaps": False,
                "logic_gaps": [],
                "required_code": [],
                "recommendations": [],
                "justifications": [],
            }
        )

        updater.update_state(base_state, {}, result)

        assert base_state.is_final is True

    def test_sets_is_final_flag_when_false(self, base_state):
        """Test that is_final state flag is False when evaluation says FALSE."""
        updater = ComprehensiveEvaluationStateUpdater()
        result = json.dumps(
            {
                "is_final": "FALSE",
                "verdict_confidence": "low",
                "exploration_gaps": [{"area": "middleware", "reason": "not checked"}],
                "has_exploration_gaps": True,
                "logic_gaps": [],
                "required_code": [],
                "recommendations": ["Check middleware"],
                "justifications": [],
            }
        )

        updater.update_state(base_state, {}, result)

        assert base_state.is_final is False

    def test_sets_verdict_when_final_and_investigation_exists(self, base_state):
        """Test that verdict is set when investigation is final and result exists."""
        # Set up investigation result first
        base_state.analysis.investigation_result = AnalysisResponse(
            investigation_result="TRUE_POSITIVE",
            is_final="TRUE",
            justifications=["SQL injection confirmed"],
            recommendations=["Use parameterized queries"],
        )

        updater = ComprehensiveEvaluationStateUpdater()
        result = json.dumps(
            {
                "is_final": "TRUE",
                "verdict_confidence": "high",
                "exploration_gaps": [],
                "has_exploration_gaps": False,
                "logic_gaps": [],
                "required_code": [],
                "recommendations": [],
                "justifications": [],
            }
        )

        updater.update_state(base_state, {}, result)

        assert base_state.is_final is True
        assert base_state.analysis.verdict == "TRUE_POSITIVE"

    def test_does_not_set_verdict_when_not_final(self, base_state):
        """Test that verdict is not set when investigation is not final."""
        base_state.analysis.investigation_result = AnalysisResponse(
            investigation_result="TRUE_POSITIVE",
            is_final="FALSE",
            justifications=["..."],
            recommendations=["..."],
        )

        updater = ComprehensiveEvaluationStateUpdater()
        result = json.dumps(
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

        updater.update_state(base_state, {}, result)

        assert base_state.is_final is False
        assert base_state.analysis.verdict is None


class TestSearchCodebaseStateUpdater:
    """Tests for SearchCodebaseStateUpdater (Phase 2)."""

    def test_parses_and_stores_search_results(self, base_state):
        """Test that search results are parsed and added to fetched_files."""
        updater = SearchCodebaseStateUpdater()
        tool_args = {"pattern": "sanitize.*", "scope": "app/"}
        result = json.dumps(
            {
                "app/utils.py": ["def sanitize(data): ...", "def sanitize_sql(query): ..."],
                "app/validators.py": ["def validate_input(data): ..."],
            }
        )

        updater.update_state(base_state, tool_args, result)

        assert "app/utils.py" in base_state.context.fetched_files
        assert len(base_state.context.fetched_files["app/utils.py"]) == 2
        assert "app/validators.py" in base_state.context.fetched_files

    def test_merges_with_existing_fetched_files(self, base_state):
        """Test that search results are merged with existing fetched files."""
        # Pre-populate with existing file
        base_state.context.fetched_files["app/utils.py"] = ["existing snippet"]

        updater = SearchCodebaseStateUpdater()
        result = json.dumps(
            {
                "app/utils.py": ["new snippet from search"],
            }
        )

        updater.update_state(base_state, {}, result)

        # Should have both existing and new snippets
        assert len(base_state.context.fetched_files["app/utils.py"]) == 2
        assert "existing snippet" in base_state.context.fetched_files["app/utils.py"]
        assert "new snippet from search" in base_state.context.fetched_files["app/utils.py"]


class TestGetStateUpdater:
    """Tests for get_state_updater registry function."""

    def test_returns_correct_updater_for_fetch_code(self):
        """Test that get_state_updater returns correct updater for fetch_code."""
        updater = get_state_updater("fetch_code")
        assert isinstance(updater, FetchCodeStateUpdater)

    def test_returns_correct_updater_for_analyze_issue(self):
        """Test that get_state_updater returns correct updater for analyze_issue."""
        updater = get_state_updater("analyze_issue")
        assert isinstance(updater, AnalyzeIssueStateUpdater)

    def test_returns_correct_updater_for_comprehensive_evaluation(self):
        """Test that get_state_updater returns correct updater."""
        updater = get_state_updater("comprehensive_evaluation")
        assert isinstance(updater, ComprehensiveEvaluationStateUpdater)

    def test_raises_error_for_unknown_tool(self):
        """Test that get_state_updater raises KeyError for unknown tool."""
        with pytest.raises(KeyError) as exc_info:
            get_state_updater("nonexistent_tool")

        assert "No state updater found for tool 'nonexistent_tool'" in str(exc_info.value)

    def test_supports_phase_2_tools(self):
        """Test that Phase 2 tools are registered."""
        search_updater = get_state_updater("search_codebase")
        list_updater = get_state_updater("list_files")

        assert isinstance(search_updater, SearchCodebaseStateUpdater)
        assert isinstance(list_updater, ListFilesStateUpdater)
