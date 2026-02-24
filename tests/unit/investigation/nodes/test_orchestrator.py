"""
Tests for investigation orchestrator in investigation/nodes/orchestrator.py.

Covers: issue filtering, ground truth loading, initial code extraction,
investigation state building, subgraph config building, tracker update,
and the main investigate node orchestration loop.
"""

from contextlib import contextmanager
from unittest.mock import AsyncMock, MagicMock, Mock, patch

import pytest

from sast_agent_workflow.nodes.sub_agents.investigation.constants import (
    INVESTIGATION_SUBGRAPH_RECURSION_LIMIT,
)
from sast_agent_workflow.nodes.sub_agents.investigation.orchestrator import (
    _build_initial_investigation_state,
    _build_subgraph_config,
    _extract_initial_code_and_build_context,
    _get_issues_to_investigate,
    _load_ground_truth_safe,
    _update_tracker_from_result,
    create_investigate_node,
)

_MOD = "sast_agent_workflow.nodes.sub_agents.investigation.orchestrator"

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def mock_config():
    """Mock configuration with standard test values."""
    config = Mock()
    config.MAX_ANALYSIS_ITERATIONS = 4
    config.PROJECT_NAME = "test-project"
    return config


@pytest.fixture
def mock_per_issue_final():
    """Mock per_issue data with a final analysis response."""
    per_issue = Mock()
    per_issue.analysis_response = Mock()
    per_issue.analysis_response.is_final = "TRUE"
    return per_issue


@pytest.fixture
def mock_per_issue_pending():
    """Mock per_issue data with a non-final analysis response."""
    per_issue = Mock()
    per_issue.analysis_response = Mock()
    per_issue.analysis_response.is_final = "FALSE"
    per_issue.issue = Mock()
    per_issue.issue.issue_type = "buffer_overflow"
    per_issue.issue.issue_label = "CWE-120"
    per_issue.issue.issue_cwe = "CWE-120"
    per_issue.issue.trace = "main.c:42 -> helper.c:10"
    return per_issue


@pytest.fixture
def mock_per_issue_no_response():
    """Mock per_issue data with no analysis response."""
    per_issue = Mock()
    per_issue.analysis_response = None
    per_issue.issue = Mock()
    per_issue.issue.issue_type = "sql_injection"
    per_issue.issue.issue_label = "CWE-89"
    per_issue.issue.issue_cwe = "CWE-89"
    per_issue.issue.trace = "app.py:15 -> db.py:30"
    return per_issue


@pytest.fixture
def mock_tracker(mock_per_issue_final, mock_per_issue_pending, mock_per_issue_no_response):
    """Mock SASTWorkflowTracker with mixed issue states."""
    tracker = Mock()
    tracker.issues = {
        "issue-1": mock_per_issue_final,
        "issue-2": mock_per_issue_pending,
        "issue-3": mock_per_issue_no_response,
    }
    return tracker


@pytest.fixture
def mock_repo_handler():
    """Mock repository handler for code extraction."""
    handler = Mock()
    handler.get_source_code_blocks_from_error_trace.return_value = {
        "main.c": "int main() { return 0; }",
        "helper.c": "void helper() {}",
    }
    return handler


@pytest.fixture
def subgraph_result():
    """Sample successful subgraph investigation result."""
    return {
        "proposed_verdict": "FALSE_POSITIVE",
        "justifications": ["Input is sanitized", "No path to sink"],
        "analysis_prompt": "Analyze CWE-120...",
        "confidence": "HIGH",
        "iteration": 2,
        "reanalysis_count": 1,
        "total_tool_calls": 5,
    }


# ---------------------------------------------------------------------------
# _get_issues_to_investigate
# ---------------------------------------------------------------------------


class TestGetIssuesToInvestigate:
    """Tests for _get_issues_to_investigate."""

    def test__skips_final_issues(self, mock_tracker):
        """Issues with is_final='TRUE' should be excluded."""
        result = _get_issues_to_investigate(mock_tracker)

        assert "issue-1" not in result

    def test__includes_pending_issues(self, mock_tracker):
        """Issues with is_final != 'TRUE' should be included."""
        result = _get_issues_to_investigate(mock_tracker)

        assert "issue-2" in result

    def test__includes_issues_without_analysis_response(self, mock_tracker):
        """Issues with no analysis_response should be included."""
        result = _get_issues_to_investigate(mock_tracker)

        assert "issue-3" in result

    def test__returns_empty_when_all_final(self, mock_per_issue_final):
        """Should return empty dict when all issues are final."""
        tracker = Mock()
        tracker.issues = {
            "issue-1": mock_per_issue_final,
        }

        result = _get_issues_to_investigate(tracker)

        assert len(result) == 0

    def test__returns_all_when_none_final(self, mock_per_issue_pending, mock_per_issue_no_response):
        """Should return all issues when none are final."""
        tracker = Mock()
        tracker.issues = {
            "issue-a": mock_per_issue_pending,
            "issue-b": mock_per_issue_no_response,
        }

        result = _get_issues_to_investigate(tracker)

        assert len(result) == 2


# ---------------------------------------------------------------------------
# _load_ground_truth_safe
# ---------------------------------------------------------------------------


class TestLoadGroundTruthSafe:
    """Tests for _load_ground_truth_safe."""

    @patch(f"{_MOD}.load_ground_truth_verdicts")
    def test__returns_verdicts_on_success(self, mock_load, mock_config):
        """Should return verdicts dict when loading succeeds."""
        mock_load.return_value = {"issue-1": "FALSE_POSITIVE"}

        result = _load_ground_truth_safe(mock_config)

        assert result == {"issue-1": "FALSE_POSITIVE"}

    @patch(f"{_MOD}.load_ground_truth_verdicts")
    def test__returns_none_on_exception(self, mock_load, mock_config):
        """Should return None when loading raises an exception."""
        mock_load.side_effect = RuntimeError("file not found")

        result = _load_ground_truth_safe(mock_config)

        assert result is None

    @patch(f"{_MOD}.load_ground_truth_verdicts")
    def test__returns_none_when_no_data(self, mock_load, mock_config):
        """Should return None when loader returns None."""
        mock_load.return_value = None

        result = _load_ground_truth_safe(mock_config)

        assert result is None


# ---------------------------------------------------------------------------
# _extract_initial_code_and_build_context
# ---------------------------------------------------------------------------


class TestExtractInitialCode:
    """Tests for _extract_initial_code_and_build_context."""

    def test__builds_code_context_from_trace(self, mock_repo_handler, mock_per_issue_pending):
        """Should build code_context string from extracted files."""
        code_context, _, _ = _extract_initial_code_and_build_context(
            mock_repo_handler, mock_per_issue_pending, "issue-1"
        )

        assert "=== main.c ===" in code_context
        assert "=== helper.c ===" in code_context
        assert "int main()" in code_context

    def test__populates_fetched_files(self, mock_repo_handler, mock_per_issue_pending):
        """Should populate fetched_files dict with trace-extracted code."""
        _, fetched, _ = _extract_initial_code_and_build_context(
            mock_repo_handler, mock_per_issue_pending, "issue-1"
        )

        assert "fetch_code_from_error_trace" in fetched
        assert len(fetched["fetch_code_from_error_trace"]) == 2

    def test__populates_gathered_code(self, mock_repo_handler, mock_per_issue_pending):
        """Should populate gathered_code_initial with formatted blocks."""
        _, _, gathered = _extract_initial_code_and_build_context(
            mock_repo_handler, mock_per_issue_pending, "issue-1"
        )

        assert "(from error trace)" in gathered
        assert len(gathered) > 0

    def test__handles_no_code_extracted(self, mock_per_issue_pending):
        """Should return empty context when no code is extracted from trace."""
        handler = Mock()
        handler.get_source_code_blocks_from_error_trace.return_value = None

        code_context, fetched, gathered = _extract_initial_code_and_build_context(
            handler, mock_per_issue_pending, "issue-1"
        )

        assert code_context == ""
        assert fetched == {}
        assert gathered == ""

    def test__handles_empty_dict_extracted(self, mock_per_issue_pending):
        """Should return empty context when trace returns empty dict."""
        handler = Mock()
        handler.get_source_code_blocks_from_error_trace.return_value = {}

        code_context, fetched, gathered = _extract_initial_code_and_build_context(
            handler, mock_per_issue_pending, "issue-1"
        )

        assert code_context == ""
        assert fetched == {}
        assert gathered == ""


# ---------------------------------------------------------------------------
# _build_initial_investigation_state
# ---------------------------------------------------------------------------


class TestBuildInitialInvestigationState:
    """Tests for _build_initial_investigation_state."""

    def test__sets_issue_metadata(self, mock_per_issue_pending, mock_config):
        """Should populate issue_id and issue_description."""
        state = _build_initial_investigation_state(
            "issue-42", mock_per_issue_pending, "code", "", {}, mock_config
        )

        assert state["issue_id"] == "issue-42"
        assert "issue-42" in state["issue_description"]
        assert "buffer_overflow" in state["issue_description"]

    def test__sets_initial_code_context(self, mock_per_issue_pending, mock_config):
        """Should set initial_code from provided code_context."""
        state = _build_initial_investigation_state(
            "issue-1", mock_per_issue_pending, "the code", "", {}, mock_config
        )

        assert state["initial_code"] == "the code"

    def test__starts_at_iteration_one(self, mock_per_issue_pending, mock_config):
        """Should start at iteration 1."""
        state = _build_initial_investigation_state(
            "issue-1", mock_per_issue_pending, "", "", {}, mock_config
        )

        assert state["iteration"] == 1

    def test__uses_config_max_iterations(self, mock_per_issue_pending, mock_config):
        """Should set max_iterations from config."""
        mock_config.MAX_ANALYSIS_ITERATIONS = 6

        state = _build_initial_investigation_state(
            "issue-1", mock_per_issue_pending, "", "", {}, mock_config
        )

        assert state["max_iterations"] == 6

    def test__defaults_max_iterations_to_four(self, mock_per_issue_pending, mock_config):
        """Should default to 4 when config value is falsy."""
        mock_config.MAX_ANALYSIS_ITERATIONS = 0

        state = _build_initial_investigation_state(
            "issue-1", mock_per_issue_pending, "", "", {}, mock_config
        )

        assert state["max_iterations"] == 4

    def test__initializes_control_flags(self, mock_per_issue_pending, mock_config):
        """Control flags should start in their initial states."""
        state = _build_initial_investigation_state(
            "issue-1", mock_per_issue_pending, "", "", {}, mock_config
        )

        assert state["is_complete"] is False
        assert state["needs_reanalysis"] is False
        assert state["stop_reason"] is None
        assert state["evaluation_rejection_streak"] == 0
        assert state["no_progress_streak"] == 0

    def test__initializes_reanalysis_count_to_zero(self, mock_per_issue_pending, mock_config):
        """reanalysis_count should be initialized to 0."""
        state = _build_initial_investigation_state(
            "issue-1", mock_per_issue_pending, "", "", {}, mock_config
        )

        assert state["reanalysis_count"] == 0

    def test__initializes_total_tool_calls_to_zero(self, mock_per_issue_pending, mock_config):
        """total_tool_calls should be initialized to 0."""
        state = _build_initial_investigation_state(
            "issue-1", mock_per_issue_pending, "", "", {}, mock_config
        )

        assert state["total_tool_calls"] == 0


# ---------------------------------------------------------------------------
# _build_subgraph_config
# ---------------------------------------------------------------------------


class TestBuildSubgraphConfig:
    """Tests for _build_subgraph_config."""

    def test__sets_recursion_limit_from_constant(self, mock_config, mock_per_issue_pending):
        """Should use INVESTIGATION_SUBGRAPH_RECURSION_LIMIT constant."""
        result = _build_subgraph_config("issue-1", mock_config, None, None, mock_per_issue_pending)

        assert result["recursion_limit"] == INVESTIGATION_SUBGRAPH_RECURSION_LIMIT

    def test__sets_run_name(self, mock_config, mock_per_issue_pending):
        """Should set runName with project and issue ID."""
        result = _build_subgraph_config("issue-42", mock_config, None, None, mock_per_issue_pending)

        assert result["runName"] == "investigate_test-project_issue-42"

    def test__adds_langfuse_when_handler_and_session(self, mock_config, mock_per_issue_pending):
        """Should add callbacks and metadata when langfuse handler is provided."""
        handler = MagicMock()
        with patch(f"{_MOD}.build_langfuse_metadata_for_investigation") as mock_meta:
            mock_meta.return_value = {"key": "value"}

            result = _build_subgraph_config(
                "issue-1", mock_config, handler, "session-1", mock_per_issue_pending
            )

        assert result["callbacks"] == [handler]
        assert result["metadata"] == {"key": "value"}

    def test__no_langfuse_when_handler_is_none(self, mock_config, mock_per_issue_pending):
        """Should not add callbacks when langfuse handler is None."""
        result = _build_subgraph_config(
            "issue-1", mock_config, None, "session-1", mock_per_issue_pending
        )

        assert "callbacks" not in result
        assert "metadata" not in result

    def test__no_langfuse_when_session_is_none(self, mock_config, mock_per_issue_pending):
        """Should not add callbacks when session_id is None."""
        result = _build_subgraph_config(
            "issue-1", mock_config, MagicMock(), None, mock_per_issue_pending
        )

        assert "callbacks" not in result
        assert "metadata" not in result


# ---------------------------------------------------------------------------
# _update_tracker_from_result
# ---------------------------------------------------------------------------


class TestUpdateTrackerFromResult:
    """Tests for _update_tracker_from_result."""

    def test__sets_verdict_with_underscores_replaced(self, mock_per_issue_pending, subgraph_result):
        """Should replace underscores with spaces in verdict."""
        _update_tracker_from_result(mock_per_issue_pending, subgraph_result, "issue-1")

        assert mock_per_issue_pending.analysis_response.investigation_result == "FALSE POSITIVE"

    def test__sets_is_final_to_true(self, mock_per_issue_pending, subgraph_result):
        """Should mark analysis as final."""
        _update_tracker_from_result(mock_per_issue_pending, subgraph_result, "issue-1")

        assert mock_per_issue_pending.analysis_response.is_final == "TRUE"

    def test__sets_justifications(self, mock_per_issue_pending, subgraph_result):
        """Should set justifications from result."""
        _update_tracker_from_result(mock_per_issue_pending, subgraph_result, "issue-1")

        assert mock_per_issue_pending.analysis_response.justifications == [
            "Input is sanitized",
            "No path to sink",
        ]

    def test__sets_prompt(self, mock_per_issue_pending, subgraph_result):
        """Should set the analysis prompt."""
        _update_tracker_from_result(mock_per_issue_pending, subgraph_result, "issue-1")

        assert mock_per_issue_pending.analysis_response.prompt == "Analyze CWE-120..."

    def test__handles_missing_analysis_response(self, subgraph_result):
        """Should not raise when analysis_response is None."""
        per_issue = Mock()
        per_issue.analysis_response = None

        _update_tracker_from_result(per_issue, subgraph_result, "issue-1")
        # Should not raise

    def test__logs_reanalysis_count(self, mock_per_issue_pending, subgraph_result, caplog):
        """Should include reanalysis_count in the log message."""
        import logging

        with caplog.at_level(logging.INFO):
            _update_tracker_from_result(mock_per_issue_pending, subgraph_result, "issue-1")

        log_text = caplog.text
        assert "reanalysis" in log_text.lower()
        assert "1" in log_text

    def test__logs_total_tool_calls(self, mock_per_issue_pending, subgraph_result, caplog):
        """Should include total_tool_calls in the log message."""
        import logging

        with caplog.at_level(logging.INFO):
            _update_tracker_from_result(mock_per_issue_pending, subgraph_result, "issue-1")

        log_text = caplog.text
        assert "tool_calls" in log_text.lower()
        assert "5" in log_text


# ---------------------------------------------------------------------------
# create_investigate_node (integration-style)
# ---------------------------------------------------------------------------


class TestCreateInvestigateNode:
    """Tests for create_investigate_node orchestration."""

    @pytest.mark.asyncio
    @patch(f"{_MOD}.build_investigation_subgraph")
    @patch(f"{_MOD}.repo_handler_factory")
    @patch(f"{_MOD}.load_ground_truth_verdicts", return_value=None)
    @patch(f"{_MOD}.langfuse_score_client_context")
    @patch(f"{_MOD}.issue_langfuse_context")
    async def test__invokes_subgraph_for_pending_issues(
        self,
        mock_issue_ctx,
        mock_score_ctx,
        mock_gt,
        mock_repo_factory,
        mock_build_sg,
        mock_config,
        mock_tracker,
        mock_repo_handler,
    ):
        """Should invoke subgraph for non-final issues and skip final ones."""
        # Setup mocks
        mock_subgraph = AsyncMock()
        mock_subgraph.ainvoke.return_value = {
            "proposed_verdict": "TRUE_POSITIVE",
            "justifications": ["Vulnerable"],
            "analysis_prompt": "prompt",
            "confidence": "HIGH",
            "iteration": 1,
        }
        mock_build_sg.return_value = mock_subgraph
        mock_repo_factory.return_value = mock_repo_handler

        # Langfuse context managers yield (None, None, None)
        @contextmanager
        def noop_score_ctx():
            yield None

        @contextmanager
        def noop_issue_ctx(issue_id, config):
            yield (None, None, None)

        mock_score_ctx.return_value = noop_score_ctx()
        mock_issue_ctx.side_effect = noop_issue_ctx

        # Create and invoke
        investigate = create_investigate_node(mock_config, MagicMock(), [])
        result = await investigate(mock_tracker)

        # Should have invoked subgraph for issue-2 and issue-3 (not issue-1 which is final)
        assert mock_subgraph.ainvoke.call_count == 2
        assert result is mock_tracker

    @pytest.mark.asyncio
    @patch(f"{_MOD}.build_investigation_subgraph")
    @patch(f"{_MOD}.repo_handler_factory")
    @patch(f"{_MOD}.load_ground_truth_verdicts", return_value=None)
    @patch(f"{_MOD}.langfuse_score_client_context")
    @patch(f"{_MOD}.issue_langfuse_context")
    async def test__handles_subgraph_exception_gracefully(
        self,
        mock_issue_ctx,
        mock_score_ctx,
        mock_gt,
        mock_repo_factory,
        mock_build_sg,
        mock_config,
        mock_per_issue_pending,
        mock_repo_handler,
    ):
        """Should catch exceptions and set NEEDS REVIEW on failure."""
        mock_subgraph = AsyncMock()
        mock_subgraph.ainvoke.side_effect = RuntimeError("LLM timeout")
        mock_build_sg.return_value = mock_subgraph
        mock_repo_factory.return_value = mock_repo_handler

        @contextmanager
        def noop_score_ctx():
            yield None

        @contextmanager
        def noop_issue_ctx(issue_id, config):
            yield (None, None, None)

        mock_score_ctx.return_value = noop_score_ctx()
        mock_issue_ctx.side_effect = noop_issue_ctx

        tracker = Mock()
        tracker.issues = {"issue-1": mock_per_issue_pending}

        investigate = create_investigate_node(mock_config, MagicMock(), [])
        result = await investigate(tracker)

        assert mock_per_issue_pending.analysis_response.investigation_result == "NEEDS REVIEW"
        assert mock_per_issue_pending.analysis_response.is_final == "TRUE"
        assert "LLM timeout" in mock_per_issue_pending.analysis_response.justifications[0]
        assert result is tracker
