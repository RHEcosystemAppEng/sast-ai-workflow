"""
Tests for Langfuse integration in investigation/observability/langfuse_integration.py.

Covers: LangfuseMetadata dataclass, metadata builders, context managers,
client/handler creation, flushing, score reporting, and graceful degradation
when the langfuse package is not installed.
"""

from unittest.mock import MagicMock, Mock, call, patch

import pytest
from langchain_core.messages import HumanMessage, ToolMessage

from sast_agent_workflow.nodes.sub_agents.investigation.observability.langfuse_integration import (
    LangfuseMetadata,
    _investigation_tags,
    _workflow_tags,
    add_langfuse_scores,
    build_langfuse_metadata_for_investigation,
    build_langfuse_metadata_for_workflow,
    create_langfuse_score_client_safe,
    create_langfuse_workflow_handler,
    flush_issue_langfuse,
    issue_langfuse_context,
    langfuse_score_client_context,
    setup_issue_langfuse,
    workflow_langfuse_context,
)

_MOD = "sast_agent_workflow.nodes.sub_agents.investigation.observability.langfuse_integration"

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def mock_config():
    """Mock configuration object with standard test values."""
    config = Mock()
    config.PROJECT_NAME = "test-project"
    config.PROJECT_VERSION = "1.0.0"
    config.TEST_RUN_ID = "run-001"
    config.LLM_MODEL_NAME = "gpt-4"
    return config


@pytest.fixture
def mock_langfuse_client():
    """Mock Langfuse client with flush and create_score methods."""
    client = MagicMock()
    client.flush = MagicMock()
    client.create_score = MagicMock()
    return client


@pytest.fixture
def sample_result_with_tool_calls():
    """Sample investigation result with ToolMessages for scoring tests."""
    return {
        "proposed_verdict": "TRUE_POSITIVE",
        "research_messages": [
            HumanMessage(content="Analyze this"),
            ToolMessage(content="result-1", tool_call_id="tc1"),
            ToolMessage(content="result-2", tool_call_id="tc2"),
        ],
    }


@pytest.fixture
def sample_result_no_tool_calls():
    """Sample investigation result with no ToolMessages."""
    return {
        "proposed_verdict": "FALSE_POSITIVE",
        "research_messages": [
            HumanMessage(content="Analyze this"),
        ],
    }


# ---------------------------------------------------------------------------
# LangfuseMetadata dataclass
# ---------------------------------------------------------------------------


class TestLangfuseMetadata:
    """Tests for the LangfuseMetadata frozen dataclass."""

    def test__to_dict__includes_required_fields(self):
        """Required fields always appear in the output dict."""
        meta = LangfuseMetadata(
            session_id="sess-1",
            user_id="user-1",
            tags=("tag-a", "tag-b"),
            project_name="proj",
            test_run_id="run-1",
        )
        d = meta.to_dict()

        assert d["langfuse_session_id"] == "sess-1"
        assert d["langfuse_user_id"] == "user-1"
        assert d["langfuse_tags"] == ["tag-a", "tag-b"]
        assert d["project_name"] == "proj"
        assert d["test_run_id"] == "run-1"

    def test__to_dict__excludes_none_optional_fields(self):
        """Optional fields that are None should not appear in the dict."""
        meta = LangfuseMetadata(
            session_id="s",
            user_id="u",
            tags=(),
            project_name="p",
            test_run_id="r",
        )
        d = meta.to_dict()

        assert "project_version" not in d
        assert "llm_model" not in d
        assert "langfuse_release" not in d
        assert "issue_id" not in d
        assert "issue_type" not in d

    def test__to_dict__includes_optional_fields_when_set(self):
        """Optional fields that are set should appear in the dict."""
        meta = LangfuseMetadata(
            session_id="s",
            user_id="u",
            tags=(),
            project_name="p",
            test_run_id="r",
            project_version="v1",
            llm_model="gpt-4",
            langfuse_release="rel-1",
            issue_id="def42",
            issue_type="sql_injection",
        )
        d = meta.to_dict()

        assert d["project_version"] == "v1"
        assert d["llm_model"] == "gpt-4"
        assert d["langfuse_release"] == "rel-1"
        assert d["issue_id"] == "def42"
        assert d["issue_type"] == "sql_injection"

    def test__frozen__raises_on_mutation(self):
        """Frozen dataclass should prevent attribute mutation."""
        meta = LangfuseMetadata(
            session_id="s",
            user_id="u",
            tags=(),
            project_name="p",
            test_run_id="r",
        )
        with pytest.raises(AttributeError):
            meta.session_id = "new"  # type: ignore[misc]


# ---------------------------------------------------------------------------
# Tag builders
# ---------------------------------------------------------------------------


class TestTagBuilders:
    """Tests for _workflow_tags and _investigation_tags."""

    def test__workflow_tags__returns_correct_tags(self, mock_config):
        """Workflow tags should contain project, version, test_run, model, and workflow."""
        tags = _workflow_tags(mock_config)

        assert "project:test-project" in tags
        assert "version:1.0.0" in tags
        assert "test_run:run-001" in tags
        assert "model:gpt-4" in tags
        assert "workflow:sast_pipeline" in tags
        assert len(tags) == 5

    def test__investigation_tags__returns_correct_tags(self, mock_config):
        """Investigation tags should contain project, test_run, model, issue info, and stage."""
        tags = _investigation_tags(mock_config, "def1", "buffer_overflow")

        assert "project:test-project" in tags
        assert "test_run:run-001" in tags
        assert "model:gpt-4" in tags
        assert "issue:def1" in tags
        assert "issue_type:buffer_overflow" in tags
        assert "stage:investigate" in tags
        assert "method:multi_stage" in tags
        assert len(tags) == 7


# ---------------------------------------------------------------------------
# Metadata builders
# ---------------------------------------------------------------------------


class TestMetadataBuilders:
    """Tests for build_langfuse_metadata_for_workflow
    and build_langfuse_metadata_for_investigation."""

    def test__build_workflow_metadata__contains_expected_keys(self, mock_config):
        """Workflow metadata should include session_id, user_id, tags, and project info."""
        result = build_langfuse_metadata_for_workflow(mock_config, "session-abc")

        assert result["langfuse_session_id"] == "session-abc"
        assert result["langfuse_user_id"] == "test-project"
        assert result["project_name"] == "test-project"
        assert result["test_run_id"] == "run-001"
        assert result["project_version"] == "1.0.0"
        assert result["llm_model"] == "gpt-4"
        assert "workflow:sast_pipeline" in result["langfuse_tags"]

    def test__build_investigation_metadata__contains_expected_keys(self, mock_config):
        """Investigation metadata should include issue-specific fields."""
        result = build_langfuse_metadata_for_investigation(
            mock_config, "session-xyz", "def5", "xss"
        )

        assert result["langfuse_session_id"] == "session-xyz"
        assert result["langfuse_user_id"] == "test-project"
        assert result["issue_id"] == "def5"
        assert result["issue_type"] == "xss"
        assert result["langfuse_release"] == "1.0.0"
        assert "issue:def5" in result["langfuse_tags"]
        assert "issue_type:xss" in result["langfuse_tags"]
        assert "stage:investigate" in result["langfuse_tags"]

    def test__build_investigation_metadata__does_not_set_project_version(self, mock_config):
        """Investigation metadata uses langfuse_release, not project_version."""
        result = build_langfuse_metadata_for_investigation(mock_config, "s", "def1", "sqli")
        # langfuse_release is set, but project_version should NOT be in the dict
        assert "project_version" not in result
        assert result["langfuse_release"] == "1.0.0"


# ---------------------------------------------------------------------------
# create_langfuse_score_client_safe
# ---------------------------------------------------------------------------


class TestCreateLangfuseScoreClientSafe:
    """Tests for create_langfuse_score_client_safe."""

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        False,
    )
    def test__returns_none_when_langfuse_not_available(self):
        """Should return None when langfuse package is not installed."""
        assert create_langfuse_score_client_safe() is None

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        True,
    )
    @patch.dict("os.environ", {}, clear=True)
    def test__returns_none_when_public_key_not_set(self):
        """Should return None when LANGFUSE_PUBLIC_KEY env var is missing."""
        assert create_langfuse_score_client_safe() is None

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        True,
    )
    @patch.dict(
        "os.environ",
        {
            "LANGFUSE_PUBLIC_KEY": "pk-test",
            "LANGFUSE_SECRET_KEY": "sk-test",
            "LANGFUSE_HOST": "https://langfuse.example.com",
        },
    )
    @patch(f"{_MOD}.Langfuse", create=True)
    def test__returns_client_when_configured(self, mock_langfuse_cls):
        """Should return a Langfuse client when all env vars are set."""
        # We need to mock the import inside the function
        mock_client = MagicMock()
        with patch.dict("sys.modules", {"langfuse": MagicMock(Langfuse=lambda **kw: mock_client)}):
            client = create_langfuse_score_client_safe()
        assert client is mock_client

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        True,
    )
    @patch.dict(
        "os.environ",
        {"LANGFUSE_PUBLIC_KEY": "pk-test"},
    )
    def test__returns_none_on_init_exception(self):
        """Should return None if Langfuse() constructor raises."""
        with patch.dict(
            "sys.modules",
            {"langfuse": MagicMock(Langfuse=Mock(side_effect=RuntimeError("connection failed")))},
        ):
            assert create_langfuse_score_client_safe() is None


# ---------------------------------------------------------------------------
# create_langfuse_workflow_handler
# ---------------------------------------------------------------------------


class TestCreateLangfuseWorkflowHandler:
    """Tests for create_langfuse_workflow_handler."""

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        False,
    )
    def test__returns_none_when_langfuse_not_available(self):
        """Should return None when langfuse not installed."""
        assert create_langfuse_workflow_handler() is None

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        True,
    )
    @patch(
        f"{_MOD}.LangfuseCallbackHandler",
        None,
    )
    def test__returns_none_when_callback_handler_is_none(self):
        """Should return None when LangfuseCallbackHandler is None."""
        assert create_langfuse_workflow_handler() is None

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        True,
    )
    @patch(
        f"{_MOD}.LangfuseCallbackHandler",
        MagicMock(),
    )
    @patch.dict("os.environ", {"LANGFUSE_TRACE_PER_ISSUE": "true", "LANGFUSE_PUBLIC_KEY": "pk"})
    def test__returns_none_when_per_issue_tracing_enabled(self):
        """Should return None when LANGFUSE_TRACE_PER_ISSUE is true."""
        assert create_langfuse_workflow_handler() is None

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        True,
    )
    @patch(
        f"{_MOD}.LangfuseCallbackHandler",
    )
    @patch.dict("os.environ", {"LANGFUSE_PUBLIC_KEY": "pk-test"}, clear=False)
    def test__returns_handler_when_configured(self, mock_handler_cls):
        """Should return handler when langfuse is available and configured."""
        mock_handler = MagicMock()
        mock_handler_cls.return_value = mock_handler

        result = create_langfuse_workflow_handler()

        assert result is mock_handler
        mock_handler_cls.assert_called_once()

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        True,
    )
    @patch(
        f"{_MOD}.LangfuseCallbackHandler",
        Mock(side_effect=RuntimeError("init error")),
    )
    @patch.dict("os.environ", {"LANGFUSE_PUBLIC_KEY": "pk-test"}, clear=False)
    def test__returns_none_on_handler_init_exception(self):
        """Should return None if handler initialization raises."""
        assert create_langfuse_workflow_handler() is None


# ---------------------------------------------------------------------------
# setup_issue_langfuse
# ---------------------------------------------------------------------------


class TestSetupIssueLangfuse:
    """Tests for setup_issue_langfuse."""

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        False,
    )
    def test__returns_all_none_when_langfuse_not_available(self, mock_config):
        """Should return (None, None, None) when langfuse is not installed."""
        assert setup_issue_langfuse("def1", mock_config) == (None, None, None)

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        True,
    )
    @patch(
        f"{_MOD}.LangfuseCallbackHandler",
        None,
    )
    @patch.dict("os.environ", {"LANGFUSE_PUBLIC_KEY": "pk"})
    def test__returns_all_none_when_handler_cls_is_none(self, mock_config):
        """Should return (None, None, None) when LangfuseCallbackHandler is None."""
        assert setup_issue_langfuse("def1", mock_config) == (None, None, None)

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        True,
    )
    @patch(
        f"{_MOD}.LangfuseCallbackHandler",
        MagicMock(),
    )
    @patch.dict("os.environ", {}, clear=True)
    def test__returns_all_none_when_public_key_missing(self, mock_config):
        """Should return (None, None, None) when LANGFUSE_PUBLIC_KEY is not set."""
        assert setup_issue_langfuse("def1", mock_config) == (None, None, None)

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        True,
    )
    @patch(
        f"{_MOD}.LangfuseCallbackHandler",
    )
    @patch.dict("os.environ", {"LANGFUSE_PUBLIC_KEY": "pk-test"})
    def test__returns_handler_session_trace_when_configured(self, mock_handler_cls, mock_config):
        """Should return (handler, session_id, trace_id) when fully configured."""
        mock_handler = MagicMock()
        mock_handler_cls.return_value = mock_handler

        handler, session_id, trace_id = setup_issue_langfuse("def1", mock_config)

        assert handler is mock_handler
        assert session_id == "test-project_run-001_def1"
        assert trace_id is not None
        assert len(trace_id) == 32  # uuid4 hex

    @patch(
        f"{_MOD}.LANGFUSE_AVAILABLE",
        True,
    )
    @patch(
        f"{_MOD}.LangfuseCallbackHandler",
        Mock(side_effect=RuntimeError("init failure")),
    )
    @patch.dict("os.environ", {"LANGFUSE_PUBLIC_KEY": "pk-test"})
    def test__returns_all_none_on_exception(self, mock_config):
        """Should return (None, None, None) if handler constructor raises."""
        assert setup_issue_langfuse("def1", mock_config) == (None, None, None)


# ---------------------------------------------------------------------------
# flush_issue_langfuse
# ---------------------------------------------------------------------------


class TestFlushIssueLangfuse:
    """Tests for flush_issue_langfuse."""

    def test__does_nothing_when_handler_is_none(self):
        """Should return immediately when handler is None."""
        flush_issue_langfuse(None, "def1")  # Should not raise

    def test__flushes_handler_client(self):
        """Should call handler.client.flush() on a valid handler."""
        handler = MagicMock()
        flush_issue_langfuse(handler, "def1")
        handler.client.flush.assert_called_once()

    def test__handles_flush_exception(self):
        """Should log warning and not raise if flush fails."""
        handler = MagicMock()
        handler.client.flush.side_effect = RuntimeError("flush failed")

        # Should not raise
        flush_issue_langfuse(handler, "def1")


# ---------------------------------------------------------------------------
# Context managers
# ---------------------------------------------------------------------------


class TestLangfuseScoreClientContext:
    """Tests for langfuse_score_client_context."""

    @patch(
        f"{_MOD}.create_langfuse_score_client_safe",
        return_value=None,
    )
    def test__yields_none_when_unavailable(self, _mock):
        """Should yield None when score client is unavailable."""
        with langfuse_score_client_context() as client:
            assert client is None

    @patch(
        f"{_MOD}.create_langfuse_score_client_safe",
    )
    def test__yields_client_and_flushes_on_exit(self, mock_create):
        """Should yield client and flush on context exit."""
        mock_client = MagicMock()
        mock_create.return_value = mock_client

        with langfuse_score_client_context() as client:
            assert client is mock_client
            mock_client.flush.assert_not_called()

        mock_client.flush.assert_called_once()

    @patch(
        f"{_MOD}.create_langfuse_score_client_safe",
    )
    def test__handles_flush_exception_on_exit(self, mock_create):
        """Should not raise if flush fails on exit."""
        mock_client = MagicMock()
        mock_client.flush.side_effect = RuntimeError("flush error")
        mock_create.return_value = mock_client

        with langfuse_score_client_context() as client:
            assert client is mock_client
        # Should complete without raising


class TestIssueLangfuseContext:
    """Tests for issue_langfuse_context."""

    @patch(
        f"{_MOD}.flush_issue_langfuse",
    )
    @patch(
        f"{_MOD}.setup_issue_langfuse",
    )
    def test__yields_handler_session_trace(self, mock_setup, mock_flush, mock_config):
        """Should yield the tuple from setup_issue_langfuse."""
        mock_handler = MagicMock()
        mock_setup.return_value = (mock_handler, "sess-1", "trace-1")

        with issue_langfuse_context("def1", mock_config) as (handler, session_id, trace_id):
            assert handler is mock_handler
            assert session_id == "sess-1"
            assert trace_id == "trace-1"

        mock_flush.assert_called_once_with(mock_handler, "def1")

    @patch(
        f"{_MOD}.flush_issue_langfuse",
    )
    @patch(
        f"{_MOD}.setup_issue_langfuse",
        return_value=(None, None, None),
    )
    def test__yields_nones_when_unavailable(self, _mock_setup, mock_flush, mock_config):
        """Should yield all Nones and still call flush on exit."""
        with issue_langfuse_context("def1", mock_config) as (handler, session_id, trace_id):
            assert handler is None
            assert session_id is None
            assert trace_id is None

        mock_flush.assert_called_once_with(None, "def1")


class TestWorkflowLangfuseContext:
    """Tests for workflow_langfuse_context."""

    @patch(
        f"{_MOD}.create_langfuse_workflow_handler",
        return_value=None,
    )
    def test__yields_none_when_unavailable(self, _mock):
        """Should yield None when handler is unavailable."""
        with workflow_langfuse_context() as handler:
            assert handler is None

    @patch(
        f"{_MOD}.create_langfuse_workflow_handler",
    )
    def test__yields_handler_and_flushes_on_exit(self, mock_create):
        """Should yield handler and flush its client on exit."""
        mock_handler = MagicMock()
        mock_create.return_value = mock_handler

        with workflow_langfuse_context() as handler:
            assert handler is mock_handler
            mock_handler.client.flush.assert_not_called()

        mock_handler.client.flush.assert_called_once()

    @patch(
        f"{_MOD}.create_langfuse_workflow_handler",
    )
    def test__handles_flush_exception_on_exit(self, mock_create):
        """Should not raise if handler.client.flush() fails on exit."""
        mock_handler = MagicMock()
        mock_handler.client.flush.side_effect = RuntimeError("flush error")
        mock_create.return_value = mock_handler

        with workflow_langfuse_context() as handler:
            assert handler is mock_handler
        # Should complete without raising


# ---------------------------------------------------------------------------
# add_langfuse_scores
# ---------------------------------------------------------------------------


class TestAddLangfuseScores:
    """Tests for add_langfuse_scores."""

    def test__creates_tool_call_count_and_verdict_scores(
        self, mock_langfuse_client, sample_result_with_tool_calls
    ):
        """Should create tool_call_count, reanalysis_count, stop_reason, and verdict scores."""
        add_langfuse_scores(
            mock_langfuse_client, "trace-1", "def1", sample_result_with_tool_calls, None
        )

        calls = mock_langfuse_client.create_score.call_args_list
        assert len(calls) == 4

        # First call: tool_call_count
        assert calls[0] == call(
            trace_id="trace-1",
            name="tool_call_count",
            value=2.0,
            data_type="NUMERIC",
            comment="Total tool executions during investigation",
        )

        # Second call: reanalysis_count
        assert calls[1] == call(
            trace_id="trace-1",
            name="reanalysis_count",
            value=0.0,
            data_type="NUMERIC",
            comment="Times evaluation triggered reanalysis without new research",
        )

        # Third call: stop_reason
        assert calls[2] == call(
            trace_id="trace-1",
            name="stop_reason",
            value="completed",
            data_type="CATEGORICAL",
            comment="Investigation exit reason: completed",
        )

        # Fourth call: verdict
        assert calls[3] == call(
            trace_id="trace-1",
            name="verdict",
            value="TRUE_POSITIVE",
            data_type="CATEGORICAL",
            comment="Investigation verdict: TRUE_POSITIVE",
        )

    def test__zero_tool_calls_when_no_tool_messages(
        self, mock_langfuse_client, sample_result_no_tool_calls
    ):
        """Should report tool_call_count=0 when there are no ToolMessages."""
        add_langfuse_scores(
            mock_langfuse_client, "trace-1", "def1", sample_result_no_tool_calls, None
        )

        first_call = mock_langfuse_client.create_score.call_args_list[0]
        assert first_call == call(
            trace_id="trace-1",
            name="tool_call_count",
            value=0.0,
            data_type="NUMERIC",
            comment="Total tool executions during investigation",
        )

    def test__adds_ground_truth_scores_when_matching(
        self, mock_langfuse_client, sample_result_with_tool_calls
    ):
        """Should add ground_truth_verdict and verdict_correct when ground truth matches."""
        ground_truth = {"def1": "TRUE_POSITIVE"}

        add_langfuse_scores(
            mock_langfuse_client, "trace-1", "def1", sample_result_with_tool_calls, ground_truth
        )

        calls = mock_langfuse_client.create_score.call_args_list
        # tool_call_count + reanalysis_count + stop_reason + verdict +
        #  ground_truth_verdict + verdict_correct
        assert len(calls) == 6

        # ground_truth_verdict
        assert calls[4] == call(
            trace_id="trace-1",
            name="ground_truth_verdict",
            value="TRUE_POSITIVE",
            data_type="CATEGORICAL",
            comment="Human-verified ground truth verdict",
        )

        # verdict_correct (TRUE_POSITIVE == TRUE_POSITIVE => 1)
        assert calls[5] == call(
            trace_id="trace-1",
            name="verdict_correct",
            value=1,
            data_type="BOOLEAN",
            comment="Verdict matches ground truth: TRUE_POSITIVE",
        )

    def test__verdict_correct_is_zero_when_mismatch(
        self, mock_langfuse_client, sample_result_with_tool_calls
    ):
        """Should set verdict_correct=0 when investigation verdict != ground truth."""
        ground_truth = {"def1": "FALSE_POSITIVE"}

        add_langfuse_scores(
            mock_langfuse_client, "trace-1", "def1", sample_result_with_tool_calls, ground_truth
        )

        calls = mock_langfuse_client.create_score.call_args_list
        assert len(calls) == 6

        # verdict_correct (TRUE_POSITIVE != FALSE_POSITIVE => 0)
        assert calls[5] == call(
            trace_id="trace-1",
            name="verdict_correct",
            value=0,
            data_type="BOOLEAN",
            comment="Verdict matches ground truth: FALSE_POSITIVE",
        )

    def test__skips_ground_truth_when_verdicts_is_none(
        self, mock_langfuse_client, sample_result_with_tool_calls
    ):
        """Should only create 4 scores when ground_truth_verdicts is None."""
        add_langfuse_scores(
            mock_langfuse_client, "trace-1", "def1", sample_result_with_tool_calls, None
        )

        assert mock_langfuse_client.create_score.call_count == 4

    def test__skips_ground_truth_when_issue_not_in_verdicts(
        self, mock_langfuse_client, sample_result_with_tool_calls
    ):
        """Should only create 4 scores when issue_id is not in ground_truth_verdicts."""
        ground_truth = {"def999": "TRUE_POSITIVE"}

        add_langfuse_scores(
            mock_langfuse_client, "trace-1", "def1", sample_result_with_tool_calls, ground_truth
        )

        assert mock_langfuse_client.create_score.call_count == 4

    def test__handles_empty_research_messages(self, mock_langfuse_client):
        """Should handle result with empty research_messages list."""
        result = {"proposed_verdict": "TRUE_POSITIVE", "research_messages": []}

        add_langfuse_scores(mock_langfuse_client, "trace-1", "def1", result, None)

        first_call = mock_langfuse_client.create_score.call_args_list[0]
        assert first_call[1]["value"] == pytest.approx(0.0)

    def test__handles_missing_research_messages_key(self, mock_langfuse_client):
        """Should handle result with no research_messages key (defaults to empty)."""
        result = {"proposed_verdict": "FALSE_POSITIVE"}

        add_langfuse_scores(mock_langfuse_client, "trace-1", "def1", result, None)

        first_call = mock_langfuse_client.create_score.call_args_list[0]
        assert first_call[1]["value"] == pytest.approx(0.0)

    def test__reanalysis_count_from_result(self, mock_langfuse_client):
        """Should use reanalysis_count from result when present."""
        result = {
            "proposed_verdict": "TRUE_POSITIVE",
            "research_messages": [],
            "reanalysis_count": 3,
        }

        add_langfuse_scores(mock_langfuse_client, "trace-1", "def1", result, None)

        reanalysis_call = mock_langfuse_client.create_score.call_args_list[1]
        assert reanalysis_call == call(
            trace_id="trace-1",
            name="reanalysis_count",
            value=3.0,
            data_type="NUMERIC",
            comment="Times evaluation triggered reanalysis without new research",
        )

    def test__stop_reason_from_result(self, mock_langfuse_client):
        """Should use stop_reason from result when present."""
        result = {
            "proposed_verdict": "NEEDS_REVIEW",
            "research_messages": [],
            "stop_reason": "max_iterations",
        }

        add_langfuse_scores(mock_langfuse_client, "trace-1", "def1", result, None)

        stop_reason_call = mock_langfuse_client.create_score.call_args_list[2]
        assert stop_reason_call == call(
            trace_id="trace-1",
            name="stop_reason",
            value="max_iterations",
            data_type="CATEGORICAL",
            comment="Investigation exit reason: max_iterations",
        )

    def test__stop_reason_defaults_to_completed(self, mock_langfuse_client):
        """Should default stop_reason to 'completed' when None or missing."""
        result = {"proposed_verdict": "TRUE_POSITIVE", "research_messages": []}

        add_langfuse_scores(mock_langfuse_client, "trace-1", "def1", result, None)

        stop_reason_call = mock_langfuse_client.create_score.call_args_list[2]
        assert stop_reason_call[1]["value"] == "completed"

    def test__handles_create_score_exception(self, mock_langfuse_client):
        """Should catch and log exception if create_score raises."""
        mock_langfuse_client.create_score.side_effect = RuntimeError("API error")
        result = {"proposed_verdict": "TRUE_POSITIVE", "research_messages": []}

        # Should not raise
        add_langfuse_scores(mock_langfuse_client, "trace-1", "def1", result, None)
