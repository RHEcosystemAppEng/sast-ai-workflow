"""
End-to-End integration test for confidence scoring through the complete SAST workflow.

Tests the FULL pipeline execution:
  Pre-Process → Filter → Investigation → Calculate Metrics → Write Results

Validates that confidence scoring is properly integrated at every stage:
1. Pre-process initializes the workflow state
2. Filter populates filter_confidence
3. Investigation populates agent_confidence and investigation metrics
4. Calculate_Metrics computes final_confidence_score
5. Final scores are within valid range (0-100%)
"""

import pytest
from unittest.mock import Mock, AsyncMock, MagicMock, patch
from pathlib import Path

from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus
from common.config import Config
from sast_agent_workflow.graph_builder import build_sast_workflow_graph
from Utils.confidence_scoring import calculate_final_confidence, calculate_aggregate_confidence_metrics


# mock_confidence_config fixture imported from conftest.py


def _create_real_confidence_metrics_node():
    """
    Create a calculate_metrics node with real confidence scoring logic.

    Shared helper to avoid duplication across E2E tests.
    Uses real confidence calculation without inject_mock_confidence_data.
    """

    async def calculate_metrics_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Calculate metrics node with real confidence scoring (no mock data injection for E2E test)."""
        if tracker.metrics is None:
            tracker.metrics = {}

        try:
            confidence_breakdowns = {}
            for issue_id, per_issue_data in tracker.issues.items():
                # Calculate confidence score directly (no mock injection)
                breakdown = calculate_final_confidence(per_issue_data, tracker.config)
                confidence_breakdowns[issue_id] = breakdown
                # Store final score in PerIssueData
                per_issue_data.final_confidence_score = breakdown.final_confidence

            # Calculate aggregate statistics
            aggregate_stats = calculate_aggregate_confidence_metrics(confidence_breakdowns)
            tracker.metrics['confidence_scores'] = aggregate_stats
        except Exception as e:
            tracker.metrics['confidence_scores'] = {
                'error': f"Confidence calculation failed: {str(e)}",
                'per_issue_scores': {},
                'total_issues': 0
            }

        return tracker

    return calculate_metrics_node


@pytest.fixture
def sample_issues():
    """Create sample security issues for testing."""
    return [
        Issue(
            id="E2E-001",
            issue_type="BUFFER_OVERFLOW",
            severity="high",
            issue_cwe="CWE-120",
            issue_label="Buffer overflow in strcpy",
            trace="src/vulnerable.c:42: strcpy(buffer, user_input);",
            file_path="src/vulnerable.c",
            line_number=42
        ),
        Issue(
            id="E2E-002",
            issue_type="NULL_DEREFERENCE",
            severity="medium",
            issue_cwe="CWE-476",
            issue_label="Potential null pointer dereference",
            trace="src/utils.c:100: if (ptr->field) { ... }",
            file_path="src/utils.c",
            line_number=100
        )
    ]


@pytest.fixture
def mock_pre_process_node(mock_confidence_config, sample_issues):
    """Mock pre-process node that initializes the workflow."""
    async def pre_process(state: dict) -> SASTWorkflowTracker:
        """Initialize tracker with sample issues."""
        tracker = SASTWorkflowTracker(
            config=mock_confidence_config,
            iteration_count=0,
            issues={}
        )

        # Initialize PerIssueData for each issue
        for issue in sample_issues:
            tracker.issues[issue.id] = PerIssueData(
                issue=issue,
                source_code={},
                similar_known_issues="",
                analysis_response=AnalysisResponse(
                    investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                    is_final=FinalStatus.FALSE.value,
                    justifications=[],
                    evaluation=[],
                    recommendations=[],
                    instructions=[],
                    prompt=""
                ),
                fetched_files=[],
                found_symbols=set()
            )

        return tracker

    return AsyncMock(side_effect=pre_process)


@pytest.fixture
def mock_filter_node():
    """Mock filter node that sets filter_confidence."""
    async def filter_issues(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Simulate filter node adding filter_confidence."""
        for issue_id, per_issue in tracker.issues.items():
            if issue_id == "E2E-001":
                # High confidence - needs investigation
                per_issue.analysis_response.filter_confidence = 0.75
                per_issue.analysis_response.faiss_similarity_score = 0.80
                per_issue.analysis_response.is_final = FinalStatus.FALSE.value
            else:
                # Known FP - short-circuit (no investigation)
                per_issue.analysis_response.filter_confidence = 0.92
                per_issue.analysis_response.faiss_similarity_score = 0.95
                per_issue.analysis_response.is_final = FinalStatus.TRUE.value
                per_issue.analysis_response.investigation_result = CVEValidationStatus.FALSE_POSITIVE.value

        return tracker

    return AsyncMock(side_effect=filter_issues)


@pytest.fixture
def mock_investigate_node():
    """Mock investigation node that sets agent_confidence and investigation metrics."""
    async def investigate(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Simulate investigation adding agent_confidence and metrics."""
        for issue_id, per_issue in tracker.issues.items():
            # Only investigate if filter didn't short-circuit
            if per_issue.analysis_response.is_final == FinalStatus.FALSE.value:
                # Simulate investigation subgraph completing
                per_issue.analysis_response.agent_confidence = 0.85
                per_issue.analysis_response.investigation_result = CVEValidationStatus.TRUE_POSITIVE.value
                per_issue.analysis_response.is_final = FinalStatus.TRUE.value
                per_issue.analysis_response.justifications = [
                    "Buffer overflow confirmed in strcpy call",
                    "See vulnerable.c:42 for vulnerable code",
                    "```c\nstrcpy(buffer, user_input);\n```",
                    "CVE-2023-1234 similar vulnerability"
                ]

                # Investigation metrics (from orchestrator)
                per_issue.investigation_tool_call_count = 12
                per_issue.investigation_reanalysis_count = 2
                per_issue.investigation_stop_reason = "approved"
                per_issue.fetched_files = ["src/vulnerable.c", "src/utils.c", "include/defs.h"]
                per_issue.found_symbols = {"strcpy", "validate_input", "safe_copy"}

        return tracker

    return AsyncMock(side_effect=investigate)


@pytest.fixture
def mock_summarize_justifications_node():
    """Mock summarize justifications node (pass-through for this test)."""
    async def summarize(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Pass-through - just return tracker."""
        return tracker

    return AsyncMock(side_effect=summarize)


@pytest.fixture
def mock_write_results_node():
    """Mock write results node (pass-through for this test)."""
    async def write_results(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Pass-through - just return tracker."""
        return tracker

    return AsyncMock(side_effect=write_results)


class TestConfidenceScoringEndToEnd:
    """End-to-end tests for confidence scoring through complete workflow."""

    @pytest.mark.asyncio
    async def test_complete_workflow_calculates_confidence_scores(
        self,
        mock_confidence_config,
        mock_pre_process_node,
        mock_filter_node,
        mock_investigate_node,
        mock_summarize_justifications_node,
        mock_write_results_node
    ):
        """
        Test complete workflow execution with confidence scoring.

        Validates:
        1. Pre-process initializes workflow
        2. Filter sets filter_confidence
        3. Investigation sets agent_confidence and metrics
        4. Calculate_Metrics computes final_confidence_score
        5. All scores are in valid range (0-100%)
        6. Different paths (full investigation vs short-circuit) work correctly
        """
        # Create calculate_metrics node using shared helper
        calculate_metrics_node = _create_real_confidence_metrics_node()

        # Build the complete workflow graph with real calculate_metrics
        workflow = build_sast_workflow_graph(
            pre_process_node=mock_pre_process_node,
            filter_node=mock_filter_node,
            investigate_node=mock_investigate_node,
            summarize_justifications_node=mock_summarize_justifications_node,
            calculate_metrics_node=calculate_metrics_node,  # REAL confidence scoring
            write_results_node=mock_write_results_node
        )

        # Execute the complete workflow
        initial_state = {}
        result = await workflow.ainvoke(initial_state)

        # ASSERTIONS

        # 1. Verify workflow completed successfully
        # LangGraph returns state as dict
        assert isinstance(result, dict)
        assert len(result["issues"]) == 2

        # 2. Verify metrics were calculated
        assert result["metrics"] is not None
        assert "confidence_scores" in result["metrics"]

        # 3. Verify E2E-001 (full investigation path)
        issue_001 = result["issues"]["E2E-001"]

        # Filter confidence was set
        assert issue_001.analysis_response.filter_confidence == pytest.approx(0.75)

        # Agent confidence was set by investigation
        assert issue_001.analysis_response.agent_confidence == pytest.approx(0.85)

        # Investigation metrics were captured
        assert issue_001.investigation_tool_call_count == 12
        assert issue_001.investigation_reanalysis_count == 2
        assert issue_001.investigation_stop_reason == "approved"
        assert len(issue_001.fetched_files) == 3
        assert len(issue_001.found_symbols) == 3

        # Final confidence score was calculated and stored in metrics
        confidence_scores = result["metrics"]["confidence_scores"]
        e2e_001_score = confidence_scores["per_issue_scores"]["E2E-001"]
        assert e2e_001_score is not None
        assert 0.0 <= e2e_001_score <= 100.0

        # High component scores should result in high final confidence
        assert e2e_001_score >= 70.0, (
            f"Expected high confidence for E2E-001, got {e2e_001_score:.1f}%"
        )

        # Verify score was also written to PerIssueData (if mutation is supported by LangGraph)
        if issue_001.final_confidence_score is not None:
            assert issue_001.final_confidence_score == pytest.approx(e2e_001_score)

        # 4. Verify E2E-002 (known FP short-circuit path)
        issue_002 = result["issues"]["E2E-002"]

        # Filter confidence was set
        assert issue_002.analysis_response.filter_confidence == pytest.approx(0.92)

        # Investigation was skipped (short-circuit)
        assert issue_002.analysis_response.agent_confidence is None
        assert issue_002.investigation_tool_call_count == 0
        assert issue_002.investigation_reanalysis_count == 0
        assert issue_002.investigation_stop_reason is None

        # Final confidence score uses filter_confidence only
        e2e_002_score = confidence_scores["per_issue_scores"]["E2E-002"]
        assert e2e_002_score is not None
        assert e2e_002_score == pytest.approx(92.0)  # filter × 100%

        # 5. Verify aggregate confidence metrics
        assert "per_issue_scores" in confidence_scores
        assert "mean_confidence" in confidence_scores
        assert "total_issues" in confidence_scores

        assert confidence_scores["total_issues"] == 2
        assert "E2E-001" in confidence_scores["per_issue_scores"]
        assert "E2E-002" in confidence_scores["per_issue_scores"]

        # Per-issue scores are present and match expected values
        assert confidence_scores["per_issue_scores"]["E2E-001"] == pytest.approx(e2e_001_score)
        assert confidence_scores["per_issue_scores"]["E2E-002"] == pytest.approx(e2e_002_score)

    @pytest.mark.asyncio
    async def test_workflow_with_all_known_fps_short_circuits_correctly(
        self,
        mock_confidence_config,
        mock_pre_process_node,
        mock_summarize_justifications_node,
        mock_write_results_node
    ):
        """
        Test workflow when ALL issues are known FPs (filter short-circuits all).

        Validates:
        - Investigation node is still called but does nothing for known FPs
        - Confidence scores use filter_confidence only
        - All scores are filter_confidence × 100%
        """
        # Create filter that marks everything as known FP
        async def filter_all_known_fps(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
            for per_issue in tracker.issues.values():
                per_issue.analysis_response.filter_confidence = 0.90
                per_issue.analysis_response.faiss_similarity_score = 0.95
                per_issue.analysis_response.is_final = FinalStatus.TRUE.value
                per_issue.analysis_response.investigation_result = CVEValidationStatus.FALSE_POSITIVE.value
            return tracker

        # Create investigation that respects is_final=TRUE (skips known FPs)
        async def investigate_skip_known_fps(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
            # Investigation should skip issues where is_final=TRUE
            for per_issue in tracker.issues.values():
                if per_issue.analysis_response.is_final != FinalStatus.TRUE.value:
                    # Would investigate, but all are is_final=TRUE
                    pass
            return tracker

        # Create calculate_metrics node using shared helper
        calculate_metrics_node = _create_real_confidence_metrics_node()

        # Build workflow
        workflow = build_sast_workflow_graph(
            pre_process_node=mock_pre_process_node,
            filter_node=AsyncMock(side_effect=filter_all_known_fps),
            investigate_node=AsyncMock(side_effect=investigate_skip_known_fps),
            summarize_justifications_node=mock_summarize_justifications_node,
            calculate_metrics_node=calculate_metrics_node,
            write_results_node=mock_write_results_node
        )

        # Execute
        result = await workflow.ainvoke({})

        # ASSERTIONS

        # All issues should have confidence = filter_confidence × 100%
        confidence_scores = result["metrics"]["confidence_scores"]
        for issue_id, per_issue in result["issues"].items():
            # Check score in metrics dict
            issue_score = confidence_scores["per_issue_scores"][issue_id]
            assert issue_score == pytest.approx(90.0)

            # Verify investigation was skipped
            assert per_issue.analysis_response.agent_confidence is None
            assert per_issue.investigation_tool_call_count == 0

    @pytest.mark.asyncio
    async def test_workflow_nodes_are_called_in_correct_order(
        self,
        mock_confidence_config,
        mock_pre_process_node,
        mock_filter_node,
        mock_investigate_node,
        mock_summarize_justifications_node,
        mock_write_results_node
    ):
        """
        Test that workflow nodes are called in the correct order.

        Validates execution order:
        Pre-Process → Filter → Investigation → Summarize → Calculate Metrics → Write Results
        """
        # Track call order
        call_order = []

        # Wrap nodes to track calls
        async def track_pre_process(state):
            call_order.append("pre_process")
            return await mock_pre_process_node(state)

        async def track_filter(tracker):
            call_order.append("filter")
            return await mock_filter_node(tracker)

        async def track_investigate(tracker):
            call_order.append("investigate")
            return await mock_investigate_node(tracker)

        async def track_summarize(tracker):
            call_order.append("summarize")
            return await mock_summarize_justifications_node(tracker)

        async def track_write(tracker):
            call_order.append("write_results")
            return await mock_write_results_node(tracker)

        # Create calculate_metrics using shared helper
        calculate_metrics_node_fn = _create_real_confidence_metrics_node()

        async def track_calculate_metrics(tracker):
            call_order.append("calculate_metrics")
            return await calculate_metrics_node_fn(tracker)

        # Build workflow
        workflow = build_sast_workflow_graph(
            pre_process_node=AsyncMock(side_effect=track_pre_process),
            filter_node=AsyncMock(side_effect=track_filter),
            investigate_node=AsyncMock(side_effect=track_investigate),
            summarize_justifications_node=AsyncMock(side_effect=track_summarize),
            calculate_metrics_node=AsyncMock(side_effect=track_calculate_metrics),
            write_results_node=AsyncMock(side_effect=track_write)
        )

        # Execute
        await workflow.ainvoke({})

        # Verify order
        expected_order = [
            "pre_process",
            "filter",
            "investigate",
            "summarize",
            "calculate_metrics",
            "write_results"
        ]

        assert call_order == expected_order, (
            f"Expected node execution order {expected_order}, got {call_order}"
        )
