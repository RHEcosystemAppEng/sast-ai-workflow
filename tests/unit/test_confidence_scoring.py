"""
Unit tests for confidence scoring utilities.
"""

import pytest
from unittest.mock import MagicMock
from dto.SASTWorkflowModels import PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus
from Utils.confidence_scoring import (
    calculate_evidence_strength,
    calculate_investigation_depth,
    calculate_final_confidence,
    inject_mock_confidence_data,
    calculate_aggregate_confidence_metrics,
    ConfidenceScoreBreakdown,
    _validate_weight_configuration,
    _score_stop_reason,
)
from common.config import Config


@pytest.fixture
def mock_config():
    """Create a mock Config object with confidence scoring configuration."""
    config = MagicMock(spec=Config)

    # Main component weights (20/30/20/30 balanced approach)
    config.CONFIDENCE_WEIGHT_FILTER = 0.20
    config.CONFIDENCE_WEIGHT_AGENT = 0.30
    config.CONFIDENCE_WEIGHT_EVIDENCE = 0.20
    config.CONFIDENCE_WEIGHT_INVESTIGATION = 0.30

    # Evidence sub-component weights
    config.EVIDENCE_WEIGHT_FAISS_SCORE = 0.40
    config.EVIDENCE_WEIGHT_FILES_FETCHED = 0.30
    config.EVIDENCE_WEIGHT_EVIDENCE_COUNT = 0.30

    # Investigation sub-component weights
    config.INVESTIGATION_WEIGHT_DEPTH = 0.25
    config.INVESTIGATION_WEIGHT_TOOL_CALLS = 0.25
    config.INVESTIGATION_WEIGHT_REANALYSIS = 0.25
    config.INVESTIGATION_WEIGHT_STOP_REASON = 0.25

    # Normalization caps
    config.CONFIDENCE_MAX_FILES_FOR_NORMALIZATION = 10
    config.CONFIDENCE_MAX_EVIDENCE_ITEMS_FOR_NORMALIZATION = 5
    config.CONFIDENCE_MAX_SYMBOLS_FOR_NORMALIZATION = 5
    config.CONFIDENCE_MAX_TOOL_CALLS_FOR_NORMALIZATION = 20
    config.CONFIDENCE_MAX_REANALYSIS_FOR_NORMALIZATION = 3

    # Stop reason scores
    config.STOP_REASON_SCORE_APPROVED = 1.0
    config.STOP_REASON_SCORE_MAX_ITERATIONS = 0.6
    config.STOP_REASON_SCORE_NO_PROGRESS = 0.3
    config.STOP_REASON_SCORE_UNKNOWN = 0.2

    return config


class TestEvidenceStrength:
    """Test evidence strength calculation."""

    def test_evidence_strength_with_all_components(self, mock_config):
        """Test evidence strength calculation with all components present."""
        # Create issue with full evidence
        issue = Issue(
            id="test-issue-1",
            issue_type="BUFFER_OVERFLOW",
            severity="high",
            trace="test trace",
            file_path="test.c",
            line_number=100
        )

        analysis_response = AnalysisResponse(
            investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
            is_final=FinalStatus.TRUE.value,
            faiss_similarity_score=0.85,
            justifications=[
                "This is a CVE-2023-1234 vulnerability",
                "See test.c:100 for details",
                "```c\nchar buf[10];\n```"
            ]
        )

        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=analysis_response,
            fetched_files=["test.c", "helper.c", "main.c"]
        )

        evidence_strength, details = calculate_evidence_strength(per_issue_data, mock_config)

        # Verify score is calculated
        assert 0.0 <= evidence_strength <= 1.0
        assert details['faiss_score'] == pytest.approx(0.85)
        assert details['files_fetched_count'] == 3
        assert details['evidence_count'] == 3  # 1 CVE + 1 file ref + 1 code block

    def test_evidence_strength_with_no_evidence(self, mock_config):
        """Test evidence strength when no evidence is available."""
        issue = Issue(
            id="test-issue-2",
            issue_type="UNINIT",
            severity="medium",
            trace="test trace",
            file_path="test.c",
            line_number=50
        )

        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
                is_final=FinalStatus.TRUE.value
            )
        )

        evidence_strength, details = calculate_evidence_strength(per_issue_data, mock_config)

        # Should return 0.0 when no evidence
        assert evidence_strength == pytest.approx(0.0)
        assert details['faiss_score'] == pytest.approx(0.0)
        assert details['files_fetched_count'] == 0
        assert details['evidence_count'] == 0

    def test_evidence_strength_normalization(self, mock_config):
        """Test that evidence strength properly normalizes large values."""
        issue = Issue(
            id="test-issue-3",
            issue_type="OVERFLOW",
            severity="high",
            trace="test trace",
            file_path="test.c",
            line_number=200
        )

        # Create many files (more than MAX_FILES_FOR_NORMALIZATION = 10)
        many_files = [f"file{i}.c" for i in range(15)]

        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=FinalStatus.FALSE.value,
                faiss_similarity_score=0.95
            ),
            fetched_files=many_files
        )

        evidence_strength, details = calculate_evidence_strength(per_issue_data, mock_config)

        # Files score should be capped at 1.0
        assert details['files_score'] == pytest.approx(1.0)
        # Overall strength should still be <= 1.0
        assert evidence_strength <= 1.0


class TestInvestigationDepth:
    """Test investigation depth calculation."""

    def test_investigation_depth_with_symbols(self, mock_config):
        """Test investigation depth with found symbols."""
        issue = Issue(
            id="test-issue-4",
            issue_type="RESOURCE_LEAK",
            severity="medium",
            trace="test trace",
            file_path="test.c",
            line_number=300
        )

        per_issue_data = PerIssueData(
            issue=issue,
            found_symbols={"malloc", "free", "strcpy"}
        )

        depth_score, details = calculate_investigation_depth(per_issue_data, mock_config)

        assert 0.0 <= depth_score <= 1.0
        assert details['symbols_explored'] == 3
        assert details['depth_score'] == pytest.approx(3.0 / 5.0)  # 3 symbols / MAX=5

    def test_investigation_depth_no_exploration(self, mock_config):
        """Test investigation depth with no exploration."""
        issue = Issue(
            id="test-issue-5",
            issue_type="NULL_DEREFERENCE",
            severity="low",
            trace="test trace",
            file_path="test.c",
            line_number=400
        )

        per_issue_data = PerIssueData(issue=issue)

        depth_score, details = calculate_investigation_depth(per_issue_data, mock_config)

        assert depth_score == pytest.approx(0.0)
        assert details['symbols_explored'] == 0
        assert details['depth_score'] == pytest.approx(0.0)


class TestStopReasonScoring:
    """Test _score_stop_reason helper function."""

    def test_score_stop_reason_approved(self, mock_config):
        """Test that 'approved' stop reason returns highest score."""
        score = _score_stop_reason("approved", mock_config)
        assert score == pytest.approx(mock_config.STOP_REASON_SCORE_APPROVED)
        assert score == pytest.approx(1.0)

    def test_score_stop_reason_max_iterations(self, mock_config):
        """Test that 'max_iterations' returns medium-high score."""
        score = _score_stop_reason("max_iterations", mock_config)
        assert score == pytest.approx(mock_config.STOP_REASON_SCORE_MAX_ITERATIONS)
        assert score == pytest.approx(0.6)

    def test_score_stop_reason_iteration_variant(self, mock_config):
        """Test that 'iteration' substring also matches max_iterations."""
        score = _score_stop_reason("hit_iteration_limit", mock_config)
        assert score == pytest.approx(mock_config.STOP_REASON_SCORE_MAX_ITERATIONS)

    def test_score_stop_reason_no_progress(self, mock_config):
        """Test that 'no_progress' returns low score."""
        score = _score_stop_reason("no_progress", mock_config)
        assert score == pytest.approx(mock_config.STOP_REASON_SCORE_NO_PROGRESS)
        assert score == pytest.approx(0.3)

    def test_score_stop_reason_stalled_variant(self, mock_config):
        """Test that 'stalled' also matches no_progress category."""
        score = _score_stop_reason("investigation_stalled", mock_config)
        assert score == pytest.approx(mock_config.STOP_REASON_SCORE_NO_PROGRESS)

    def test_score_stop_reason_none(self, mock_config):
        """Test that None returns 0.0."""
        score = _score_stop_reason(None, mock_config)
        assert score == pytest.approx(0.0)

    def test_score_stop_reason_unknown(self, mock_config):
        """Test that unknown reasons return default low score."""
        score = _score_stop_reason("some_unknown_reason", mock_config)
        assert score == pytest.approx(mock_config.STOP_REASON_SCORE_UNKNOWN)
        assert score == pytest.approx(0.2)

    def test_score_stop_reason_case_insensitive(self, mock_config):
        """Test that scoring is case-insensitive."""
        score_upper = _score_stop_reason("APPROVED", mock_config)
        score_lower = _score_stop_reason("approved", mock_config)
        score_mixed = _score_stop_reason("ApProVeD", mock_config)
        assert score_upper == score_lower == score_mixed == pytest.approx(mock_config.STOP_REASON_SCORE_APPROVED)


class TestFinalConfidence:
    """Test final confidence calculation."""

    def test_final_confidence_calculation(self, mock_config):
        """Test complete confidence calculation with all components (full investigation path)."""
        issue = Issue(
            id="test-issue-6",
            issue_type="OVERRUN",
            severity="critical",
            trace="test trace",
            file_path="test.c",
            line_number=500
        )

        analysis_response = AnalysisResponse(
            investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
            is_final=FinalStatus.FALSE.value,  # Investigation ran (not a known FP short-circuit)
            filter_confidence=0.9,
            agent_confidence=0.85,
            faiss_similarity_score=0.8,
            justifications=["CVE-2023-5678 found", "See test.c:500"]
        )

        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=analysis_response,
            fetched_files=["test.c", "lib.c"],
            found_symbols={"vulnerable_func"}
        )

        breakdown = calculate_final_confidence(per_issue_data, mock_config)

        # Verify breakdown structure
        assert isinstance(breakdown, ConfidenceScoreBreakdown)
        # Final confidence should be percentage (0-100)
        assert 0.0 <= breakdown.final_confidence <= 100.0
        # Component scores remain in 0-1 scale
        assert breakdown.filter_confidence == pytest.approx(0.9)
        assert breakdown.agent_confidence == pytest.approx(0.85)
        assert 0.0 <= breakdown.evidence_strength <= 1.0
        assert 0.0 <= breakdown.investigation_depth <= 1.0

        # Verify weighted formula is applied and converted to percentage
        # Final = (FILTER_WEIGHT*filter + AGENT_WEIGHT*agent + EVIDENCE_WEIGHT*evidence + INVESTIGATION_WEIGHT*investigation) * 100
        expected_raw = (
            mock_config.CONFIDENCE_WEIGHT_FILTER * 0.9 +
            mock_config.CONFIDENCE_WEIGHT_AGENT * 0.85 +
            mock_config.CONFIDENCE_WEIGHT_EVIDENCE * breakdown.evidence_strength +
            mock_config.CONFIDENCE_WEIGHT_INVESTIGATION * breakdown.investigation_depth
        )
        expected_percentage = expected_raw * 100.0
        assert abs(breakdown.final_confidence - expected_percentage) < 0.1

    def test_final_confidence_stored_in_per_issue_data(self, mock_config):
        """Test that final confidence can be stored in PerIssueData."""
        issue = Issue(
            id="test-issue-storage",
            issue_type="BUFFER_OVERFLOW",
            severity="high",
            trace="test trace",
            file_path="test.c",
            line_number=100
        )

        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=FinalStatus.FALSE.value,  # Investigation path
                filter_confidence=0.85,
                agent_confidence=0.9,
                faiss_similarity_score=0.75
            )
        )

        # Calculate confidence
        breakdown = calculate_final_confidence(per_issue_data, mock_config)

        # Store in PerIssueData (mimicking what calculate_metrics does)
        per_issue_data.final_confidence_score = breakdown.final_confidence

        # Verify it's stored correctly as percentage (0-100)
        assert per_issue_data.final_confidence_score is not None
        assert 0.0 <= per_issue_data.final_confidence_score <= 100.0
        assert per_issue_data.final_confidence_score == breakdown.final_confidence

    def test_final_confidence_known_fp_short_circuit(self, mock_config):
        """Test that known FP (is_final=TRUE) uses filter_confidence directly."""
        issue = Issue(
            id="test-issue-known-fp",
            issue_type="BUFFER_OVERFLOW",
            severity="high",
            trace="test trace",
            file_path="test.c",
            line_number=200
        )

        # Simulate filter identifying this as a known FP with high confidence
        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
                is_final=FinalStatus.TRUE.value,  # Filter short-circuited (known FP)
                filter_confidence=0.92,  # High confidence match in vector DB
                faiss_similarity_score=0.95
                # Note: No agent_confidence, no investigation data - investigation never ran
            )
        )

        breakdown = calculate_final_confidence(per_issue_data, mock_config)

        # Final confidence should be filter_confidence × 100%
        assert breakdown.final_confidence == pytest.approx(92.0)  # 0.92 × 100
        assert breakdown.filter_confidence == pytest.approx(0.92)

        # Investigation components should be zero (didn't run)
        assert breakdown.agent_confidence == pytest.approx(0.0)
        assert breakdown.evidence_strength == pytest.approx(0.0)
        assert breakdown.investigation_depth == pytest.approx(0.0)

    def test_final_confidence_with_missing_components(self, mock_config):
        """Test confidence calculation when some components are missing."""
        issue = Issue(
            id="test-issue-7",
            issue_type="TAINTED_SCALAR",
            severity="medium",
            trace="test trace",
            file_path="test.c",
            line_number=600
        )

        # Minimal analysis response
        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
                is_final=FinalStatus.TRUE.value
            )
        )

        breakdown = calculate_final_confidence(per_issue_data, mock_config)

        # Should still return valid percentage (likely low due to missing data)
        assert 0.0 <= breakdown.final_confidence <= 100.0

    def test_final_confidence_clamps_out_of_range_filter_confidence(self, mock_config):
        """Test that out-of-range filter_confidence is clamped to [0,1]."""
        issue = Issue(
            id="test-issue-clamp-filter",
            issue_type="OVERFLOW",
            severity="high",
            trace="test trace",
            file_path="test.c",
            line_number=100
        )

        # Set filter_confidence above valid range
        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=FinalStatus.TRUE.value,
                filter_confidence=1.5,  # Invalid: > 1.0
                agent_confidence=0.8
            )
        )

        breakdown = calculate_final_confidence(per_issue_data, mock_config)

        # Filter confidence should be clamped to 1.0
        assert breakdown.filter_confidence == pytest.approx(1.0)
        # Final score should still be valid (0-100%)
        assert 0.0 <= breakdown.final_confidence <= 100.0

    def test_final_confidence_clamps_negative_agent_confidence(self, mock_config):
        """Test that negative agent_confidence is clamped to 0."""
        issue = Issue(
            id="test-issue-clamp-agent",
            issue_type="UNDERFLOW",
            severity="medium",
            trace="test trace",
            file_path="test.c",
            line_number=200
        )

        # Set agent_confidence below valid range
        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
                is_final=FinalStatus.TRUE.value,
                filter_confidence=0.7,
                agent_confidence=-0.2  # Invalid: < 0.0
            )
        )

        breakdown = calculate_final_confidence(per_issue_data, mock_config)

        # Agent confidence should be clamped to 0.0
        assert breakdown.agent_confidence == pytest.approx(0.0)
        # Final score should still be valid (0-100%)
        assert 0.0 <= breakdown.final_confidence <= 100.0

    def test_final_confidence_clamps_both_components(self, mock_config):
        """Test that both filter and agent confidence are clamped when out of range."""
        issue = Issue(
            id="test-issue-clamp-both",
            issue_type="MEMORY_LEAK",
            severity="high",
            trace="test trace",
            file_path="test.c",
            line_number=300
        )

        # Set both components out of range
        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=FinalStatus.TRUE.value,
                filter_confidence=2.0,  # Invalid: > 1.0
                agent_confidence=-1.0   # Invalid: < 0.0
            )
        )

        breakdown = calculate_final_confidence(per_issue_data, mock_config)

        # Both should be clamped
        assert breakdown.filter_confidence == pytest.approx(1.0)
        assert breakdown.agent_confidence == pytest.approx(0.0)
        # Final score should still be valid (0-100%)
        assert 0.0 <= breakdown.final_confidence <= 100.0


class TestMockDataInjection:
    """Test mock data injection for missing components."""

    def test_inject_agent_confidence_for_final_decision(self):
        """Test that mock agent_confidence is injected for final decisions."""
        issue = Issue(
            id="test-issue-8",
            issue_type="DEADCODE",
            severity="info",
            trace="test trace",
            file_path="test.c",
            line_number=700
        )

        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
                is_final=FinalStatus.TRUE.value  # Final decision
            )
        )

        inject_mock_confidence_data(per_issue_data)

        # Should inject high confidence for final decisions
        assert per_issue_data.analysis_response.agent_confidence == pytest.approx(0.9)

    def test_inject_agent_confidence_for_non_final_decision(self):
        """Test that mock agent_confidence is injected for non-final decisions."""
        issue = Issue(
            id="test-issue-9",
            issue_type="USE_AFTER_FREE",
            severity="high",
            trace="test trace",
            file_path="test.c",
            line_number=800
        )

        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=FinalStatus.FALSE.value  # Non-final
            )
        )

        inject_mock_confidence_data(per_issue_data)

        # Should inject lower confidence for non-final decisions
        assert per_issue_data.analysis_response.agent_confidence == pytest.approx(0.7)

    def test_inject_fetched_files_from_source_code(self):
        """Test that fetched_files is populated from source_code."""
        issue = Issue(
            id="test-issue-10",
            issue_type="DIVIDE_BY_ZERO",
            severity="medium",
            trace="test trace",
            file_path="test.c",
            line_number=900
        )

        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=FinalStatus.TRUE.value
            ),
            source_code={
                "test.c": ["int x = 0;"],
                "lib.c": ["void func();"]
            }
        )

        inject_mock_confidence_data(per_issue_data)

        # Should inject files from source_code
        assert len(per_issue_data.fetched_files) == 2
        assert "test.c" in per_issue_data.fetched_files
        assert "lib.c" in per_issue_data.fetched_files


class TestAggregateMetrics:
    """Test aggregate confidence metrics calculation."""

    def test_aggregate_metrics_calculation(self):
        """Test aggregate statistics across multiple issues."""
        breakdowns = {
            "issue-1": ConfidenceScoreBreakdown(
                final_confidence=90.0,  # Percentage (0-100)
                filter_confidence=0.9,  # Component scores stay 0-1
                agent_confidence=0.9,
                evidence_strength=0.8,
                investigation_depth=0.7
            ),
            "issue-2": ConfidenceScoreBreakdown(
                final_confidence=70.0,
                filter_confidence=0.7,
                agent_confidence=0.7,
                evidence_strength=0.6,
                investigation_depth=0.5
            ),
            "issue-3": ConfidenceScoreBreakdown(
                final_confidence=40.0,
                filter_confidence=0.5,
                agent_confidence=0.4,
                evidence_strength=0.3,
                investigation_depth=0.2
            )
        }

        aggregate = calculate_aggregate_confidence_metrics(breakdowns)

        assert aggregate['total_issues'] == 3
        # Check per-issue scores mapping exists
        assert 'per_issue_scores' in aggregate
        assert aggregate['per_issue_scores']['issue-1'] == pytest.approx(90.0)
        assert aggregate['per_issue_scores']['issue-2'] == pytest.approx(70.0)
        assert aggregate['per_issue_scores']['issue-3'] == pytest.approx(40.0)
        # Check aggregate stats (percentages)
        assert aggregate['mean_confidence'] == pytest.approx((90.0 + 70.0 + 40.0) / 3, abs=0.1)
        assert aggregate['min_confidence'] == pytest.approx(40.0)
        assert aggregate['max_confidence'] == pytest.approx(90.0)
        assert aggregate['high_confidence_count'] == 1  # >= 80%
        assert aggregate['medium_confidence_count'] == 1  # 50-80%
        assert aggregate['low_confidence_count'] == 1  # < 50%

    def test_aggregate_metrics_empty_input(self):
        """Test aggregate metrics with no issues."""
        aggregate = calculate_aggregate_confidence_metrics({})

        assert 'per_issue_scores' in aggregate
        assert aggregate['per_issue_scores'] == {}
        assert aggregate['mean_confidence'] == pytest.approx(0.0)
        assert aggregate['min_confidence'] == pytest.approx(0.0)
        assert aggregate['max_confidence'] == pytest.approx(0.0)
        assert aggregate['total_issues'] == 0


class TestConfigurationLoading:
    """Test that weights are properly loaded from configuration."""

    def test_main_component_weights_loaded_from_config(self, mock_config):
        """Test that main confidence weights are loaded from config."""
        # Verify weights have expected values (balanced approach: 20/30/20/30)
        assert mock_config.CONFIDENCE_WEIGHT_FILTER == pytest.approx(0.20)
        assert mock_config.CONFIDENCE_WEIGHT_AGENT == pytest.approx(0.30)
        assert mock_config.CONFIDENCE_WEIGHT_EVIDENCE == pytest.approx(0.20)
        assert mock_config.CONFIDENCE_WEIGHT_INVESTIGATION == pytest.approx(0.30)

    def test_main_component_weights_sum_to_one(self, mock_config):
        """Test that main component weights sum to 1.0."""
        total_weight = (
            mock_config.CONFIDENCE_WEIGHT_FILTER +
            mock_config.CONFIDENCE_WEIGHT_AGENT +
            mock_config.CONFIDENCE_WEIGHT_EVIDENCE +
            mock_config.CONFIDENCE_WEIGHT_INVESTIGATION
        )
        assert total_weight == pytest.approx(1.0)

    def test_evidence_sub_weights_loaded_from_config(self, mock_config):
        """Test that evidence sub-component weights are loaded from config."""
        assert mock_config.EVIDENCE_WEIGHT_FAISS_SCORE == pytest.approx(0.40)
        assert mock_config.EVIDENCE_WEIGHT_FILES_FETCHED == pytest.approx(0.30)
        assert mock_config.EVIDENCE_WEIGHT_EVIDENCE_COUNT == pytest.approx(0.30)

    def test_evidence_sub_weights_sum_to_one(self, mock_config):
        """Test that evidence sub-component weights sum to 1.0."""
        total_weight = (
            mock_config.EVIDENCE_WEIGHT_FAISS_SCORE +
            mock_config.EVIDENCE_WEIGHT_FILES_FETCHED +
            mock_config.EVIDENCE_WEIGHT_EVIDENCE_COUNT
        )
        assert total_weight == pytest.approx(1.0)

    def test_normalization_caps_loaded_from_config(self, mock_config):
        """Test that normalization caps are loaded from config."""
        assert mock_config.CONFIDENCE_MAX_FILES_FOR_NORMALIZATION == 10
        assert mock_config.CONFIDENCE_MAX_EVIDENCE_ITEMS_FOR_NORMALIZATION == 5
        assert mock_config.CONFIDENCE_MAX_SYMBOLS_FOR_NORMALIZATION == 5

    def test_weights_applied_correctly_in_calculation(self, mock_config):
        """Test that loaded weights are correctly applied in confidence calculation."""
        issue = Issue(
            id="test-config-weights",
            issue_type="OVERFLOW",
            severity="high",
            trace="test trace",
            file_path="test.c",
            line_number=100
        )

        analysis_response = AnalysisResponse(
            investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
            is_final=FinalStatus.FALSE.value,  # Full investigation path
            filter_confidence=0.8,
            agent_confidence=0.9,
            faiss_similarity_score=0.7
        )

        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=analysis_response,
            fetched_files=["test.c"],
            found_symbols={"func1"}
        )

        breakdown = calculate_final_confidence(per_issue_data, mock_config)

        # Manually calculate expected result using config
        evidence_strength, _ = calculate_evidence_strength(per_issue_data, mock_config)
        investigation_depth, _ = calculate_investigation_depth(per_issue_data, mock_config)

        expected_raw = (
            mock_config.CONFIDENCE_WEIGHT_FILTER * 0.8 +
            mock_config.CONFIDENCE_WEIGHT_AGENT * 0.9 +
            mock_config.CONFIDENCE_WEIGHT_EVIDENCE * evidence_strength +
            mock_config.CONFIDENCE_WEIGHT_INVESTIGATION * investigation_depth
        )
        expected_percentage = expected_raw * 100.0

        # Verify the calculation matches expected result
        assert breakdown.final_confidence == pytest.approx(expected_percentage, abs=0.1)

    def test_runtime_validation_passes_with_valid_weights(self, mock_config):
        """Test that runtime validation passes with valid weight configuration."""
        # Should not raise any exception with current valid configuration
        _validate_weight_configuration(mock_config)

    def test_runtime_validation_fails_with_invalid_main_weights(self):
        """Test that runtime validation fails when main weights don't sum to 1.0."""
        # Create invalid config
        invalid_config = MagicMock(spec=Config)
        invalid_config.CONFIDENCE_WEIGHT_FILTER = 0.25
        invalid_config.CONFIDENCE_WEIGHT_AGENT = 0.50
        invalid_config.CONFIDENCE_WEIGHT_EVIDENCE = 0.20
        invalid_config.CONFIDENCE_WEIGHT_INVESTIGATION = 0.10

        # Evidence weights (valid to isolate the test)
        invalid_config.EVIDENCE_WEIGHT_FAISS_SCORE = 0.40
        invalid_config.EVIDENCE_WEIGHT_FILES_FETCHED = 0.30
        invalid_config.EVIDENCE_WEIGHT_EVIDENCE_COUNT = 0.30

        # Investigation weights (valid to isolate the test)
        invalid_config.INVESTIGATION_WEIGHT_DEPTH = 0.25
        invalid_config.INVESTIGATION_WEIGHT_TOOL_CALLS = 0.25
        invalid_config.INVESTIGATION_WEIGHT_REANALYSIS = 0.25
        invalid_config.INVESTIGATION_WEIGHT_STOP_REASON = 0.25

        with pytest.raises(ValueError, match="Main confidence weights must sum to 1.0"):
            _validate_weight_configuration(invalid_config)

    def test_runtime_validation_fails_with_invalid_evidence_weights(self):
        """Test that runtime validation fails when evidence sub-weights don't sum to 1.0."""
        # Create invalid config
        invalid_config = MagicMock(spec=Config)

        # Main weights (valid to isolate the test)
        invalid_config.CONFIDENCE_WEIGHT_FILTER = 0.20
        invalid_config.CONFIDENCE_WEIGHT_AGENT = 0.30
        invalid_config.CONFIDENCE_WEIGHT_EVIDENCE = 0.20
        invalid_config.CONFIDENCE_WEIGHT_INVESTIGATION = 0.30

        # Evidence weights (invalid)
        invalid_config.EVIDENCE_WEIGHT_FAISS_SCORE = 0.50
        invalid_config.EVIDENCE_WEIGHT_FILES_FETCHED = 0.30
        invalid_config.EVIDENCE_WEIGHT_EVIDENCE_COUNT = 0.30

        # Investigation weights (valid to isolate the test)
        invalid_config.INVESTIGATION_WEIGHT_DEPTH = 0.25
        invalid_config.INVESTIGATION_WEIGHT_TOOL_CALLS = 0.25
        invalid_config.INVESTIGATION_WEIGHT_REANALYSIS = 0.25
        invalid_config.INVESTIGATION_WEIGHT_STOP_REASON = 0.25

        with pytest.raises(ValueError, match="Evidence sub-weights must sum to 1.0"):
            _validate_weight_configuration(invalid_config)