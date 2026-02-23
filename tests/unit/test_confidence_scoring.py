"""
Unit tests for confidence scoring utilities.
"""

import pytest
from dto.SASTWorkflowModels import PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus
from Utils.confidence_scoring import (
    calculate_evidence_strength,
    calculate_investigation_depth,
    calculate_final_confidence,
    inject_mock_confidence_data,
    calculate_aggregate_confidence_metrics,
    ConfidenceScoreBreakdown
)


class TestEvidenceStrength:
    """Test evidence strength calculation."""

    def test_evidence_strength_with_all_components(self):
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

        evidence_strength, details = calculate_evidence_strength(per_issue_data)

        # Verify score is calculated
        assert 0.0 <= evidence_strength <= 1.0
        assert details['faiss_score'] == 0.85
        assert details['files_fetched_count'] == 3
        assert details['evidence_count'] == 3  # 1 CVE + 1 file ref + 1 code block

    def test_evidence_strength_with_no_evidence(self):
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

        evidence_strength, details = calculate_evidence_strength(per_issue_data)

        # Should return 0.0 when no evidence
        assert evidence_strength == 0.0
        assert details['faiss_score'] == 0.0
        assert details['files_fetched_count'] == 0
        assert details['evidence_count'] == 0

    def test_evidence_strength_normalization(self):
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

        evidence_strength, details = calculate_evidence_strength(per_issue_data)

        # Files score should be capped at 1.0
        assert details['files_score'] == 1.0
        # Overall strength should still be <= 1.0
        assert evidence_strength <= 1.0


class TestInvestigationDepth:
    """Test investigation depth calculation."""

    def test_investigation_depth_with_symbols(self):
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
            found_symbols={"malloc", "free", "strcpy"},
            exploration_depth=3
        )

        depth_score, details = calculate_investigation_depth(per_issue_data)

        assert 0.0 <= depth_score <= 1.0
        assert details['symbols_explored'] == 3
        assert details['explicit_depth'] == 3

    def test_investigation_depth_no_exploration(self):
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

        depth_score, details = calculate_investigation_depth(per_issue_data)

        assert depth_score == 0.0
        assert details['symbols_explored'] == 0
        assert details['explicit_depth'] == 0


class TestFinalConfidence:
    """Test final confidence calculation."""

    def test_final_confidence_calculation(self):
        """Test complete confidence calculation with all components."""
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
            is_final=FinalStatus.TRUE.value,
            filter_confidence=0.9,
            agent_confidence=0.85,
            faiss_similarity_score=0.8,
            justifications=["CVE-2023-5678 found", "See test.c:500"]
        )

        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=analysis_response,
            fetched_files=["test.c", "lib.c"],
            found_symbols={"vulnerable_func"},
            exploration_depth=1
        )

        breakdown = calculate_final_confidence(per_issue_data)

        # Verify breakdown structure
        assert isinstance(breakdown, ConfidenceScoreBreakdown)
        assert 0.0 <= breakdown.final_confidence <= 1.0
        assert breakdown.filter_confidence == 0.9
        assert breakdown.agent_confidence == 0.85
        assert breakdown.evidence_strength > 0.0
        assert breakdown.investigation_depth > 0.0

        # Verify weighted formula is applied
        # Final = 0.20*filter + 0.50*agent + 0.20*evidence + 0.10*investigation
        expected = (
            0.20 * 0.9 +
            0.50 * 0.85 +
            0.20 * breakdown.evidence_strength +
            0.10 * breakdown.investigation_depth
        )
        assert abs(breakdown.final_confidence - expected) < 0.001

    def test_final_confidence_stored_in_per_issue_data(self):
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
                is_final=FinalStatus.TRUE.value,
                filter_confidence=0.85,
                agent_confidence=0.9,
                faiss_similarity_score=0.75
            )
        )

        # Calculate confidence
        breakdown = calculate_final_confidence(per_issue_data)

        # Store in PerIssueData (mimicking what calculate_metrics does)
        per_issue_data.final_confidence_score = breakdown.final_confidence

        # Verify it's stored correctly
        assert per_issue_data.final_confidence_score is not None
        assert 0.0 <= per_issue_data.final_confidence_score <= 1.0
        assert per_issue_data.final_confidence_score == breakdown.final_confidence

    def test_final_confidence_with_missing_components(self):
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

        breakdown = calculate_final_confidence(per_issue_data)

        # Should still return valid score (likely low due to missing data)
        assert 0.0 <= breakdown.final_confidence <= 1.0


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
        assert per_issue_data.analysis_response.agent_confidence == 0.9

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
        assert per_issue_data.analysis_response.agent_confidence == 0.7

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
                final_confidence=0.9,
                filter_confidence=0.9,
                agent_confidence=0.9,
                evidence_strength=0.8,
                investigation_depth=0.7
            ),
            "issue-2": ConfidenceScoreBreakdown(
                final_confidence=0.7,
                filter_confidence=0.7,
                agent_confidence=0.7,
                evidence_strength=0.6,
                investigation_depth=0.5
            ),
            "issue-3": ConfidenceScoreBreakdown(
                final_confidence=0.4,
                filter_confidence=0.5,
                agent_confidence=0.4,
                evidence_strength=0.3,
                investigation_depth=0.2
            )
        }

        aggregate = calculate_aggregate_confidence_metrics(breakdowns)

        assert aggregate['total_issues'] == 3
        assert aggregate['mean_confidence'] == pytest.approx((0.9 + 0.7 + 0.4) / 3, abs=0.01)
        assert aggregate['min_confidence'] == 0.4
        assert aggregate['max_confidence'] == 0.9
        assert aggregate['high_confidence_count'] == 1  # >= 0.8
        assert aggregate['medium_confidence_count'] == 1  # 0.5-0.8
        assert aggregate['low_confidence_count'] == 1  # < 0.5

    def test_aggregate_metrics_empty_input(self):
        """Test aggregate metrics with no issues."""
        aggregate = calculate_aggregate_confidence_metrics({})

        assert aggregate['mean_confidence'] == 0.0
        assert aggregate['min_confidence'] == 0.0
        assert aggregate['max_confidence'] == 0.0
        assert aggregate['total_issues'] == 0