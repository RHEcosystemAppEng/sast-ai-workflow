"""
Integration test for confidence scoring across the complete SAST workflow.

Tests the end-to-end flow:
  Filter → Agent Investigation → Confidence Calculation

Validates that:
1. agent_confidence is properly captured from the investigation subgraph
2. filter_confidence flows through from the filter node
3. investigation metrics (tool_calls, reanalysis, stop_reason) are tracked
4. final_confidence_score is calculated and within valid range (0-100%)
5. All confidence components contribute to the final score
"""

import pytest
from unittest.mock import Mock, MagicMock, patch
from pathlib import Path

from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, FinalStatus, CVEValidationStatus
from common.config import Config
from Utils.confidence_scoring import calculate_final_confidence, ConfidenceScoreBreakdown


# mock_confidence_config fixture imported from conftest.py

@pytest.fixture
def sample_issue():
    """Create a sample security issue for testing."""
    return Issue(
        id="TEST-001",
        issue_type="BUFFER_OVERFLOW",
        severity="high",
        issue_cwe="CWE-120",
        issue_label="Buffer overflow in strcpy",
        trace="test.c:42: strcpy(buffer, user_input);",
        file_path="src/test.c",
        line_number=42
    )


class TestConfidenceScoringWorkflow:
    """Integration tests for confidence scoring across full workflow."""

    def test_full_workflow_with_investigation_captures_all_confidence_components(
        self, mock_confidence_config, sample_issue
    ):
        """
        Test complete workflow: filter → investigation → confidence calculation.

        Validates that all confidence components are properly captured:
        - filter_confidence from filter node (0.75)
        - agent_confidence from investigation finalize_analysis (0.85)
        - evidence metrics (FAISS score, files, code snippets)
        - investigation metrics (tool calls, reanalysis, symbols, stop reason)
        - final_confidence_score is calculated and in valid range (0-100%)
        """
        # STEP 1: Simulate filter node output (filter decided to investigate)
        filter_response = AnalysisResponse(
            investigation_result=CVEValidationStatus.NEEDS_REVIEW.value,
            is_final=FinalStatus.FALSE.value,  # Not a known FP - needs investigation
            filter_confidence=0.75,  # Filter confidence in its decision
            faiss_similarity_score=0.82,  # Similarity to known issues
            justifications=["Potential buffer overflow detected"],
            prompt="Filter analysis prompt"
        )

        # STEP 2: Simulate investigation subgraph completing
        # The investigation would populate these fields in PerIssueData
        per_issue_data = PerIssueData(
            issue=sample_issue,
            analysis_response=filter_response,
            # Evidence metrics (from investigation research)
            fetched_files=["src/test.c", "src/utils.c", "include/string_utils.h"],
            found_symbols={"strcpy", "validate_input", "safe_copy"},
            # Investigation quality metrics (from orchestrator._update_tracker_from_result)
            investigation_tool_call_count=12,  # Number of tool calls made
            investigation_reanalysis_count=2,   # Reanalysis cycles
            investigation_stop_reason="approved"  # Investigation completed successfully
        )

        # STEP 3: Simulate finalize_analysis setting agent_confidence
        # This is what the investigation subgraph's analysis node should do
        per_issue_data.analysis_response.agent_confidence = 0.85  # Agent's confidence in verdict
        per_issue_data.analysis_response.investigation_result = CVEValidationStatus.TRUE_POSITIVE.value
        per_issue_data.analysis_response.is_final = FinalStatus.TRUE.value
        per_issue_data.analysis_response.justifications = [
            "Buffer overflow confirmed in strcpy call",
            "See test.c:42 for vulnerable code",
            "```c\nstrcpy(buffer, user_input);\n```",
            "CVE-2023-1234 similar vulnerability"
        ]

        # STEP 4: Calculate confidence score (what calculate_metrics node does)
        breakdown = calculate_final_confidence(per_issue_data, mock_confidence_config)

        # ASSERTIONS

        # 1. Verify breakdown structure
        assert isinstance(breakdown, ConfidenceScoreBreakdown)

        # 2. Verify filter confidence was captured
        assert breakdown.filter_confidence == pytest.approx(0.75)

        # 3. Verify agent confidence was captured from investigation
        assert breakdown.agent_confidence == pytest.approx(0.85)

        # 4. Verify evidence strength was calculated
        assert breakdown.evidence_strength > 0.0
        assert breakdown.evidence_strength <= 1.0
        assert breakdown.faiss_score == pytest.approx(0.82)
        assert breakdown.files_fetched_count == 3
        assert breakdown.evidence_count == 3  # 1 code block + 1 CVE + 1 file ref

        # 5. Verify investigation depth was calculated
        assert breakdown.investigation_depth > 0.0
        assert breakdown.investigation_depth <= 1.0
        assert breakdown.symbols_explored == 3
        assert breakdown.tool_calls == 12
        assert breakdown.reanalysis_count == 2
        assert breakdown.stop_reason == "approved"

        # 6. Verify final confidence is in valid range (0-100%)
        assert 0.0 <= breakdown.final_confidence <= 100.0

        # 7. Verify final confidence is a percentage (not 0-1 scale)
        assert breakdown.final_confidence > 1.0  # Should be in percentage

        # 8. Verify final confidence is reasonable given high component scores
        # With filter=0.75, agent=0.85, and good evidence/investigation metrics,
        # we expect a high final confidence score (>70%)
        assert breakdown.final_confidence >= 70.0, (
            f"Expected high confidence given strong components, got {breakdown.final_confidence:.1f}%"
        )

    def test_known_fp_short_circuit_uses_filter_confidence_directly(
        self, mock_confidence_config, sample_issue
    ):
        """
        Test known false positive path: filter identifies known FP and skips investigation.

        When is_final=TRUE from filter, agent_confidence and investigation metrics
        should be 0 since investigation never ran. Final confidence should be
        filter_confidence × 100%.
        """
        # STEP 1: Filter identifies this as a known FP (high FAISS similarity)
        filter_response = AnalysisResponse(
            investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
            is_final=FinalStatus.TRUE.value,  # Filter short-circuited - investigation skipped
            filter_confidence=0.92,  # High confidence in known FP match
            faiss_similarity_score=0.95,  # Very high similarity to known FP
            justifications=["Matches known false positive in vector DB"],
            prompt="Filter analysis prompt"
        )

        per_issue_data = PerIssueData(
            issue=sample_issue,
            analysis_response=filter_response,
            # No investigation metrics - investigation never ran
            fetched_files=[],
            found_symbols=set(),
            investigation_tool_call_count=0,
            investigation_reanalysis_count=0,
            investigation_stop_reason=None
        )

        # STEP 2: Calculate confidence (investigation was skipped)
        breakdown = calculate_final_confidence(per_issue_data, mock_confidence_config)

        # ASSERTIONS

        # 1. Final confidence should be filter_confidence × 100%
        assert breakdown.final_confidence == pytest.approx(92.0)  # 0.92 × 100

        # 2. Filter confidence should be set
        assert breakdown.filter_confidence == pytest.approx(0.92)

        # 3. Agent confidence should be 0 (investigation never ran)
        assert breakdown.agent_confidence == pytest.approx(0.0)

        # 4. Investigation metrics should be 0 (investigation never ran)
        assert breakdown.investigation_depth == pytest.approx(0.0)
        assert breakdown.evidence_strength == pytest.approx(0.0)

        # 5. Verify final score is in valid range
        assert 0.0 <= breakdown.final_confidence <= 100.0

    def test_low_confidence_investigation_with_max_iterations_stop(
        self, mock_confidence_config, sample_issue
    ):
        """
        Test investigation that hit iteration limit with inconclusive results.

        Validates that:
        - Stop reason "max_iterations" results in medium confidence (0.6)
        - Low agent confidence is reflected in final score
        - Final confidence is lower when investigation struggled
        """
        # STEP 1: Filter sends to investigation
        filter_response = AnalysisResponse(
            investigation_result=CVEValidationStatus.NEEDS_REVIEW.value,
            is_final=FinalStatus.FALSE.value,
            filter_confidence=0.60,
            faiss_similarity_score=0.55,
            justifications=["Uncertain - requires deeper investigation"]
        )

        # STEP 2: Investigation completes but hits iteration limit
        per_issue_data = PerIssueData(
            issue=sample_issue,
            analysis_response=filter_response,
            fetched_files=["src/test.c"],  # Only got 1 file
            found_symbols={"strcpy"},  # Only found 1 symbol
            investigation_tool_call_count=8,  # Some tool calls
            investigation_reanalysis_count=1,  # One reanalysis
            investigation_stop_reason="max_iterations"  # Hit limit
        )

        # STEP 3: Agent has low confidence in verdict
        per_issue_data.analysis_response.agent_confidence = 0.55
        per_issue_data.analysis_response.investigation_result = CVEValidationStatus.NEEDS_REVIEW.value
        per_issue_data.analysis_response.is_final = FinalStatus.TRUE.value
        per_issue_data.analysis_response.justifications = ["Insufficient evidence for definitive verdict"]

        # STEP 4: Calculate confidence
        breakdown = calculate_final_confidence(per_issue_data, mock_confidence_config)

        # ASSERTIONS

        # 1. Verify components are captured
        assert breakdown.filter_confidence == pytest.approx(0.60)
        assert breakdown.agent_confidence == pytest.approx(0.55)
        assert breakdown.stop_reason == "max_iterations"

        # 2. Verify final confidence is lower (all components are weaker)
        assert 0.0 <= breakdown.final_confidence <= 100.0

        # 3. Final confidence should be moderate (not high, not extremely low)
        # With filter=0.60, agent=0.55, limited evidence/investigation
        assert 40.0 <= breakdown.final_confidence <= 70.0, (
            f"Expected moderate confidence for inconclusive investigation, got {breakdown.final_confidence:.1f}%"
        )

    def test_confidence_score_stored_in_tracker(self, mock_confidence_config, sample_issue):
        """
        Test that final_confidence_score can be stored in PerIssueData.

        This simulates what the calculate_metrics node does:
        1. Calculate confidence breakdown
        2. Store final_confidence_score in per_issue_data
        """
        # Setup: Create a complete analysis
        per_issue_data = PerIssueData(
            issue=sample_issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=FinalStatus.TRUE.value,
                filter_confidence=0.80,
                agent_confidence=0.85,
                faiss_similarity_score=0.78,
                justifications=["Confirmed vulnerability"]
            ),
            fetched_files=["test.c"],
            found_symbols={"vulnerable_func"},
            investigation_tool_call_count=10,
            investigation_reanalysis_count=1,
            investigation_stop_reason="approved"
        )

        # Calculate confidence
        breakdown = calculate_final_confidence(per_issue_data, mock_confidence_config)

        # Store in PerIssueData (what calculate_metrics does)
        per_issue_data.final_confidence_score = breakdown.final_confidence

        # ASSERTIONS

        # 1. Verify it's stored as percentage
        assert per_issue_data.final_confidence_score is not None
        assert 0.0 <= per_issue_data.final_confidence_score <= 100.0

        # 2. Verify it matches the breakdown
        assert per_issue_data.final_confidence_score == breakdown.final_confidence

        # 3. Verify it's a reasonable score
        assert per_issue_data.final_confidence_score > 50.0


class TestConfidenceScoringEdgeCases:
    """Test edge cases and error conditions in confidence scoring."""

    def test_missing_agent_confidence_defaults_to_zero(self, mock_confidence_config, sample_issue):
        """
        Test that missing agent_confidence is handled gracefully.

        This can happen if the investigation node doesn't set it properly.
        The final score should still be calculated using other components.
        """
        per_issue_data = PerIssueData(
            issue=sample_issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=FinalStatus.TRUE.value,
                filter_confidence=0.75,
                # agent_confidence is None (not set)
                faiss_similarity_score=0.80
            )
        )

        breakdown = calculate_final_confidence(per_issue_data, mock_confidence_config)

        # Should handle missing agent_confidence gracefully
        assert breakdown.agent_confidence == pytest.approx(0.0)
        assert 0.0 <= breakdown.final_confidence <= 100.0

    def test_out_of_range_agent_confidence_is_clamped(self, mock_confidence_config, sample_issue):
        """
        Test that invalid agent_confidence values are clamped to [0, 1].

        Protects against LLM returning invalid confidence scores.
        """
        per_issue_data = PerIssueData(
            issue=sample_issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=FinalStatus.FALSE.value,  # Use FALSE to avoid short-circuit path
                filter_confidence=0.75,
                agent_confidence=1.5,  # Invalid: > 1.0
                faiss_similarity_score=0.80
            )
        )

        breakdown = calculate_final_confidence(per_issue_data, mock_confidence_config)

        # Should clamp to 1.0
        assert breakdown.agent_confidence == pytest.approx(1.0)
        assert 0.0 <= breakdown.final_confidence <= 100.0

    def test_minimal_data_produces_valid_score(self, mock_confidence_config, sample_issue):
        """
        Test that even with minimal data, a valid confidence score is produced.

        This ensures robustness when nodes fail to populate all fields.
        """
        per_issue_data = PerIssueData(
            issue=sample_issue,
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
                is_final=FinalStatus.TRUE.value
                # No confidence scores, no evidence metrics
            )
        )

        breakdown = calculate_final_confidence(per_issue_data, mock_confidence_config)

        # Should produce a valid score (likely low due to missing data)
        assert 0.0 <= breakdown.final_confidence <= 100.0

        # All components should be 0 or very low
        assert breakdown.filter_confidence == pytest.approx(0.0)
        assert breakdown.agent_confidence == pytest.approx(0.0)
