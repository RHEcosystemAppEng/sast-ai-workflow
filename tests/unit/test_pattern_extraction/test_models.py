"""Tests for pattern extraction data models."""

import pytest
from pydantic import ValidationError

from pattern_extraction.models import (
    ExtractedPattern,
    FalsePositiveIndicator,
    MatchingCriterion,
    PatternExtractionResponse,
)


class TestExtractedPattern:
    def test__valid_fields_accepted(self):
        pattern = ExtractedPattern(
            issue_type="INTEGER_OVERFLOW",
            cwe="CWE-190",
            pattern_summary="Cast to signed type with proper error checking",
            code_pattern="Unsigned-to-signed cast where result is checked against -1",
            file_pattern=None,
            false_positive_indicators=[
                FalsePositiveIndicator(
                    indicator_type="error_handling",
                    description="Return value checked against -1",
                )
            ],
            matching_criteria=[
                MatchingCriterion(
                    field="issue_type",
                    value="INTEGER_OVERFLOW",
                    weight=0.9,
                )
            ],
            analyst_reasoning_summary="lseek returns -1 on negative offset",
            confidence=0.85,
        )

        assert pattern.issue_type == "INTEGER_OVERFLOW"
        assert pattern.cwe == "CWE-190"
        assert pattern.confidence == 0.85
        assert len(pattern.false_positive_indicators) == 1
        assert len(pattern.matching_criteria) == 1

    def test__optional_fields_default_to_none(self):
        pattern = ExtractedPattern(
            issue_type="UNINIT",
            pattern_summary="Variable initialized through non-obvious path",
            code_pattern="Variable set by prior function call not in trace",
            false_positive_indicators=[],
            matching_criteria=[],
            analyst_reasoning_summary="Analyst confirmed initialization",
            confidence=0.5,
        )

        assert pattern.cwe is None
        assert pattern.file_pattern is None

    def test__confidence_out_of_range_rejected(self):
        with pytest.raises(ValidationError):
            ExtractedPattern(
                issue_type="UNINIT",
                pattern_summary="test",
                code_pattern="test",
                false_positive_indicators=[],
                matching_criteria=[],
                analyst_reasoning_summary="test",
                confidence=1.5,
            )

    def test__matching_criterion_weight_out_of_range_rejected(self):
        with pytest.raises(ValidationError):
            MatchingCriterion(field="issue_type", value="UNINIT", weight=2.0)


class TestPatternExtractionResponse:
    def test__valid_response_with_patterns(self):
        response = PatternExtractionResponse(
            patterns=[
                ExtractedPattern(
                    issue_type="UNINIT",
                    pattern_summary="test",
                    code_pattern="test",
                    false_positive_indicators=[],
                    matching_criteria=[],
                    analyst_reasoning_summary="test",
                    confidence=0.7,
                )
            ]
        )

        assert len(response.patterns) == 1

    def test__extra_fields_rejected(self):
        with pytest.raises(ValidationError):
            PatternExtractionResponse(
                patterns=[],
                extra_field="should fail",
            )

    def test__empty_patterns_list_accepted(self):
        response = PatternExtractionResponse(patterns=[])
        assert response.patterns == []
