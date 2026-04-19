"""
Data models for the pattern extraction pipeline.

ParsedFalsePositive: intermediate representation of a parsed SAST entry.
ExtractedPattern / PatternExtractionResponse: Pydantic models for LLM structured output.
"""

from dataclasses import dataclass
from typing import List, Optional

from pydantic import BaseModel, Field


@dataclass
class ParsedFalsePositive:
    """Represents one parsed SAST false positive entry from input data."""

    package_name: str
    issue_type: str
    cwe: Optional[str]
    error_trace: str
    source_code: Optional[str]
    analyst_justification: str
    verdict: str
    entry_index: int


class FalsePositiveIndicator(BaseModel):
    """A single indicator that contributes to false positive classification."""

    indicator_type: str = Field(
        description=(
            "Category of indicator: 'error_handling', 'type_safety', "
            "'bounds_checking', 'initialization', 'api_contract', "
            "'data_flow', 'other'"
        )
    )
    description: str = Field(
        description=(
            "Concise description of the indicator, e.g. "
            "'Return value of lseek checked against -1 before use'"
        )
    )


class MatchingCriterion(BaseModel):
    """A criterion for matching future issues to this pattern."""

    field: str = Field(
        description=(
            "Which field to match on: 'issue_type', 'cwe', "
            "'function_name', 'code_pattern', 'file_path_pattern', "
            "'error_trace_keyword'"
        )
    )
    value: str = Field(description="The value or regex pattern to match against")
    weight: float = Field(
        default=1.0,
        ge=0.0,
        le=1.0,
        description="Relative importance of this criterion (0.0-1.0)",
    )


class ExtractedPattern(BaseModel):
    """LLM-extracted pattern from a false positive SAST finding."""

    issue_type: str = Field(
        description="SAST issue type, e.g. 'INTEGER_OVERFLOW', 'UNINIT', 'RESOURCE_LEAK'"
    )
    cwe: Optional[str] = Field(default=None, description="CWE identifier, e.g. 'CWE-190'")
    pattern_summary: str = Field(
        description="One-sentence summary of why this class of finding is a false positive"
    )
    code_pattern: str = Field(
        description=(
            "Description of the code pattern that triggers the false positive, "
            "e.g. 'Unsigned-to-signed cast in offset argument to lseek, "
            "where negative result is explicitly checked'"
        )
    )
    file_pattern: Optional[str] = Field(
        default=None,
        description=(
            "File path pattern if this is specific to certain files/modules, "
            "e.g. '*/killrpath.c' or None if generic"
        ),
    )
    false_positive_indicators: List[FalsePositiveIndicator] = Field(
        description="Key indicators that make this a false positive"
    )
    matching_criteria: List[MatchingCriterion] = Field(
        description="Criteria for matching future SAST findings to this pattern"
    )
    analyst_reasoning_summary: str = Field(
        description="Summarized version of the human analyst's reasoning"
    )
    confidence: float = Field(
        ge=0.0,
        le=1.0,
        description=(
            "Confidence that this pattern generalizes beyond "
            "the specific instance (0.0-1.0)"
        ),
    )


class PatternExtractionResponse(BaseModel):
    """Structured LLM response containing one or more extracted patterns."""

    patterns: List[ExtractedPattern] = Field(
        description="List of extracted false positive patterns from the provided entries"
    )

    model_config = {"extra": "forbid"}
