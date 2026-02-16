"""Schemas for investigation subgraph."""

from typing import Annotated, Dict, List, Literal, Optional, Sequence, TypedDict

from langchain_core.messages import BaseMessage
from pydantic import BaseModel, Field


class AnalysisResultOutput(BaseModel):
    """Structured output from Analysis LLM."""

    verdict: Literal["FALSE_POSITIVE", "TRUE_POSITIVE", "NEEDS_REVIEW"] = Field(
        description=(
            "Analysis verdict:\n"
            "- FALSE_POSITIVE: Issue is a false alarm (proven safe)\n"
            "- TRUE_POSITIVE: Issue is a real vulnerability (proven unsafe)\n"
            "- NEEDS_REVIEW: Cannot determine yet, need more code"
        )
    )
    reasoning: str = Field(
        description=(
            "Detailed analysis narrative explaining:\n"
            "- What code was examined\n"
            "- What execution paths were traced\n"
            "- Why the verdict was reached\n"
            "- What security controls were found (if any)"
        )
    )
    confidence: Literal["HIGH", "MEDIUM", "LOW"] = Field(
        default="MEDIUM", description="Confidence in the verdict"
    )
    justifications: List[str] = Field(
        default_factory=list,
        description="List of key findings that support the verdict (3-5 bullet points)",
    )


class InvestigationState(TypedDict):
    """State for investigation subgraph."""

    issue_id: str
    issue_cwe: str
    issue_description: str
    initial_code: str

    # Research phase
    research_messages: Annotated[Sequence[BaseMessage], "Messages from research agent"]
    gathered_code: str  # Accumulated code from research
    fetched_files: Dict[str, List[str]]  # Track successful fetches by tool name
    tool_call_history: List[str]  # Track all tool calls with parameters to avoid retrying

    # Analysis phase
    analysis: str  # Detailed analysis text
    analysis_prompt: str  # The prompt used for analysis (needed for summarization)
    proposed_verdict: str  # TRUE_POSITIVE or FALSE_POSITIVE
    justifications: List[str]
    confidence: str  # HIGH, MEDIUM, LOW

    # Evaluation phase
    evaluation_result: str  # APPROVED, NEEDS_MORE_RESEARCH, INSUFFICIENT_DATA
    evaluation_feedback: str  # Feedback on what's missing
    required_information: List[str]  # What needs to be gathered

    # Circuit breaker tracking
    evaluation_rejection_streak: int  # Count consecutive NEEDS_MORE_RESEARCH
    no_progress_streak: int  # Count iterations with no new code
    previous_code_length: int  # Track code growth
    stop_reason: Optional[str]  # Why investigation ended

    # Control
    iteration: int
    max_iterations: int
    is_complete: bool
    needs_reanalysis: bool  # Route back to analysis with feedback (no new research needed)
