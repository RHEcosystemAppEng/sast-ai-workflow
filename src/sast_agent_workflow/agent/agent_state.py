"""
Agent state schema for per-issue SAST investigation.

This module defines the state models used by the ReAct agent for investigating
individual SAST findings. The agent state is separate from the batch workflow
tracker (SASTWorkflowTracker) and manages a single issue investigation.
"""

from typing import Dict, List, Literal, Optional

from pydantic import BaseModel, Field

from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse


class ToolError(BaseModel):
    """
    Captures tool execution failures for error recovery.

    When a tool fails, this error is stored in the agent state so the agent
    can see what went wrong and try a different approach.
    """

    tool_name: str = Field(description="Name of the tool that failed")
    error_message: str = Field(description="Error message from the tool")
    attempted_args: dict = Field(description="Arguments that were passed to the tool")
    timestamp: str = Field(description="ISO format timestamp of the error")


class ProjectContext(BaseModel):
    """
    Shared project metadata initialized once per workflow.

    This context is discovered at the start of the workflow and reused across
    all issue investigations. Helps the agent understand the project structure
    and make informed decisions about where to search for code.
    """

    structure: Dict[str, List[str]] = Field(
        default_factory=dict,
        description="Directory tree (3 levels deep), e.g., {'app/': ['views.py', 'models.py']}",
    )
    security_files: List[str] = Field(
        default_factory=list,
        description="Files matching security patterns (sanitize|validate|clean|escape)",
    )
    frameworks: List[str] = Field(
        default_factory=list,
        description="Detected frameworks (Django, Flask, Rails, Express, etc.)",
    )


class ExplorationGap(BaseModel):
    """
    Represents an area of the codebase not yet explored (from process audit).
    """

    area: Literal["global middleware", "config", "framework defaults", "validators", "other"] = (
        Field(description="Category of the exploration gap")
    )
    reason: str = Field(description="Why this area needs checking")
    suggested_files: List[str] = Field(
        default_factory=list, description="Specific files from project_context to investigate"
    )


class RequiredCode(BaseModel):
    """
    Specific code that needs to be fetched to verify analysis claims (from logic audit).
    """

    expression_name: str = Field(description="Function or class name")
    file_path: str = Field(description="Path where this code is located")
    reason: str = Field(description="Why this code is needed")


class ComprehensiveEvaluationResponse(BaseModel):
    """
    Response from comprehensive_evaluation combining process and logic audits.
    """

    # Final decision
    is_final: str = Field(description="'TRUE' if investigation complete, 'FALSE' if more needed")
    verdict_confidence: str = Field(description="'high', 'medium', or 'low'")

    # Process audit (reflection) results
    exploration_gaps: List[ExplorationGap] = Field(
        default_factory=list, description="Areas of codebase not yet explored"
    )
    has_exploration_gaps: bool = Field(description="True if exploration incomplete")

    # Logic audit (evaluation) results
    logic_gaps: List[str] = Field(
        default_factory=list, description="Unverified claims or missing evidence in analysis"
    )
    required_code: List[RequiredCode] = Field(
        default_factory=list, description="Specific functions/classes that need to be examined"
    )

    # Unified guidance for agent
    recommendations: List[str] = Field(
        default_factory=list,
        description="Prioritized next steps (exploration gaps first, then specific code)",
    )
    justifications: List[str] = Field(
        default_factory=list, description="Reasoning for why investigation is/isn't final"
    )


class InvestigationContext(BaseModel):
    """
    Code and symbols gathered during investigation.

    This tracks all the context the agent has fetched to understand the issue.
    """

    fetched_files: Dict[str, List[str]] = Field(
        default_factory=dict,
        description="Code gathered during investigation (file_path -> code lines)",
    )
    found_symbols: List[str] = Field(
        default_factory=list,
        description="Symbols already fetched (prevents duplicate fetches, deduped on add)",
    )


class AnalysisState(BaseModel):
    """
    Results from security analysis and evaluation.

    This tracks the agent's investigative findings and verdicts.
    """

    investigation_result: Optional[AnalysisResponse] = Field(
        default=None, description="Latest security analysis from analyze_issue tool"
    )
    verdict: Optional[Literal["TRUE_POSITIVE", "FALSE_POSITIVE", "NEEDS_HUMAN_REVIEW"]] = Field(
        default=None, description="Final verdict (set when is_final=TRUE)"
    )
    evaluation_result: Optional[ComprehensiveEvaluationResponse] = Field(
        default=None, description="Latest comprehensive evaluation feedback"
    )


class ErrorState(BaseModel):
    """
    Error tracking and recovery state.

    Tracks tool failures so the agent can see what went wrong and try
    alternative approaches.
    """

    errors: List[ToolError] = Field(
        default_factory=list, description="Complete history of all tool errors"
    )
    last_error: Optional[ToolError] = Field(
        default=None, description="Most recent error (shown to agent for recovery)"
    )
    error_recovery_attempts: int = Field(
        default=0, description="Consecutive errors (circuit breaker triggers at 3)"
    )


class AgentMemory(BaseModel):
    """
    Agent-specific memory and reasoning traces.

    This stores the agent's thought process, feedback from evaluation,
    and conversation history.
    """

    reflection_notes: List[str] = Field(
        default_factory=list, description="Exploration gaps identified by comprehensive_evaluation"
    )
    evaluation_feedback: List[str] = Field(
        default_factory=list, description="Justifications from comprehensive_evaluation"
    )
    messages: List[dict] = Field(
        default_factory=list, description="LLM conversation history (for agent reasoning)"
    )
    tool_call_history: List[tuple] = Field(
        default_factory=list,
        description="Last 2 tool calls for duplicate detection: [(tool_name, args), ...]",
    )


class SASTAgentState(BaseModel):
    """
    Per-issue agent investigation state.

    This state is managed by the LangGraph agent and tracks a single issue's
    investigation from start to finish. It's separate from the batch workflow
    tracker and is created fresh for each issue.

    Organized into logical groups for clarity:
    - context: Code and symbols gathered during investigation
    - analysis: Verdicts and evaluation results
    - error_state: Error tracking and recovery
    - memory: Agent reasoning traces and feedback
    """

    model_config = {"arbitrary_types_allowed": True}

    # ==================== CORE IDENTITY ====================
    issue_id: str = Field(description="Unique identifier for this issue")
    issue: Issue = Field(description="The SAST finding being investigated")

    # ==================== GROUPED STATE ====================
    context: InvestigationContext = Field(
        default_factory=InvestigationContext,
        description="Investigation context (fetched code and symbols)",
    )
    analysis: AnalysisState = Field(
        default_factory=AnalysisState, description="Analysis results (verdict, evaluation)"
    )
    error_state: ErrorState = Field(
        default_factory=ErrorState, description="Error tracking and recovery"
    )
    memory: AgentMemory = Field(
        default_factory=AgentMemory, description="Agent memory (notes, feedback, conversation)"
    )

    # ==================== CONTROL FLOW ====================
    is_final: bool = Field(
        default=False,
        description="True when investigation is complete and verdict can be finalized",
    )
    iteration_count: int = Field(
        default=0, description="Number of tool calls made (circuit breaker at 15)"
    )

    # ==================== SHARED RESOURCES ====================
    project_context: Optional[ProjectContext] = Field(
        default=None, description="Reference to shared project metadata"
    )
