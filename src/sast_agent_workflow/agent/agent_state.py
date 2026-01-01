"""
Agent state schema for per-issue SAST investigation.

This module defines the state models used by the ReAct agent for investigating
individual SAST findings. The agent state is separate from the batch workflow
tracker (SASTWorkflowTracker) and manages a single issue investigation.
"""

from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field

from dto.Issue import Issue

# Type aliases for clarity
ClaimStatus = Literal["supported", "tentative", "rejected", "conflicting"]
VerdictType = Literal["TRUE_POSITIVE", "FALSE_POSITIVE", "NEEDS_REVIEW"]
ToolChoice = Literal["fetch_code", "evaluator"]


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


class Claim(BaseModel):
    """Atomic, checkable security-relevant assertion."""

    claim_id: str = Field(description="Stable identifier for this claim (unique within issue)")
    text: str = Field(description="The claim itself, phrased as a checkable statement")
    status: ClaimStatus = Field(description="Current support status of the claim")
    supporting_evidence_ids: List[str] = Field(
        default_factory=list, description="Evidence IDs that support this claim"
    )


class Evidence(BaseModel):
    """Concrete citation backing claims."""

    evidence_id: str = Field(description="Stable identifier for this evidence item (unique)")
    path_or_identifier: str = Field(
        description="File path or identifier used to fetch this evidence"
    )
    excerpt: str = Field(description="Raw code snippet (verbatim)")
    start_line: Optional[int] = Field(default=None, description="1-based start line, if known")
    end_line: Optional[int] = Field(default=None, description="1-based end line, if known")
    why_it_matters: str = Field(description="Why this snippet is relevant to the verdict")


class Unknown(BaseModel):
    """Prioritized unanswered question that blocks a final verdict."""

    unknown_id: str = Field(description="Stable identifier for this unknown (unique)")
    question: str = Field(description="What we still need to know")
    priority: int = Field(default=50, description="Higher = more important (used for ordering)")
    blocking: bool = Field(
        default=True, description="If true, blocks evaluator-verifiable finalization"
    )


class FetchCodeParams(BaseModel):
    """Parameters for fetch_code tool."""

    referring_source_code_path: str = Field(
        description="Source file path where the reference/symbol appears"
    )
    expression_name: str = Field(description="Symbol or expression name to fetch")
    reason: str = Field(description="Why this code needs to be fetched")


class EvaluatorParams(BaseModel):
    """Parameters for evaluator tool (evidence package)."""

    analysis: str = Field(description="Summary of investigation findings")
    claims: List[Dict[str, Any]] = Field(description="All claims as dicts")
    evidence: List[Dict[str, Any]] = Field(description="All evidence as dicts")
    unknowns: List[Dict[str, Any]] = Field(description="Remaining unknowns as dicts")
    proposed_verdict: Literal["TRUE_POSITIVE", "FALSE_POSITIVE"] = Field(
        description="Proposed verdict based on evidence"
    )


class ReasoningStateUpdate(BaseModel):
    """
    Structured output schema for agent reasoning.

    This is the schema that the LLM outputs using with_structured_output().
    It contains both the reasoning state update AND the tool decision.
    """

    analysis: str = Field(
        description="Detailed analysis of the code and vulnerability based on fetched evidence"
    )
    claims: List[Claim] = Field(
        default_factory=list,
        description="Updated list of all claims (atomic, checkable assertions)",
    )
    evidence: List[Evidence] = Field(
        default_factory=list,
        description="Updated list of all evidence (code citations with snippets)",
    )
    unknowns: List[Unknown] = Field(
        default_factory=list,
        description="Updated list of unknowns (blocking questions that need resolution)",
    )
    next_tool: ToolChoice = Field(
        description="Which tool to call next: 'fetch_code' if unknowns exist, 'evaluator' if ready to verify"
    )
    tool_reasoning: str = Field(description="Explanation of why this specific tool is needed now")
    tool_parameters: Optional[Dict[str, Any]] = Field(
        default=None, description="Parameters for the tool call (structure depends on next_tool)"
    )


class RequiredNextFetch(BaseModel):
    """Evaluator-suggested next retrieval action (tool implementation is external)."""

    tool: str = Field(description="Tool name to call next (e.g., fetch_code, search_codebase)")
    args: Dict[str, Any] = Field(default_factory=dict, description="Tool arguments")
    reason: str = Field(description="Why this fetch is required")


class EvaluatorReport(BaseModel):
    """Structured gate response from the `evaluator` tool."""

    verification_passed: bool = Field(description="True only if verdict is fully proven")
    verdict: Optional[Literal["TRUE_POSITIVE", "FALSE_POSITIVE"]] = Field(
        default=None, description="Final verdict if verification_passed==true"
    )
    blocking_gaps: List[str] = Field(
        default_factory=list, description="What is missing/unknown and blocks verification"
    )
    required_next_fetches: List[RequiredNextFetch] = Field(
        default_factory=list, description="Concrete next retrieval steps"
    )
    justifications: List[str] = Field(
        default_factory=list, description="Evaluator reasoning (auditor-style)"
    )
    stop_reason: Optional[str] = Field(
        default=None, description="Optional stop reason provided by evaluator"
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
    source_code: Dict[str, List[str]] = Field(
        default_factory=dict,
        description="Source code gathered during investigation (file_path -> code lines)",
    )


class AnalysisState(BaseModel):
    """
    Analysis-driven investigation reasoning state (ADR-0002).

    The agent maintains explicit claims/evidence/unknowns and is only allowed to
    finalize after a successful evaluator verification.
    """

    claims: List[Claim] = Field(default_factory=list, description="Atomic assertions")
    evidence: List[Evidence] = Field(default_factory=list, description="Concrete citations")
    unknowns: List[Unknown] = Field(default_factory=list, description="Blocking questions/gaps")

    evaluator_reports: List[EvaluatorReport] = Field(
        default_factory=list, description="Full history of evaluator gate reports"
    )
    last_evaluator_report: Optional[EvaluatorReport] = Field(
        default=None, description="Most recent evaluator report"
    )

    verdict: Optional[VerdictType] = Field(
        default=None, description="Final verdict (set only when is_final==True)"
    )
    final_justifications: List[str] = Field(
        default_factory=list, description="Final justification bullets for output"
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

    messages: List[Any] = Field(
        default_factory=list,
        description="LLM conversation history (LangChain messages: Human/AI/Tool/System)",
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
    no_progress_streak: int = Field(
        default=0,
        description="Consecutive retrieval tool calls that added no new evidence/claims",
    )
    guard_attempt_count: int = Field(
        default=0, description="Number of evaluator attempts (guard attempts)"
    )
    guard_rejection_streak: int = Field(
        default=0,
        description="Consecutive evaluator rejections without resolving blocking gaps",
    )
    stop_reason: Optional[str] = Field(
        default=None,
        description="Reason investigation stopped (verified, breaker, guard_rejections, etc.)",
    )
