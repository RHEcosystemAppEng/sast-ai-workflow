"""
Agent decision node - the sole decision maker in the investigation.

This module implements the ReAct agent that decides which tool to call next
based on the current investigation state, evaluator feedback, and error context.
"""

import functools
import json
import logging
import os
import re
import uuid
from datetime import datetime
from string import Template
from typing import Any, Dict, List, Optional, Tuple

import yaml
from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.messages import AIMessage, HumanMessage, SystemMessage
from langchain_nvidia_ai_endpoints import ChatNVIDIA
from langchain_openai.chat_models.base import ChatOpenAI

from .agent_state import (
    Claim,
    EvaluatorReport,
    Evidence,
    ReasoningStateUpdate,
    SASTAgentState,
    ToolError,
    Unknown,
)

logger = logging.getLogger(__name__)


@functools.lru_cache(maxsize=1)
def get_agent_prompts() -> Tuple[str, Dict]:
    """
    Lazy-load and cache agent prompts from YAML files.

    Uses lru_cache to load prompts only once on first access, improving:
    - Testability: Can clear cache and mock for tests
    - Error handling: Defers load errors to first use, not import
    - Startup time: Module imports instantly

    Returns:
        Tuple of (system_prompt, formatting_templates)
    """
    prompts_dir = os.path.join(os.path.dirname(__file__), "../../templates/prompts")

    # Load main system prompt
    system_prompt_file = os.path.join(prompts_dir, "agent_decision_prompt.yaml")
    try:
        with open(system_prompt_file, "r") as f:
            system_prompt_data = yaml.safe_load(f)
            system_prompt = system_prompt_data.get("template", "")
    except Exception as e:
        logger.error(f"Failed to load agent system prompt from {system_prompt_file}: {e}")
        raise

    # Load formatting templates
    formatting_file = os.path.join(prompts_dir, "agent_formatting_templates.yaml")
    try:
        with open(formatting_file, "r") as f:
            formatting_templates = yaml.safe_load(f)
    except Exception as e:
        logger.error(f"Failed to load formatting templates from {formatting_file}: {e}")
        raise

    logger.debug("Agent prompts loaded and cached")
    return system_prompt, formatting_templates


def format_error_context(error_state) -> str:
    """Format last error for agent prompt."""
    _, formatting_templates = get_agent_prompts()

    if not error_state or not error_state.last_error:
        return formatting_templates["error_context_no_errors"]

    last_error = error_state.last_error
    template = Template(formatting_templates["error_context_template"])
    return template.safe_substitute(
        tool_name=last_error.tool_name,
        attempted_args=last_error.attempted_args,
        error_message=last_error.error_message,
    )


def format_evaluator_feedback(state: SASTAgentState) -> str:
    """Format evaluator feedback for agent."""
    _, formatting_templates = get_agent_prompts()

    if not state.analysis.last_evaluator_report:
        return formatting_templates["evaluator_feedback_no_report"]

    report = state.analysis.last_evaluator_report
    required_fetches = (
        "\n".join(
            f"- {f.tool}({json.dumps(f.args, sort_keys=True)}): {f.reason}"
            for f in report.required_next_fetches[:10]
        )
        if report.required_next_fetches
        else "  ✓ None suggested"
    )
    gaps = (
        "\n".join(f"- {g}" for g in report.blocking_gaps[:15])
        if report.blocking_gaps
        else "  ✓ None reported"
    )
    justifications = (
        "\n".join(f"- {j}" for j in report.justifications[:10])
        if report.justifications
        else "  (no details)"
    )

    template = Template(formatting_templates["evaluator_feedback_template"])
    return template.safe_substitute(
        verification_passed=report.verification_passed,
        verdict=report.verdict or "null",
        blocking_gaps=gaps,
        required_next_fetches=required_fetches,
        justifications=justifications,
    )


def format_state_summary(state: SASTAgentState) -> str:
    """Format current investigation state summary."""
    _, formatting_templates = get_agent_prompts()

    issue_type = state.issue.issue_type
    trace_preview = state.issue.trace if hasattr(state.issue, "trace") else "No trace available"
    files_count = len(state.context.fetched_files)
    files_list = ", ".join(list(state.context.fetched_files.keys())[:10])
    claims_count = len(state.analysis.claims)
    evidence_count = len(state.analysis.evidence)
    blocking_unknowns = sum(1 for u in state.analysis.unknowns if u.blocking)
    last_guard = (
        "none"
        if not state.analysis.last_evaluator_report
        else ("PASS" if state.analysis.last_evaluator_report.verification_passed else "FAIL")
    )

    template = Template(formatting_templates["state_summary_template"])
    return template.safe_substitute(
        issue_type=issue_type,
        trace_preview=trace_preview,
        files_count=files_count,
        files_list=files_list,
        claims_count=claims_count,
        evidence_count=evidence_count,
        blocking_unknowns=blocking_unknowns,
        last_guard=last_guard,
        iteration_count=state.iteration_count,
        is_final=state.is_final,
        errors_count=len(state.error_state.errors),
    )


def format_all_fetched_code(fetched_files: Dict[str, List[str]], max_chars: int = 15000) -> str:
    """
    Format ALL fetched code for agent analysis.

    Critical: Agent must see actual code to reason about it, not just metadata.
    Truncates if total exceeds max_chars to avoid prompt bloat.
    """
    if not fetched_files:
        return "No code fetched yet. Start by fetching code from the trace."

    sections = []
    total_chars = 0
    truncated = False

    for identifier, code_blocks in fetched_files.items():
        header = f"=== {identifier} ==="
        sections.append(header)
        total_chars += len(header)

        for block in code_blocks:
            if total_chars + len(block) > max_chars:
                truncated = True
                break
            sections.append(block)
            total_chars += len(block)
            sections.append("")  # Blank line

        if truncated:
            break

    result = "\n".join(sections)
    if truncated:
        result += (
            f"\n\n[... truncated at {max_chars} chars. {len(fetched_files)} total files available]"
        )

    return result


def format_claims_for_prompt(claims: List[Claim]) -> str:
    """Format current claims so agent can review and update them."""
    if not claims:
        return "None yet. As you analyze code, create claims about data flow, sanitization, etc."

    lines = ["**EXISTING CLAIMS (review and update):**"]
    for c in claims:
        evidence_refs = (
            f" [supported by: {', '.join(c.supporting_evidence_ids)}]"
            if c.supporting_evidence_ids
            else ""
        )
        status_marker = {
            "supported": "✓",
            "tentative": "?",
            "rejected": "✗",
            "conflicting": "⚠",
        }.get(c.status, "?")
        lines.append(f"{status_marker} [{c.claim_id}] {c.text}{evidence_refs}")

    return "\n".join(lines)


def format_evidence_for_prompt(evidence: List[Evidence]) -> str:
    """Format current evidence so agent can see what code was already cited."""
    if not evidence:
        return "None yet. Extract evidence (code snippets + line numbers) from fetched code."

    lines = ["**EXISTING EVIDENCE (citations):**"]
    for e in evidence[:20]:  # Limit to avoid bloat
        loc = f":{e.start_line}-{e.end_line}" if e.start_line else ""
        snippet = e.excerpt[:100] + "..." if len(e.excerpt) > 100 else e.excerpt
        lines.append(f"[{e.evidence_id}] {e.path_or_identifier}{loc}")
        lines.append(f"  Snippet: {snippet}")
        lines.append(f"  Why: {e.why_it_matters}")

    if len(evidence) > 20:
        lines.append(f"... and {len(evidence) - 20} more evidence items")

    return "\n".join(lines)


def format_unknowns_for_prompt(unknowns: List[Unknown]) -> str:
    """Format unknowns to show what's blocking verdict."""
    if not unknowns:
        return "None! You may be ready to call evaluator."

    blocking = [u for u in unknowns if u.blocking]
    non_blocking = [u for u in unknowns if not u.blocking]

    lines = []
    if blocking:
        lines.append("**BLOCKING UNKNOWNS (must resolve):**")
        for u in sorted(blocking, key=lambda x: x.priority, reverse=True):
            lines.append(f"[{u.unknown_id}] (P{u.priority}) {u.question}")

    if non_blocking:
        lines.append("\n**Non-blocking unknowns:**")
        for u in non_blocking[:5]:
            lines.append(f"[{u.unknown_id}] {u.question}")

    return "\n".join(lines)


async def _robust_structured_output_with_retry(
    llm: BaseChatModel,
    messages: List[Any],
    schema: type[ReasoningStateUpdate],
    max_retries: int = 2,
) -> Optional[ReasoningStateUpdate]:
    """
    Get structured output from LLM with retry logic for NVIDIA NIM compatibility.

    NVIDIA NIM with Llama 3.3 requires special handling:
    - Uses with_structured_output() for reasoning (no regex parsing)
    - May return None on parse failure instead of raising exception
    - Needs retry logic with clearer prompting

    Args:
        llm: The LLM instance (bound with tools or not)
        messages: List of messages for the LLM
        schema: Pydantic schema for structured output (ReasoningStateUpdate)
        max_retries: Number of retries on failure

    Returns:
        ReasoningStateUpdate instance or None if all retries fail
    """
    for attempt in range(max_retries):
        try:
            # Check LLM type for appropriate handling
            if isinstance(llm, ChatNVIDIA):
                # NVIDIA: with_structured_output may return None on failure
                structured_llm = llm.with_structured_output(schema)
                result = await structured_llm.ainvoke(messages)

                if result is not None:
                    logger.info(f"Structured output successful (attempt {attempt + 1})")
                    return result
                else:
                    logger.warning(
                        f"NVIDIA NIM returned None for structured output (attempt {attempt + 1}). "
                        "Response may not match schema."
                    )
                    if attempt < max_retries - 1:
                        # Add clarifying message for retry
                        messages.append(
                            HumanMessage(
                                content=(
                                    "Your previous response did not match the required JSON schema. "
                                    "Please provide a valid JSON response with ALL required fields: "
                                    "analysis, claims, evidence, unknowns, next_tool, tool_reasoning, tool_parameters"
                                )
                            )
                        )

            elif isinstance(llm, ChatOpenAI):
                # OpenAI: with_structured_output with strict mode
                structured_llm = llm.with_structured_output(
                    schema, method="json_schema", strict=True
                )
                result = await structured_llm.ainvoke(messages)
                logger.info(f"Structured output successful (attempt {attempt + 1})")
                return result

            else:
                # Generic LLM: try basic with_structured_output
                structured_llm = llm.with_structured_output(schema)
                result = await structured_llm.ainvoke(messages)

                if result is not None:
                    logger.info(f"Structured output successful (attempt {attempt + 1})")
                    return result

        except Exception as e:
            logger.warning(
                f"Structured output attempt {attempt + 1} failed: {type(e).__name__}: {str(e)}"
            )
            if attempt < max_retries - 1:
                # Add error feedback for retry
                messages.append(
                    HumanMessage(
                        content=(
                            f"Error in previous response: {str(e)}. "
                            "Please ensure your JSON response is valid and includes all required fields."
                        )
                    )
                )

    logger.error(f"All {max_retries} structured output attempts failed")
    return None


def _create_tool_call_from_reasoning(
    reasoning: ReasoningStateUpdate, issue_id: str
) -> Dict[str, Any]:
    """
    Create a synthetic tool call from structured reasoning output.

    Since NVIDIA NIM doesn't reliably return tool_calls, we synthesize them
    from the structured reasoning output's next_tool and tool_parameters fields.

    Args:
        reasoning: Structured reasoning output from LLM
        issue_id: Issue ID for logging

    Returns:
        Tool call dict compatible with LangGraph routing
    """
    tool_name = reasoning.next_tool

    # Extract parameters from reasoning
    if reasoning.tool_parameters:
        tool_args = reasoning.tool_parameters
    elif tool_name == "fetch_code":
        # Infer fetch_code params from highest priority unknown
        if reasoning.unknowns:
            highest_priority = max(
                reasoning.unknowns, key=lambda u: u.priority if u.blocking else 0
            )
            tool_args = {
                "identifier": "unknown_code",  # Fallback - should be in tool_parameters
                "reason": reasoning.tool_reasoning or highest_priority.question,
            }
            logger.warning(
                f"[{issue_id}] No tool_parameters provided for fetch_code, "
                f"inferring from unknown: {highest_priority.question[:100]}"
            )
        else:
            logger.error(f"[{issue_id}] No unknowns and no tool_parameters for fetch_code")
            tool_args = {
                "identifier": "unknown",
                "reason": reasoning.tool_reasoning or "Investigation requires more context",
            }
    elif tool_name == "evaluator":
        # Build evidence package from reasoning state
        tool_args = {
            "analysis": reasoning.analysis,
            "claims": [c.model_dump() for c in reasoning.claims],
            "evidence": [e.model_dump() for e in reasoning.evidence],
            "unknowns": [u.model_dump() for u in reasoning.unknowns],
            "proposed_verdict": "TRUE_POSITIVE",  # Default - should be in tool_parameters
        }
        if reasoning.tool_parameters and "proposed_verdict" in reasoning.tool_parameters:
            tool_args["proposed_verdict"] = reasoning.tool_parameters["proposed_verdict"]
    else:
        logger.error(f"[{issue_id}] Unknown tool name: {tool_name}")
        tool_args = {}

    tool_call = {
        "name": tool_name,
        "args": tool_args,
        "id": f"synth_{tool_name}_{uuid.uuid4().hex[:8]}",
    }

    logger.info(f"[{issue_id}] Synthesized tool call: {tool_name} with {len(tool_args)} args")
    return tool_call


async def agent_decision_node(state: SASTAgentState, llm: BaseChatModel) -> SASTAgentState:
    """
    Agent decision node - performs ANALYSIS and DECISION in one step.

    HYBRID APPROACH (2025):
    - Uses with_structured_output() for reliable reasoning (Pydantic validation)
    - Supports BOTH native tool calling AND synthesized tool calls
    - Automatically detects which mode the model supports
    - Works with GPT-4 (native), Claude (native), and NVIDIA NIM (synthesis)

    The agent must:
    1. See ALL fetched code
    2. Analyze it to produce/update Claims, Evidence, Unknowns (structured output)
    3. Decide next tool call based on reasoning (native or synthesized)

    Args:
        state: Current investigation state
        llm: LLM for decision making (already bound with tools in graph)

    Returns:
        Updated state with new tool call decision in messages
    """
    logger.info(f"[{state.issue_id}] Agent reasoning cycle (iteration {state.iteration_count})")

    # Get prompts (lazy-loaded and cached)
    system_prompt, _ = get_agent_prompts()

    # Build rich context including ALL evidence
    error_ctx = format_error_context(state.error_state)
    evaluator_feedback = format_evaluator_feedback(state)
    state_summary = format_state_summary(state)

    # Show agent current reasoning state to review/update
    formatted_claims = format_claims_for_prompt(state.analysis.claims)
    formatted_evidence = format_evidence_for_prompt(state.analysis.evidence)
    formatted_unknowns = format_unknowns_for_prompt(state.analysis.unknowns)
    has_blocking_unknowns = any(u.blocking for u in state.analysis.unknowns)

    # Build system message with ALL context using Template for safe formatting
    template = Template(system_prompt)
    formatted_content = template.safe_substitute(
        state_summary=state_summary,
        formatted_claims=formatted_claims,
        formatted_evidence=formatted_evidence,
        formatted_unknowns=formatted_unknowns,
        evaluator_feedback=evaluator_feedback,
        must_retrieve="TRUE" if has_blocking_unknowns else "FALSE",
        error_context=error_ctx,
    )

    system_msg = SystemMessage(content=formatted_content)

    # Build user message
    user_msg = HumanMessage(
        content=(
            "ANALYZE all fetched code and UPDATE your reasoning state. "
            "Provide a complete JSON response with: analysis, claims, evidence, unknowns, "
            "next_tool (fetch_code or evaluator), tool_reasoning, and tool_parameters."
        )
    )

    # Prepare messages for LLM
    messages = [system_msg] + state.memory.messages[-10:] + [user_msg]

    try:
        logger.info(
            f"[{state.issue_id}] Getting structured reasoning with with_structured_output()..."
        )

        # Use with_structured_output for reliable Pydantic validation
        reasoning = await _robust_structured_output_with_retry(
            llm=llm, messages=messages, schema=ReasoningStateUpdate, max_retries=2
        )

        if reasoning is None:
            # All attempts to get structured reasoning failed
            logger.error(
                f"[{state.issue_id}] Failed to get structured reasoning after all attempts. "
                "Recording error and forcing retry."
            )

            error = ToolError(
                tool_name="agent_reasoning",
                error_message=(
                    "Failed to produce valid structured output. "
                    "Ensure your response is valid JSON matching the schema: "
                    "{analysis, claims, evidence, unknowns, next_tool, tool_reasoning, tool_parameters}"
                ),
                attempted_args={},
                timestamp=datetime.now().isoformat(),
            )
            state.error_state.errors.append(error)
            state.error_state.last_error = error
            state.error_state.error_recovery_attempts += 1

            # Create a placeholder response to store in messages
            response = AIMessage(content="[Structured reasoning failed - see error state]")
            state.memory.messages.append(user_msg)
            state.memory.messages.append(response)

            return state

        # Successfully got structured reasoning!
        logger.info(
            f"[{state.issue_id}] Structured reasoning received: "
            f"{len(reasoning.claims)} claims, {len(reasoning.evidence)} evidence, "
            f"{len(reasoning.unknowns)} unknowns, next_tool={reasoning.next_tool}"
        )

        # Update state with validated reasoning
        state.analysis.claims = reasoning.claims
        state.analysis.evidence = reasoning.evidence
        state.analysis.unknowns = reasoning.unknowns

        # Reset error counter on successful reasoning
        state.error_state.error_recovery_attempts = 0

        # Synthesize tool call from structured reasoning
        logger.info(f"[{state.issue_id}] Synthesizing tool call from structured reasoning")
        tool_call = _create_tool_call_from_reasoning(reasoning, state.issue_id)

        # Create AI message with both content (reasoning) and tool_calls
        response = AIMessage(
            content=reasoning.analysis,  # Store analysis text for transparency
            tool_calls=[tool_call],
        )

        # Store in message history
        state.memory.messages.append(user_msg)
        state.memory.messages.append(response)

        logger.info(f"[{state.issue_id}] Agent decision complete: {tool_call['name']}")

    except Exception as e:
        logger.error(f"[{state.issue_id}] Agent decision node failed: {e}", exc_info=True)

        # Record error
        error = ToolError(
            tool_name="agent_reasoning",
            error_message=f"Unexpected error during reasoning: {str(e)}",
            attempted_args={},
            timestamp=datetime.now().isoformat(),
        )
        state.error_state.errors.append(error)
        state.error_state.last_error = error
        state.error_state.error_recovery_attempts += 1

        raise

    return state
