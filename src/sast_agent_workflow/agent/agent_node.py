"""
Agent decision node - the sole decision maker in the investigation.

This module implements the ReAct agent that decides which tool to call next
based on the current investigation state, evaluation feedback, and error context.
"""

import functools
import logging
import os
from typing import Dict, Tuple

import yaml
from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.messages import HumanMessage, SystemMessage

from .agent_state import ProjectContext, SASTAgentState

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


def format_project_context(project_context: ProjectContext) -> str:
    """Format project context for agent prompt."""
    _, formatting_templates = get_agent_prompts()

    if not project_context:
        return formatting_templates["project_context_no_context"]

    directory_list = ", ".join(list(project_context.structure.keys())[:10])
    security_files_list = chr(10).join(f"  - {f}" for f in project_context.security_files[:15])
    security_files_more = "  ... and more" if len(project_context.security_files) > 15 else ""
    frameworks_list = (
        ", ".join(project_context.frameworks) if project_context.frameworks else "None detected"
    )

    return formatting_templates["project_context_template"].format(
        directory_list=directory_list,
        security_files_count=len(project_context.security_files),
        security_files_list=security_files_list,
        security_files_more=security_files_more,
        frameworks_list=frameworks_list,
    )


def format_error_context(error_state) -> str:
    """Format last error for agent prompt."""
    _, formatting_templates = get_agent_prompts()

    if not error_state or not error_state.last_error:
        return formatting_templates["error_context_no_errors"]

    last_error = error_state.last_error
    return formatting_templates["error_context_template"].format(
        tool_name=last_error.tool_name,
        attempted_args=last_error.attempted_args,
        error_message=last_error.error_message,
    )


def format_evaluation_feedback(state: SASTAgentState) -> str:
    """Format comprehensive_evaluation feedback for agent."""
    _, formatting_templates = get_agent_prompts()

    if not state.analysis.evaluation_result:
        return formatting_templates["evaluation_feedback_no_evaluation"]

    eval_result = state.analysis.evaluation_result

    # Format exploration gaps
    if eval_result.exploration_gaps:
        exploration_gaps_lines = []
        for gap in eval_result.exploration_gaps:
            exploration_gaps_lines.append(f"- {gap.area}: {gap.reason}")
            if gap.suggested_files:
                exploration_gaps_lines.append(
                    f"  Suggested files: {', '.join(gap.suggested_files[:3])}"
                )
        exploration_gaps_section = "\n".join(exploration_gaps_lines)
    else:
        exploration_gaps_section = formatting_templates["exploration_gaps_complete"]

    # Format logic gaps
    if eval_result.logic_gaps:
        logic_gaps_section = "\n".join(f"- {gap}" for gap in eval_result.logic_gaps)
    else:
        logic_gaps_section = formatting_templates["logic_gaps_complete"]

    # Format required code
    if eval_result.required_code:
        required_code_lines = ["\n**REQUIRED CODE:**"]
        for code in eval_result.required_code:
            required_code_lines.append(
                f"- {code.expression_name} from {code.file_path}: {code.reason}"
            )
        required_code_section = "\n".join(required_code_lines)
    else:
        required_code_section = ""

    # Format recommendations
    recommendations_list = "\n".join(
        f"{i}. {rec}" for i, rec in enumerate(eval_result.recommendations[:5], 1)
    )

    return formatting_templates["evaluation_feedback_template"].format(
        is_final=eval_result.is_final,
        verdict_confidence=eval_result.verdict_confidence,
        exploration_gaps_section=exploration_gaps_section,
        logic_gaps_section=logic_gaps_section,
        required_code_section=required_code_section,
        recommendations_list=recommendations_list,
    )


def format_state_summary(state: SASTAgentState) -> str:
    """Format current investigation state summary."""
    _, formatting_templates = get_agent_prompts()

    issue_type = state.issue.issue_type
    file_path = state.issue.file_path if hasattr(state.issue, "file_path") else "unknown"
    trace_preview = state.issue.trace if hasattr(state.issue, "trace") else "No trace available"
    files_count = len(state.context.fetched_files)
    files_list = ", ".join(list(state.context.fetched_files.keys())[:10])
    analysis_result = (
        state.analysis.investigation_result.investigation_result
        if state.analysis.investigation_result
        else "Not yet analyzed"
    )

    return formatting_templates["state_summary_template"].format(
        issue_type=issue_type,
        file_path=file_path,
        trace_preview=trace_preview,
        files_count=files_count,
        files_list=files_list,
        analysis_result=analysis_result,
        iteration_count=state.iteration_count,
        is_final=state.is_final,
        errors_count=len(state.error_state.errors),
    )


async def agent_decision_node(state: SASTAgentState, llm: BaseChatModel) -> SASTAgentState:
    """
    Agent decision node - decides next tool call based on state.

    This is the ONLY node that makes decisions. All other nodes (tools, analyze,
    comprehensive_evaluation) are executors that update state and return results.

    Args:
        state: Current investigation state
        llm: LLM for decision making

    Returns:
        Updated state with new tool call decision in messages
    """
    logger.info(f"[{state.issue_id}] Agent making decision (iteration {state.iteration_count})")

    # Get prompts (lazy-loaded and cached)
    system_prompt, _ = get_agent_prompts()

    # Build context strings using templates
    project_ctx = format_project_context(state.project_context)
    error_ctx = format_error_context(state.error_state)
    eval_feedback = format_evaluation_feedback(state)
    state_summary = format_state_summary(state)

    # Build system message
    system_msg = SystemMessage(
        content=system_prompt.format(
            project_context=project_ctx,
            state_summary=state_summary,
            evaluation_feedback=eval_feedback,
            error_context=error_ctx,
        )
    )

    # Build user message
    user_msg = HumanMessage(content="What should I do next? Decide ONE tool call.")

    # Get LLM decision with tool binding
    try:
        # Tool definitions will be bound to LLM in graph construction
        response = await llm.ainvoke([system_msg] + state.memory.messages[-10:] + [user_msg])

        # Store reasoning and decision
        state.memory.messages.append(user_msg)
        state.memory.messages.append(response)

        content_preview = response.content[:100] if hasattr(response, "content") else "tool_call"
        logger.info(f"[{state.issue_id}] Agent decision: {content_preview}")

    except Exception as e:
        logger.error(f"[{state.issue_id}] Agent decision failed: {e}")
        # Don't store error here - let routing handle it
        raise

    return state
