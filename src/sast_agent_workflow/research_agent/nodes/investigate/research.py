"""Research node - Agent for code gathering with middleware-based state updates.

Upgraded to LangChain v1 create_agent API with middleware pattern.

Design: STATELESS per model call - model sees only system prompt (instructions + CODE BANK),
NOT conversation history. All context is embedded in the dynamic prompt.
"""

import logging
import uuid
from typing import Annotated, Dict, List, Optional

from langchain.agents import AgentState, create_agent
from langchain.agents.middleware import (
    ModelCallLimitMiddleware,
    ModelRequest,
    ModelRetryMiddleware,
    wrap_model_call,
    wrap_tool_call,
)
from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.messages import AIMessage, AnyMessage, SystemMessage, ToolMessage
from langchain_core.tools import BaseTool
from langgraph.checkpoint.memory import InMemorySaver
from langgraph.errors import GraphRecursionError
from langgraph.prebuilt.tool_node import ToolCallRequest
from langgraph.types import Command
from typing_extensions import NotRequired

from ...core import (
    CODE_GATHERING_TOOLS,
    ERROR_MSG_TRUNCATE_CHARS,
    FAILED_MSG_PREVIEW_CHARS,
    MAX_MODEL_CALLS,
    MAX_TOOL_RESULT_CHARS,
    NOT_FOUND,
    RESEARCH_AGENT_RECURSION_LIMIT,
    build_code_bank,
    build_research_instructions,
)
from .schemas import InvestigationState

logger = logging.getLogger(__name__)


# ============================================================================
# State Reducers for Custom State Management
# ============================================================================


def merge_dicts(left: Optional[Dict], right: Optional[Dict]) -> Dict:
    """
    Merge two dictionaries, combining list values and overwriting other values.

    Used for fetched_files state field to accumulate tool results.
    """
    left = left or {}
    right = right or {}
    result = dict(left)
    for key, value in right.items():
        if value is None:
            continue
        if key in result and isinstance(result[key], list) and isinstance(value, list):
            # Merge lists
            result[key] = result[key] + value
        else:
            # Overwrite
            result[key] = value
    return result


def unique_list_add(left: Optional[List], right: Optional[List]) -> List:
    """
    Add items from right to left, avoiding duplicates.

    Used for tool_call_history to track unique tool calls.
    """
    left = left or []
    right = right or []
    result = list(left)
    for item in right:
        if item not in result:
            result.append(item)
    return result


def _format_tool_call(tool_name: str, args: dict, success: bool, reason: str = "") -> str:
    """
    Format tool call for history tracking.

    Args:
        tool_name: Name of the tool
        args: Tool arguments dictionary
        success: Whether the tool call succeeded
        reason: Short reason for failure (only used if success=False)

    Returns:
        Formatted string like "✓ fetch_code(file_path='src/main.c')" or
        "✗ search_codebase(pattern='malloc') → No matches"
    """
    # Format args as key='value' pairs, excluding None values
    args_str = ", ".join(f"{k}='{v}'" for k, v in args.items() if v is not None)
    prefix = "✓" if success else "✗"
    suffix = f" → {reason}" if reason and not success else ""
    return f"{prefix} {tool_name}({args_str}){suffix}"


def _truncate_tool_message(msg: ToolMessage) -> Optional[AnyMessage]:
    """Truncate or summarize a ToolMessage; return None to signal skip (e.g. tool definition)."""
    content = msg.content
    if isinstance(content, dict) and content.get("type") == "function":
        return None
    content_str = content if isinstance(content, str) else str(content)
    if len(content_str) <= MAX_TOOL_RESULT_CHARS:
        return msg
    tool_name = getattr(msg, "name", "tool")
    is_error = "Error:" in content_str or NOT_FOUND in content_str.lower()
    if is_error:
        summary = content_str[:MAX_TOOL_RESULT_CHARS] + "..."
    else:
        lines = content_str.count("\n")
        chars = len(content_str)
        summary = f"[{tool_name}: {lines} lines, {chars:,} chars - see CODE BANK in prompt]"
    return ToolMessage(
        content=summary,
        tool_call_id=msg.tool_call_id,
        name=getattr(msg, "name", None),
    )


def _strip_ai_reasoning(msg: AIMessage) -> Optional[AIMessage]:
    """Return a new AIMessage with reasoning stripped, or None if nothing to strip."""
    if hasattr(msg, "additional_kwargs") and msg.additional_kwargs:
        cleaned_kwargs = {
            k: v
            for k, v in msg.additional_kwargs.items()
            if k not in ("reasoning_content", "reasoning", "thinking")
        }
        if len(cleaned_kwargs) < len(msg.additional_kwargs):
            return AIMessage(
                content=msg.content,
                tool_calls=getattr(msg, "tool_calls", []),
                additional_kwargs=cleaned_kwargs,
                id=msg.id,
            )
    if isinstance(msg.content, list):
        cleaned_content = [
            block
            for block in msg.content
            if not (isinstance(block, dict) and block.get("type") in ("reasoning", "thinking"))
        ]
        if len(cleaned_content) < len(msg.content):
            return AIMessage(
                content=cleaned_content,
                tool_calls=getattr(msg, "tool_calls", []),
                additional_kwargs=getattr(msg, "additional_kwargs", {}),
                id=msg.id,
            )
    return None


def _truncate_message(msg: AnyMessage) -> Optional[AnyMessage]:
    """
    Truncate a single message if it's too long. Returns None to signal skip (e.g. tool def).

    - ToolMessage: Summarize long results (full code is in prompt CODE BANK)
    - AIMessage: Strip reasoning_content (thinking model bloat)
    - SystemMessage: Skip (handled by dynamic prompt)
    """
    if isinstance(msg, ToolMessage):
        return _truncate_tool_message(msg)
    if isinstance(msg, AIMessage):
        stripped = _strip_ai_reasoning(msg)
        return stripped if stripped is not None else msg
    return msg


def messages_reducer(
    left: Optional[List[AnyMessage]], right: Optional[List[AnyMessage]]
) -> List[AnyMessage]:
    """
    Custom reducer for messages that truncates long content as messages are added.

    This ensures:
    - Tool results are summarized (full code is in prompt CODE BANK)
    - AI reasoning is stripped (thinking model bloat)
    - History stays bounded automatically
    """
    left = left or []
    right = right or []

    # Process new messages (right) before merging
    cleaned_right = []
    for msg in right:
        truncated = _truncate_message(msg)
        if truncated is not None:  # None means skip
            cleaned_right.append(truncated)

    return left + cleaned_right


# ============================================================================
# Research Agent State Schema
# ============================================================================


class ResearchAgentState(AgentState):
    """
    State schema for the research agent.

    Extends LangChain v1 AgentState with domain-specific fields
    for tracking code gathering progress. Uses NotRequired for optional fields.

    Custom messages reducer truncates long content automatically:
    - Tool results → summarized (full code in prompt CODE BANK)
    - AI reasoning → stripped (thinking model bloat)
    """

    # Override messages with custom reducer that truncates content
    messages: Annotated[List[AnyMessage], messages_reducer]

    # Domain-specific state with custom reducers
    fetched_files: Annotated[NotRequired[Dict[str, List[str]]], merge_dicts]
    tool_call_history: Annotated[NotRequired[List[str]], unique_list_add]

    # Investigation context (passed through from parent state)
    issue_id: NotRequired[str]
    iteration: NotRequired[int]
    issue_description: NotRequired[str]
    initial_code: NotRequired[str]
    evaluation_feedback: NotRequired[str]
    required_information: NotRequired[List[str]]


# ============================================================================
# Middleware (LangChain v1 Pattern)
# ============================================================================


@wrap_model_call
async def stateless_model_middleware(request: ModelRequest, handler):
    """
    Build model input with STATELESS design and separate SystemMessages.

    Original design: model sees ONLY:
    1. SystemMessage(instructions) - iteration-dependent research instructions
    2. SystemMessage(CODE BANK) - all previously fetched code
    3. Tool schemas (handled internally)

    NOT the conversation history. All context is embedded in the prompt.

    This is critical because:
    - CODE BANK already contains all fetched code
    - Instructions are iteration-aware
    - No need to replay tool call/response history
    """
    state = request.state

    # Build separate messages for instructions and CODE BANK
    # Using functions from core.prompts module
    instructions = build_research_instructions(state)
    code_bank = build_code_bank(state.get("fetched_files", {}))

    # Override: set messages to our SystemMessages, clear system_message
    # This gives us two separate SystemMessages like the original design
    request = request.override(
        system_message=None,
        messages=[
            SystemMessage(content=instructions),
            SystemMessage(content=code_bank),
        ],
    )
    return await handler(request)


def _get_tool_result_failure_reason(content: str, content_lower: str) -> Optional[str]:
    """Return failure reason string if tool result indicates error/not found, else None."""
    if "Error:" in content:
        return "Error"
    if "no matches found" in content_lower:
        return "No matches"
    if NOT_FOUND in content_lower:
        return "Not found"
    return None


@wrap_tool_call
async def code_gathering_middleware(request: ToolCallRequest, handler):
    """
    Track code-gathering tool results in state via Command.

    This middleware:
    1. Executes the tool via handler
    2. For code-gathering tools, returns Command with state updates
    3. Handles errors gracefully

    Returns Command to update fetched_files/tool_call_history state fields.
    """
    tool_name = request.tool_call.get("name", "unknown")
    tool_call_id = request.tool_call["id"]
    tool_args = request.tool_call.get("args", {})

    # Execute the tool
    try:
        result = await handler(request)
    except Exception as e:
        # Handle execution errors
        error_msg = f"Tool error: {tool_name} failed - {str(e)[:ERROR_MSG_TRUNCATE_CHARS]}"

        if tool_name in CODE_GATHERING_TOOLS:
            # Track the failure in tool call history
            history_entry = _format_tool_call(tool_name, tool_args, success=False, reason="Error")
            return Command(
                update={
                    "tool_call_history": [history_entry],
                    "messages": [ToolMessage(content=error_msg, tool_call_id=tool_call_id)],
                }
            )
        else:
            # Non-code-gathering tool - just return error message
            return ToolMessage(content=error_msg, tool_call_id=tool_call_id)

    # For non-code-gathering tools, pass through the result unchanged
    if tool_name not in CODE_GATHERING_TOOLS:
        return result

    # Extract content from successful result
    content = result.content if isinstance(result, ToolMessage) else str(result)
    content_lower = content.lower()
    failure_reason = _get_tool_result_failure_reason(content, content_lower)

    if failure_reason:
        history_entry = _format_tool_call(
            tool_name, tool_args, success=False, reason=failure_reason
        )
        return Command(
            update={
                "tool_call_history": [history_entry],
                "messages": [
                    ToolMessage(
                        content=f"Failed: {content[:FAILED_MSG_PREVIEW_CHARS]}",
                        tool_call_id=tool_call_id,
                    )
                ],
            }
        )
    else:
        # Success - store full content in state, return summary to model
        history_entry = _format_tool_call(tool_name, tool_args, success=True)

        # For search tools, prepend the query info to the content for CODE BANK clarity
        if tool_name in ("search_codebase", "file_search"):
            args_str = ", ".join(f"{k}='{v}'" for k, v in tool_args.items() if v is not None)
            content_with_header = f"[Query: {tool_name}({args_str})]\n{content}"
        else:
            content_with_header = content

        return Command(
            update={
                "fetched_files": {tool_name: [content_with_header]},
                "tool_call_history": [history_entry],
                "messages": [
                    ToolMessage(
                        content=f"Success ({len(content):,} chars) - see CODE BANK in prompt",
                        tool_call_id=tool_call_id,
                    )
                ],
            }
        )


# ============================================================================
# Research Node Constructor (LangChain v1 API)
# ============================================================================


def create_research_node(llm: BaseChatModel, tools: List[BaseTool]):
    """
    Create research node using LangChain v1 create_agent with middleware.

    This version uses:
    - LangChain v1 create_agent (replaces deprecated create_react_agent)
    - Custom state schema (ResearchAgentState) with domain-specific fields
    - Middleware pattern for dynamic prompts and state tracking
    - Command-returning tool middleware for state updates
    - InMemorySaver checkpointer to preserve state on recursion limit

    Design: STATELESS per model call
    - Model sees ONLY: SystemMessage(instructions) + SystemMessage(CODE BANK) + tool schemas
    - Model does NOT see: conversation history (state["messages"])
    - All context is embedded in the SystemMessages

    Middleware order (important):
    1. ModelRetryMiddleware: Retries on transient errors (500, 429, timeouts)
       with exponential backoff
    2. ModelCallLimitMiddleware: Limits total model calls, graceful exit on limit
    3. stateless_model_middleware: Builds [SystemMessage(instructions), SystemMessage(CODE BANK)]
    4. code_gathering_middleware: Tracks tool results, returns Command for state updates
    """
    # Create checkpointer to preserve state when recursion limit is hit
    # This allows us to retrieve accumulated code via get_state() on error
    checkpointer = InMemorySaver()

    # Create agent with LangChain v1 API
    # Note: Don't pre-bind tools - create_agent handles this internally
    research_agent = create_agent(
        model=llm,
        tools=tools,
        state_schema=ResearchAgentState,
        middleware=[
            ModelRetryMiddleware(
                max_retries=3,
                backoff_factor=2.0,
                initial_delay=1.0,
            ),
            ModelCallLimitMiddleware(
                run_limit=MAX_MODEL_CALLS,
                exit_behavior="end",  # Graceful termination instead of GraphRecursionError
            ),
            stateless_model_middleware,  # Build SystemMessages (instructions + CODE BANK)
            code_gathering_middleware,  # Track tool results via Command
        ],
        checkpointer=checkpointer,  # Preserve state for recovery on recursion limit
    )

    async def research(state: InvestigationState) -> InvestigationState:
        """Execute research phase using the agent."""
        logger.info(
            "[%s] Research phase (iteration %s)",
            state["issue_id"],
            state["iteration"],
        )
        thread_id = "%s_iter%s_%s" % (
            state["issue_id"],
            state["iteration"],
            uuid.uuid4().hex[:8],
        )
        agent_state = _build_research_agent_state(state)
        invoke_config = {
            "recursion_limit": RESEARCH_AGENT_RECURSION_LIMIT,
            "configurable": {"thread_id": thread_id},
            "run_name": "research_node",
            "type": "actor",
            "run_type": "agent",
        }

        try:
            result = await research_agent.ainvoke(agent_state, config=invoke_config)
            return _handle_research_success(state, result)
        except GraphRecursionError:
            logger.warning(
                "[%s] Research agent hit recursion limit (%s steps) at iteration %s. "
                "Attempting to preserve accumulated code via checkpointer.",
                state["issue_id"],
                RESEARCH_AGENT_RECURSION_LIMIT,
                state["iteration"],
            )
            return _handle_research_recursion_limit(state, research_agent, thread_id)
        except Exception as e:
            logger.error(
                "[%s] Research agent error: %s",
                state["issue_id"],
                e,
                exc_info=True,
            )
            return _handle_research_error(state, research_agent, thread_id, e)

    return research


def _handle_research_success(state: InvestigationState, result: dict) -> InvestigationState:
    """Build state from successful research agent result."""
    fetched = result.get("fetched_files", {})
    history = result.get("tool_call_history", [])
    gathered_code = _merge_gathered_code(state.get("gathered_code", ""), fetched)
    logger.info(
        "[%s] Research complete. Total gathered code: %s chars, %s tools used, %s tool calls",
        state["issue_id"],
        len(gathered_code),
        len(fetched),
        len(history),
    )
    return {
        **state,
        "research_messages": result["messages"],
        "gathered_code": gathered_code,
        "fetched_files": fetched,
        "tool_call_history": history,
    }


def _handle_research_recursion_limit(
    state: InvestigationState, agent, thread_id: str
) -> InvestigationState:
    """Build state when research hits recursion limit; preserve via checkpointer."""
    fetched, history = state.get("fetched_files", {}), state.get("tool_call_history", [])
    fetched, history, from_checkpointer = _get_preserved_state(
        agent, thread_id, fetched, history, log_failure=True
    )
    if from_checkpointer:
        logger.info(
            "[%s] Preserved state via checkpointer: %s tool results, %s tool calls",
            state["issue_id"],
            len(fetched),
            len(history),
        )
    gathered_code = _merge_gathered_code(state.get("gathered_code", ""), fetched)
    return {
        **state,
        "research_messages": [],
        "gathered_code": gathered_code,
        "fetched_files": fetched,
        "tool_call_history": history,
        "stop_reason": ("research_recursion_limit_hit (limit=%s)" % RESEARCH_AGENT_RECURSION_LIMIT),
    }


def _handle_research_error(
    state: InvestigationState, agent, thread_id: str, e: Exception
) -> InvestigationState:
    """Build state when research raises; preserve via checkpointer if possible."""
    fetched, history = state.get("fetched_files", {}), state.get("tool_call_history", [])
    fetched, history, _ = _get_preserved_state(agent, thread_id, fetched, history)
    gathered_code = _merge_gathered_code(state.get("gathered_code", ""), fetched)
    return {
        **state,
        "research_messages": [],
        "gathered_code": gathered_code,
        "fetched_files": fetched,
        "tool_call_history": history,
        "stop_reason": "research_error: %s" % str(e)[:100],
    }


def _build_research_agent_state(state: InvestigationState) -> ResearchAgentState:
    """Build ResearchAgentState from InvestigationState for agent.invoke."""
    return ResearchAgentState(
        messages=[],
        fetched_files=state.get("fetched_files", {}),
        tool_call_history=state.get("tool_call_history", []),
        issue_id=state["issue_id"],
        iteration=state["iteration"],
        issue_description=state["issue_description"],
        initial_code=state["initial_code"],
        evaluation_feedback=state.get("evaluation_feedback", ""),
        required_information=state.get("required_information", []),
    )


def _get_preserved_state(
    agent, thread_id: str, fallback_fetched: dict, fallback_history: list, log_failure: bool = False
):
    """Get fetched_files and tool_call_history from checkpointer or return fallbacks.
    Returns (fetched, history, from_checkpointer: bool).
    If log_failure is True and get_state raises, logs a warning.
    """
    try:
        preserved = agent.get_state({"configurable": {"thread_id": thread_id}})
        if preserved and preserved.values:
            return (
                preserved.values.get("fetched_files", fallback_fetched),
                preserved.values.get("tool_call_history", fallback_history),
                True,
            )
    except Exception as state_err:
        if log_failure:
            logger.warning(
                "Could not retrieve state from checkpointer: %s. Using input state as fallback.",
                state_err,
            )
    return (fallback_fetched, fallback_history, False)


def _merge_gathered_code(existing: str, fetched_files: Dict) -> str:
    """Append code from fetched_files to existing, avoiding duplicates."""
    for _tool_name, code_list in fetched_files.items():
        for code in code_list:
            if code not in existing:
                existing += f"\n\n{code}\n"
    return existing
