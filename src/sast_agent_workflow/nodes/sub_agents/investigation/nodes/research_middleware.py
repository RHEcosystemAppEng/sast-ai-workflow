"""Research response_format + bridge middleware + termination observability."""

from __future__ import annotations

import json
import logging
import re
from typing import Any, Optional

from langchain.agents.middleware import (
    AgentMiddleware,
    ModelRequest,
    ModelResponse,
    hook_config,
    wrap_model_call,
)
from langchain.agents.structured_output import ToolStrategy
from langchain_core.messages import AIMessage, ToolMessage
from pydantic import ValidationError

from ..constants import CODE_GATHERING_TOOLS
from .research_response import (
    STRUCTURED_TURN_TOOL_NAME,
    ResearchTurnResponse,
    parse_gather_tool_call_from_content,
    parse_turn_from_content,
)

logger = logging.getLogger(__name__)

# Investigation-level stop_reason values propagated from abnormal research exits
RESEARCH_MALFORMED_STOP_REASON = "research_malformed_response"
RESEARCH_COMPLETED_REASON = "research_completed"

# Detail["cause"] sub-codes for logging and debugging
CAUSE_MODEL_COMPLETED = "model_set_completed_true"
CAUSE_INCOMPLETE_TURN_NO_TOOL = "structured_turn_incomplete_no_tool"
CAUSE_NO_AI_MESSAGE = "no_assistant_message"
CAUSE_NO_TURN_NO_TOOLS = "no_structured_turn_and_no_gather_tool_calls"


def create_research_response_format() -> ToolStrategy[ResearchTurnResponse]:
    """Build the ``response_format`` for ``create_agent``.

    Registers ``ResearchTurnResponse`` as a synthetic tool so the model must return
    ``reasoning`` and ``completed`` via tool calling. Gather actions use native tools only.
    LangChain handles parsing and automatic retries on validation errors (``handle_errors=True``).
    """
    return ToolStrategy(ResearchTurnResponse, handle_errors=True)


def create_research_turn_bridge_middleware(state_schema: type):
    """Create ``wrap_model_call`` middleware that bridges structured turns to gather tools.

    Runs immediately after the model (and LangChain structured-output handling). It does
    not re-parse JSON; it only adjusts the ``ModelResponse`` so the agent graph can route
    correctly:

    - ``completed=True`` → keep ``structured_response`` so the loop exits cleanly.
    - ``completed=False`` with native gather ``tool_calls`` → clear ``structured_response``
      so ``ToolNode`` runs instead of exiting early.
    - Strips synthetic ``ResearchTurnResponse`` ``ToolMessage`` rows so LangGraph does not
      treat the gather phase as finished after the first tool (see ``tools_to_model``).
    """

    @wrap_model_call(state_schema=state_schema, name="research_turn_bridge")
    async def research_turn_bridge(request: ModelRequest, handler) -> ModelResponse:
        """Invoke the model, then normalize the response for gather-tool routing."""
        response = await handler(request)
        ai_msg = _first_ai_message(response.result)
        if ai_msg is None:
            return response

        turn = _as_turn(response.structured_response)
        tool_calls = _gather_tool_calls_only(ai_msg.tool_calls)

        if turn is not None:
            return _bridge_structured_turn(ai_msg, turn, tool_calls)

        if tool_calls:
            return _with_gather_calls(ai_msg, tool_calls, response.structured_response)

        fallback_turn = parse_turn_from_content(ai_msg.content)
        if fallback_turn is not None:
            return _bridge_structured_turn(ai_msg, fallback_turn, tool_calls)

        fallback_gather = parse_gather_tool_call_from_content(ai_msg.content)
        if fallback_gather is not None:
            turn = ResearchTurnResponse(
                reasoning=_reasoning_from_content(ai_msg.content) or "Gathering code",
                completed=False,
            )
            calls = list(tool_calls)
            if not any(tc.get("name") == fallback_gather.get("name") for tc in calls):
                calls.append(fallback_gather)
            return _bridge_structured_turn(ai_msg, turn, calls)

        return response

    return research_turn_bridge


class ResearchTerminationMiddleware(AgentMiddleware):
    """Observe and enforce research-loop termination; emit rich stop diagnostics.

    Runs after each model call. Clears a stale ``structured_response`` while research
    continues (prevents exiting after the first gather tool). Stops the loop on policy
    violations and logs structured detail for every abnormal exit.
    """

    def __init__(self, state_schema: type):
        """Attach the agent state schema expected by the research subgraph."""
        super().__init__()
        self._state_schema = state_schema

    @property
    def state_schema(self) -> type:
        return self._state_schema

    @hook_config(can_jump_to=["end", "model"])
    async def aafter_model(self, state: dict[str, Any], runtime) -> Optional[dict[str, Any]]:
        """Clear stale structured state, log normal completion, or abort malformed turns."""
        if state.get("research_stop_reason"):
            return {"jump_to": "end"}

        last_ai = _last_ai_message(state.get("messages") or [])
        turn = _resolve_structured_turn(state, last_ai)
        pending_gather = _pending_gather_tool_calls(state, last_ai)

        if turn is not None and turn.completed:
            detail = _build_termination_detail(
                state,
                cause=CAUSE_MODEL_COMPLETED,
                turn=turn,
                last_ai=last_ai,
                pending_gather=pending_gather,
            )
            _log_termination(
                logging.INFO,
                state,
                RESEARCH_COMPLETED_REASON,
                detail,
                "Model returned completed=true; research loop will end",
            )
            return {"research_termination_detail": detail}

        updates: dict[str, Any] = {}
        if turn is not None and not turn.completed:
            # Prevent LangGraph from exiting on structured_response after tool runs
            updates["structured_response"] = None

        if turn is not None:
            if pending_gather:
                if updates:
                    return updates
                return None
            return _terminate(
                state,
                RESEARCH_MALFORMED_STOP_REASON,
                CAUSE_INCOMPLETE_TURN_NO_TOOL,
                turn=turn,
                last_ai=last_ai,
                pending_gather=pending_gather,
            )

        if last_ai is None:
            return _terminate(
                state,
                RESEARCH_MALFORMED_STOP_REASON,
                CAUSE_NO_AI_MESSAGE,
                last_ai=last_ai,
                pending_gather=pending_gather,
            )

        if pending_gather:
            if updates:
                return updates
            return None

        return _terminate(
            state,
            RESEARCH_MALFORMED_STOP_REASON,
            CAUSE_NO_TURN_NO_TOOLS,
            turn=turn,
            last_ai=last_ai,
            pending_gather=pending_gather,
            extra={"content_preview": _content_preview(last_ai.content)},
        )

    async def aafter_agent(self, state: dict[str, Any], runtime) -> Optional[dict[str, Any]]:
        """Log a final research-phase summary when the agent graph stops."""
        detail = _build_termination_detail(
            state,
            cause="research_agent_finished",
            turn=_as_turn(state.get("structured_response")),
            last_ai=_last_ai_message(state.get("messages") or []),
            pending_gather=_pending_gather_tool_calls(
                state, _last_ai_message(state.get("messages") or [])
            ),
        )
        st = detail.get("structured_turn") or {}
        reason = state.get("research_stop_reason") or (
            RESEARCH_COMPLETED_REASON if st.get("completed") else "research_agent_exit"
        )
        level = logging.ERROR if state.get("research_stop_reason") else logging.INFO
        _log_termination(
            level,
            state,
            reason,
            detail,
            "Research agent graph finished",
        )
        return {"research_termination_detail": detail}


# Backward-compatible alias
ResearchMalformedMiddleware = ResearchTerminationMiddleware


def _bridge_structured_turn(
    ai_msg: AIMessage,
    turn: ResearchTurnResponse,
    existing_gather_calls: list[dict],
) -> ModelResponse:
    """Convert a parsed ``ResearchTurnResponse`` into a ``ModelResponse`` the graph understands.

    Returns only an ``AIMessage`` (no synthetic structured ``ToolMessage``) so the gather
    ``tools`` node is not skipped after the first tool execution.
    """
    if turn.completed:
        return ModelResponse(
            result=[AIMessage(content=turn.reasoning, tool_calls=[], id=ai_msg.id)],
            structured_response=turn,
        )

    # Keep the parsed turn in state so ResearchTerminationMiddleware sees this turn
    # (model_node only writes structured_response when it is not None; clearing here
    # left stale values from earlier iterations and allowed silent graph exit).
    return ModelResponse(
        result=[
            AIMessage(
                content=turn.reasoning,
                tool_calls=list(existing_gather_calls),
                id=ai_msg.id,
            )
        ],
        structured_response=turn,
    )


def _gather_tool_calls_only(tool_calls: list) -> list[dict]:
    """Keep only code-gathering tool calls, excluding ``ResearchTurnResponse``."""
    return [tc for tc in (tool_calls or []) if tc.get("name") in CODE_GATHERING_TOOLS]


def _with_gather_calls(
    ai_msg: AIMessage, tool_calls: list[dict], structured_response: Any
) -> ModelResponse:
    """Return gather-only tool calls; drop structured tool messages from routing."""
    turn = _as_turn(structured_response)
    structured = structured_response if (turn and turn.completed) else None
    return ModelResponse(
        result=[AIMessage(content=ai_msg.content, tool_calls=tool_calls, id=ai_msg.id)],
        structured_response=structured,
    )


def _pending_gather_tool_calls(state: dict[str, Any], last_ai: Optional[AIMessage]) -> list[dict]:
    """Gather tool calls on the last AI message that do not yet have a ``ToolMessage``."""
    if last_ai is None:
        return []
    messages = state.get("messages") or []
    answered_ids = {
        m.tool_call_id for m in messages if isinstance(m, ToolMessage) and m.tool_call_id
    }
    return [
        tc for tc in _gather_tool_calls_only(last_ai.tool_calls) if tc.get("id") not in answered_ids
    ]


def _as_turn(raw: Any) -> Optional[ResearchTurnResponse]:
    """Coerce agent ``structured_response`` state to ``ResearchTurnResponse``, if valid."""
    if raw is None:
        return None
    if isinstance(raw, ResearchTurnResponse):
        return raw
    try:
        return ResearchTurnResponse.model_validate(raw)
    except ValidationError:
        return None


def _turn_from_research_tool_call(ai_msg: Optional[AIMessage]) -> Optional[ResearchTurnResponse]:
    """Parse ``ResearchTurnResponse`` args from the structured-output tool call on an AIMessage."""
    if ai_msg is None:
        return None
    for tc in ai_msg.tool_calls or []:
        if tc.get("name") == STRUCTURED_TURN_TOOL_NAME:
            return _as_turn(tc.get("args"))
    return None


def _resolve_structured_turn(
    state: dict[str, Any], last_ai: Optional[AIMessage]
) -> Optional[ResearchTurnResponse]:
    """Resolve the current turn from state or the latest assistant message."""
    return _as_turn(state.get("structured_response")) or _turn_from_research_tool_call(last_ai)


def _first_ai_message(messages: list) -> Optional[AIMessage]:
    """Return the first AIMessage in a list (used for a single-turn model ``result``)."""
    for msg in messages or []:
        if isinstance(msg, AIMessage):
            return msg
    return None


def _last_ai_message(messages: list) -> Optional[AIMessage]:
    """Return the most recent AIMessage in agent state ``messages``."""
    for msg in reversed(messages or []):
        if isinstance(msg, AIMessage):
            return msg
    return None


def _reasoning_from_content(content: Any) -> str:
    """Extract ``Reasoning: ...`` prefix from plain message content, if present."""
    text = content if isinstance(content, str) else str(content or "")
    match = re.match(r"(?is)^\s*reasoning:\s*(.+?)(?:\n\n|\n)", text.strip(), re.DOTALL)
    return match.group(1).strip() if match else ""


def _content_preview(content: Any, max_len: int = 200) -> str:
    text = content if isinstance(content, str) else str(content)
    text = text.replace("\n", " ").strip()
    return text[:max_len] if len(text) > max_len else text


def _build_termination_detail(
    state: dict[str, Any],
    *,
    cause: str,
    turn: Optional[ResearchTurnResponse] = None,
    last_ai: Optional[AIMessage] = None,
    pending_gather: Optional[list[dict]] = None,
    extra: Optional[dict[str, Any]] = None,
) -> dict[str, Any]:
    """Build a JSON-serializable diagnostic payload for logs and investigation state."""
    history = state.get("tool_call_history") or []
    raw_turn = turn if turn is not None else _as_turn(state.get("structured_response"))
    if raw_turn is None:
        structured_turn = None
    elif isinstance(raw_turn, dict):
        structured_turn = raw_turn
    else:
        structured_turn = raw_turn.model_dump()

    return {
        "cause": cause,
        "issue_id": state.get("issue_id"),
        "iteration": state.get("iteration"),
        "tool_calls_in_history": len(history),
        "tool_history_tail": history[-5:],
        "fetched_file_tools": list((state.get("fetched_files") or {}).keys()),
        "structured_turn": structured_turn,
        "structured_response_in_state": state.get("structured_response") is not None,
        "last_ai_tool_calls": [tc.get("name") for tc in (last_ai.tool_calls or []) if last_ai],
        "last_ai_gather_tool_calls": [
            tc.get("name") for tc in _gather_tool_calls_only(last_ai.tool_calls if last_ai else [])
        ],
        "pending_gather_tool_calls": [tc.get("name") for tc in (pending_gather or [])],
        "last_ai_content_preview": _content_preview(last_ai.content) if last_ai else None,
        "has_structured_tool_message": _has_structured_tool_message(state.get("messages") or []),
        **(extra or {}),
    }


def _has_structured_tool_message(messages: list) -> bool:
    return any(isinstance(m, ToolMessage) and m.name == STRUCTURED_TURN_TOOL_NAME for m in messages)


def _log_termination(
    level: int,
    state: dict[str, Any],
    reason: str,
    detail: dict[str, Any],
    summary: str,
) -> None:
    issue_id = state.get("issue_id", "unknown")
    logger.log(
        level,
        "[%s] %s | reason=%s | %s",
        issue_id,
        summary,
        reason,
        json.dumps(detail, default=str),
    )


def _terminate(
    state: dict[str, Any],
    reason_code: str,
    cause: str,
    *,
    turn: Optional[ResearchTurnResponse] = None,
    last_ai: Optional[AIMessage] = None,
    pending_gather: Optional[list[dict]] = None,
    extra: Optional[dict[str, Any]] = None,
) -> dict[str, Any]:
    """Stop research and attach reason + diagnostic detail to agent state."""
    detail = _build_termination_detail(
        state,
        cause=cause,
        turn=turn,
        last_ai=last_ai,
        pending_gather=pending_gather,
        extra=extra,
    )
    _log_termination(
        logging.ERROR,
        state,
        reason_code,
        detail,
        "Research terminated abnormally",
    )
    return {
        "research_stop_reason": reason_code,
        "research_termination_detail": detail,
        "jump_to": "end",
        "structured_response": None,
    }


def format_research_stop_reason(reason_code: str, detail: Optional[dict[str, Any]]) -> str:
    """Format investigation ``stop_reason`` from research termination metadata."""
    if not detail:
        return reason_code
    cause = detail.get("cause", "unknown")
    tool_calls = detail.get("tool_calls_in_history", "?")
    completed = (detail.get("structured_turn") or {}).get("completed")
    return (
        f"{reason_code} (cause={cause}, tool_calls_in_history={tool_calls}, "
        f"model_completed={completed})"
    )
