"""Structured research turn schema and content fallback (when the model skips tools)."""

from __future__ import annotations

import ast
import re
import uuid
from typing import Any, Optional

from pydantic import BaseModel

from ..constants import CODE_GATHERING_TOOLS
from ..tools.schemas import FetchCodeInput, ReadFileInput, SearchCodebaseInput

STRUCTURED_TURN_TOOL_NAME = "ResearchTurnResponse"

_TOOL_ARG_SCHEMAS: dict[str, type[BaseModel]] = {
    "fetch_code": FetchCodeInput,
    "search_codebase": SearchCodebaseInput,
    "read_file": ReadFileInput,
}

_TOOL_CALL_PATTERN = re.compile(
    r"(?P<name>fetch_code|search_codebase|read_file|list_directory|file_search)"
    r"\s*\((?P<args>[^)]*)\)",
    re.IGNORECASE,
)


class ResearchTurnResponse(BaseModel):
    """Control-plane turn for research (``response_format`` / ``ResearchTurnResponse`` tool).

    - ``reasoning`` + ``completed``: required every turn.
    - Code gathering uses native gather tool calls only (``file_search``, ``fetch_code``, …).
    """

    reasoning: str
    completed: bool = False


def validate_gather_tool_arguments(tool_name: str, arguments: dict[str, Any]) -> dict[str, Any]:
    """Validate and normalize arguments for a code-gathering tool.

    Tools with a dedicated Pydantic input schema (fetch_code, search_codebase, read_file)
    are validated strictly; others (list_directory, file_search) pass through unchanged.

    Raises:
        ValueError: If ``tool_name`` is not a known gather tool.
        ValidationError: If arguments fail schema validation.
    """
    if tool_name not in CODE_GATHERING_TOOLS:
        raise ValueError(f"Unknown gather tool: {tool_name}")
    schema = _TOOL_ARG_SCHEMAS.get(tool_name)
    if schema is None:
        return dict(arguments)
    return schema.model_validate(arguments).model_dump(exclude_none=False)


def make_gather_tool_call(name: str, arguments: dict[str, Any]) -> dict[str, Any]:
    """Build a LangChain/OpenAI-style ``tool_calls`` dict for the gather ToolNode."""
    return {
        "name": name,
        "args": dict(arguments),
        "id": f"call_{uuid.uuid4().hex[:12]}",
        "type": "tool_call",
    }


def parse_turn_from_content(content: Any) -> Optional[ResearchTurnResponse]:
    """Recover research turn metadata from plain ``AIMessage.content``.

    Handles legacy ``Reasoning: ...`` + ``RESEARCH_COMPLETE`` patterns when the model
    did not call the ``ResearchTurnResponse`` structured tool.

    Gather actions in free text are handled by ``parse_gather_tool_call_from_content``.
    """
    text = _content_to_str(content).strip()
    if not text:
        return None

    reasoning, body = _split_reasoning(text)
    if "RESEARCH_COMPLETE" in body.upper():
        return ResearchTurnResponse(
            reasoning=reasoning or "Research complete",
            completed=True,
        )
    return None


def parse_gather_tool_call_from_content(content: Any) -> Optional[dict[str, Any]]:
    """Recover a native gather ``tool_calls`` entry from pseudo-calls in message content.

    Example: ``search_codebase(pattern='x', file_pattern='*.go')`` in text with no
    native ``tool_calls``. Returns ``None`` if the content cannot be parsed.
    """
    text = _content_to_str(content).strip()
    if not text:
        return None

    _, body = _split_reasoning(text)
    match = _TOOL_CALL_PATTERN.search(body)
    if not match:
        return None

    try:
        arguments = _parse_kw_args(match.group("args"))
        tool_name = match.group("name").lower()
        arguments = validate_gather_tool_arguments(tool_name, arguments)
    except ValueError:
        return None

    return make_gather_tool_call(tool_name, arguments)


def _content_to_str(content: Any) -> str:
    """Normalize AIMessage content (str or multimodal blocks) to a single string."""
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return "\n".join(
            block if isinstance(block, str) else str(block.get("text", ""))
            for block in content
            if isinstance(block, (str, dict))
        )
    return str(content) if content else ""


def _split_reasoning(text: str) -> tuple[str, str]:
    """Split ``Reasoning: ...`` prefix from the rest of the message body.

    Returns:
        ``(reasoning, body)`` — ``reasoning`` is empty if no prefix was found.
    """
    match = re.match(r"(?is)^\s*reasoning:\s*(.+?)(?:\n\n|\n)(.*)$", text, re.DOTALL)
    if match:
        return match.group(1).strip(), match.group(2).strip()
    return "", text.strip()


def _parse_kw_args(args_str: str) -> dict[str, Any]:
    """Parse ``key='value', n=1`` style argument lists from free-text tool calls.

    Uses ``ast`` so only literal values are accepted (no arbitrary code execution).

    Raises:
        ValueError: If the string is not a well-formed keyword argument list.
    """
    parsed = ast.parse(f"f({args_str.strip()})", mode="eval")
    call = parsed.body
    if not isinstance(call, ast.Call):
        raise ValueError(f"Expected a function call, got: {args_str!r}")
    return {kw.arg: ast.literal_eval(kw.value) for kw in call.keywords if kw.arg is not None}
