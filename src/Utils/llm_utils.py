import json
import logging
import re
import time
import traceback
from typing import Any, Dict, List, Optional, Type

from langchain_core.exceptions import LangChainException
from langchain_core.runnables import RunnableSerializable
from langchain_nvidia_ai_endpoints import ChatNVIDIA
from langchain_openai.chat_models.base import ChatOpenAI
from pydantic import BaseModel

# from langchain.globals import set_debug

# set_debug(True)  # Enables LangChain debug mode globally

logger = logging.getLogger(__name__)


ERROR_MESSAGE = (
    "Parsing failed after {max_retries} retries: {exception}"
    "\nFailed on input (truncated): {input_truncated}"
    "\nThis indicates a persistent issue with the model or the input data."
    "\nPlease investigate the root cause to resolve this problem."
)
WARNING_MESSAGE = "\033[91mWARNING: An error occurred during model output parsing. \
    Model type is {model_type}. Retrying now. \033[0m"


def robust_structured_output(
    llm: ChatNVIDIA | ChatOpenAI,
    schema: Type[BaseModel],
    input: str,
    prompt_chain: RunnableSerializable,
    max_retries: int = 1,
    config: Optional[Dict[str, Any]] = None,
) -> BaseModel:
    """
    Determines the type of LLM and delegates to the appropriate handler.

    Args:
        config: Optional LangChain config dict. Supports 'run_name' for Langfuse tracing.
    """
    if isinstance(llm, ChatOpenAI):
        return _handle_chat_openai(llm, schema, input, prompt_chain, max_retries, config)
    elif isinstance(llm, ChatNVIDIA):
        return _handle_chat_nvidia(llm, schema, input, prompt_chain, max_retries, config)
    else:
        raise ValueError(f"Unsupported LLM type: {type(llm)}")


def _handle_chat_openai(
    llm: ChatOpenAI,
    schema: Type[BaseModel],
    input: str,
    prompt_chain: RunnableSerializable,
    max_retries: int,
    config: Optional[Dict[str, Any]] = None,
) -> BaseModel:
    """
    Handles structured output for ChatOpenAI.
    Uses json_schema with strict=True mode, which either returns a valid
    parsed object or raises an exception. No fallback logic needed.
    """
    start_time = time.time()
    try:
        structured_llm = llm.with_structured_output(schema, method="json_schema", strict=True)
        llm_chain = prompt_chain | structured_llm

        result = llm_chain.invoke(input, config=config)
        duration = time.time() - start_time

        logger.info("Parsed successfully! Duration: %.2fs", duration)
        return result

    except Exception as e:
        duration = time.time() - start_time
        logger.error("Parsing failed. Duration: %.2fs", duration)
        logger.error("Error in _handle_chat_openai: %s: %s", type(e).__name__, e)
        logger.error(traceback.format_exc())
        raise


def _handle_chat_nvidia(
    llm: ChatNVIDIA,
    schema: Type[BaseModel],
    input: str,
    prompt_chain: RunnableSerializable,
    max_retries: int,
    config: Optional[Dict[str, Any]] = None,
) -> BaseModel:
    """
    Handles structured output for ChatNVIDIA with retry logic and JSON fallback.

    ChatNVIDIA doesn't support include_raw=True, instead it returns None when
    structured output fails. This implementation includes a fallback to JSON mode
    with manual parsing when with_structured_output() returns None.

    Approach:
    1. Try with_structured_output() first (native structured output)
    2. If it returns None, fallback to JSON mode:
       - Explicitly request JSON response from LLM
       - Extract JSON from response (handles markdown code blocks)
       - Manually parse and construct Pydantic object
    """
    last_exception = None

    for attempt in range(max_retries):
        structured_llm = llm.with_structured_output(schema)
        llm_chain = prompt_chain | structured_llm

        try:
            result = llm_chain.invoke(input, config=config)
            if result is not None:
                logger.info("✓ Structured output successful on attempt %s", attempt + 1)
                return result

            logger.warning(
                "Attempt %s/%s: with_structured_output() returned None. "
                "Falling back to JSON mode with manual parsing...",
                attempt + 1,
                max_retries,
            )
            fallback_result, fallback_error = _run_json_fallback(
                llm, schema, input, config, attempt, max_retries
            )
            if fallback_result is not None:
                return fallback_result
            if fallback_error is not None:
                last_exception = fallback_error

        except Exception as e:
            last_exception = e
            logger.warning(
                "Attempt %s/%s: Unexpected error: %s: %s",
                attempt + 1,
                max_retries,
                type(e).__name__,
                str(e)[:100],
            )

        if attempt < max_retries - 1:
            logger.info("Retrying... (attempt %s/%s)", attempt + 2, max_retries)
        logger.warning(WARNING_MESSAGE.format(model_type=schema.__name__))

    # Truncate input to prevent massive error messages that could cause prompt bloat
    input_truncated = input[:500] + "..." if len(input) > 500 else input
    raise LangChainException(
        ERROR_MESSAGE.format(
            max_retries=max_retries, exception=last_exception, input_truncated=input_truncated
        )
    )


def _run_json_fallback(
    llm: ChatNVIDIA,
    schema: Type[BaseModel],
    input: str,
    config: Optional[Dict[str, Any]],
    attempt: int,
    max_retries: int,
) -> tuple[Optional[BaseModel], Optional[Any]]:
    """
    Run one JSON fallback attempt: request JSON from LLM, extract and parse.
    Returns (result, None) on success, (None, last_exception) on failure.
    """
    json_prompt = _build_json_fallback_prompt(input, schema)
    logger.debug("Requesting JSON response (attempt %s)...", attempt + 1)
    raw_response = llm.invoke(json_prompt, config=config)
    raw_text = raw_response.content if hasattr(raw_response, "content") else str(raw_response)
    logger.debug("Raw response preview: %s...", raw_text[:300])

    json_text = _extract_json_from_response(raw_text)
    try:
        parsed_data = json.loads(json_text)
        result = schema(**parsed_data)
        logger.info(
            "✓ JSON fallback successful on attempt %s (%s chars parsed)",
            attempt + 1,
            len(json_text),
        )
        return (result, None)
    except json.JSONDecodeError as json_error:
        logger.warning(
            "Attempt %s/%s: JSON parsing failed: %s\nJSON text preview: %s...",
            attempt + 1,
            max_retries,
            json_error,
            json_text[:200],
        )
        return (None, f"JSON parsing failed: {json_error}")
    except (ValueError, TypeError) as validation_error:
        logger.warning(
            "Attempt %s/%s: Pydantic validation failed: %s\nParsed data: %s...",
            attempt + 1,
            max_retries,
            validation_error,
            str(parsed_data)[:200],
        )
        return (None, f"Pydantic validation failed: {validation_error}")


def _build_json_fallback_prompt(input: str, schema: Type[BaseModel]) -> str:
    """Build prompt that explicitly requests JSON matching the given schema."""
    schema_json = schema.schema_json(indent=2)
    return (
        f"{input}\n\n"
        f"═══════════════════════════════════════════════════════════\n"
        f"CRITICAL: You MUST respond with VALID JSON matching this schema:\n"
        f"═══════════════════════════════════════════════════════════\n"
        f"{schema_json}\n"
        f"═══════════════════════════════════════════════════════════\n\n"
        f"REQUIREMENTS:\n"
        f"1. Your ENTIRE response must be valid JSON\n"
        f"2. Include ALL required fields from the schema\n"
        f"3. Do not include any text before or after the JSON\n"
        f"4. You may wrap JSON in markdown code block: ```json {{...}} ```\n"
    )


def _extract_json_from_response(raw_text: str) -> str:
    """Extract JSON string from LLM response (markdown block, raw object, or whole response)."""
    json_match = re.search(r"```(?:json)?\s*(\{[^\}]*\})\s*```", raw_text, re.DOTALL)
    if json_match:
        return json_match.group(1)
    json_match = re.search(r"\{.*\}", raw_text, re.DOTALL)
    if json_match:
        return json_match.group(0)
    return raw_text.strip()


def format_source_code_for_analysis(source_code: Dict[str, List[str]]) -> str:
    """
    Convert the structured source_code dict to formatted string for LLM analysis.

    Args:
        source_code: Dict mapping file paths to lists of code snippets

    Returns:
        Formatted string with proper separators and headers
    """
    if not source_code:
        return ""

    formatted_sections = []
    for path, code_snippets in source_code.items():
        if code_snippets:
            # Join with double newlines to separate different code snippets
            combined_code = "\n\n".join(code_snippets)
            formatted_sections.append(f"\ncode of {path} file:\n{combined_code}")

    return "".join(formatted_sections)
