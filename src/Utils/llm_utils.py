import logging
import time
import traceback
from typing import Dict, List, Type

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
    "\nFailed on input: {input}"
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
) -> BaseModel:
    """
    Determines the type of LLM and delegates to the appropriate handler.
    """
    if isinstance(llm, ChatOpenAI):
        return _handle_chat_openai(llm, schema, input, prompt_chain, max_retries)
    elif isinstance(llm, ChatNVIDIA):
        return _handle_chat_nvidia(llm, schema, input, prompt_chain, max_retries)
    else:
        raise ValueError(f"Unsupported LLM type: {type(llm)}")


def _handle_chat_openai(
    llm: ChatOpenAI,
    schema: Type[BaseModel],
    input: str,
    prompt_chain: RunnableSerializable,
    max_retries: int,
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

        result = llm_chain.invoke(input)
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
) -> BaseModel:
    """
    Handles structured output for ChatNVIDIA with retry logic.
    ChatNVIDIA not soppurted include_raw=True, instead it return None when failed to parse.
    """
    for attempt in range(max_retries):
        structured_llm = llm.with_structured_output(schema)
        llm_chain = prompt_chain | structured_llm
        try:
            result = llm_chain.invoke(input)
            if result is not None:
                return result
            else:
                last_exception = (
                    "No exception was raised, but the response is None."
                    "\nThis indicates that the response data was either "
                    "insufficient for object construction or was invalid."
                )
        except Exception as e:
            last_exception = e
        logger.warning(WARNING_MESSAGE.format(model_type=schema))

    raise LangChainException(
        ERROR_MESSAGE.format(max_retries=max_retries, exception=last_exception, input=input)
    )


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
