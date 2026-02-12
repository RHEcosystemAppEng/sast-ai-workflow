"""
LLM Factory - Direct LLM initialization using existing Config.

This module creates LLM instances without NAT Builder dependency.
"""

import logging
import os
from typing import Optional

import httpx
from langchain_core.language_models.chat_models import BaseChatModel
from langchain_nvidia_ai_endpoints import ChatNVIDIA
from langchain_openai import ChatOpenAI

from common.config import Config

from ..constants import DEFAULT_MAX_TOKENS

logger = logging.getLogger(__name__)


def create_llm(
    config: Config, temperature: Optional[float] = None, max_tokens: Optional[int] = None
) -> BaseChatModel:
    """
    Create LLM instance from configuration (no NAT Builder).

    Args:
        config: Config instance with LLM settings
        temperature: Override temperature (defaults to 0.0)
        max_tokens: Override max tokens (defaults to DEFAULT_MAX_TOKENS)

    Returns:
        BaseChatModel instance (ChatOpenAI or ChatNVIDIA)

    Raises:
        ValueError: If LLM_API_TYPE is unsupported or API key is missing

    Note:
        Built-in retry handles HTTP 500/502/503/429 and timeout errors at the
        LLM call level, preserving agent state between retries.
    """
    if not config.LLM_API_KEY:
        raise ValueError("LLM_API_KEY is required but not set")

    temp = temperature if temperature is not None else 0.0
    tokens = max_tokens if max_tokens is not None else DEFAULT_MAX_TOKENS

    logger.info(f"Creating LLM: {config.LLM_API_TYPE}/{config.LLM_MODEL_NAME}")

    if config.LLM_API_TYPE == "openai":
        # Check if SSL verification should be disabled (for self-signed certs)
        disable_ssl = os.environ.get("DISABLE_SSL_VERIFY", "").lower() in ("true", "1", "yes")
        http_client = None
        async_http_client = None
        if disable_ssl:
            logger.warning("SSL verification disabled for OpenAI client")
            http_client = httpx.Client(verify=False)
            async_http_client = httpx.AsyncClient(verify=False)

        return ChatOpenAI(
            model=config.LLM_MODEL_NAME,
            base_url=config.LLM_URL if config.LLM_URL else None,
            api_key=config.LLM_API_KEY,
            temperature=temp,
            max_tokens=tokens,
            http_client=http_client,
            http_async_client=async_http_client,
        )

    elif config.LLM_API_TYPE == "nim":
        return ChatNVIDIA(
            model=config.LLM_MODEL_NAME,
            base_url=config.LLM_URL,
            api_key=config.LLM_API_KEY,
            temperature=temp,
            max_tokens=tokens,
        )

    else:
        raise ValueError(
            f"Unsupported LLM_API_TYPE: {config.LLM_API_TYPE}. " f"Supported types: 'openai', 'nim'"
        )


def create_embedding_llm(config: Config) -> BaseChatModel:
    """
    Create embedding LLM instance for filter node.

    Args:
        config: Config instance with embedding LLM settings

    Returns:
        BaseChatModel instance for embeddings
    """
    if not config.EMBEDDINGS_LLM_API_KEY:
        logger.warning("EMBEDDINGS_LLM_API_KEY not set, using main LLM API key")
        api_key = config.LLM_API_KEY
    else:
        api_key = config.EMBEDDINGS_LLM_API_KEY

    model_name = config.EMBEDDINGS_LLM_MODEL_NAME or config.LLM_MODEL_NAME
    base_url = config.EMBEDDINGS_LLM_URL or config.LLM_URL
    logger.info("Creating embedding LLM: %s", model_name)

    if config.LLM_API_TYPE == "openai":
        return _create_openai_embedding_llm(model_name, base_url, api_key)
    if config.LLM_API_TYPE == "nim":
        return _create_nim_embedding_llm(model_name, base_url, api_key)
    raise ValueError("Unsupported LLM type for embeddings: %s" % config.LLM_API_TYPE)


def _create_openai_embedding_llm(
    model_name: str, base_url: Optional[str], api_key: str
) -> ChatOpenAI:
    """Build ChatOpenAI instance for embeddings."""
    return ChatOpenAI(
        model=model_name,
        base_url=base_url if base_url else None,
        api_key=api_key,
        temperature=0.0,
    )


def _create_nim_embedding_llm(model_name: str, base_url: str, api_key: str) -> ChatNVIDIA:
    """Build ChatNVIDIA instance for embeddings."""
    return ChatNVIDIA(
        model=model_name,
        base_url=base_url,
        api_key=api_key,
        temperature=0.0,
    )
