"""
LLMClientFactory - Responsible for creating and managing different LLM clients.
Handles OpenAI and NVIDIA LLM client creation with proper configuration.
"""

import logging

import httpx
from langchain_core.embeddings import Embeddings
from langchain_core.language_models.chat_models import BaseChatModel
from langchain_nvidia_ai_endpoints import ChatNVIDIA
from langchain_openai import OpenAIEmbeddings
from langchain_openai.chat_models.base import ChatOpenAI
from pydantic.types import SecretStr

from common.config import Config
from handlers.embedding_connection_pool import get_embedding_client

logger = logging.getLogger(__name__)


class LLMClientFactory:
    """Factory class for creating different types of LLM clients"""

    def create_main_llm(self, config: Config) -> BaseChatModel:
        """Create the main LLM client for analysis tasks"""
        http_client = httpx.Client(verify=False)

        if "nvidia" in config.LLM_URL.lower():
            return ChatNVIDIA(
                base_url=config.LLM_URL,
                model=config.LLM_MODEL_NAME,
                api_key=SecretStr(config.LLM_API_KEY) if config.LLM_API_KEY else None,
                temperature=0,
            )
        else:
            return ChatOpenAI(
                base_url=config.LLM_URL,
                model=config.LLM_MODEL_NAME,
                api_key=SecretStr(config.LLM_API_KEY) if config.LLM_API_KEY else None,
                temperature=0,
                http_client=http_client,
            )

    def create_embedding_llm(self, config: Config) -> Embeddings:
        """Create the embedding LLM client for vector operations with connection pooling"""
        try:
            pooled_http_client = get_embedding_client(config)
            logger.debug("Using pooled HTTP client for embedding requests")
        except Exception as e:
            logger.warning(f"Failed to get pooled embedding client: {e}")
            logger.debug("Falling back to fresh HTTP client for embedding requests")
            pooled_http_client = httpx.Client(verify=False)

        return OpenAIEmbeddings(
            openai_api_base=config.EMBEDDINGS_LLM_URL,
            openai_api_key=(
                SecretStr(config.EMBEDDINGS_LLM_API_KEY) if config.EMBEDDINGS_LLM_API_KEY else None
            ),
            model=config.EMBEDDINGS_LLM_MODEL_NAME,
            tiktoken_enabled=False,
            show_progress_bar=True,
            http_client=pooled_http_client,
        )

    def create_critique_llm(self, config: Config) -> BaseChatModel:
        """Create the critique LLM client for evaluation tasks"""
        http_client = httpx.Client(verify=False)

        if "nvidia" in config.CRITIQUE_LLM_URL.lower():
            return ChatNVIDIA(
                base_url=config.CRITIQUE_LLM_URL,
                model=config.CRITIQUE_LLM_MODEL_NAME,
                api_key=(
                    SecretStr(config.CRITIQUE_LLM_API_KEY)
                    if hasattr(config, "CRITIQUE_LLM_API_KEY") and config.CRITIQUE_LLM_API_KEY
                    else None
                ),
                temperature=0.6,
            )
        else:
            return ChatOpenAI(
                base_url=config.CRITIQUE_LLM_URL,
                model=config.CRITIQUE_LLM_MODEL_NAME,
                api_key="dummy_key",
                temperature=0,
                top_p=0.01,
                http_client=http_client,
            )
