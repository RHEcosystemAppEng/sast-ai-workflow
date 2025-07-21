import httpx
import logging
from typing import Optional
from langchain_openai import OpenAIEmbeddings
from langchain_openai.chat_models.base import ChatOpenAI
from langchain_nvidia_ai_endpoints import ChatNVIDIA
from langchain_core.language_models.base import BaseLanguageModel

from common.config import Config

logger = logging.getLogger(__name__)


class LLMClientFactory:
    """Factory class for creating different types of LLM clients."""
    
    def __init__(self):
        self._main_llm_cache: Optional[BaseLanguageModel] = None
        self._embedding_llm_cache: Optional[OpenAIEmbeddings] = None
        self._critique_llm_cache: Optional[BaseLanguageModel] = None
    
    def create_main_llm(self, config: Config) -> BaseLanguageModel:
        """Create main LLM client with caching."""
        if self._main_llm_cache is None:
            self._main_llm_cache = self._create_llm_client(
                base_url=config.LLM_URL,
                model_name=config.LLM_MODEL_NAME,
                api_key=config.LLM_API_KEY,
                temperature=0
            )
        return self._main_llm_cache
    
    def create_embedding_llm(self, config: Config) -> OpenAIEmbeddings:
        """Create embedding LLM client with caching."""
        if self._embedding_llm_cache is None:
            # Create a custom httpx client with SSL verification disabled
            custom_embedding_http_client = httpx.Client(verify=False)
            
            self._embedding_llm_cache = OpenAIEmbeddings(
                openai_api_base=config.EMBEDDINGS_LLM_URL,
                openai_api_key=config.EMBEDDINGS_LLM_API_KEY,
                model=config.EMBEDDINGS_LLM_MODEL_NAME,
                tiktoken_enabled=False,
                show_progress_bar=True,
                http_client=custom_embedding_http_client
            )
        return self._embedding_llm_cache
    
    def create_critique_llm(self, config: Config) -> BaseLanguageModel:
        """Create critique LLM client with caching."""
        if self._critique_llm_cache is None:
            critique_api_key = getattr(config, "CRITIQUE_LLM_API_KEY", "dummy_key")
            
            self._critique_llm_cache = self._create_llm_client(
                base_url=config.CRITIQUE_LLM_URL,
                model_name=config.CRITIQUE_LLM_MODEL_NAME,
                api_key=critique_api_key,
                temperature=0.6 if "nvidia" in config.CRITIQUE_LLM_URL.lower() else 0,
                top_p=None if "nvidia" in config.CRITIQUE_LLM_URL.lower() else 0.01
            )
        return self._critique_llm_cache
    
    def _create_llm_client(
        self, 
        base_url: str, 
        model_name: str, 
        api_key: str, 
        temperature: float, 
        top_p: Optional[float] = None
    ) -> BaseLanguageModel:
        """Create LLM client based on the base URL."""
        http_client = httpx.Client(verify=False)
        
        if "nvidia" in base_url.lower():
            return ChatNVIDIA(
                base_url=base_url,
                model=model_name,
                api_key=api_key,
                temperature=temperature,
            )
        else:
            client_kwargs = {
                "base_url": base_url,
                "model": model_name,
                "api_key": api_key,
                "temperature": temperature,
                "http_client": http_client,
            }
            if top_p is not None:
                client_kwargs["top_p"] = top_p
            
            return ChatOpenAI(**client_kwargs)
    
    def clear_cache(self):
        """Clear all cached LLM clients."""
        self._main_llm_cache = None
        self._embedding_llm_cache = None
        self._critique_llm_cache = None 