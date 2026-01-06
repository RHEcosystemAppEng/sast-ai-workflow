"""
Extended OpenAI embedder (provider and client) to support tiktoken_enabled,
show_progress_bar, and custom http_client.
"""

import httpx
from nat.builder.builder import Builder
from nat.builder.embedder import EmbedderProviderInfo
from nat.builder.framework_enum import LLMFrameworkEnum
from nat.cli.register_workflow import (
    register_embedder_client,
    register_embedder_provider,
)
from nat.embedder.openai_embedder import OpenAIEmbedderModelConfig


class ExtendedOpenAIEmbedderConfig(OpenAIEmbedderModelConfig, name="extended_openai"):
    """Extended OpenAI embedder config - uses native config with custom http_client handling.

    All extra parameters (tiktoken_enabled, show_progress_bar, etc.) are supported
    via the parent class's extra="allow" setting.
    """

    pass


@register_embedder_provider(config_type=ExtendedOpenAIEmbedderConfig)
async def register_extended_openai_embedder(config: ExtendedOpenAIEmbedderConfig, builder: Builder):
    """Register the extended OpenAI embedder provider."""
    yield EmbedderProviderInfo(
        config=config, description="An OpenAI model for use with an Embedder client."
    )


@register_embedder_client(
    config_type=ExtendedOpenAIEmbedderConfig, wrapper_type=LLMFrameworkEnum.LANGCHAIN
)
async def create_extended_openai_langchain(
    embedder_config: ExtendedOpenAIEmbedderConfig, builder: Builder
):
    """Register the extended OpenAI embedder client with http_client dict conversion.

    This client converts http_client dict config to httpx.Client object,
    which is required by langchain_openai.OpenAIEmbeddings.
    """
    from langchain_openai import OpenAIEmbeddings

    config_dict = embedder_config.model_dump(exclude={"type"}, by_alias=True)

    # Convert http_client dict to httpx.Client object
    if "http_client" in config_dict:
        if config_dict["http_client"]:
            config_dict["http_client"] = httpx.Client(**config_dict["http_client"])
        else:
            del config_dict["http_client"]

    # Force tiktoken_enabled to False for non-OpenAI embedders (e.g., sentence-transformers)
    # tiktoken is designed for OpenAI models and uses GPT tokenization, not BERT-based tokenization
    # To allow config override, change to: if "tiktoken_enabled" not in config_dict:
    if "tiktoken_enabled" not in config_dict or config_dict["tiktoken_enabled"]:
        config_dict["tiktoken_enabled"] = False

    yield OpenAIEmbeddings(**config_dict)
