"""
Extended OpenAI embedder (provider and client) to support tiktoken_enabled, show_progress_bar, and custom http_client.
"""

import httpx
from pydantic import Field

from nat.builder.builder import Builder
from nat.builder.embedder import EmbedderProviderInfo
from nat.cli.register_workflow import register_embedder_provider
from nat.embedder.openai_embedder import OpenAIEmbedderModelConfig
from nat.builder.framework_enum import LLMFrameworkEnum
from nat.cli.register_workflow import register_embedder_client


class ExtendedOpenAIEmbedderConfig(OpenAIEmbedderModelConfig, name="extended_openai"):
    """Extended OpenAI embedder that inherits from NAT's base config and adds missing parameters."""

    # Additional parameters needed for SAST workflow
    tiktoken_enabled: bool = Field(default=False, description="Whether to use tiktoken for tokenization.")
    show_progress_bar: bool = Field(default=True, description="Whether to show progress bar during embedding.")
    http_client: dict = Field(
        default={},
        description="Optional dict with httpx.Client parameters. Only used if provided."
    )


@register_embedder_provider(config_type=ExtendedOpenAIEmbedderConfig)
async def extended_openai_embedder(config: ExtendedOpenAIEmbedderConfig, builder: Builder):
    """Register the extended OpenAI embedder with extra parameters support."""
    # Don't modify the config object directly - the client creation will be handled in the client registration
    yield EmbedderProviderInfo(config=config, description="An OpenAI model for use with an Embedder client.")


@register_embedder_client(config_type=ExtendedOpenAIEmbedderConfig, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
async def extended_openai_langchain(embedder_config: ExtendedOpenAIEmbedderConfig, builder: Builder):
    """Register the extended OpenAI embedder client with extra parameters support.
       This implementation is exactly the same as the original, but since NAT maps each client to a provider,
       we needed to create a new client for the extended provider.
    """
    from langchain_openai import OpenAIEmbeddings

    config_dict = embedder_config.model_dump(exclude={"type"}, by_alias=True)

    if "http_client" in config_dict:
        if config_dict["http_client"]:
            config_dict["http_client"] = httpx.Client(**config_dict["http_client"])
        else:
            config_dict["http_client"] = httpx.Client(verify=False)

    if "tiktoken_enabled" not in config_dict or config_dict["tiktoken_enabled"]:
        config_dict["tiktoken_enabled"] = False

    yield OpenAIEmbeddings(**config_dict)
