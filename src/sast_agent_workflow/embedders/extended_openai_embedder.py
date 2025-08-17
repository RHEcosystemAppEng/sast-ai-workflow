"""
Extended OpenAI embedder (provider and client) to support tiktoken_enabled, show_progress_bar, and custom http_client.
"""

import httpx
from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.embedder import EmbedderProviderInfo
from aiq.cli.register_workflow import register_embedder_provider
from aiq.embedder.openai_embedder import OpenAIEmbedderModelConfig
from aiq.builder.framework_enum import LLMFrameworkEnum
from aiq.cli.register_workflow import register_embedder_client


class ExtendedOpenAIEmbedderConfig(OpenAIEmbedderModelConfig, name="extended_openai"):
    """Extended OpenAI embedder that inherits from AIQ's base config and adds missing parameters."""

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
    # Create custom http client based on config    
    if config.http_client:
        config.http_client = httpx.Client(**config.http_client)
    
    yield EmbedderProviderInfo(config=config, description="An OpenAI model for use with an Embedder client.")


@register_embedder_client(config_type=ExtendedOpenAIEmbedderConfig, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
async def extended_openai_langchain(embedder_config: ExtendedOpenAIEmbedderConfig, builder: Builder):
    """Register the extended OpenAI embedder client with extra parameters support.
       This implementation is exactly the same as the original, but since AIQ maps each client to a provider,
       we needed to create a new client for the extended provider.
    """
    from langchain_openai import OpenAIEmbeddings

    yield OpenAIEmbeddings(**embedder_config.model_dump(exclude={"type"}, by_alias=True))
