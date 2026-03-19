"""Extended OpenAI embedder (provider and client) with typed http_client support."""

from nat.builder.builder import Builder
from nat.builder.embedder import EmbedderProviderInfo
from nat.builder.framework_enum import LLMFrameworkEnum
from nat.cli.register_workflow import (
    register_embedder_client,
    register_embedder_provider,
)
from nat.embedder.openai_embedder import OpenAIEmbedderModelConfig

from sast_agent_workflow.llms.http_client_mixin import HttpClientMixin


class ExtendedOpenAIEmbedderConfig(
    OpenAIEmbedderModelConfig, HttpClientMixin, name="extended_openai_embedder"
):
    """Extended OpenAI embedder config with typed http_client support.

    Inherits HttpClientMixin to expose http_client as a validated field.
    Other OpenAI-native parameters (tiktoken_enabled, show_progress_bar, etc.) are
    supported via the parent's extra="allow" setting.
    """

    pass


@register_embedder_provider(config_type=ExtendedOpenAIEmbedderConfig)
async def register_extended_openai_embedder(config: ExtendedOpenAIEmbedderConfig, builder: Builder):
    yield EmbedderProviderInfo(
        config=config, description="An OpenAI model for use with an Embedder client."
    )


@register_embedder_client(
    config_type=ExtendedOpenAIEmbedderConfig, wrapper_type=LLMFrameworkEnum.LANGCHAIN
)
async def create_extended_openai_langchain(
    embedder_config: ExtendedOpenAIEmbedderConfig, builder: Builder
):
    from langchain_openai import OpenAIEmbeddings

    config_dict = embedder_config.model_dump(exclude={"type"}, by_alias=True)
    embedder_config.resolve_httpx_clients(config_dict, include_async=False)

    # Force tiktoken_enabled to False for non-OpenAI embedders (e.g., sentence-transformers)
    # tiktoken is designed for OpenAI models and uses GPT tokenization, not BERT-based tokenization
    # To allow config override, change to: if "tiktoken_enabled" not in config_dict:
    if "tiktoken_enabled" not in config_dict or config_dict["tiktoken_enabled"]:
        config_dict["tiktoken_enabled"] = False

    # Disable context-length checks so embed_documents batches purely by chunk_size.
    # This avoids sending all documents in a single request and respects the server's
    # max_batch_requests limit (configure chunk_size in the embedder config).
    config_dict.setdefault("check_embedding_ctx_length", False)

    yield OpenAIEmbeddings(**config_dict)
