"""Extended NIM LLM wrapper to support the extended_${LLM_API_TYPE} config pattern."""

from langchain_nvidia_ai_endpoints import ChatNVIDIA
from nat.builder.builder import Builder
from nat.builder.framework_enum import LLMFrameworkEnum
from nat.builder.llm import LLMProviderInfo
from nat.cli.register_workflow import register_llm_client, register_llm_provider
from nat.llm.nim_llm import NIMModelConfig


class ExtendedNIMModelConfig(NIMModelConfig, name="extended_nim"):
    pass


@register_llm_provider(config_type=ExtendedNIMModelConfig)
async def register_extended_nim_llm(config: ExtendedNIMModelConfig, builder: Builder):
    yield LLMProviderInfo(config=config, description="A NIM model for use with an LLM client.")


@register_llm_client(config_type=ExtendedNIMModelConfig, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
async def create_extended_nim_langchain(llm_config: ExtendedNIMModelConfig, builder: Builder):
    # Explicitly exclude http_client: NIM does not support custom httpx client configuration.
    # When using the shared extended_${LLM_API_TYPE} config block, http_client may appear
    # as an extra field (NIMModelConfig has extra="allow") and must be dropped here.
    yield ChatNVIDIA(**llm_config.model_dump(exclude={"type", "http_client"}, by_alias=True))
