"""Extended OpenAI LLM (provider and client) with typed http_client support."""

from nat.builder.builder import Builder
from nat.builder.framework_enum import LLMFrameworkEnum
from nat.builder.llm import LLMProviderInfo
from nat.cli.register_workflow import register_llm_client, register_llm_provider
from nat.llm.openai_llm import OpenAIModelConfig

from sast_agent_workflow.llms.http_client_mixin import HttpClientMixin


class ExtendedOpenAIModelConfig(OpenAIModelConfig, HttpClientMixin, name="extended_openai"):
    """Extended OpenAI LLM config with typed http_client support.

    Inherits HttpClientMixin to expose http_client as a validated field.
    Other OpenAI-native parameters are supported via the parent's extra="allow" setting.
    """

    pass


@register_llm_provider(config_type=ExtendedOpenAIModelConfig)
async def register_extended_openai_llm(config: ExtendedOpenAIModelConfig, builder: Builder):
    yield LLMProviderInfo(config=config, description="An OpenAI model for use with an LLM client.")


@register_llm_client(config_type=ExtendedOpenAIModelConfig, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
async def create_extended_openai_langchain(llm_config: ExtendedOpenAIModelConfig, builder: Builder):
    from langchain_openai import ChatOpenAI

    config_dict = {"stream_usage": True, **llm_config.model_dump(exclude={"type"}, by_alias=True)}
    llm_config.resolve_httpx_clients(config_dict, include_async=True)
    yield ChatOpenAI(**config_dict)
