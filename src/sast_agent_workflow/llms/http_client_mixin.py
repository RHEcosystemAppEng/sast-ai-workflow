"""Mixin providing typed http_client configuration for OpenAI-compatible configs."""

from typing import Optional

import httpx
from pydantic import BaseModel, Field


class HttpClientConfig(BaseModel):
    """Typed httpx client configuration.

    Fields map directly to httpx.Client / httpx.AsyncClient kwargs.
    Add new fields here as new httpx kwargs are needed (e.g. timeout, cert, proxies).
    """

    verify: bool = Field(
        default=True,
        description="SSL certificate verification. Set to false for internal/airgapped endpoints.",
    )


class HttpClientMixin(BaseModel):
    """Opt-in mixin for OpenAI-compatible configs that support custom httpx client configuration.

    Inherit from this to declare http_client as a typed, validated field.
    Non-OpenAI configs (e.g. NIM) should NOT inherit this mixin.
    """

    http_client: Optional[HttpClientConfig] = Field(
        default=None,
        description="Optional httpx client configuration for custom SSL/network settings.",
    )

    def resolve_httpx_clients(self, config_dict: dict, include_async: bool = True) -> dict:
        """Replace the http_client entry in config_dict with instantiated httpx objects.

        Modifies config_dict in-place and returns it.

        Args:
            config_dict: Dict from model_dump(), will be passed to a LangChain client.
            include_async: If True, also sets http_async_client (required by ChatOpenAI).
                           Set to False for embedders which only use sync clients.
        """
        if not config_dict.get("http_client"):
            config_dict.pop("http_client", None)
            return config_dict

        kwargs = config_dict.pop("http_client")
        config_dict["http_client"] = httpx.Client(**kwargs)
        if include_async:
            config_dict["http_async_client"] = httpx.AsyncClient(**kwargs)
        return config_dict
