"""
fetch_code tool - NAT-registered tool for retrieving source code.

Fetches source code by file path or symbol name from the repository using
the repo handler. This is the primary code retrieval tool for investigations.
"""

import logging

from langchain_core.tools import StructuredTool
from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.function import FunctionBaseConfig
from pydantic import BaseModel, Field

from common.config import Config
from handlers.repo_handler_factory import repo_handler_factory

logger = logging.getLogger(__name__)


class FetchCodeInput(BaseModel):
    """Input schema for fetch_code tool."""

    identifier: str = Field(description="File path (e.g., 'app/views.py') or symbol name to fetch")
    reason: str = Field(description="Reason for fetching this code (for investigation reasoning)")


class FetchCodeToolConfig(FunctionBaseConfig, name="fetch_code"):
    """Configuration for fetch_code tool."""

    description: str = Field(
        default=(
            "Fetches source code by file path or symbol name from the repository. "
            "Use for known files from SAST trace OR exact symbol names. "
            "Fastest retrieval method but requires exact identifiers."
        ),
        description="Tool description",
    )


@register_function(config_type=FetchCodeToolConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN])
async def register_fetch_code_tool(config: FetchCodeToolConfig, builder: Builder):
    """Register the fetch_code tool with NAT as a LangChain StructuredTool."""
    logger.info("Registering fetch_code tool...")

    # Get repo handler - will be shared across tool calls
    # NOTE: In production, this should come from a context/config injection system
    try:
        global_config = Config()
        _repo_handler = repo_handler_factory(global_config)  # noqa: F841
        logger.info(f"Initialized repo_handler for: {global_config.REPO_LOCAL_PATH}")
    except Exception as e:
        logger.error(f"Failed to initialize repo_handler: {e}")
        raise

    def _fetch_code(identifier: str, reason: str) -> str:
        """
        Fetch source code by file path or symbol name.

        TODO: Implement actual code fetching logic - current implementation has bugs
        with repo_handler that need to be fixed in a separate task.

        Args:
            identifier: File path or symbol name
            reason: Reason for fetching (for logging)

        Returns:
            Dummy placeholder response
        """
        logger.info(f"fetch_code (PLACEHOLDER): identifier={identifier}, reason={reason}")

        # Return dummy response for now
        return f"""=== PLACEHOLDER: Code fetching not implemented ===
Requested: {identifier}
Reason: {reason}

TODO: Implement actual code fetching logic.
The repo_handler integration needs fixes for proper symbol/file resolution.
"""

    # Create LangChain StructuredTool
    fetch_code_tool = StructuredTool.from_function(
        func=_fetch_code,
        name="fetch_code",
        description=config.description,
        args_schema=FetchCodeInput,
    )

    # Create async wrapper for NAT 1.3.1
    async def fetch_code_fn(input_data: FetchCodeInput) -> str:
        """Async wrapper for fetch_code tool."""
        return fetch_code_tool.invoke(
            {"identifier": input_data.identifier, "reason": input_data.reason}
        )

    try:
        # NAT 1.3.1+ uses from_fn with async functions
        yield FunctionInfo.from_fn(
            fetch_code_fn,
            description=config.description,
            input_schema=FetchCodeInput,
        )
    except GeneratorExit:
        logger.info("fetch_code tool exited!")
    finally:
        logger.debug("Cleaning up fetch_code tool")
