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

    referring_source_code_path: str = Field(
        description=(
            "Source file path where the reference/symbol appears " "(e.g., 'modules/cyrus-sasl.c')"
        )
    )
    expression_name: str = Field(
        description=(
            "Symbol or expression name to fetch "
            "(e.g., 'sasl_bind_mech', 'authtype_requires_creds')"
        )
    )
    reason: str = Field(description="Reason for fetching this code (for investigation reasoning)")


class FetchCodeToolConfig(FunctionBaseConfig, name="fetch_code"):
    """Configuration for fetch_code tool."""

    description: str = Field(
        default=(
            "Fetches source code definition for a symbol/expression from the repository. "
            "Provide the file path where you saw the reference and the symbol name to fetch. "
            "Use this to understand function implementations, macro definitions, etc."
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

    def _fetch_code(referring_source_code_path: str, expression_name: str, reason: str) -> str:
        """
        Fetch source code by file path or symbol name.

        TODO: Implement actual code fetching logic - current implementation has bugs
        with repo_handler that need to be fixed in a separate task.

        Args:
            referring_source_code_path: Source file path where the reference appears
            expression_name: Symbol or expression name to fetch
            reason: Reason for fetching (for logging)

        Returns:
            Dummy placeholder response
        """
        logger.info(
            f"fetch_code (PLACEHOLDER): referring_path={referring_source_code_path}, "
            f"expression={expression_name}, reason={reason}"
        )

        # Return dummy response for now
        return f"""=== PLACEHOLDER: Code fetching not implemented ===
Referring source: {referring_source_code_path}
Expression to fetch: {expression_name}
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
            {
                "referring_source_code_path": input_data.referring_source_code_path,
                "expression_name": input_data.expression_name,
                "reason": input_data.reason,
            }
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
