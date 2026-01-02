"""
fetch_code tool - NAT-registered tool for retrieving source code.

Fetches source code by file path or symbol name from the repository using
the repo handler. This is the primary code retrieval tool for investigations.
"""

import logging
from typing import Dict, List

from langchain_core.tools import StructuredTool
from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.function import FunctionBaseConfig
from pydantic import BaseModel, Field

from common.config import Config
from handlers.repo_handler_factory import repo_handler_factory
from ..agent_state import SASTAgentState

logger = logging.getLogger(__name__)


def fetch_code_from_error_trace(error_trace: str, repo_handler, issue_id: str = "unknown") -> Dict[str, List[str]]:
    """
    Extract and fetch initial code from SAST error trace.

    This is a STATELESS HELPER FUNCTION (not a tool) called once at investigation start
    to extract initial code context from the error trace.

    Uses repo_handler.get_source_code_blocks_from_error_trace() to extract
    file paths and line numbers from the trace and fetch the relevant code.

    Args:
        error_trace: SAST error trace string (format: "file:line:message")
        repo_handler: Repository handler instance for code retrieval
        issue_id: Issue ID for logging purposes (optional)

    Returns:
        Dict[str, List[str]]: Mapping of file paths to code blocks (each as a list for consistency)
        Returns empty dict if extraction fails or no code found.
    """
    logger.info(f"[{issue_id}] Fetching initial code from error trace")

    try:
        result_dict = repo_handler.get_source_code_blocks_from_error_trace(error_trace)

        if not result_dict:
            logger.warning(f"[{issue_id}] No code extracted from error trace")
            return {}

        # Convert to Dict[str, List[str]] format (wrap each code string in a list)
        formatted_result = {file_path: [code] for file_path, code in result_dict.items()}

        logger.info(
            f"[{issue_id}] Fetched {len(formatted_result)} files from trace: "
            f"{list(formatted_result.keys())}"
        )

        return formatted_result

    except Exception as e:
        logger.error(f"[{issue_id}] Failed to fetch code from trace: {e}", exc_info=True)
        return {}

class FetchCodeInput(BaseModel):
    """Input schema for fetch_code tool."""
    expression_name: str = Field(
        description="Symbol name to fetch (e.g., 'sanitize_input', 'User', 'validate_data')"
    )
    referring_source_code_path: str = Field(
        description="File path where this symbol was referenced (e.g., 'app/views.py')"
    )


class FetchCodeToolConfig(FunctionBaseConfig, name="fetch_code"):
    """Configuration for fetch_code tool."""

    description: str = Field(
        default=(
            "Fetches source code for a specific symbol (function, class, macro) from the repository. "
            "Use when you know the exact symbol name and the file where it's referenced. "
            "Requires two parameters: "
            "(1) expression_name: the symbol to fetch (e.g., 'sanitize_input'), "
            "(2) referring_source_code_path: the file where you saw this symbol (e.g., 'app/views.py'). "
            "Example: If analyzing 'app/views.py' and you see a call to 'clean_data()', "
            "call fetch_code(expression_name='clean_data', referring_source_code_path='app/views.py')."
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

    def _fetch_code(expression_name: str, referring_source_code_path: str) -> str:
        """
        Fetch source code by symbol name (function, class, macro).

        This is a STATELESS tool - it does not access or modify agent state.

        NOTE: For initial code from SAST error trace (file+line), use the
        fetch_code_from_error_trace() helper function instead.

        Examples:
        - fetch_code('sanitize_input', 'app/views.py')
        - fetch_code('validate_user_data', 'lib/validators.py')

        Args:
            expression_name: Symbol name (e.g., 'sanitize_input', 'User', 'MAX_LENGTH')
            referring_source_code_path: File path where this symbol is referenced

        Returns:
            Formatted code with metadata OR error message

        Raises:
            None - all errors returned as error messages in the result string
        """
        logger.info("=" * 80)
        logger.info("FETCH_CODE TOOL CALLED")
        logger.info(f"  expression_name: '{expression_name}' (type: {type(expression_name).__name__})")
        logger.info(
            f"  referring_source_code_path: '{referring_source_code_path}' "
            f"(type: {type(referring_source_code_path).__name__})"
        )
        logger.info("=" * 80)

        # ============================================================
        # Fetch symbol using extract_missing_functions_or_macros
        # ============================================================
        try:
            from types import SimpleNamespace

            instruction = SimpleNamespace(
                expression_name=expression_name,
                referring_source_code_path=referring_source_code_path,
            )

            logger.debug(
                f"Fetching symbol: {expression_name} "
                f"(referring_path={referring_source_code_path})"
            )
            missing_code, new_symbols = _repo_handler.extract_missing_functions_or_macros(
                [instruction], set()
            )

            if not missing_code or expression_name not in new_symbols:
                error_msg = (
                    f"Error: Symbol '{expression_name}' not found in repository.\n"
                    f"The symbol name might be incorrect or not exist.\n"
                    f"Referring file: {referring_source_code_path}\n"
                    f"Try:\n"
                    f"  1. Check the exact spelling (case-sensitive)\n"
                    f"  2. Use search_codebase tool for pattern matching"
                )
                logger.warning(f"Symbol fetch failed: {expression_name}")
                return error_msg

            # extract_missing_functions_or_macros returns formatted code string
            # The code includes file path information in the format "code of {file_path} file:\n{code}"
            code = missing_code
            # Try to extract file path from the code string
            # Format: "code of src/file.c file:\n..."
            import re

            file_match = re.search(r"code of (.+?) file:", code)
            file_path = file_match.group(1) if file_match else "unknown"

            # ============================================================
            # Extract line_range from code and format structured result
            # ============================================================
            # The code from repo_handler includes line numbers in format: "line_number| code"
            # Extract first and last line numbers to get the range
            line_numbers = re.findall(r"^(\d+)\|", code, re.MULTILINE)
            if line_numbers:
                line_range = f"{line_numbers[0]}-{line_numbers[-1]}"
            else:
                line_range = "unknown"

            # Format structured result with explicit metadata fields
            result = f"""=== Fetched Code: {expression_name} ===
File Path: {file_path}
Line Range: {line_range}
Type: Symbol
Referring File: {referring_source_code_path}

{code}

=== End of {expression_name} ==="""

            logger.info(
                f"Successfully fetched: {expression_name} (file={file_path}, lines={line_range})"
            )
            return result

        except Exception as e:
            # Handle any unexpected errors
            error_msg = (
                f"Error: Unexpected failure while fetching '{expression_name}'.\n"
                f"Exception: {str(e)}\n"
                f"This may indicate a repo_handler issue or repository access problem.\n"
                f"Referring file: {referring_source_code_path}"
            )
            logger.error(f"fetch_code exception: {e}", exc_info=True)
            return error_msg

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
                "expression_name": input_data.expression_name,
                "referring_source_code_path": input_data.referring_source_code_path,
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
