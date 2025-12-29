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
from ..agent_state import SASTAgentState

logger = logging.getLogger(__name__)


def fetch_code_from_error_trace(state, error_trace: str, repo_handler) -> None:
    """
    Extract and fetch initial code from SAST error trace.

    This is a HELPER FUNCTION (not a tool) called once at investigation start
    to populate initial code context from the error trace.

    Uses repo_handler.get_source_code_blocks_from_error_trace() to extract
    file paths and line numbers from the trace and fetch the relevant code.

    Args:
        state: SASTAgentState to update with fetched code
        error_trace: SAST error trace string (format: "file:line:message")
        repo_handler: Repository handler instance for code retrieval

    Returns:
        None - updates state.context.fetched_files and state.context.found_symbols in-place
    """
    logger.info(f"[{state.issue_id}] Fetching initial code from error trace")

    try:
        result_dict = repo_handler.get_source_code_blocks_from_error_trace(error_trace)

        if not result_dict:
            logger.warning(f"[{state.issue_id}] No code extracted from error trace")
            return

        for file_path, code in result_dict.items():
            state.context.fetched_files[file_path] = [code]
            state.context.found_symbols.add(file_path)
            logger.debug(f"[{state.issue_id}] Fetched from trace: {file_path}")

        logger.info(
            f"[{state.issue_id}] Fetched {len(result_dict)} files from trace: "
            f"{list(result_dict.keys())}"
        )

    except Exception as e:
        logger.error(f"[{state.issue_id}] Failed to fetch code from trace: {e}", exc_info=True)

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

    def _fetch_code(state: SASTAgentState, identifier: str, reason: str) -> str:
        """
        Fetch source code by symbol name (function, class, macro).

        Use for exact symbol names when you know the identifier.
        Automatically determines where the symbol was referenced from already-fetched code.

        NOTE: For initial code from SAST error trace (file+line), use the
        fetch_code_from_error_trace() helper function instead.

        Examples:
        - fetch_code(state, 'sanitize_input', 'Need to verify sanitization logic')
        - fetch_code(state, 'validate_user_data', 'Check validation function')

        Args:
            state: Agent state (for accessing fetched_files and context)
            identifier: Symbol name (e.g., 'sanitize_input', 'User', 'MAX_LENGTH')
            reason: Reason for fetching (for investigation reasoning)

        Returns:
            Formatted code with metadata OR error message

        Raises:
            None - all errors returned as error messages in the result string
        """
        logger.info(f"fetch_code: identifier={identifier}, reason={reason}")

        # ============================================================
        # STEP 3: Anti-loop protection - check for duplicates
        # ============================================================
        # Check found_symbols (not fetched_files) to allow retries with different params
        if identifier in state.context.found_symbols:
            error_msg = (
                f"Error: '{identifier}' was already successfully fetched.\n"
                f"The code is already in your context. Review existing fetched files instead."
            )
            logger.warning(f"Duplicate fetch attempt: {identifier}")
            return error_msg

        # ============================================================
        # STEP 4: Determine referring path from already-fetched code
        # ============================================================
        # Search through fetched files to find where this symbol might be referenced
        referring_path = "unknown"
        for file_path, code_blocks in state.context.fetched_files.items():
            for code in code_blocks:
                if identifier in code:
                    referring_path = file_path
                    logger.debug(f"Found '{identifier}' referenced in {file_path}")
                    break
            if referring_path != "unknown":
                break

        # ============================================================
        # STEP 5: Fetch symbol using extract_missing_functions_or_macros
        # ============================================================
        try:
            from types import SimpleNamespace

            instruction = SimpleNamespace(
                expression_name=identifier,
                referring_source_code_path=referring_path,
            )

            logger.debug(f"Fetching symbol: {identifier} (referring_path={referring_path})")
            missing_code, new_symbols = _repo_handler.extract_missing_functions_or_macros(
                [instruction], set()
            )

            if not missing_code or identifier not in new_symbols:
                error_msg = (
                    f"Error: Symbol '{identifier}' not found in repository.\n"
                    f"The symbol name might be incorrect or not exist.\n"
                    f"Try:\n"
                    f"  1. Check the exact spelling (case-sensitive)\n"
                    f"  2. Use search_codebase tool for pattern matching\n"
                    f"Reason for request: {reason}"
                )
                logger.warning(f"Symbol fetch failed: {identifier}")
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
            # STEP 7: Extract line_range from code and format structured result
            # ============================================================
            # The code from repo_handler includes line numbers in format: "line_number| code"
            # Extract first and last line numbers to get the range
            import re

            line_numbers = re.findall(r"^(\d+)\|", code, re.MULTILINE)
            if line_numbers:
                line_range = f"{line_numbers[0]}-{line_numbers[-1]}"
            else:
                line_range = "unknown"

            # Format structured result with explicit metadata fields
            result = f"""=== Fetched Code: {identifier} ===
File Path: {file_path}
Line Range: {line_range}
Type: Symbol
Reason: {reason}

{code}

=== End of {identifier} ==="""

            logger.info(f"Successfully fetched: {identifier} (file={file_path}, lines={line_range})")
            return result

        except Exception as e:
            # Handle any unexpected errors
            error_msg = (
                f"Error: Unexpected failure while fetching '{identifier}'.\n"
                f"Exception: {str(e)}\n"
                f"This may indicate a repo_handler issue or repository access problem.\n"
                f"Reason for request: {reason}"
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
