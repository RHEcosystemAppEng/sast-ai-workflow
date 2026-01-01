"""
analyze_issue tool - NAT-registered tool for security analysis.

Analyzes SAST security issues using fetched source code and returns
TRUE_POSITIVE or FALSE_POSITIVE verdict with justifications.
"""

import json
import logging

from langchain_core.tools import StructuredTool
from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.component_ref import LLMRef
from nat.data_models.function import FunctionBaseConfig
from pydantic import BaseModel, Field

from common.config import Config
from dto.ResponseStructures import JudgeLLMResponse
from handlers.repo_handler_factory import repo_handler_factory
from services.issue_analysis_service import IssueAnalysisService
from services.vector_store_service import VectorStoreService
from ..agent_state import SASTAgentState
from .fetch_code import fetch_code_from_error_trace

logger = logging.getLogger(__name__)


class AnalyzeIssueInput(BaseModel):
    """Input schema for analyze_issue tool."""

    issue_trace: str = Field(description="The SAST issue trace to analyze")
    fetched_code: str = Field(description="All fetched source code (formatted with === markers)")


class AnalyzeIssueToolConfig(FunctionBaseConfig, name="analyze_issue"):
    """Configuration for analyze_issue tool."""

    description: str = Field(
        default=(
            "Analyzes a SAST security issue using fetched source code. "
            "Returns TRUE_POSITIVE or FALSE_POSITIVE verdict with justifications."
        ),
        description="Tool description",
    )
    llm_name: LLMRef = Field(default="main_llm", description="LLM to use for security analysis")


@register_function(
    config_type=AnalyzeIssueToolConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN]
)
async def register_analyze_issue_tool(config: AnalyzeIssueToolConfig, builder: Builder):
    """Register the analyze_issue tool with NAT."""
    logger.info("Registering analyze_issue tool...")

    # Get LLM for analysis
    llm = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
    logger.info(f"Initialized LLM: {config.llm_name}")

    # Initialize repo_handler for fetching initial code from error trace
    try:
        global_config = Config()
        _repo_handler = repo_handler_factory(global_config)
        logger.info(f"Initialized repo_handler for: {global_config.REPO_LOCAL_PATH}")
    except Exception as e:
        logger.error(f"Failed to initialize repo_handler: {e}")
        raise

    # Initialize VectorStoreService and IssueAnalysisService
    try:
        vector_service = VectorStoreService()
        analysis_service = IssueAnalysisService(config=global_config, vector_service=vector_service)
        logger.info("Initialized IssueAnalysisService with vector_service")
    except Exception as e:
        logger.error(f"Failed to initialize IssueAnalysisService: {e}")
        raise

    def _extract_context_from_state(state: SASTAgentState) -> str:
        """
        Extract all fetched code from state and format as context string.

        Args:
            state: Agent state containing fetched_files

        Returns:
            Formatted context string with all code blocks
        """
        # TODO: Validate context format - should we preserve === markers from fetch_code?
        # Current implementation preserves the structured format for LLM readability

        if not state.context.fetched_files:
            logger.warning(f"[{state.issue_id}] No code fetched yet - context is empty")
            return ""

        context_blocks = []
        for identifier, code_list in state.context.fetched_files.items():
            # Each identifier can have multiple code blocks (usually just one)
            for code in code_list:
                context_blocks.append(code)

        context = "\n\n".join(context_blocks)

        # Log context size for observability
        num_files = len(state.context.fetched_files)
        context_size = len(context)
        logger.info(
            f"[{state.issue_id}] Extracted context: {num_files} files, "
            f"{context_size} characters"
        )

        return context

    def _analyze_issue(state: SASTAgentState, issue_trace: str, fetched_code: str) -> str:
        """
        Analyze SAST issue for TRUE_POSITIVE/FALSE_POSITIVE.

        NOTE: This function is called once at investigation start and automatically
        fetches initial code from the error trace before analysis.

        Args:
            state: Agent state (for storing fetched code from trace)
            issue_trace: The SAST finding trace
            fetched_code: Source code context

        Returns:
            Analysis result as JSON string with verdict and justifications
        """
        logger.info(f"[{state.issue_id}] analyze_issue called")

        # STEP 1: Fetch initial code from error trace (file+line extraction)
        # This is called ONCE at investigation start to populate initial context
        fetch_code_from_error_trace(state, issue_trace, _repo_handler)
        logger.info(
            f"[{state.issue_id}] Fetched initial code from trace. "
            f"Files in context: {list(state.context.fetched_files.keys())}"
        )

        # STEP 2: Extract all gathered code from state
        context = _extract_context_from_state(state)

        if not context:
            logger.warning(f"[{state.issue_id}] Empty context - returning default result")
            # TODO: Validate error default - should we return TRUE_POSITIVE (safer) or FALSE_POSITIVE?
            # Currently defaulting to TRUE_POSITIVE to avoid missing real vulnerabilities
            return json.dumps({
                "investigation_result": "TRUE_POSITIVE",
                "justifications": ["No code context available for analysis"],
                "prompt_used": "N/A - no context"
            })

        # STEP 3: Call IssueAnalysisService for LLM-based analysis
        try:
            logger.info(f"[{state.issue_id}] Calling IssueAnalysisService.analyze_issue_core_only")

            prompt_used, analysis_response = analysis_service.analyze_issue_core_only(
                issue=state.issue,
                context=context,
                main_llm=llm
            )

            logger.info(
                f"[{state.issue_id}] Analysis complete: {analysis_response.investigation_result}"
            )
            logger.debug(f"[{state.issue_id}] Prompt used:\n{prompt_used}")

            # STEP 4: Format structured result
            result = {
                "investigation_result": analysis_response.investigation_result,
                "justifications": analysis_response.justifications,
                "prompt_used": prompt_used
            }

            return json.dumps(result, indent=2)

        except Exception as e:
            logger.error(
                f"[{state.issue_id}] Analysis failed: {e}",
                exc_info=True
            )

            # TODO: Validate error default - should we return TRUE_POSITIVE (safer) or FALSE_POSITIVE?
            # Currently defaulting to TRUE_POSITIVE to avoid missing real vulnerabilities when analysis fails
            return json.dumps({
                "investigation_result": "TRUE_POSITIVE",
                "justifications": [
                    f"Analysis failed with error: {str(e)}",
                    "Defaulting to TRUE_POSITIVE for safety"
                ],
                "prompt_used": "N/A - analysis failed"
            })

    analyze_tool = StructuredTool.from_function(
        func=_analyze_issue,
        name="analyze_issue",
        description=config.description,
        args_schema=AnalyzeIssueInput,
    )

    # Create async wrapper for NAT 1.3.1
    async def analyze_issue_fn(input_data: dict) -> str:
        """
        Async wrapper for analyze_issue tool.

        Accepts a dict instead of AnalyzeIssueInput to allow passing through
        the injected 'state' parameter from agent_graph.
        """
        return analyze_tool.invoke(input_data)

    try:
        # NAT 1.3.1+ uses from_fn with async functions
        yield FunctionInfo.from_fn(
            analyze_issue_fn,
            description=config.description,
            input_schema=AnalyzeIssueInput,
        )
    except GeneratorExit:
        logger.info("analyze_issue tool exited!")
    finally:
        logger.debug("Cleaning up analyze_issue tool")
