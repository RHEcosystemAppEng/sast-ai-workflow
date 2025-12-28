"""
analyze_issue tool - NAT-registered tool for security analysis.

Analyzes SAST security issues using fetched source code and returns
TRUE_POSITIVE or FALSE_POSITIVE verdict with justifications.
"""

import logging

from langchain_core.tools import StructuredTool
from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.component_ref import LLMRef
from nat.data_models.function import FunctionBaseConfig
from pydantic import BaseModel, Field

from ....common.config import Config
from ....handlers.repo_handler_factory import repo_handler_factory
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

    # Get LLM for analysis (will be used when implementing actual analysis)
    _ = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)

    # Initialize repo_handler for fetching initial code from error trace
    try:
        global_config = Config()
        _repo_handler = repo_handler_factory(global_config)
        logger.info(f"Initialized repo_handler for: {global_config.REPO_LOCAL_PATH}")
    except Exception as e:
        logger.error(f"Failed to initialize repo_handler: {e}")
        raise

    # TODO: Load analysis prompt when implementing actual analysis
    # prompts_dir = os.path.join(os.path.dirname(__file__), "../../../templates/prompts")
    # prompt_file = os.path.join(prompts_dir, "analysis_prompt.yaml")
    # For now, using placeholder implementation
    logger.info("Using placeholder analysis implementation (LLM integration pending)")

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

        # TODO: Implement actual LLM call with structured output
        # from langchain_core.output_parsers import PydanticOutputParser
        # parser = PydanticOutputParser(pydantic_object=AnalysisResponse)
        # prompt = analysis_prompt.format(issue_trace=issue_trace, code_context=fetched_code)
        # response = llm.invoke(prompt)
        # result = parser.parse(response.content)

        # Placeholder
        import json

        result = {
            "investigation_result": "FALSE_POSITIVE",
            "justifications": [
                "Placeholder justification - actual LLM analysis not yet implemented"
            ],
            "recommendations": ["Implement actual analysis"],
        }
        return json.dumps(result, indent=2)

    analyze_tool = StructuredTool.from_function(
        func=_analyze_issue,
        name="analyze_issue",
        description=config.description,
        args_schema=AnalyzeIssueInput,
    )

    # Create async wrapper for NAT 1.3.1
    async def analyze_issue_fn(input_data: AnalyzeIssueInput) -> str:
        """Async wrapper for analyze_issue tool."""
        return analyze_tool.invoke(
            {"issue_trace": input_data.issue_trace, "fetched_code": input_data.fetched_code}
        )

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
