"""
comprehensive_evaluation tool - NAT-registered tool for investigation evaluation.

Evaluates investigation completeness via process and logic audits.
Returns is_final decision, exploration gaps, and recommendations.
"""

import logging
from typing import List

from langchain_core.tools import StructuredTool
from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.component_ref import LLMRef
from nat.data_models.function import FunctionBaseConfig
from pydantic import BaseModel, Field

logger = logging.getLogger(__name__)


class ComprehensiveEvaluationInput(BaseModel):
    """Input schema for comprehensive_evaluation tool."""

    issue_trace: str = Field(description="The SAST issue being investigated")
    analysis_verdict: str = Field(
        description="Current analysis verdict (TRUE_POSITIVE/FALSE_POSITIVE)"
    )
    analysis_justifications: List[str] = Field(description="Justifications from analysis")
    fetched_files: List[str] = Field(description="List of files fetched so far")
    iteration_count: int = Field(description="Current iteration number")


class ComprehensiveEvaluationToolConfig(FunctionBaseConfig, name="comprehensive_evaluation"):
    """Configuration for comprehensive_evaluation tool."""

    description: str = Field(
        default=(
            "Evaluates investigation completeness via process and logic audits. "
            "Returns is_final (TRUE/FALSE), exploration gaps, and recommendations."
        ),
        description="Tool description",
    )
    llm_name: LLMRef = Field(default="main_llm", description="LLM to use for evaluation")


@register_function(
    config_type=ComprehensiveEvaluationToolConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN]
)
async def register_comprehensive_evaluation_tool(
    config: ComprehensiveEvaluationToolConfig, builder: Builder
):
    """Register the comprehensive_evaluation tool with NAT."""
    logger.info("Registering comprehensive_evaluation tool...")

    # Get LLM for evaluation (will be used when implementing actual evaluation)
    _ = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)

    # TODO: Load evaluation prompt when implementing actual evaluation
    # prompts_dir = os.path.join(os.path.dirname(__file__), "../../../templates/prompts")
    # prompt_file = os.path.join(prompts_dir, "comprehensive_evaluation_prompt.yaml")
    # For now, using placeholder implementation
    logger.info("Using placeholder evaluation implementation (LLM integration pending)")

    def _comprehensive_evaluation(
        issue_trace: str,
        analysis_verdict: str,
        analysis_justifications: List[str],
        fetched_files: List[str],
        iteration_count: int,
    ) -> str:
        """
        Perform comprehensive evaluation of investigation.

        Args:
            issue_trace: The SAST issue
            analysis_verdict: Current verdict
            analysis_justifications: Analysis reasoning
            fetched_files: Files examined
            iteration_count: Iteration number

        Returns:
            Evaluation result as JSON with is_final, gaps, recommendations
        """
        logger.info(f"comprehensive_evaluation called (iteration {iteration_count})")

        # TODO: Implement actual evaluation
        # Placeholder
        import json

        result = {
            "is_final": "TRUE" if iteration_count >= 3 else "FALSE",
            "verdict_confidence": "medium",
            "exploration_gaps": [],
            "has_exploration_gaps": False,
            "logic_gaps": [],
            "required_code": [],
            "recommendations": [
                "Continue investigation" if iteration_count < 3 else "Investigation complete"
            ],
            "justifications": ["Placeholder evaluation"],
        }
        return json.dumps(result, indent=2)

    eval_tool = StructuredTool.from_function(
        func=_comprehensive_evaluation,
        name="comprehensive_evaluation",
        description=config.description,
        args_schema=ComprehensiveEvaluationInput,
    )

    # Create async wrapper for NAT 1.3.1
    async def comprehensive_evaluation_fn(input_data: ComprehensiveEvaluationInput) -> str:
        """Async wrapper for comprehensive_evaluation tool."""
        return eval_tool.invoke(
            {
                "issue_trace": input_data.issue_trace,
                "analysis_verdict": input_data.analysis_verdict,
                "analysis_justifications": input_data.analysis_justifications,
                "fetched_files": input_data.fetched_files,
                "iteration_count": input_data.iteration_count,
            }
        )

    try:
        # NAT 1.3.1+ uses from_fn with async functions
        yield FunctionInfo.from_fn(
            comprehensive_evaluation_fn,
            description=config.description,
            input_schema=ComprehensiveEvaluationInput,
        )
    except GeneratorExit:
        logger.info("comprehensive_evaluation tool exited!")
    finally:
        logger.debug("Cleaning up comprehensive_evaluation tool")
