"""
comprehensive_evaluation tool - NAT-registered tool for investigation evaluation.

Evaluates investigation completeness via process and logic audits.
Returns is_final decision, exploration gaps, and recommendations.
"""

import json
import logging
from typing import List

from langchain_core.tools import StructuredTool
from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.component_ref import LLMRef
from nat.data_models.function import FunctionBaseConfig
from pydantic import BaseModel, Field

from ....common.config import Config
from ....dto.ResponseStructures import JudgeLLMResponse
from ....services.issue_analysis_service import IssueAnalysisService
from ....services.vector_store_service import VectorStoreService
from ..agent_state import SASTAgentState

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

    llm = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
    logger.info(f"Initialized LLM: {config.llm_name}")

    # Initialize VectorStoreService and IssueAnalysisService
    # TODO: Validate VectorStoreService initialization - currently using placeholder None
    # Need to verify if vector store is required for evaluation or can remain None
    try:
        global_config = Config()
        vector_service = VectorStoreService(config=global_config, vector_store=None)
        analysis_service = IssueAnalysisService(config=global_config, vector_service=vector_service)
        logger.info("Initialized IssueAnalysisService with placeholder vector_service")
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

    def _build_analysis_response(
        analysis_verdict: str, analysis_justifications: List[str]
    ) -> JudgeLLMResponse:
        """
        Build JudgeLLMResponse from tool inputs.

        Args:
            analysis_verdict: Current verdict (TRUE_POSITIVE/FALSE_POSITIVE with underscores)
            analysis_justifications: Justifications from analysis

        Returns:
            JudgeLLMResponse object for IssueAnalysisService.recommend()
        """

        verdict_with_space = analysis_verdict.replace("_", " ")

        return JudgeLLMResponse(
            investigation_result=verdict_with_space, justifications=analysis_justifications
        )

    def _comprehensive_evaluation(
        state: SASTAgentState,
        issue_trace: str,
        analysis_verdict: str,
        analysis_justifications: List[str],
        fetched_files: List[str],
        iteration_count: int,
    ) -> str:
        """
        Evaluate investigation completeness and decide if investigation should terminate.

        NOTE: This function receives state via injection (agent_graph.py line 192).
        The LLM does NOT construct state - it only provides the other parameters.

        Args:
            state: Agent state (injected by tool wrapper, contains fetched_files and issue)
            issue_trace: The SAST issue trace
            analysis_verdict: Current verdict (TRUE_POSITIVE/FALSE_POSITIVE)
            analysis_justifications: Justifications from analysis
            fetched_files: List of files fetched (for LLM visibility)
            iteration_count: Current iteration number

        Returns:
            Evaluation result as JSON with is_final, gaps, recommendations
        """
        logger.info(f"[{state.issue_id}] comprehensive_evaluation called (iteration {iteration_count})")

        # STEP 1: Extract context from state
        context = _extract_context_from_state(state)

        if not context:
            logger.warning(f"[{state.issue_id}] Empty context - cannot evaluate completeness")
            # TODO: Validate error default - should we return is_final=TRUE to avoid infinite loops?
            # Currently defaulting to FALSE to give agent a chance to fetch code
            result = {
                "is_final": "FALSE",
                "verdict_confidence": "low",
                "exploration_gaps": ["No code context available for evaluation"],
                "has_exploration_gaps": True,
                "logic_gaps": [],
                "required_code": [],
                "recommendations": ["Fetch code before evaluating"],
                "justifications": ["No code context available for evaluation"],
            }
            return json.dumps(result, indent=2)

        # STEP 2: Build JudgeLLMResponse from tool inputs
        analysis_response = _build_analysis_response(analysis_verdict, analysis_justifications)

        # STEP 3: Call IssueAnalysisService.recommend() for evaluation
        try:
            logger.info(f"[{state.issue_id}] Calling IssueAnalysisService.recommend()")

            recommendations_response = analysis_service.recommend(
                issue=state.issue,
                context=context,
                analysis_response=analysis_response,
                main_llm=llm,
            )

            logger.info(
                f"[{state.issue_id}] Evaluation complete: is_final={recommendations_response.is_final}"
            )
            logger.debug(
                f"[{state.issue_id}] Recommendations: {recommendations_response.recommendations}"
            )

            # STEP 4: Map RecommendationsResponse to ComprehensiveEvaluationResponse format
            # TODO: Validate verdict_confidence derivation - currently using simple heuristic
            # If is_final=TRUE, confidence is high; otherwise medium
            verdict_confidence = "high" if recommendations_response.is_final == "TRUE" else "medium"

            # TODO: Validate exploration_gaps derivation - currently extracting from recommendations
            # The service returns recommendations as strings, but we need structured exploration_gaps
            # For now, use recommendations as exploration_gaps when is_final=FALSE
            exploration_gaps = []
            logic_gaps = []
            required_code = []

            if recommendations_response.is_final == "FALSE":
                # Investigation not complete - extract gaps from recommendations
                # TODO: Implement proper gap categorization logic
                # For now, treat all recommendations as exploration_gaps
                exploration_gaps = recommendations_response.recommendations

                # Map instructions to required_code format
                # instructions is List[InstructionResponse] with fields:
                # - expression_name: str
                # - referring_source_code_path: str
                for instruction in recommendations_response.instructions:
                    required_code.append(
                        {
                            "symbol": instruction.expression_name,
                            "file": instruction.referring_source_code_path,
                            "reason": "Required for investigation",
                        }
                    )

            has_exploration_gaps = len(exploration_gaps) > 0

            result = {
                "is_final": recommendations_response.is_final,
                "verdict_confidence": verdict_confidence,
                "exploration_gaps": exploration_gaps,
                "has_exploration_gaps": has_exploration_gaps,
                "logic_gaps": logic_gaps,
                "required_code": required_code,
                "recommendations": recommendations_response.recommendations,
                "justifications": recommendations_response.justifications,
            }

            return json.dumps(result, indent=2)

        except Exception as e:
            logger.error(
                f"[{state.issue_id}] Evaluation failed: {e}",
                exc_info=True,
            )

            # TODO: Validate error default - should we return is_final=TRUE to avoid infinite loops?
            # Currently defaulting to TRUE for safety when evaluation fails
            result = {
                "is_final": "TRUE",
                "verdict_confidence": "low",
                "exploration_gaps": [],
                "has_exploration_gaps": False,
                "logic_gaps": [],
                "required_code": [],
                "recommendations": ["Evaluation failed - terminating investigation"],
                "justifications": [
                    f"Evaluation failed with error: {str(e)}",
                    "Defaulting to is_final=TRUE for safety",
                ],
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
