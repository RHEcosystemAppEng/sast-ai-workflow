"""
evaluator - NAT-registered tool for investigation evaluation.

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

from common.config import Config
from dto.ResponseStructures import JudgeLLMResponse
from services.issue_analysis_service import IssueAnalysisService
from services.vector_store_service import VectorStoreService
from ..agent_state import SASTAgentState

# Import context variable from agent_graph
from ..agent_graph import _current_agent_state

logger = logging.getLogger(__name__)


class EvaluatorToolInput(BaseModel):
    """Input schema for evaluator."""

    issue_trace: str = Field(description="The SAST issue being investigated")
    analysis_verdict: str = Field(
        description="Current analysis verdict (TRUE_POSITIVE/FALSE_POSITIVE)"
    )
    analysis_justifications: List[str] = Field(description="Justifications from analysis")
    fetched_files: List[str] = Field(description="List of files fetched so far")
    iteration_count: int = Field(description="Current iteration number")

    # State injection field (NOT provided by LLM, injected by agent_graph wrapper)
    # This field is excluded from tool description but allows NAT to pass it through
    class Config:
        extra = "allow"  # Allow extra fields like 'state' to pass through


class EvaluatorToolConfig(FunctionBaseConfig, name="evaluator"):
    """Configuration for evaluator."""

    description: str = Field(
        default=(
            "Evaluates investigation completeness via process and logic audits. "
            "Returns is_final (TRUE/FALSE), exploration gaps, and recommendations."
        ),
        description="Tool description",
    )
    llm_name: LLMRef = Field(default="main_llm", description="LLM to use for evaluation")


@register_function(
    config_type=EvaluatorToolConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN]
)
async def register_evaluator(
    config: EvaluatorToolConfig, builder: Builder
):
    """Register the evaluator with NAT."""
    logger.info("Registering evaluator...")

    llm = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
    logger.info(f"Initialized LLM: {config.llm_name}")

    # Initialize VectorStoreService and IssueAnalysisService
    try:
        global_config = Config()
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

    def _evaluator(**kwargs) -> str:
        """
        Evaluate investigation completeness and decide if investigation should terminate.

        NOTE: This function accesses state via context variable set by agent_graph.py.
        The state is NOT passed through NAT's validation - it's accessed via contextvars.

        Other parameters (issue_trace, analysis_verdict, etc.) are provided by the LLM
        based on the input schema.

        Args:
            **kwargs: Contains LLM-provided params (issue_trace, analysis_verdict, etc.)

        Returns:
            Evaluation result as JSON with is_final, gaps, recommendations
        """
        # Get state from context variable (set by agent_graph wrapper)
        state: SASTAgentState = _current_agent_state.get()
        if not state:
            logger.error("evaluator called without state in context variable")
            return json.dumps({
                "is_final": "TRUE",
                "verdict_confidence": "low",
                "exploration_gaps": [],
                "has_exploration_gaps": False,
                "logic_gaps": [],
                "required_code": [],
                "recommendations": ["Error: No state provided"],
                "justifications": ["State injection failed - cannot evaluate"]
            }, indent=2)

        # Extract other parameters from kwargs with explicit logging
        # This helps diagnose whether parameters are being passed correctly from NAT

        # issue_trace
        if "issue_trace" in kwargs:
            issue_trace = kwargs["issue_trace"]
            logger.debug(f"[{state.issue_id}] issue_trace extracted from kwargs: '{issue_trace[:100]}...'")
        else:
            issue_trace = ""
            logger.warning(f"[{state.issue_id}] issue_trace NOT in kwargs, using default: ''")

        # analysis_verdict (critical for validation)
        if "analysis_verdict" in kwargs:
            analysis_verdict = kwargs["analysis_verdict"]
            logger.info(f"[{state.issue_id}] analysis_verdict extracted from kwargs: '{analysis_verdict}' (type: {type(analysis_verdict).__name__})")
        else:
            analysis_verdict = "TRUE_POSITIVE"  # Safe default to prevent validation errors
            logger.warning(f"[{state.issue_id}] analysis_verdict NOT in kwargs, using default: '{analysis_verdict}'")

        # Additional check for empty string (which causes validation error)
        if analysis_verdict == "":
            logger.error(f"[{state.issue_id}] analysis_verdict is EMPTY STRING - this will cause validation error!")
            analysis_verdict = "TRUE_POSITIVE"  # Override with safe default
            logger.warning(f"[{state.issue_id}] Overriding empty analysis_verdict with default: '{analysis_verdict}'")

        # analysis_justifications
        if "analysis_justifications" in kwargs:
            analysis_justifications = kwargs["analysis_justifications"]
            logger.debug(f"[{state.issue_id}] analysis_justifications extracted from kwargs: {len(analysis_justifications)} items")
        else:
            analysis_justifications = []
            logger.warning(f"[{state.issue_id}] analysis_justifications NOT in kwargs, using default: []")

        # fetched_files
        if "fetched_files" in kwargs:
            fetched_files = kwargs["fetched_files"]
            logger.debug(f"[{state.issue_id}] fetched_files extracted from kwargs: {len(fetched_files)} items")
        else:
            fetched_files = []
            logger.warning(f"[{state.issue_id}] fetched_files NOT in kwargs, using default: []")

        # iteration_count
        if "iteration_count" in kwargs:
            iteration_count = kwargs["iteration_count"]
            logger.debug(f"[{state.issue_id}] iteration_count extracted from kwargs: {iteration_count}")
        else:
            iteration_count = state.iteration_count
            logger.debug(f"[{state.issue_id}] iteration_count NOT in kwargs, using state value: {iteration_count}")

        logger.info(f"[{state.issue_id}] evaluator called (iteration {iteration_count})")

        # STEP 1: Extract context from state
        context = _extract_context_from_state(state)

        if not context:
            logger.warning(f"[{state.issue_id}] Empty context - cannot evaluate completeness")
            # TODO: Validate error default - should we return is_final=TRUE to avoid infinite loops?
            # Currently defaulting to FALSE to give agent a chance to fetch code
            result = {
                "is_final": "FALSE",
                "verdict_confidence": "low",
                "exploration_gaps": [
                    {
                        "area": "other",
                        "reason": "No code context available for evaluation",
                        "suggested_files": [],
                    }
                ],
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

            # Convert recommendations to structured ExplorationGap objects
            # The service returns recommendations as strings, but we need structured objects
            exploration_gaps = []
            logic_gaps = []
            required_code = []

            if recommendations_response.is_final == "FALSE":
                # Investigation not complete - convert recommendations to ExplorationGap objects
                # Map each recommendation string to an ExplorationGap with default "other" category
                for recommendation in recommendations_response.recommendations:
                    exploration_gaps.append(
                        {
                            "area": "other",  # Default category for now
                            "reason": recommendation,
                            "suggested_files": [],  # Service doesn't provide file suggestions
                        }
                    )

                # Map instructions to required_code format with CORRECT field names
                # instructions is List[InstructionResponse] with fields:
                # - expression_name: str
                # - referring_source_code_path: str
                for instruction in recommendations_response.instructions:
                    required_code.append(
                        {
                            "expression_name": instruction.expression_name,  # ✅ Correct field name
                            "file_path": instruction.referring_source_code_path,  # ✅ Correct field name
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

    # Create async wrapper for NAT 1.3.1
    async def evaluator_fn(
        issue_trace: str,
        analysis_verdict: str,
        analysis_justifications: List[str],
        fetched_files: List[str],
        iteration_count: int,
        **kwargs  # Captures 'state' injected by agent_graph
    ) -> str:
        """
        Async wrapper for evaluator.

        NAT calls this with **input_data.model_dump(), so we accept individual fields.
        The state parameter is injected via **kwargs by agent_graph.py.

        NOTE: Directly calls _evaluator instead of using StructuredTool intermediary.
        Using StructuredTool.from_function with **kwargs signature creates empty schema
        that strips all parameters during invoke().
        """
        # Directly call _evaluator with all parameters
        return _evaluator(
            issue_trace=issue_trace,
            analysis_verdict=analysis_verdict,
            analysis_justifications=analysis_justifications,
            fetched_files=fetched_files,
            iteration_count=iteration_count,
            **kwargs  # Includes 'state' injected by agent_graph
        )

    try:
        # NAT 1.3.1+ uses from_fn with async functions
        yield FunctionInfo.from_fn(
            evaluator_fn,
            description=config.description,
            input_schema=EvaluatorToolInput,
        )
    except GeneratorExit:
        logger.info("evaluator exited!")
    finally:
        logger.debug("Cleaning up evaluator")
