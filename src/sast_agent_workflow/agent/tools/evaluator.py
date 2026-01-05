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
                "verification_passed": True,
                "verdict": "TRUE_POSITIVE",
                "blocking_gaps": ["State injection failed"],
                "required_next_fetches": [],
                "justifications": ["State injection failed - cannot evaluate"],
                "stop_reason": "error_no_state"
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
            # Return verification_passed=False to give agent a chance to fetch code
            result = {
                "verification_passed": False,
                "verdict": None,
                "blocking_gaps": ["No code context available for evaluation"],
                "required_next_fetches": [],
                "justifications": ["No code context available for evaluation - need to fetch code first"],
                "stop_reason": None
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

            # STEP 4: Map RecommendationsResponse to EvaluatorReport format
            # Convert is_final string to verification_passed boolean
            verification_passed = (recommendations_response.is_final == "TRUE")

            # Extract verdict from analysis_verdict parameter (passed by agent)
            verdict = analysis_verdict if verification_passed else None

            # Convert recommendations to blocking_gaps (simple string list)
            blocking_gaps = []
            required_next_fetches = []

            if not verification_passed:
                # Investigation not complete - extract blocking gaps from recommendations
                blocking_gaps = recommendations_response.recommendations

                # Map instructions to required_next_fetches format
                # instructions is List[InstructionResponse] with fields:
                # - expression_name: str
                # - referring_source_code_path: str
                for instruction in recommendations_response.instructions:
                    required_next_fetches.append(
                        {
                            "tool": "fetch_code",
                            "args": {
                                "expression_name": instruction.expression_name,
                                "referring_source_code_path": instruction.referring_source_code_path,
                                "reason": "Required for investigation"
                            },
                            "reason": "Required for investigation"
                        }
                    )

            result = {
                "verification_passed": verification_passed,
                "verdict": verdict,
                "blocking_gaps": blocking_gaps,
                "required_next_fetches": required_next_fetches,
                "justifications": recommendations_response.justifications,
                "stop_reason": None
            }

            return json.dumps(result, indent=2)

        except Exception as e:
            logger.error(
                f"[{state.issue_id}] Evaluation failed: {e}",
                exc_info=True,
            )

            # Return verification_passed=True to terminate on error and avoid infinite loops
            result = {
                "verification_passed": True,
                "verdict": analysis_verdict,  # Use verdict from agent
                "blocking_gaps": [],
                "required_next_fetches": [],
                "justifications": [
                    f"Evaluation failed with error: {str(e)}",
                    "Terminating investigation for safety",
                ],
                "stop_reason": "error_evaluation_failed"
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
