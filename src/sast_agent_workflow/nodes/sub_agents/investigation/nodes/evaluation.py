"""Evaluation node - Critique analysis quality."""

import logging

from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.prompts import ChatPromptTemplate

from common.config import Config
from Utils.llm_utils import robust_structured_output

from ..constants import LANGFUSE_EVALUATION_TRACE_NAME, NEEDS_MORE_RESEARCH
from ..prompts import build_evaluation_prompt
from .schemas import EvaluationResult, InvestigationState

logger = logging.getLogger(__name__)


def create_evaluation_node(llm: BaseChatModel, config: Config):
    """
    Create evaluation node that critiques analysis.

    This node decides if analysis is sufficient or if more research is needed.
    Includes defense verification for FALSE_POSITIVE verdicts.
    """

    def evaluate(state: InvestigationState) -> InvestigationState:
        """Execute evaluation/critique phase."""
        issue_id = state["issue_id"]
        logger.info(f"[{issue_id}] Evaluation phase")

        # Build evaluation prompt using the prompts module
        eval_prompt = build_evaluation_prompt(state)

        # Use robust_structured_output with retry logic
        max_retries = getattr(config, "structured_output_max_retries", 3)

        try:
            # Create prompt chain
            prompt_chain = ChatPromptTemplate.from_template("{input}")

            # Use robust_structured_output utility
            result: EvaluationResult = robust_structured_output(
                llm=llm,
                schema=EvaluationResult,
                input=eval_prompt,
                prompt_chain=prompt_chain,
                max_retries=max_retries,
                config={"run_name": LANGFUSE_EVALUATION_TRACE_NAME},
            )
        except Exception as e:
            # Handle token limit errors by retrying with reduced max_tokens
            if "context length" in str(e).lower():
                logger.warning(f"[{issue_id}] Token limit error, retrying with reduced output...")

                # Extract actual input tokens from error and calculate safe output
                import re
                match = re.search(r"(\d+)\s+input tokens", str(e))
                if match:
                    input_tokens = int(match.group(1))
                    safe_output = max(2000, 65536 - input_tokens - 1000)  # 1000 token buffer
                    logger.info(f"[{issue_id}] Input: {input_tokens} tokens, adjusted output: {safe_output}")
                else:
                    safe_output = 2000  # Fallback if we can't parse the error

                try:
                    reduced_llm = llm.__class__(**{**llm.dict(), "max_tokens": safe_output})
                    result = robust_structured_output(
                        reduced_llm, EvaluationResult, eval_prompt, prompt_chain, 1,
                        {"run_name": LANGFUSE_EVALUATION_TRACE_NAME}
                    )
                except Exception as retry_error:
                    logger.error(f"[{issue_id}] Retry failed: {retry_error}")
                    return {**state, "is_complete": True, "stop_reason": "evaluation_error",
                            "evaluation_feedback": f"Evaluation failed: {retry_error}", "proposed_verdict": "NEEDS_REVIEW"}
            else:
                logger.error(f"[{issue_id}] Evaluation error: {e}", exc_info=True)
                return {**state, "is_complete": True, "stop_reason": "evaluation_error",
                        "evaluation_feedback": f"Evaluation failed: {e}", "proposed_verdict": "NEEDS_REVIEW"}

        # Handle disagreement without missing evidence:
        # When evaluator returns NEEDS_MORE_RESEARCH with empty required_information,
        # it means they disagree with the analysis but have all the code needed.
        # Route back to analysis with feedback instead of requesting more research.
        needs_reanalysis = False
        reanalysis_count = state.get("reanalysis_count", 0)
        if result.result == NEEDS_MORE_RESEARCH and len(result.required_information) == 0:
            logger.info(
                f"[{issue_id}] Evaluator disagrees with analysis but has no missing evidence. "
                "Routing back to analysis with feedback for reconsideration."
            )
            needs_reanalysis = True
            reanalysis_count += 1

        # Track rejection streak
        rejection_streak = state.get("evaluation_rejection_streak", 0)
        if result.result == NEEDS_MORE_RESEARCH:
            rejection_streak += 1
        else:
            rejection_streak = 0

        # Track progress (no new code gathered)
        current_code_length = len(state.get("gathered_code", ""))
        previous_length = state.get("previous_code_length", 0)
        no_progress_streak = 0
        if current_code_length == previous_length and state["iteration"] > 1:
            no_progress_streak = state.get("no_progress_streak", 0) + 1

        # Use required_information directly
        required_info = list(result.required_information)

        # Determine completion
        # (don't check max_iterations or rejection_streak here - router handles it)
        is_complete = result.result == "APPROVED"

        if is_complete:
            logger.info(f"[{issue_id}] Investigation APPROVED by evaluation")
        else:
            logger.info(
                f"[{issue_id}] Needs more research "
                f"(iteration {state['iteration']}/{state['max_iterations']}, "
                f"rejection={rejection_streak}, no_progress={no_progress_streak})"
            )

        updates = {
            **state,
            "evaluation_result": result.result,
            "evaluation_feedback": result.feedback,
            "required_information": required_info,
            "is_complete": is_complete,
            "needs_reanalysis": needs_reanalysis,
            "reanalysis_count": reanalysis_count,
            "evaluation_rejection_streak": rejection_streak,
            "previous_code_length": current_code_length,
            "no_progress_streak": no_progress_streak,
        }
        if is_complete:
            updates["stop_reason"] = "approved"
        return updates

    return evaluate
