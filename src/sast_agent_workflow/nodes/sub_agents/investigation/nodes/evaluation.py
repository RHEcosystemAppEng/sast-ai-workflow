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
            error_msg = str(e)

            # Only handle token limit errors
            if "context length" not in error_msg.lower():
                logger.error(f"[{issue_id}] Evaluation error: {e}", exc_info=True)
                return {
                    **state,
                    "is_complete": True,
                    "stop_reason": "evaluation_error",
                    "evaluation_feedback": f"Evaluation failed: {e}",
                    "proposed_verdict": "NEEDS_REVIEW",
                }

            logger.warning(f"[{issue_id}] Token limit error, retrying with reduced output...")

            # Extract input tokens from error
            import re
            match = re.search(r"\b([0-9]{1,12}) input tokens\b", error_msg[:1000])
            if not match:
                safe_output = 2000
            else:
                input_tokens = int(match.group(1))
                # Account for structured output overhead (~1000 tokens) + safety buffer
                safe_output = max(2000, 65536 - input_tokens - 2500)
                logger.info(f"[{issue_id}] Input: {input_tokens} tokens, calculated output: {safe_output}")

            # Check if we can modify max_tokens
            if not hasattr(llm, "max_tokens"):
                logger.error(f"[{issue_id}] Cannot reduce max_tokens for LLM type {type(llm).__name__}")
                return {
                    **state,
                    "is_complete": True,
                    "stop_reason": "evaluation_error",
                    "evaluation_feedback": "Cannot adjust max_tokens",
                    "proposed_verdict": "NEEDS_REVIEW",
                }

            # Cap safe_output to never exceed original max_tokens
            original_max_tokens = getattr(llm, "max_tokens", None)
            if original_max_tokens is not None:
                safe_output = min(safe_output, original_max_tokens)

            try:
                llm.max_tokens = safe_output
                logger.info(f"[{issue_id}] set llm.max_tokens to {safe_output}")

                result = robust_structured_output(
                    llm=llm,
                    schema=EvaluationResult,
                    input=eval_prompt,
                    prompt_chain=prompt_chain,
                    max_retries=1,
                    config={"run_name": LANGFUSE_EVALUATION_TRACE_NAME},
                )

                logger.info(f"[{issue_id}] Retry with reduced tokens succeeded!")

            except Exception as retry_error:
                logger.error(f"[{issue_id}] Retry failed: {retry_error}")
                return {
                    **state,
                    "is_complete": True,
                    "stop_reason": "evaluation_error",
                    "evaluation_feedback": f"Evaluation failed: {retry_error}",
                    "proposed_verdict": "NEEDS_REVIEW",
                }
            finally:
                llm.max_tokens = original_max_tokens
                logger.debug(f"[{issue_id}] Restored llm.max_tokens to {original_max_tokens}")

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
