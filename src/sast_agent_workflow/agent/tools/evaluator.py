"""
evaluator - NAT-registered tool for investigation evaluation.

Evaluates investigation completeness via process and logic audits.
Returns is_final decision, exploration gaps, and recommendations.
"""

import json
import logging
import os
import yaml
from typing import List

from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.component_ref import LLMRef
from nat.data_models.function import FunctionBaseConfig
from pydantic import BaseModel, Field
from openai import LengthFinishReasonError

from ..agent_state import SASTAgentState, EvaluatorReport

from ..agent_graph import _current_agent_state

logger = logging.getLogger(__name__)


class EvaluatorToolInput(BaseModel):
    """Input schema for evaluator."""

    analysis: str = Field(description="Agent's analysis summary")
    proposed_verdict: str = Field(
        description="Agent's proposed verdict (TRUE_POSITIVE/FALSE_POSITIVE/NEEDS_REVIEW)"
    )
    claims: List[dict] = Field(
        description="Structured claims - atomic assertions about the vulnerability"
    )
    evidence: List[dict] = Field(
        description="Structured evidence - code citations supporting claims"
    )
    unknowns: List[dict] = Field(
        description="Structured unknowns - unresolved questions blocking the verdict"
    )

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

        # Extract parameters from kwargs with explicit logging
        # Agent sends: analysis, proposed_verdict, claims, evidence, unknowns

        # analysis (agent's summary - use as issue_trace for prompt)
        if "analysis" in kwargs:
            analysis = kwargs["analysis"]
            logger.debug(f"[{state.issue_id}] analysis extracted from kwargs: '{analysis[:100]}...'")
        else:
            analysis = state.issue.trace if hasattr(state.issue, 'trace') else state.issue.issue_type
            logger.warning(f"[{state.issue_id}] analysis NOT in kwargs, using issue trace from state")

        # proposed_verdict (critical for validation)
        if "proposed_verdict" in kwargs:
            proposed_verdict = kwargs["proposed_verdict"]
            logger.info(f"[{state.issue_id}] proposed_verdict extracted from kwargs: '{proposed_verdict}' (type: {type(proposed_verdict).__name__})")
        else:
            proposed_verdict = "TRUE_POSITIVE"  # Safe default to prevent validation errors
            logger.warning(f"[{state.issue_id}] proposed_verdict NOT in kwargs, using default: '{proposed_verdict}'")

        # Additional check for empty string (which causes validation error)
        if proposed_verdict == "":
            logger.error(f"[{state.issue_id}] proposed_verdict is EMPTY STRING - this will cause validation error!")
            proposed_verdict = "TRUE_POSITIVE"  # Override with safe default
            logger.warning(f"[{state.issue_id}] Overriding empty proposed_verdict with default: '{proposed_verdict}'")

        # claims (structured)
        if "claims" in kwargs:
            claims = kwargs["claims"]
            logger.debug(f"[{state.issue_id}] claims extracted from kwargs: {len(claims)} items")
        else:
            claims = []
            logger.warning(f"[{state.issue_id}] claims NOT in kwargs, using default: []")

        # evidence (structured)
        if "evidence" in kwargs:
            evidence = kwargs["evidence"]
            logger.debug(f"[{state.issue_id}] evidence extracted from kwargs: {len(evidence)} items")
        else:
            evidence = []
            logger.warning(f"[{state.issue_id}] evidence NOT in kwargs, using default: []")

        # unknowns (structured)
        if "unknowns" in kwargs:
            unknowns = kwargs["unknowns"]
            logger.debug(f"[{state.issue_id}] unknowns extracted from kwargs: {len(unknowns)} items")
        else:
            unknowns = []
            logger.warning(f"[{state.issue_id}] unknowns NOT in kwargs, using default: []")

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
            result = EvaluatorReport(
                verification_passed=False,
                verdict=None,
                blocking_gaps=["No code context available for evaluation"],
                required_next_fetches=[],
                justifications=["No code context available for evaluation - need to fetch code first"],
                stop_reason=None
            )
            return json.dumps(result.model_dump(), indent=2)

        # STEP 2: Call LLM with structured output to generate EvaluatorReport
        try:
            logger.info(f"[{state.issue_id}] Calling LLM for direct evaluation")

            # Load evaluation prompt from YAML template
            prompts_dir = os.path.join(os.path.dirname(__file__), "../../../templates/prompts")
            prompt_file = os.path.join(prompts_dir, "evaluator_agent_prompt.yaml")

            with open(prompt_file, "r", encoding="utf-8") as f:
                prompt_data = yaml.safe_load(f)
                prompt_template = prompt_data.get("template", "")

            # Format structured data for prompt
            formatted_claims = "\n".join(
                f"- [{c.get('claim_id', 'N/A')}] {c.get('text', 'N/A')} "
                f"(status: {c.get('status', 'N/A')}, evidence_ids: {c.get('supporting_evidence_ids', [])})"
                for c in claims
            ) if claims else "No claims yet"

            formatted_evidence = "\n".join(
                f"- [{e.get('evidence_id', 'N/A')}] {e.get('path_or_identifier', 'N/A')}:{e.get('start_line', '?')}-{e.get('end_line', '?')}\n"
                f"  Excerpt: {e.get('excerpt', 'N/A')[:200]}...\n"
                f"  Why: {e.get('why_it_matters', 'N/A')}"
                for e in evidence
            ) if evidence else "No evidence yet"

            formatted_unknowns = "\n".join(
                f"- [{u.get('unknown_id', 'N/A')}] {u.get('question', 'N/A')} "
                f"(priority: {u.get('priority', 50)}, blocking: {u.get('blocking', True)})"
                for u in unknowns
            ) if unknowns else "No unknowns"

            # Format prompt with variables
            evaluation_prompt = prompt_template.format(
                issue_trace=analysis,  # Use agent's analysis summary as context
                analysis_verdict=proposed_verdict,  # Use agent's proposed verdict
                claims=formatted_claims,
                evidence=formatted_evidence,
                unknowns=formatted_unknowns,
                context=context
            )

            # Use LLM structured output to generate EvaluatorReport
            # Set max_tokens to prevent hitting token limit (leave room for structured output)
            structured_llm = llm.with_structured_output(EvaluatorReport)
            evaluator_report: EvaluatorReport = structured_llm.invoke(
                evaluation_prompt,
                config={"max_tokens": 2048}  # Limit output to prevent length errors
            )

            logger.info(
                f"[{state.issue_id}] Evaluation complete: verification_passed={evaluator_report.verification_passed}"
            )
            logger.debug(
                f"[{state.issue_id}] Blocking gaps: {evaluator_report.blocking_gaps}"
            )

            result = evaluator_report.model_dump()

            return json.dumps(result, indent=2)

        except LengthFinishReasonError as e:
            # Specific handling for token limit errors
            logger.warning(
                f"[{state.issue_id}] Evaluation hit token limit (output too long): {e}"
            )

            # Return verification_passed=False to allow agent to continue investigating
            # with more focused context rather than terminating incorrectly
            result = EvaluatorReport(
                verification_passed=False,
                verdict=None,
                blocking_gaps=[
                    "Evaluation output exceeded token limit - response too verbose",
                    "Need more focused investigation to reduce context size"
                ],
                required_next_fetches=[],
                justifications=[
                    "Evaluator response exceeded maximum token limit",
                    "This typically means the context is too large or justifications too verbose",
                    "Try to focus on most critical evidence"
                ],
                stop_reason=None
            )
            return json.dumps(result.model_dump(), indent=2)

        except Exception as e:
            logger.error(
                f"[{state.issue_id}] Evaluation failed: {e}",
                exc_info=True,
            )

            # Return verification_passed=True to terminate on error and avoid infinite loops
            result = EvaluatorReport(
                verification_passed=True,
                verdict=proposed_verdict,  # Use verdict from agent
                blocking_gaps=[],
                required_next_fetches=[],
                justifications=[
                    f"Evaluation failed with error: {str(e)}",
                    "Terminating investigation for safety",
                ],
                stop_reason="error_evaluation_failed"
            )
            return json.dumps(result.model_dump(), indent=2)

    # Create async wrapper for NAT 1.3.1
    async def evaluator_fn(
        analysis: str,
        proposed_verdict: str,
        claims: List[dict],
        evidence: List[dict],
        unknowns: List[dict],
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
        return _evaluator(
            analysis=analysis,
            proposed_verdict=proposed_verdict,
            claims=claims,
            evidence=evidence,
            unknowns=unknowns,
            **kwargs
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
