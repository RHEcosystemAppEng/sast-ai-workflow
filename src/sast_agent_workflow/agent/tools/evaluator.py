"""
evaluator tool - NAT-registered gate for investigation verification (ADR-0002).

IMPORTANT: This is a placeholder implementation. The real evaluator logic/prompting
is implemented in a later stage. The agent core only depends on the JSON contract.
"""

import json
import logging
from typing import Any, Dict

from langchain_core.tools import StructuredTool
from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.function import FunctionBaseConfig
from pydantic import BaseModel, Field

logger = logging.getLogger(__name__)


class EvaluatorInput(BaseModel):
    """Input schema for evaluator tool."""

    evidence_package: Dict[str, Any] = Field(
        description="Evidence package containing narrative + structured claims/evidence/unknowns + code snippets"
    )


class EvaluatorToolConfig(FunctionBaseConfig, name="evaluator"):
    """Configuration for evaluator tool."""

    description: str = Field(
        default=(
            "Gate tool for investigation verification. Returns JSON with verification_passed, "
            "blocking_gaps, required_next_fetches, and (if verified) a verdict."
        ),
        description="Tool description",
    )


@register_function(config_type=EvaluatorToolConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN])
async def register_evaluator_tool(config: EvaluatorToolConfig, builder: Builder):
    """Register the evaluator tool with NAT (placeholder)."""
    logger.info("Registering evaluator tool (placeholder)...")

    def _evaluator(evidence_package: Dict[str, Any]) -> str:
        # Placeholder: never verify, always request more evidence.
        result = {
            "verification_passed": False,
            "verdict": None,
            "blocking_gaps": [
                "Evaluator tool is not implemented yet; cannot verify without real audit."
            ],
            "required_next_fetches": [],
            "justifications": ["Placeholder evaluator (implementation deferred)."],
            "stop_reason": None,
        }
        return json.dumps(result, indent=2)

    eval_tool = StructuredTool.from_function(
        func=_evaluator,
        name="evaluator",
        description=config.description,
        args_schema=EvaluatorInput,
    )

    async def evaluator_fn(input_data: EvaluatorInput) -> str:
        return eval_tool.invoke({"evidence_package": input_data.evidence_package})

    try:
        yield FunctionInfo.from_fn(
            evaluator_fn,
            description=config.description,
            input_schema=EvaluatorInput,
        )
    except GeneratorExit:
        logger.info("evaluator tool exited!")
    finally:
        logger.debug("Cleaning up evaluator tool")
