import logging

from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.function import FunctionBaseConfig
from pydantic import Field

from common.config import Config
from dto.SASTWorkflowModels import SASTWorkflowTracker
from sast_agent_workflow.nodes.sub_agents.investigation import (
    create_investigate_node,
    create_investigation_tools,
)

logger = logging.getLogger(__name__)


class InvestigateConfig(FunctionBaseConfig, name="investigate"):
    """Investigation function for SAST workflow."""

    description: str = Field(
        default="Investigation function that autonomously investigates SAST issues",
        description="Function description",
    )
    llm_name: str = Field(description="LLM name to use for investigation")


@register_function(config_type=InvestigateConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN])
async def investigate(config: InvestigateConfig, builder: Builder):
    """Register the Investigate function."""

    logger.info("Initializing Investigate function...")

    llm = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
    tools = create_investigation_tools(Config())
    investigate_node_fn = create_investigate_node(Config(), llm, tools)

    async def _investigate_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        if tracker is None:
            raise ValueError("Tracker must not be None")

        logger.info("Running Investigate node")
        logger.info(f"Investigate node processing tracker with {len(tracker.issues)} issues")

        return await investigate_node_fn(tracker)

    try:
        yield FunctionInfo.create(
            single_fn=_investigate_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker,
        )
    except GeneratorExit:
        logger.info("Investigate function exited early!")
    finally:
        logger.info("Cleaning up Investigate function.")
