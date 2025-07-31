import logging

from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import SASTWorkflowTracker

logger = logging.getLogger(__name__)


class SummarizeJustificationsConfig(FunctionBaseConfig, name="summarize_justifications"):
    """
    Summarize justifications function for SAST workflow.
    """
    description: str = Field(
        default="Summarize justifications function that summarizes analysis justifications",
        description="Function description"
    )


@register_function(config_type=SummarizeJustificationsConfig)
async def summarize_justifications(
    config: SummarizeJustificationsConfig, builder: Builder
):
    """Register the Summarize_Justifications function."""
    
    logger.info("Initializing Summarize_Justifications function...")
    
    async def _summarize_justifications_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Summarize justifications function for SAST workflow.
        """
        logger.info("Running Summarize_Justifications node - summarizing justifications")
        logger.info(f"Summarize_Justifications node processing tracker with {len(tracker.issues)} issues")
        
        # TODO: Implement actual justification summarization logic here
        
        logger.info("Summarize_Justifications node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_summarize_justifications_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker
        )
    except GeneratorExit:
        logger.info("Summarize_Justifications function exited early!")
    finally:
        logger.info("Cleaning up Summarize_Justifications function.")
