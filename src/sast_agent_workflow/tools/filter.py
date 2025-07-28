import logging

from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import SASTWorkflowTracker

logger = logging.getLogger(__name__)


class FilterConfig(FunctionBaseConfig, name="filter"):
    """
    Filter function for SAST workflow.
    """
    description: str = Field(
        default="Filter function that filters SAST issues",
        description="Function description"
    )


@register_function(config_type=FilterConfig)
async def filter(
    config: FilterConfig, builder: Builder
):
    """Register the Filter function."""
    
    logger.info("Initializing Filter function...")
    
    async def _filter_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Filter function for SAST workflow.
        """
        logger.info("Running Filter node - filtering issues")
        logger.info(f"Filter node processing tracker with {len(tracker.issues)} issues")
        
        # TODO: Implement actual filtering logic here
        
        logger.info("Filter node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_filter_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker
        )
    except GeneratorExit:
        logger.info("Filter function exited early!")
    finally:
        logger.info("Cleaning up Filter function.") 