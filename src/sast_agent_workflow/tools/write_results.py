import logging

from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

# Import data models without src prefix
from dto.SASTWorkflowModels import SASTWorkflowTracker

logger = logging.getLogger(__name__)


class WriteResultsConfig(FunctionBaseConfig, name="write_results"):
    """
    Write results function for SAST workflow.
    """
    description: str = Field(
        default="Write results function that writes the final SAST analysis results",
        description="Function description"
    )


@register_function(config_type=WriteResultsConfig)
async def write_results(
    config: WriteResultsConfig, builder: Builder
):
    """Register the Write_Results function."""
    
    logger.info("Initializing Write_Results function...")
    
    async def _write_results_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Write results function for SAST workflow.
        
        TODO: Implement actual results writing logic
        Takes a SASTWorkflowTracker and returns it after writing results.
        """
        logger.info("Running Write_Results node - writing results")
        logger.info(f"Write_Results node processing tracker with {len(tracker.issues)} issues")
        
        # TODO: Implement actual results writing logic here
        
        logger.info("Write_Results node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_write_results_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker
        )
    except GeneratorExit:
        logger.info("Write_Results function exited early!")
    finally:
        logger.info("Cleaning up Write_Results function.") 