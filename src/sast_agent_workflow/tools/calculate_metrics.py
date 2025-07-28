import logging

from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

# Import data models without src prefix
from dto.SASTWorkflowModels import SASTWorkflowTracker

logger = logging.getLogger(__name__)


class CalculateMetricsConfig(FunctionBaseConfig, name="calculate_metrics"):
    """
    Calculate metrics function for SAST workflow.
    """
    description: str = Field(
        default="Calculate metrics function that calculates performance metrics for SAST analysis",
        description="Function description"
    )


@register_function(config_type=CalculateMetricsConfig)
async def calculate_metrics(
    config: CalculateMetricsConfig, builder: Builder
):
    """Register the Calculate_Metrics function."""
    
    logger.info("Initializing Calculate_Metrics function...")
    
    async def _calculate_metrics_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Calculate metrics function for SAST workflow.
        
        TODO: Implement actual metrics calculation logic
        Takes a SASTWorkflowTracker and returns it after calculating metrics.
        """
        logger.info("Running Calculate_Metrics node - calculating metrics")
        logger.info(f"Calculate_Metrics node processing tracker with {len(tracker.issues)} issues")
        
        # TODO: Implement actual metrics calculation logic here
        
        logger.info("Calculate_Metrics node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_calculate_metrics_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker
        )
    except GeneratorExit:
        logger.info("Calculate_Metrics function exited early!")
    finally:
        logger.info("Cleaning up Calculate_Metrics function.") 