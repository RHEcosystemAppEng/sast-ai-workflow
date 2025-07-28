import logging

from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

# Import data models without src prefix
from dto.SASTWorkflowModels import SASTWorkflowTracker

logger = logging.getLogger(__name__)


class DataFetcherConfig(FunctionBaseConfig, name="data_fetcher"):
    """
    Data fetcher function for SAST workflow.
    """
    description: str = Field(
        default="Data fetcher function that fetches required data for analysis",
        description="Function description"
    )


@register_function(config_type=DataFetcherConfig)
async def data_fetcher(
    config: DataFetcherConfig, builder: Builder
):
    """Register the Data_Fetcher function."""
    
    logger.info("Initializing Data_Fetcher function...")
    
    async def _data_fetcher_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Data fetcher function for SAST workflow.
        """
        logger.info("Running Data_Fetcher node - fetching data")
        logger.info(f"Data_Fetcher node processing tracker with {len(tracker.issues)} issues")
        
        # TODO: Implement actual data fetching logic here
        
        logger.info("Data_Fetcher node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_data_fetcher_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker
        )
    except GeneratorExit:
        logger.info("Data_Fetcher function exited early!")
    finally:
        logger.info("Cleaning up Data_Fetcher function.") 