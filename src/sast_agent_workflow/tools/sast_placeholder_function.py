import logging

from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

# Import data models without src prefix
from dto.LLMResponse import AnalysisResponse
from dto.SASTWorkflowModels import SASTWorkflowTracker

logger = logging.getLogger(__name__)


class SASTPlaceholderFunctionConfig(FunctionBaseConfig, name="sast_placeholder_function"):
    """
    Placeholder function for SAST analysis - to be implemented later.
    """
    placeholder_param: str = Field(
        default="placeholder_value", 
        description="Placeholder parameter - to be replaced with actual SAST configuration"
    )
    description: str = Field(
        default="Placeholder function for SAST analysis that processes batches of issues",
        description="Function description"
    )


@register_function(config_type=SASTPlaceholderFunctionConfig)
async def sast_placeholder_function(
    config: SASTPlaceholderFunctionConfig, builder: Builder
):
    """Register the SAST placeholder function."""
    
    logger.info("Initializing SAST placeholder function...")
    
    async def _sast_analysis_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Placeholder function for SAST analysis that processes the entire batch.
        
        Accepts a SASTWorkflowTracker and processes all issues in the batch,
        updating each issue's analysis_response.
        """
        logger.info(f"Processing SAST batch with {len(tracker.issues)} issues")
        logger.info(f"Using placeholder_param: {config.placeholder_param}")
        
        # Process all issues in the batch
        for issue_id, issue_data in tracker.issues.items():
            logger.info(f"Processing issue {issue_id}")
                        
            # Create and update the AnalysisResponse for this issue
            analysis_response = AnalysisResponse(
                investigation_result="FALSE POSITIVE", 
                is_final="TRUE",
                prompt=f"Analysis prompt for {issue_id}",
                justifications=[f"Analysis placeholder result for issue: {issue_id}"],
                short_justifications="Placeholder analysis completed",
                recommendations=[],
                instructions=[],
                evaluation=[]
            )
            
            # Update the issue's analysis response
            issue_data.analysis_response = analysis_response
        
        # Update workflow tracking
        tracker.iteration_count += 1
        
        logger.info(f"Completed batch processing of {len(tracker.issues)} issues")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_sast_analysis_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker
        )
    except GeneratorExit:
        logger.info("SAST function exited early!")
    finally:
        logger.info("Cleaning up SAST placeholder function.")


