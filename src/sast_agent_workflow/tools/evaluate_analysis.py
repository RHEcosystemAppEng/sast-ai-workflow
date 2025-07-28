import logging

from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import SASTWorkflowTracker

logger = logging.getLogger(__name__)


class EvaluateAnalysisConfig(FunctionBaseConfig, name="evaluate_analysis"):
    """
    Evaluate analysis function for SAST workflow.
    """
    description: str = Field(
        default="Evaluate analysis function that evaluates the results of SAST analysis",
        description="Function description"
    )


@register_function(config_type=EvaluateAnalysisConfig)
async def evaluate_analysis(
    config: EvaluateAnalysisConfig, builder: Builder
):
    """Register the Evaluate_Analysis function."""
    
    logger.info("Initializing Evaluate_Analysis function...")
    
    async def _evaluate_analysis_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Evaluate analysis function for SAST workflow.
        """
        logger.info("Running Evaluate_Analysis node - evaluating analysis results")
        logger.info(f"Evaluate_Analysis node processing tracker with {len(tracker.issues)} issues")
        
        # TODO: Implement actual analysis evaluation logic here
        
        logger.info("Evaluate_Analysis node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_evaluate_analysis_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker
        )
    except GeneratorExit:
        logger.info("Evaluate_Analysis function exited early!")
    finally:
        logger.info("Cleaning up Evaluate_Analysis function.")
