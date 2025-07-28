import logging

from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import SASTWorkflowTracker

logger = logging.getLogger(__name__)


class JudgeLLMAnalysisConfig(FunctionBaseConfig, name="judge_llm_analysis"):
    """
    Judge LLM analysis function for SAST workflow.
    """
    description: str = Field(
        default="Judge LLM analysis function that performs LLM-based analysis of SAST issues",
        description="Function description"
    )


@register_function(config_type=JudgeLLMAnalysisConfig)
async def judge_llm_analysis(
    config: JudgeLLMAnalysisConfig, builder: Builder
):
    """Register the Judge_LLM_Analysis function."""
    
    logger.info("Initializing Judge_LLM_Analysis function...")
    
    async def _judge_llm_analysis_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Judge LLM analysis function for SAST workflow.
        """
        logger.info("Running Judge_LLM_Analysis node - performing LLM analysis")
        logger.info(f"Judge_LLM_Analysis node processing tracker with {len(tracker.issues)} issues")
        
        # TODO: Implement actual LLM analysis logic here
        
        # Update iteration count to show processing
        tracker.iteration_count += 1
        
        logger.info("Judge_LLM_Analysis node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_judge_llm_analysis_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker
        )
    except GeneratorExit:
        logger.info("Judge_LLM_Analysis function exited early!")
    finally:
        logger.info("Cleaning up Judge_LLM_Analysis function.")
