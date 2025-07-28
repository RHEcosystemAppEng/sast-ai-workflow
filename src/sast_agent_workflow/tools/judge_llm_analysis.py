import logging

from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

# Import data models without src prefix
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
        
        TODO: Implement actual LLM analysis logic
        Takes a SASTWorkflowTracker and returns it after LLM analysis.
        """
        logger.info("Running Judge_LLM_Analysis node - performing LLM analysis")
        logger.info(f"Judge_LLM_Analysis node processing tracker with {len(tracker.issues)} issues")
        
        try:
            from LLMService import LLMService
            from langchain_core.prompts import ChatPromptTemplate
            
            if not tracker.config or not tracker.config.LLM_URL or not tracker.config.LLM_API_KEY:
                logger.error("Missing required LLM configuration (LLM_URL or LLM_API_KEY)")
                return tracker
            
            llm_service = LLMService(config=tracker.config)
            
            # Create a simple test prompt
            test_prompt = ChatPromptTemplate.from_messages([
                ("system", "You are a helpful assistant."),
                ("user", "Where is the capital of France?")
            ])
            
            # Make a test call to the LLM
            try:
                response = llm_service.main_llm.invoke(test_prompt.format())
                logger.info(f"LLM Test Response: {response.content}")
                tracker.issues["def1"].analysis_response = response.content
            except Exception as e:
                logger.error(f"Error during LLM test call: {str(e)}")
                raise
                
        except Exception as e:
            logger.error(f"Error in Judge_LLM_Analysis: {str(e)}")
            # Don't fail the pipeline, just log the error
            
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