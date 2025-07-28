import logging

from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

# Import data models without src prefix
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
        
        TODO: Implement actual analysis evaluation logic
        Takes a SASTWorkflowTracker and returns it after evaluating analysis.
        """
        logger.info("Running Evaluate_Analysis node - evaluating analysis results")
        logger.info(f"Evaluate_Analysis node processing tracker with {len(tracker.issues)} issues")
        
        # TODO: Implement actual analysis evaluation logic here
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