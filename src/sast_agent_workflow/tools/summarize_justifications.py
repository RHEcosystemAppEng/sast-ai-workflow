import logging
from typing import Optional

from pydantic import Field

from aiq.builder.builder import Builder, LLMFrameworkEnum
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.ResponseStructures import JudgeLLMResponse
from services.issue_analysis_service import IssueAnalysisService
from services.vector_store_service import VectorStoreService
from Utils.deepeval_utils import LangChainLLMWrapper, SummaryEvaluator

logger = logging.getLogger(__name__)


class SummarizeJustificationsConfig(FunctionBaseConfig, name="summarize_justifications"):
    """
    Summarize justifications function for SAST workflow.
    """
    description: str = Field(
        default="Summarize justifications function that summarizes analysis justifications",
        description="Function description"
    )
    llm_name: str = Field(description="LLM name to use for summarization")
    enable_evaluation: bool = Field(default=False, description="Enable DeepEval evaluation of summaries")
    evaluation_output_path: Optional[str] = Field(default=None, description="Path to save evaluation results JSON")
    faithfulness_threshold: float = Field(default=0.8, description="Threshold for faithfulness evaluation")
    relevancy_threshold: float = Field(default=0.75, description="Threshold for relevancy evaluation")


@register_function(config_type=SummarizeJustificationsConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN])
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
        
        if not tracker.config:
            logger.error("No config found in tracker - cannot proceed with summarization")
            return tracker
            
        llm = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
        vector_service = VectorStoreService()
        issue_analysis_service = IssueAnalysisService(tracker.config, vector_service)
        
        # Initialize evaluator if evaluation is enabled
        evaluator = None
        if config.enable_evaluation:
            llm_wrapper = LangChainLLMWrapper(llm)
            evaluator = SummaryEvaluator(
                llm_wrapper=llm_wrapper,
                faithfulness_threshold=config.faithfulness_threshold,
                relevancy_threshold=config.relevancy_threshold
            )
            logger.info("DeepEval evaluation enabled")
        
        for issue_id, per_issue_data in tracker.issues.items():
            if (per_issue_data.analysis_response and 
                not per_issue_data.analysis_response.short_justifications):
                
                logger.debug(f"Summarizing justifications for issue {issue_id}")
                try:
                    response_to_summarize = JudgeLLMResponse(
                        investigation_result=per_issue_data.analysis_response.investigation_result,
                        justifications=per_issue_data.analysis_response.justifications
                    )
                    summary_response = issue_analysis_service.summarize_justification(
                        actual_prompt=per_issue_data.analysis_response.prompt,
                        response=response_to_summarize,
                        issue_id=issue_id,
                        main_llm=llm
                    )
                    per_issue_data.analysis_response.short_justifications = summary_response.short_justifications
                    logger.debug(f"Successfully summarized justifications for issue {issue_id}")
                    
                    # Evaluate summary if evaluation is enabled
                    if evaluator:
                        try:
                            await evaluator.evaluate_summary(
                                issue_id=issue_id,
                                original_justification=" ".join(response_to_summarize.justifications),
                                generated_summary=summary_response.short_justifications
                            )
                            logger.debug(f"Evaluated summary for issue {issue_id}")
                        except Exception as eval_e:
                            logger.error(f"Failed to evaluate summary for issue {issue_id}: {eval_e}")
                            
                except Exception as e:
                    logger.error(f"Failed to summarize justifications for issue {issue_id}: {e}")
        
        # Save evaluation report if evaluation was enabled
        if evaluator and config.evaluation_output_path:
            try:
                evaluator.save_report(config.evaluation_output_path)
                logger.info(f"Evaluation report saved to {config.evaluation_output_path}")
            except Exception as e:
                logger.error(f"Failed to save evaluation report: {e}")
        
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
