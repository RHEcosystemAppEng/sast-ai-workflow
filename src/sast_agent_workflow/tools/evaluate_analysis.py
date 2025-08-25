import logging

from pydantic import Field

from aiq.builder.builder import Builder, LLMFrameworkEnum
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from Utils.validation_utils import ValidationError, validate_issue_dict
from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.LLMResponse import FinalStatus
from dto.ResponseStructures import JudgeLLMResponse
from services.issue_analysis_service import IssueAnalysisService
from services.vector_store_service import VectorStoreService

logger = logging.getLogger(__name__)


def _process_single_issue_evaluation(issue_id: str, issue_data: PerIssueData, 
                                    issue_analysis_service: IssueAnalysisService, llm) -> None:
    """
    Process evaluation for a single issue using IssueAnalysisService.recommend.
    
    Args:
        issue_id: The issue identifier
        issue_data: The PerIssueData object
        issue_analysis_service: The IssueAnalysisService instance
        llm: The LLM to use for recommendations
    """
    if not issue_id or not isinstance(issue_id, str):
        logger.error("Invalid issue_id: must be a non-empty string")
        return
    
    if not isinstance(issue_data, PerIssueData):
        logger.error(f"{issue_id}: Invalid issue data: must be a PerIssueData object, got {type(issue_data)}")
        return
    
    if not issue_data.analysis_response:
        logger.debug(f"{issue_id}: No analysis response found, skipping evaluation")
        return
    
    if issue_data.analysis_response.is_final == FinalStatus.TRUE.value:
        logger.debug(f"{issue_id}: Analysis already marked as final, skipping evaluation")
        return
    
    try:
        # Create JudgeLLMResponse from existing analysis_response
        judge_response = JudgeLLMResponse(
            investigation_result=issue_data.analysis_response.investigation_result,
            justifications=issue_data.analysis_response.justifications
        )
        
        # Call recommend to get recommendations and final status
        recommendations_response = issue_analysis_service.recommend(
            issue=issue_data.issue,
            context=issue_data.similar_known_issues,
            analysis_response=judge_response,
            main_llm=llm
        )
        
        # Update the analysis response with recommendation results
        issue_data.analysis_response.is_final = recommendations_response.is_final
        issue_data.analysis_response.recommendations = recommendations_response.recommendations
        issue_data.analysis_response.instructions = recommendations_response.instructions
        issue_data.analysis_response.evaluation = recommendations_response.justifications
        
        logger.info(f"{issue_id}: Evaluation completed. is_final: {recommendations_response.is_final}")
            
    except Exception as e:
        logger.error(f"Unexpected error evaluating issue {issue_id}: {e}", exc_info=True)
        # Default values already set in pre_process_node


class EvaluateAnalysisConfig(FunctionBaseConfig, name="evaluate_analysis"):
    """
    Evaluate analysis function for SAST workflow.
    """
    description: str = Field(
        default="Evaluate analysis function that evaluates the results of SAST analysis and decides if analysis is final",
        description="Function description"
    )
    llm_name: str = Field(
        default="main_llm",
        description="LLM name to use for evaluation"
    )


@register_function(config_type=EvaluateAnalysisConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN])
async def evaluate_analysis(
    config: EvaluateAnalysisConfig, builder: Builder
):
    """
    Register the Evaluate_Analysis function.
    Evaluates issues with non-final analysis responses, determines if analysis is final,
    and provides recommendations on what data is missing for unresolved cases.
    """
    
    logger.info("Initializing Evaluate_Analysis function...")
    
    async def _evaluate_analysis_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        logger.info("Running Evaluate_Analysis node - evaluating analysis results")
        logger.info(f"Evaluate_Analysis node processing tracker with {len(tracker.issues)} issues")
        
        # Input validation
        validate_issue_dict(tracker.issues)
        
        if not tracker.config:
            error_msg = "No config found in tracker, cannot initialize issue analysis service"
            logger.error(error_msg)
            raise ValidationError(error_msg)
        
        if tracker.iteration_count >= tracker.config.MAX_ANALYSIS_ITERATIONS:
            logger.info(f"Max analysis iterations reached: {tracker.iteration_count} >= {tracker.config.MAX_ANALYSIS_ITERATIONS}, skipping evaluate_analysis node")
            return tracker
        
        # Get LLM and create services
        llm = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
        vector_service = VectorStoreService()
        issue_analysis_service = IssueAnalysisService(tracker.config, vector_service)
        
        # Count issues that need evaluation (is_final == False)
        non_final_issues = 0
        evaluated_issues = 0
        finalized_issues = 0
        
        # Process each issue
        for issue_id, issue_data in tracker.issues.items():
            # Only evaluate issues with analysis_response where is_final is False
            if (issue_data.analysis_response and 
                issue_data.analysis_response.is_final == FinalStatus.FALSE.value):
                non_final_issues += 1
                
                # Process the issue evaluation using IssueAnalysisService
                _process_single_issue_evaluation(issue_id, issue_data, issue_analysis_service, llm)
                evaluated_issues += 1
                
                # Check if it was finalized
                if issue_data.analysis_response.is_final == FinalStatus.TRUE.value:
                    finalized_issues += 1
        
        logger.info(f"Evaluate_Analysis node completed. "
                   f"Non-final issues: {non_final_issues}, "
                   f"Evaluated: {evaluated_issues}, "
                   f"Newly finalized: {finalized_issues}")
        
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
