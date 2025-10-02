import logging

from pydantic import Field

from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from Utils.validation_utils import ValidationError
from services.issue_analysis_service import IssueAnalysisService
from services.vector_store_service import VectorStoreService
from dto.LLMResponse import FinalStatus, AnalysisResponse
from Utils.workflow_utils import build_analysis_context

logger = logging.getLogger(__name__)


class JudgeLLMAnalysisConfig(FunctionBaseConfig, name="judge_llm_analysis"):
    """
    Judge LLM analysis function for SAST workflow.
    """
    description: str = Field(
        default="Judge LLM analysis function that performs LLM-based analysis of SAST issues",
        description="Function description"
    )
    llm_name: str = Field(description="LLM name to use for issue analysis")


@register_function(config_type=JudgeLLMAnalysisConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN])
async def judge_llm_analysis(
    config: JudgeLLMAnalysisConfig, builder: Builder
):
    """Register the Judge_LLM_Analysis function."""
    
    logger.info("Initializing Judge_LLM_Analysis function...")
    
    async def _judge_llm_analysis_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Performs LLM-based analysis on issues requiring further investigation.
        """
        if tracker is None:
            raise ValueError("Tracker must not be None")
            
        if not tracker.config:
            error_msg = "No config found in tracker, cannot initialize LLM service"
            logger.error(error_msg)
            raise ValidationError(error_msg)

        logger.info("Running Judge_LLM_Analysis node - performing LLM analysis")
        logger.info(f"Judge_LLM_Analysis node processing tracker with {len(tracker.issues)} issues")
        
        llm = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
        vector_service = VectorStoreService()
        issue_analysis_service = IssueAnalysisService(tracker.config, vector_service)
        
        for issue_id, per_issue in tracker.issues.items():
            if not isinstance(per_issue, PerIssueData):
                logger.warning(f"Skipping issue {issue_id}: unexpected data type {type(per_issue)}")
                continue
                
            # Skip if analysis is already final
            if per_issue.analysis_response and per_issue.analysis_response.is_final == FinalStatus.TRUE.value:
                logger.info(f"Skipping issue {issue_id}: analysis already final")
                continue
                
            # Build full analysis context
            context = build_analysis_context(per_issue)
            
            try:
                # Call the core analysis method
                prompt_string, llm_response = issue_analysis_service.analyze_issue_core_only(
                    issue=per_issue.issue,
                    context=context,
                    main_llm=llm
                )
                
                # Update the per-issue analysis response
                per_issue.analysis_response.investigation_result = llm_response.investigation_result
                per_issue.analysis_response.is_final = FinalStatus.FALSE.value
                per_issue.analysis_response.prompt = prompt_string
                per_issue.analysis_response.justifications = llm_response.justifications
                
                logger.info(f"Completed analysis for issue {issue_id}")
                
            except Exception as e:
                logger.error(f"Failed to analyze issue {issue_id}: {e}")
        
        # Increment global iteration count
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