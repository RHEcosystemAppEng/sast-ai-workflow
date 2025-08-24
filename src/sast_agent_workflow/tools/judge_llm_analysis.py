import logging

from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from LLMService import LLMService
from common.constants import FALSE
from src.Utils.validation_utils import ValidationError

logger = logging.getLogger(__name__)


def _build_analysis_context(per_issue: PerIssueData) -> str:
    """
    Build full analysis context by combining source_code, similar_known_issues, and other relevant data.
    """
    # Build source code context
    source_code_context = ""
    if per_issue.source_code:
        source_code_parts = []
        for file_path, code_snippets in per_issue.source_code.items():
            for snippet in code_snippets:
                source_code_parts.append(f"\ncode of {file_path} file:\n{snippet}")
        source_code_context = "".join(source_code_parts)
    
    # Combine source code and examples in structured format
    context = (
        f"*** Source Code Context ***\n{source_code_context}\n\n"
        f"*** Examples ***\n{per_issue.similar_known_issues}"
    )
    
    return context


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
        
        # Initialize LLM service if config is available
        llm_service = None
        if tracker.config is not None:
            try:
                llm_service = LLMService(tracker.config)
            except Exception as e:
                logger.error(f"Failed to initialize LLM service: {e}")
                raise RuntimeError(f"LLM service initialization failed: {e}") from e
        
        for issue_id, per_issue in tracker.issues.items():
            if not isinstance(per_issue, PerIssueData):
                logger.warning(f"Skipping issue {issue_id}: unexpected data type {type(per_issue)}")
                continue
                
            # Skip if analysis is already final
            if per_issue.analysis_response and per_issue.analysis_response.is_final != FALSE:
                logger.info(f"Skipping issue {issue_id}: analysis already final")
                continue
                
            # Build full analysis context
            context = _build_analysis_context(per_issue)
            
            if llm_service is None:
                logger.warning(f"Skipping analysis for issue {issue_id}: LLM service not available")
                continue
                
            try:
                # Call primary analysis LLM
                analysis_response, _ = llm_service.investigate_issue(context, per_issue.issue)
                
                # Update the per-issue analysis response
                per_issue.analysis_response = analysis_response
                
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