import logging

from Utils.metrics_utils import count_known_false_positives
from Utils.validation_utils import ValidationError, validate_issue_dict
from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from common.constants import KNOWN_ISSUES_SHORT_JUSTIFICATION
from LLMService import LLMService
from stage.filter_known_issues import (
    create_known_issue_retriever,
    is_known_false_positive,
    convert_similar_issues_to_examples_context_string
)

logger = logging.getLogger(__name__)


def _create_false_positive_response(equal_error_trace: list) -> AnalysisResponse:
    """Create analysis response for known false positives."""
    context = (
        "\n".join(equal_error_trace)
        if equal_error_trace
        else "No matching trace found"
    )
    
    return AnalysisResponse(
        investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
        is_final="TRUE",
        recommendations=["No fix required."],
        justifications=[
            f"The error is similar to one found in the provided context: {context}"
        ],
        short_justifications=KNOWN_ISSUES_SHORT_JUSTIFICATION,
    )


def _process_single_issue(issue_id: str, issue_data, known_issue_retriever, llm_service: LLMService) -> None:
    """Process a single issue for known false positive detection."""
    try:
        # Get similar known issues from vector store
        similar_known_issues_list = known_issue_retriever.get_relevant_known_issues(
            issue_data.issue.trace, 
            issue_data.issue.issue_type
        )
        
        # Convert similar issues to context string for other tools
        issue_data.similar_known_issues = convert_similar_issues_to_examples_context_string(
            similar_known_issues_list
        )
        
        # Check if issue is a known false positive
        is_finding_known_false_positive, equal_error_trace = is_known_false_positive(
            issue_data.issue, similar_known_issues_list, llm_service
        )
        
        if is_finding_known_false_positive:
            logger.info(f"Issue {issue_id} identified as known false positive")
            issue_data.analysis_response = _create_false_positive_response(equal_error_trace)
                            
    except Exception as e:
        logger.error(f"Error processing issue {issue_id} in filter: {e}")


class FilterConfig(FunctionBaseConfig, name="filter"):
    """
    Filter function for SAST workflow.
    """
    description: str = Field(
        default="Filter function that queries Vector DB to find similar issues and identify known false positives",
        description="Function description"
    )


@register_function(config_type=FilterConfig)
async def filter(
    config: FilterConfig, builder: Builder
):
    """Register the Filter function."""
    
    logger.info("Initializing Filter function...")
    
    async def _filter_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Filter function for SAST workflow.
        Queries Vector DB to find similar issues and identifies known false positives.
        """
        logger.info("Running Filter node - filtering issues")        
        
        # Input validation
        validate_issue_dict(tracker.issues)

        if not tracker.config:
            error_msg = "No config found in tracker, cannot initialize LLM service"
            logger.error(error_msg)
            raise ValidationError(error_msg)
            
        # Check if filtering is enabled in config
        if not tracker.config.USE_KNOWN_FALSE_POSITIVE_FILE:
            logger.info("Known false positive file filtering disabled in config, skipping filter")
            return tracker
                
        # Create LLM service instance from config
        llm_service = LLMService(tracker.config)
        
        # Create known issue retriever
        try:
            known_issue_retriever = create_known_issue_retriever(llm_service, tracker.config)
        except Exception as e:
            logger.error(f"Failed to create known issue retriever: {e}")
            return tracker
        
        # Process each issue
        for issue_id, issue_data in tracker.issues.items():
            _process_single_issue(issue_id, issue_data, known_issue_retriever, llm_service)
        
        known_fps = count_known_false_positives(tracker.issues)
        
        logger.info(f"Filter node completed. Known false positives: {known_fps}/{len(tracker.issues)}")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_filter_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker
        )
    except GeneratorExit:
        logger.info("Filter function exited early!")
    finally:
        logger.info("Cleaning up Filter function.")
