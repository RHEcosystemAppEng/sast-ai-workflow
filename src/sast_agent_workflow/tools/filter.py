import logging

from Utils.metrics_utils import categorize_issues_by_status
from Utils.validation_utils import ValidationError, validate_issue_dict
from dto.Issue import Issue
from langchain_core.language_models import BaseChatModel
from langchain_openai import OpenAIEmbeddings
from pydantic import Field

from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import PerIssueData, SASTWorkflowTracker
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus
from common.constants import KNOWN_FALSE_POSITIVES, KNOWN_ISSUES_SHORT_JUSTIFICATION, NO_MATCHING_TRACE_FOUND
from LLMService import LLMService
from FilterKnownIssues import (
    create_known_issue_retriever,
    is_known_false_positive,
    convert_similar_issues_to_examples_context_string
)
import sys
import os

# Import evaluation converters for NAT integration
try:
    # Add project root to path for evaluation imports
    from . import PROJECT_ROOT
    if PROJECT_ROOT not in sys.path:
        sys.path.insert(0, PROJECT_ROOT)

    from evaluation.converter_tools.filter_converters import (
        convert_str_to_sast_tracker,
        convert_sast_tracker_to_str
    )
    _filter_converters = [convert_str_to_sast_tracker, convert_sast_tracker_to_str]
    _filter_converters_available = True
except ImportError as e:
    _filter_converters = None
    _filter_converters_available = False
    _filter_converters_error = str(e)

logger = logging.getLogger(__name__)


def _create_false_positive_response(equal_error_trace: list) -> AnalysisResponse:
    """Create analysis response for known false positives."""
    context = (
        "\n".join(equal_error_trace)
        if equal_error_trace
        else NO_MATCHING_TRACE_FOUND
    )
    
    return AnalysisResponse(
        investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
        is_final=FinalStatus.TRUE.value,
        recommendations=["No fix required."],
        justifications=[
            f"The error is similar to one found in the provided context: {context}"
        ],
        short_justifications=KNOWN_ISSUES_SHORT_JUSTIFICATION,
    )


def _process_single_issue(issue_id: str, issue_data, known_issue_retriever, llm_service: LLMService) -> None:
    """Process a single issue for known false positive detection."""
    if not issue_id or not isinstance(issue_id, str):
        logger.error("Invalid issue_id: must be a non-empty string")
        return
    if not isinstance(issue_data, PerIssueData):
        logger.error(f"{issue_id}: Invalid issue data: must be a PerIssueData object, got {type(issue_data)}")
        return
    if not isinstance(issue_data.issue, Issue):
        logger.error(f"{issue_id}: Invalid issue: must be an Issue object, got {type(issue_data.issue)}")
        return
    if not known_issue_retriever or not llm_service:
        logger.error(f"{issue_id}: Invalid known issue retriever or LLM service")
        return
    
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
        else:
            logger.debug(f"Issue {issue_id} not identified as known false positive")
                            
    except Exception as e:
        logger.error(f"Unexpected error processing issue {issue_id} as part of filter: {e}", exc_info=True)


class FilterConfig(FunctionBaseConfig, name="filter"):
    """
    Filter function for SAST workflow.
    """
    description: str = Field(
        default="Filter function that queries Vector DB to find similar issues and identify known false positives",
        description="Function description"
    )
    llm_name: str = Field(
        default="main_llm",
        description="LLM name to use for filtering"
    )
    embedder_llm_name: str = Field(
        default="filter_embedding",
        description="Embedder LLM name to use for filtering"
    )


@register_function(config_type=FilterConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN])
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
        
        # Create LLMService instance with existing LLM and embedder
        llm: BaseChatModel = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
        embedder: OpenAIEmbeddings = await builder.get_embedder(config.embedder_llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
        llm_service = LLMService(tracker.config, llm, embedder)
        
        # Create known issue retriever
        try:
            known_issue_retriever = create_known_issue_retriever(llm_service, tracker.config)
        except Exception as e:
            logger.error(f"Failed to create known issue retriever: {e}")
            return tracker
        
        # Process each issue
        for issue_id, issue_data in tracker.issues.items():
            _process_single_issue(issue_id, issue_data, known_issue_retriever, llm_service)
        
        known_fps = categorize_issues_by_status(tracker.issues)[KNOWN_FALSE_POSITIVES]
        
        logger.info(f"Filter node completed. Known false positives: {known_fps}/{len(tracker.issues)}")
        return tracker

    # Use module-level converters
    if _filter_converters_available:
        logger.info("NAT evaluation converters loaded successfully")
    else:
        logger.info(f"NAT evaluation converters not available: {_filter_converters_error}")

    try:
        yield FunctionInfo.create(
            single_fn=_filter_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker,
            converters=_filter_converters
        )
    except GeneratorExit:
        logger.info("Filter function exited early!")
    finally:
        logger.info("Cleaning up Filter function.")
