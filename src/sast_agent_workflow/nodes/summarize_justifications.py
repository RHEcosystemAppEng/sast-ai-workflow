import logging

from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.function import FunctionBaseConfig
from pydantic import Field

from common.constants import DEFAULT_FIELD_VALUE
from dto.ResponseStructures import JudgeLLMResponse
from dto.SASTWorkflowModels import SASTWorkflowTracker
from services.issue_analysis_service import IssueAnalysisService
from services.vector_store_service import VectorStoreService

logger = logging.getLogger(__name__)

# Import evaluation converters for NAT integration
try:
    from evaluation.converter_tools.summarize_converters import (
        convert_sast_tracker_to_str,
        convert_str_to_sast_tracker,
    )

    _summarize_converters = [convert_str_to_sast_tracker, convert_sast_tracker_to_str]
    _summarize_converters_available = True
except ImportError as e:
    _summarize_converters = None
    _summarize_converters_available = False
    _summarize_converters_error = str(e)


class SummarizeJustificationsConfig(FunctionBaseConfig, name="summarize_justifications"):
    """
    Summarize justifications function for SAST workflow.
    """

    description: str = Field(
        default="Summarize justifications function that summarizes analysis justifications",
        description="Function description",
    )
    llm_name: str = Field(description="LLM name to use for summarization")


@register_function(
    config_type=SummarizeJustificationsConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN]
)
async def summarize_justifications(config: SummarizeJustificationsConfig, builder: Builder):
    """Register the Summarize_Justifications function."""

    logger.info("Initializing Summarize_Justifications function...")

    async def _summarize_justifications_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Summarize justifications function for SAST workflow.
        """
        logger.info("Running Summarize_Justifications node - summarizing justifications")
        logger.info(
            f"Summarize_Justifications node processing tracker with {len(tracker.issues)} issues"
        )

        if not tracker.config:
            logger.error("No config found in tracker - cannot proceed with summarization")
            return tracker

        llm = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
        vector_service = VectorStoreService()
        issue_analysis_service = IssueAnalysisService(tracker.config, vector_service)

        for issue_id, per_issue_data in tracker.issues.items():
            if (
                per_issue_data.analysis_response
                and per_issue_data.analysis_response.short_justifications == DEFAULT_FIELD_VALUE
            ):

                logger.debug(f"Summarizing justifications for issue {issue_id}")
                try:
                    response_to_summarize = JudgeLLMResponse(
                        investigation_result=per_issue_data.analysis_response.investigation_result,
                        justifications=per_issue_data.analysis_response.justifications,
                    )
                    summary_response = issue_analysis_service.summarize_justification(
                        actual_prompt=per_issue_data.analysis_response.prompt,
                        response=response_to_summarize,
                        issue_id=issue_id,
                        main_llm=llm,
                    )
                    per_issue_data.analysis_response.short_justifications = (
                        summary_response.short_justifications
                    )
                    logger.debug(f"Successfully summarized justifications for issue {issue_id}")
                except Exception as e:
                    logger.error(f"Failed to summarize justifications for issue {issue_id}: {e}")

        logger.info("Summarize_Justifications node completed")
        return tracker

    # Use module-level converters
    if _summarize_converters_available:
        logger.info("NAT evaluation converters loaded successfully")
    else:
        logger.info(f"NAT evaluation converters not available: {_summarize_converters_error}")

    try:
        yield FunctionInfo.create(
            single_fn=_summarize_justifications_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker,
            converters=_summarize_converters,
        )
    except GeneratorExit:
        logger.info("Summarize_Justifications function exited early!")
    finally:
        logger.info("Cleaning up Summarize_Justifications function.")
