from io import TextIOWrapper
import json
import logging

from ReportReader import read_sast_report
from common.config import Config
from common.constants import FALSE
from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from dto.SASTWorkflowModels import PerIssueData, SASTWorkflowTracker
from handlers.repo_handler_factory import repo_handler_factory


logger = logging.getLogger(__name__)


class PreProcessConfig(FunctionBaseConfig, name="pre_process"):
    """
    Pre-processing function for SAST workflow.
    """
    description: str = Field(
        default="Pre-processing function that initializes the SAST workflow",
        description="Function description"
    )


@register_function(config_type=PreProcessConfig)
async def pre_process(
    config: PreProcessConfig, builder: Builder
):
    """Register the Pre_Process function."""
    
    logger.info("Initializing Pre_Process function...")
    
    def _create_default_analysis_response() -> AnalysisResponse:
        default_field_value = "This is a default value, if it's not replaced, something went wrong"
        return AnalysisResponse(investigation_result=CVEValidationStatus.TRUE_POSITIVE.value, 
                                is_final=FALSE, justifications=[default_field_value], 
                                short_justifications=default_field_value, recommendations=[default_field_value], 
                                instructions=[], evaluation=[default_field_value], prompt=default_field_value)
    
    async def _pre_process_fn(empty_input: dict) -> SASTWorkflowTracker:
        """
        Pre-processing function for SAST workflow.
        """
        logger.info("Running Pre_Process node - initializing workflow")
        
        config = Config()
        issue_list = read_sast_report(config)
  
        issues_dict = {}
        for issue in issue_list:
          issues_dict[issue.id] = PerIssueData(issue=issue, source_code={}, similar_known_issues="", analysis_response=_create_default_analysis_response())
        
        tracker = SASTWorkflowTracker(issues=issues_dict, config=config, iteration_count=0, metrics={})
        
        # Initialize the repo handler, just to download the repo if needed
        repo_handler_factory(config)
        
        logger.info("Pre_Process node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_pre_process_fn,
            description=config.description,
            input_schema=None  # No input for pre_process
        )
    except GeneratorExit:
        logger.info("Pre_Process function exited early!")
    finally:
        logger.info("Cleaning up Pre_Process function.") 
        
