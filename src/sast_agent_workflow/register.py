# pylint: disable=unused-import
# flake8: noqa

import json
import logging
from io import TextIOWrapper
from typing import Any, Dict, Optional

from pydantic import BaseModel, Field

from aiq.builder.builder import Builder
from aiq.builder.framework_enum import LLMFrameworkEnum
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

# Import existing data models without src prefix
from common.config import Config
from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData

# Import any tools which need to be automatically registered here
from sast_agent_workflow.tools.sast_placeholder_function import sast_placeholder_function

logger = logging.getLogger(__name__)


class SASTAgentConfig(FunctionBaseConfig, name="sast_agent"):
    """Configuration for SAST Agent workflow type."""
    
    sast_placeholder_function_name: str = Field(description="Function name for SAST placeholder analysis")
    description: str = Field(default="SAST agent workflow for static application security testing",
                           description="Workflow function description")
    # Placeholder configuration - to be extended with actual SAST agent parameters


@register_function(config_type=SASTAgentConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN])
async def register_sast_agent(config: SASTAgentConfig, builder: Builder):
    """
    Register the SAST Agent workflow type.
    
    This defines how the sast_agent workflow type works using LangGraph.
    """
    logger.info("Initializing SAST Agent workflow...")
    
    from langgraph.graph import END
    from langgraph.graph import START
    from langgraph.graph import StateGraph
    
    # Access the SAST placeholder function
    sast_placeholder_fn = builder.get_function(name=config.sast_placeholder_function_name)
    
    # Define langgraph node functions
    async def placeholder_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Performs SAST analysis on the entire batch of issues using the placeholder function.
        Passes the entire SASTWorkflowTracker to the function for batch processing.
        """
        logger.info(f"SAST Agent processing batch of {len(tracker.issues)} issues")
        
        # Pass the entire tracker to the placeholder function for batch processing
        return await sast_placeholder_fn.ainvoke(tracker)
    
    # Build the LangGraph workflow
    graph_builder = StateGraph(SASTWorkflowTracker)
    graph_builder.add_node("placeholder_analysis", placeholder_node)
    
    graph_builder.add_edge(START, "placeholder_analysis")
    graph_builder.add_edge("placeholder_analysis", END)
    
    graph = graph_builder.compile()
    
    # Converter functions for different input types
    def convert_str_to_sast_tracker(input_str: str) -> SASTWorkflowTracker:
        """Convert string input to SASTWorkflowTracker"""
        logger.debug("Converting input to SASTWorkflowTracker: %s", input_str)
        try:
            # (moved import json to the top of the file)
            data = json.loads(input_str)
            
            # Check if config field exists, if not create it
            if 'config' not in data:
                logger.info("Missing 'config' field in JSON, creating config object")
                data['config'] = Config()
            
            # Now create SASTWorkflowTracker with the complete data
            return SASTWorkflowTracker.model_validate(data)
        except Exception as e:
            logger.error("Failed to convert string to SASTWorkflowTracker: %s", e)
            raise e
    
    def convert_textio_to_sast_tracker(input_textio: TextIOWrapper) -> SASTWorkflowTracker:
        """Convert TextIOWrapper input to SASTWorkflowTracker"""
        logger.debug("Converting TextIOWrapper to SASTWorkflowTracker")
        try:
            data = input_textio.read()
            return convert_str_to_sast_tracker(data)
        except Exception as e:
            logger.error("Failed to convert TextIOWrapper to SASTWorkflowTracker: %s", e)
            raise e
    
    def convert_sast_tracker_to_str(tracker: SASTWorkflowTracker) -> str:
        """Convert SASTWorkflowTracker to string"""
        logger.debug("Converting SASTWorkflowTracker to str")
        try:
            return tracker.model_dump_json(
                indent=2,
                exclude={'config'},  # Exclude config field since it's complex
            )
        except Exception as e:
            logger.error("Failed to convert SASTWorkflowTracker to str: %s", e)
            raise e
    
    async def _response_fn(input_message: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Main response function that runs the LangGraph workflow"""
        results = await graph.ainvoke(input_message)
        graph_output = SASTWorkflowTracker(**results)
        return graph_output
    
    try:
        yield FunctionInfo.from_fn(
            _response_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker,
            converters=[
                convert_str_to_sast_tracker,
                convert_textio_to_sast_tracker,
                convert_sast_tracker_to_str
            ]
        )
    except GeneratorExit:
        logger.info("SAST Agent workflow exited early!")
    finally:
        logger.info("Cleaning up SAST Agent workflow.")