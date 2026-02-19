import json
import logging
import os

from nat.builder.builder import Builder
from nat.builder.framework_enum import LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.function import FunctionBaseConfig
from pydantic import Field

from dto.SASTWorkflowModels import SASTWorkflowTracker

# Import token usage callback for LLM token tracking
from sast_agent_workflow.callbacks.token_usage_callback import TokenUsageCallback

# Import extended embedder for automatic registration
from sast_agent_workflow.embedders import extended_openai_embedder  # noqa: F401
from sast_agent_workflow.graph_builder import (
    build_sast_workflow_graph,
    verify_graph_structure,
)
from Utils.metrics_utils import categorize_issues_by_status

# Constants for timing and token tracking
DEFAULT_TOKEN_METRICS_PATH = "/shared-data/token_usage.json"

# Import nodes for automatic registration (used for side effects)
from sast_agent_workflow.nodes import (  # noqa: F401, E402
    calculate_metrics,
    filter,
    investigate,
    pre_process,
    summarize_justifications,
    write_results,
)

logger = logging.getLogger(__name__)

# Log NAT version on module load
try:
    from importlib.metadata import version

    nat_version = version("nvidia-nat")
    logger.info(f"NAT version: {nat_version}")
except Exception as e:
    logger.warning(f"Could not determine NAT version: {e}")


class SASTAgentConfig(FunctionBaseConfig, name="sast_agent"):
    """Configuration for SAST Agent workflow type."""

    pre_process_function_name: str = Field(description="Function name for Pre_Process node")
    filter_function_name: str = Field(description="Function name for Filter node")
    investigate_function_name: str = Field(description="Function name for Investigate node")
    summarize_justifications_function_name: str = Field(
        description="Function name for Summarize_Justifications node"
    )
    calculate_metrics_function_name: str = Field(
        description="Function name for Calculate_Metrics node"
    )
    write_results_function_name: str = Field(description="Function name for Write_Results node")

    description: str = Field(
        default="SAST agent workflow for analyzing SAST issues",
        description="Workflow function description",
    )


@register_function(config_type=SASTAgentConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN])
async def register_sast_agent(config: SASTAgentConfig, builder: Builder):
    """
    Register the SAST Agent workflow type.

    This defines how the sast_agent workflow type works using LangGraph.
    """
    logger.info("Initializing SAST Agent workflow...")

    # Initialize token usage callback for tracking LLM token usage
    metrics_path = os.getenv("TOKEN_METRICS_PATH", DEFAULT_TOKEN_METRICS_PATH)
    token_callback = TokenUsageCallback(output_path=metrics_path)
    logger.info(f"Token usage callback initialized with output path: {metrics_path}")

    # Access all the placeholder functions (get_function is async in NAT 1.3.1+)
    pre_process_fn = await builder.get_function(name=config.pre_process_function_name)
    filter_fn = await builder.get_function(name=config.filter_function_name)
    investigate_fn = await builder.get_function(name=config.investigate_function_name)
    summarize_justifications_fn = await builder.get_function(
        name=config.summarize_justifications_function_name
    )
    calculate_metrics_fn = await builder.get_function(name=config.calculate_metrics_function_name)
    write_results_fn = await builder.get_function(name=config.write_results_function_name)

    # Define langgraph node functions with timing decorator
    @token_callback.track_node_timing("pre_process")
    async def pre_process_node(state: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Pre_Process node that initializes the workflow."""
        logger.info("Running Pre_Process node")
        return await pre_process_fn.ainvoke({})

    @token_callback.track_node_timing("filter")
    async def filter_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Filter node that filters issues."""
        logger.info("Running Filter node")
        return await filter_fn.ainvoke(tracker)

    @token_callback.track_node_timing("investigate")
    async def investigate_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Investigate node that autonomously investigates issues."""
        logger.info("Running Investigate node")
        return await investigate_fn.ainvoke(tracker)

    @token_callback.track_node_timing("summarize_justifications")
    async def summarize_justifications_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Summarize_Justifications node that summarizes justifications."""
        logger.info("Running Summarize_Justifications node")
        return await summarize_justifications_fn.ainvoke(tracker)

    @token_callback.track_node_timing("calculate_metrics")
    async def calculate_metrics_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Calculate_Metrics node that calculates metrics."""
        logger.info("Running Calculate_Metrics node")
        return await calculate_metrics_fn.ainvoke(tracker)

    @token_callback.track_node_timing("write_results")
    async def write_results_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Write_Results node that writes results."""
        logger.info("Running Write_Results node")
        return await write_results_fn.ainvoke(tracker)

    # Build the LangGraph workflow using the graph builder
    graph = build_sast_workflow_graph(
        pre_process_node=pre_process_node,
        filter_node=filter_node,
        investigate_node=investigate_node,
        summarize_justifications_node=summarize_justifications_node,
        calculate_metrics_node=calculate_metrics_node,
        write_results_node=write_results_node,
    )

    # Verify graph was built successfully
    verify_graph_structure(graph)

    # Converter functions for different input types
    def convert_str_to_sast_tracker(input_str: str) -> SASTWorkflowTracker:
        """Convert string input to SASTWorkflowTracker
        LangGraph requires a non-empty state to be passed to the workflow.
        This function creates a default SASTWorkflowTracker with a dummy config field.

        Note: The input_str value is ignored. It is only used to satisfy NeMo's --input flag.
        """
        logger.info("Creating empty SASTWorkflowTracker to satisfy LangGraph requirements")
        empty_state = SASTWorkflowTracker()
        empty_state.config = (
            None  # Dummy update to avoid LangGraph error ("Must write to at least one of [...]")
        )
        return empty_state

    def convert_sast_tracker_to_str(tracker: SASTWorkflowTracker) -> str:
        """Convert SASTWorkflowTracker to summary statistics string"""
        logger.debug("Converting SASTWorkflowTracker to summary stats")
        try:
            # For debug, print the tracker to the console
            from pprint import pformat

            tracker_dict = tracker.model_dump(exclude={"config"})
            logger.debug("SASTWorkflowTracker contents:\n%s", pformat(tracker_dict))

            # Calculate summary statistics
            counter = categorize_issues_by_status(tracker.issues)

            return json.dumps(counter, indent=2)

        except Exception as e:
            logger.error("Failed to convert SASTWorkflowTracker to summary stats: %s", e)
            raise e

    async def _response_fn(input_message: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Main response function that runs the LangGraph workflow with callback tracking"""
        # Run workflow with callback for token usage and timing tracking
        results = await graph.ainvoke(input_message, config={"callbacks": [token_callback]})
        return SASTWorkflowTracker(**results)

    try:
        yield FunctionInfo.from_fn(
            _response_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker,
            converters=[convert_str_to_sast_tracker, convert_sast_tracker_to_str],
        )
    except GeneratorExit:
        logger.info("SAST Agent workflow exited early!")
    finally:
        logger.info("Cleaning up SAST Agent workflow.")
        # Ensure token usage data is written to file
        if "token_callback" in locals():
            logger.info("Writing token usage data from callback")
            token_callback._write_file()
