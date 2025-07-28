import logging
from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.framework_enum import LLMFrameworkEnum
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import SASTWorkflowTracker

# Import any tools which need to be automatically registered here, its actually used even though they marked as unused
from sast_agent_workflow.tools import pre_process, \
        filter, data_fetcher, judge_llm_analysis, \
        evaluate_analysis, summarize_justifications, \
        calculate_metrics, write_results

logger = logging.getLogger(__name__)


class SASTAgentConfig(FunctionBaseConfig, name="sast_agent"):
    """Configuration for SAST Agent workflow type."""
    
    pre_process_function_name: str = Field(description="Function name for Pre_Process node")
    filter_function_name: str = Field(description="Function name for Filter node")
    data_fetcher_function_name: str = Field(description="Function name for Data_Fetcher node")
    judge_llm_analysis_function_name: str = Field(description="Function name for Judge_LLM_Analysis node")
    evaluate_analysis_function_name: str = Field(description="Function name for Evaluate_Analysis node")
    summarize_justifications_function_name: str = Field(description="Function name for Summarize_Justifications node")
    calculate_metrics_function_name: str = Field(description="Function name for Calculate_Metrics node")
    write_results_function_name: str = Field(description="Function name for Write_Results node")
    
    description: str = Field(default="SAST agent workflow for analyzing SAST issues and determining if they are false alarms",
                           description="Workflow function description")


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
    
    # Access all the placeholder functions
    pre_process_fn = builder.get_function(name=config.pre_process_function_name)
    filter_fn = builder.get_function(name=config.filter_function_name)
    data_fetcher_fn = builder.get_function(name=config.data_fetcher_function_name)
    judge_llm_analysis_fn = builder.get_function(name=config.judge_llm_analysis_function_name)
    evaluate_analysis_fn = builder.get_function(name=config.evaluate_analysis_function_name)
    summarize_justifications_fn = builder.get_function(name=config.summarize_justifications_function_name)
    calculate_metrics_fn = builder.get_function(name=config.calculate_metrics_function_name)
    write_results_fn = builder.get_function(name=config.write_results_function_name)
    
    # Define langgraph node functions
    async def pre_process_node(state: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Pre_Process node that initializes the workflow."""
        logger.info("Running Pre_Process node")
        return await pre_process_fn.ainvoke({})
    
    async def filter_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Filter node that filters issues."""
        logger.info("Running Filter node")
        return await filter_fn.ainvoke(tracker)
    
    async def data_fetcher_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Data_Fetcher node that fetches data."""
        logger.info("Running Data_Fetcher node")
        return await data_fetcher_fn.ainvoke(tracker)
    
    async def judge_llm_analysis_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Judge_LLM_Analysis node that performs LLM analysis."""
        logger.info("Running Judge_LLM_Analysis node")
        return await judge_llm_analysis_fn.ainvoke(tracker)
    
    async def evaluate_analysis_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Evaluate_Analysis node that evaluates analysis results."""
        logger.info("Running Evaluate_Analysis node")
        return await evaluate_analysis_fn.ainvoke(tracker)
    
    async def summarize_justifications_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Summarize_Justifications node that summarizes justifications."""
        logger.info("Running Summarize_Justifications node")
        return await summarize_justifications_fn.ainvoke(tracker)
    
    async def calculate_metrics_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Calculate_Metrics node that calculates metrics."""
        logger.info("Running Calculate_Metrics node")
        return await calculate_metrics_fn.ainvoke(tracker)
    
    async def write_results_node(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """Write_Results node that writes results."""
        logger.info("Running Write_Results node")
        return await write_results_fn.ainvoke(tracker)
    
    # Build the LangGraph workflow
    graph_builder = StateGraph(SASTWorkflowTracker)
    graph_builder.add_node("pre_process", pre_process_node)
    graph_builder.add_node("filter", filter_node)
    graph_builder.add_node("data_fetcher", data_fetcher_node)
    graph_builder.add_node("judge_llm_analysis", judge_llm_analysis_node)
    graph_builder.add_node("evaluate_analysis", evaluate_analysis_node)
    graph_builder.add_node("summarize_justifications", summarize_justifications_node)
    graph_builder.add_node("calculate_metrics", calculate_metrics_node)
    graph_builder.add_node("write_results", write_results_node)
    
    # Connect nodes in sequence
    graph_builder.add_edge(START, "pre_process")
    graph_builder.add_edge("pre_process", "filter")
    graph_builder.add_edge("filter", "data_fetcher")
    graph_builder.add_edge("data_fetcher", "judge_llm_analysis")
    graph_builder.add_edge("judge_llm_analysis", "evaluate_analysis")
    graph_builder.add_edge("evaluate_analysis", "summarize_justifications")
    graph_builder.add_edge("summarize_justifications", "calculate_metrics")
    graph_builder.add_edge("calculate_metrics", "write_results")
    graph_builder.add_edge("write_results", END)
    
    graph = graph_builder.compile()
    
    # Converter functions for different input types
    def convert_str_to_sast_tracker(input_str: str) -> SASTWorkflowTracker:
        """Convert string input to SASTWorkflowTracker
        LangGraph requires a non-empty state to be passed to the workflow.
        This function creates a default SASTWorkflowTracker with a dummy config field.
        
        Note: The input_str value is ignored. It is only used to satisfy NeMo's --input flag.
        """
        logger.info("Creating empty SASTWorkflowTracker to satisfy LangGraph requirements")
        empty_state = SASTWorkflowTracker()
        empty_state.config = None # Dummy update to avoid LangGraph error ("Must write to at least one of [...]")
        return empty_state
    
    def convert_sast_tracker_to_str(tracker: SASTWorkflowTracker) -> str:
        """Convert SASTWorkflowTracker to string"""
        logger.debug("Converting SASTWorkflowTracker to str")
        try:
            return tracker.model_dump_json(
                indent=2,
                exclude={'config'},  # Exclude config field since it's complex and not relevant to the output
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
                convert_sast_tracker_to_str
            ]
        )
    except GeneratorExit:
        logger.info("SAST Agent workflow exited early!")
    finally:
        logger.info("Cleaning up SAST Agent workflow.")
