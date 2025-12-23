"""
SAST Investigation Agent - NAT Registration.

This module registers the SAST Investigation Agent with NAT as a workflow function.
The agent uses a custom LangGraph for specialized SAST triage routing, but integrates
with NAT for LLM/tool access and observability.

Agent Architecture:
- Custom state (SASTAgentState) with nested investigation context
- Specialized routing: agent → tools → analyze → evaluate → conditional END
- Circuit breakers: max iterations, duplicate calls, error recovery
- Tools from NAT: fetch_code, analyze_issue, comprehensive_evaluation
"""

import logging
from datetime import datetime

from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.agent import AgentBaseConfig
from nat.data_models.component_ref import FunctionRef, LLMRef
from pydantic import Field

from .agent_graph import create_agent_graph
from .agent_state import SASTAgentState, ToolError

logger = logging.getLogger(__name__)


class SASTInvestigationAgentConfig(AgentBaseConfig, name="sast_investigation_agent"):
    """
    Configuration for SAST Investigation Agent.

    This agent performs deep investigation of SAST findings using a ReAct-style
    loop with specialized security evaluation feedback.
    """

    description: str = Field(
        default="SAST Investigation Agent - analyzes security findings",
        description="Agent description",
    )

    # LLM configuration
    llm_name: LLMRef = Field(description="LLM model for agent decision-making and analysis")

    # Tool configuration
    tool_names: list[FunctionRef] = Field(
        default_factory=lambda: ["fetch_code", "analyze_issue", "comprehensive_evaluation"],
        description="Tools available to the agent for SAST investigation",
    )

    # Agent parameters
    max_iterations: int = Field(
        default=15, description="Maximum investigation iterations before circuit breaker"
    )

    max_error_recovery_attempts: int = Field(
        default=3, description="Maximum consecutive error recovery attempts before termination"
    )

    enable_duplicate_detection: bool = Field(
        default=True, description="Enable circuit breaker for duplicate tool calls"
    )

    # Prompt configuration
    system_prompt_file: str = Field(
        default="agent_decision_prompt.yaml",
        description="YAML file with agent system prompt (in templates/prompts/)",
    )

    formatting_templates_file: str = Field(
        default="agent_formatting_templates.yaml",
        description="YAML file with formatting templates (in templates/prompts/)",
    )


@register_function(
    config_type=SASTInvestigationAgentConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN]
)
async def sast_investigation_agent(config: SASTInvestigationAgentConfig, builder: Builder):
    """
    Register the SAST Investigation Agent with NAT.

    This creates a custom LangGraph workflow that:
    1. Uses NAT-provided LLM and tools
    2. Maintains rich investigation state (SASTAgentState)
    3. Implements specialized SAST investigation routing
    4. Provides circuit breakers and error recovery

    Args:
        config: Agent configuration
        builder: NAT builder for accessing LLM and tools

    Yields:
        FunctionInfo wrapping the agent graph invocation
    """
    logger.info("Initializing SAST Investigation Agent...")

    # Get LLM from NAT
    llm = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
    logger.info(f"Loaded LLM: {config.llm_name}")

    # Get tools from NAT
    tools = await builder.get_tools(
        tool_names=config.tool_names, wrapper_type=LLMFrameworkEnum.LANGCHAIN
    )
    if not tools:
        raise ValueError(
            f"No tools found for SAST Investigation Agent. Expected: {config.tool_names}"
        )
    logger.info(f"Loaded {len(tools)} tools: {[t.name for t in tools]}")

    # Build agent graph with specialized SAST investigation routing
    # NOTE: Config object provides repo handler and project paths
    # TODO: Inject Config from NAT context instead of global singleton
    from common.config import Config as RepoConfig

    repo_config = RepoConfig()

    agent_graph = create_agent_graph(
        llm=llm,
        tools=tools,
        config=repo_config,
        max_iterations=config.max_iterations,
        max_error_recovery_attempts=config.max_error_recovery_attempts,
        enable_duplicate_detection=config.enable_duplicate_detection,
    )

    logger.info("SAST Investigation Agent graph compiled successfully")

    # Converter functions for NAT input/output handling
    def convert_str_to_agent_state(input_str: str) -> SASTAgentState:
        """
        Convert string input to SASTAgentState.

        Note: The input_str value is ignored. This function creates a minimal
        SASTAgentState to satisfy NAT's --input flag requirement. In production,
        the agent would be invoked programmatically with proper Issue data.
        """
        logger.info("Creating minimal SASTAgentState from string input (test mode)")
        from dto.Issue import Issue

        # Create a minimal test issue
        test_issue = Issue(
            id="test-issue-001",
            issue_type="SQL Injection",
            trace="test_input -> query -> execute",
            file_path="test.py",
            line_number=42,
        )

        # Create minimal agent state
        empty_state = SASTAgentState(
            issue_id="test-issue-001",
            issue=test_issue,
        )
        return empty_state

    def convert_agent_state_to_str(state: SASTAgentState) -> str:
        """Convert SASTAgentState to summary string."""
        logger.debug("Converting SASTAgentState to summary")
        import json

        summary = {
            "issue_id": state.issue_id,
            "verdict": state.analysis.verdict if state.analysis.verdict else "INCOMPLETE",
            "is_final": state.is_final,
            "iterations": state.iteration_count,
            "files_fetched": len(state.context.fetched_files),
            "errors": len(state.error_state.errors),
        }
        return json.dumps(summary, indent=2)

    async def _response_fn(agent_state: SASTAgentState) -> SASTAgentState:
        """
        Main agent invocation function.

        Args:
            agent_state: Initial investigation state (issue, context, project_context)

        Returns:
            Final investigation state with analysis and verdict
        """
        issue_id = agent_state.issue_id
        logger.info(f"[{issue_id}] Starting SAST investigation")

        try:
            # Invoke agent graph with recursion limit
            # Formula: max_iterations * 2 (each iteration: agent node + tool node)
            # + buffer for analyze + evaluation nodes
            recursion_limit = (config.max_iterations * 2) + 10

            final_state = await agent_graph.ainvoke(
                agent_state,
                config={
                    "recursion_limit": recursion_limit,
                    # Future: Add Langfuse callbacks here
                    # "callbacks": [langfuse_callback]
                },
            )

            final_state_typed = SASTAgentState(**final_state)

            # Log results
            verdict = final_state_typed.analysis.verdict or "INCOMPLETE"
            iterations = final_state_typed.iteration_count
            files_count = len(final_state_typed.context.fetched_files)

            logger.info(
                f"[{issue_id}] Investigation complete: "
                f"verdict={verdict}, iterations={iterations}, files={files_count}"
            )

            return final_state_typed

        except Exception as e:
            logger.error(f"[{issue_id}] Investigation failed with exception: {e}", exc_info=True)
            # Return state with error information
            agent_state.is_final = True
            agent_state.analysis.verdict = "NEEDS_HUMAN_REVIEW"
            agent_state.error_state.last_error = ToolError(
                tool_name="agent_graph",
                error_message=str(e),
                attempted_args={},
                timestamp=datetime.now().isoformat(),
            )
            return agent_state

    try:
        yield FunctionInfo.from_fn(
            _response_fn,
            description=config.description,
            input_schema=SASTAgentState,
            converters=[convert_str_to_agent_state, convert_agent_state_to_str],
        )
    except GeneratorExit:
        logger.info("SAST Investigation Agent exited early!")
    finally:
        logger.info("Cleaning up SAST Investigation Agent")
