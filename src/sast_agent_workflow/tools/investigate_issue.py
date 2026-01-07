"""
Investigate Issue node - wraps agent graph for batch workflow integration.

This node replaces the 3-iteration loop (data_fetcher → judge → evaluate)
with agent-based investigation. It runs once per workflow and processes
each non-final issue using the agent graph.
"""

import logging

from nat.builder.builder import Builder, LLMFrameworkEnum
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.function import FunctionBaseConfig
from pydantic import Field

from dto.LLMResponse import AnalysisResponse, FinalStatus
from dto.SASTWorkflowModels import SASTWorkflowTracker
from sast_agent_workflow.agent.agent_graph import create_agent_graph
from sast_agent_workflow.agent.agent_state import SASTAgentState

logger = logging.getLogger(__name__)


class InvestigateIssueConfig(FunctionBaseConfig, name="investigate_issue"):
    """Investigate issue function using agent-based investigation."""

    description: str = Field(
        default="Agent-based investigation replacing fixed iteration loop",
        description="Function description",
    )
    llm_name: str = Field(default="main_llm", description="LLM name for agent and analysis")


@register_function(
    config_type=InvestigateIssueConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN]
)
async def investigate_issue(config: InvestigateIssueConfig, builder: Builder):
    """
    Register the investigate_issue function.

    This replaces the data_fetcher → judge_llm → evaluate_analysis loop.
    """
    logger.info("Initializing Investigate_Issue function...")

    async def _investigate_issue_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Run agent investigation for each non-final issue.

        For each issue in the tracker:
        1. Create per-issue agent state
        2. Run agent graph (with timeout + recursion limit)
        3. Update tracker with investigation results
        """
        if tracker is None:
            raise ValueError("Tracker must not be None")

        logger.info("Running Investigate_Issue node - agent-based investigation")

        # Count non-final issues that need investigation
        issues_to_investigate = {
            issue_id: per_issue
            for issue_id, per_issue in tracker.issues.items()
            if not (
                per_issue.analysis_response
                and per_issue.analysis_response.is_final == FinalStatus.TRUE.value
            )
        }

        total_issues = len(tracker.issues)
        filtered_count = total_issues - len(issues_to_investigate)

        logger.info(
            f"Total issues: {total_issues}, Filtered: {filtered_count}, "
            f"To investigate: {len(issues_to_investigate)}"
        )

        # If all issues are filtered, skip initialization
        if not issues_to_investigate:
            logger.info("All issues filtered - skipping agent initialization")
            return tracker

        # Get LLM
        llm = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)

        # Initialize repo_handler for initial code extraction from error traces
        from common.config import Config
        from handlers.repo_handler_factory import repo_handler_factory

        try:
            global_config = Config()
            repo_handler = repo_handler_factory(global_config)
            logger.info(f"Initialized repo_handler for: {global_config.REPO_LOCAL_PATH}")
        except Exception as e:
            logger.error(f"Failed to initialize repo_handler: {e}")
            raise

        # Get tools from NAT
        # Agent has access to code exploration tools for thorough investigation
        tool_names = ["fetch_code", "evaluator", "list_files", "read_file", "search_codebase"]
        tools = await builder.get_tools(
            tool_names=tool_names, wrapper_type=LLMFrameworkEnum.LANGCHAIN
        )
        if not tools:
            raise ValueError(
                f"Failed to load tools: {tool_names}. Ensure tools are registered with NAT."
            )
        logger.info(f"Loaded {len(tools)} tools: {[t.name for t in tools]}")

        # Build agent graph with NAT tools
        max_iterations = 25  # Increased from 15 to allow for more thorough investigation
        agent_graph = create_agent_graph(
            llm=llm,
            tools=tools,
            config=tracker.config,
            max_iterations=max_iterations,
            max_error_recovery_attempts=3,
            enable_duplicate_detection=True,
        )

        # Process each non-final issue
        for issue_id, per_issue in issues_to_investigate.items():

            logger.info("=" * 60)
            logger.info(f"Investigating issue: {issue_id}")
            logger.info("=" * 60)

            # Extract initial code from error trace BEFORE creating state
            from sast_agent_workflow.agent.tools.fetch_code import fetch_code_from_error_trace
            from sast_agent_workflow.agent.agent_state import InvestigationContext

            initial_code = fetch_code_from_error_trace(
                error_trace=per_issue.issue.trace,
                repo_handler=repo_handler,
                issue_id=issue_id,
            )

            # Log initial code extraction results
            if initial_code:
                logger.info(f"[{issue_id}] Initial code extracted from trace: {len(initial_code)} files")
                for file_path, code_list in initial_code.items():
                    logger.info(f"[{issue_id}]   - {file_path}")
                    for code in code_list:
                        # Log first 500 chars of code for debugging
                        code_preview = code[:500] if len(code) > 500 else code
                        logger.debug(f"[{issue_id}] Code preview from {file_path}:\n{code_preview}")
            else:
                logger.warning(f"[{issue_id}] No initial code extracted from trace")

            # Create per-issue agent state with pre-loaded code
            agent_state = SASTAgentState(
                issue_id=issue_id,
                issue=per_issue.issue,
                context=InvestigationContext(
                    fetched_files=dict(per_issue.source_code) if per_issue.source_code else {},
                    found_symbols=(
                        sorted(per_issue.found_symbols) if per_issue.found_symbols else []
                    ),
                    source_code=initial_code,  # Pre-loaded code from error trace
                ),
            )

            # Run agent investigation with timeout and recursion limit
            try:
                # Calculate recursion_limit based on max_iterations
                # Formula: each iteration = agent node + tool node (×2), plus buffer for routing
                recursion_limit = (max_iterations * 2) + 10  # = (25 * 2) + 10 = 60

                final_state_dict = await agent_graph.ainvoke(
                    agent_state,
                    config={
                        "recursion_limit": recursion_limit,
                        "timeout": 300,  # 5 minutes
                    },
                )

                # LangGraph returns a dict, convert back to SASTAgentState
                if isinstance(final_state_dict, dict):
                    # Use model_validate for proper nested object handling
                    final_state = SASTAgentState.model_validate(final_state_dict)
                else:
                    final_state = final_state_dict

                # Update tracker with results
                investigation_result = final_state.analysis.verdict or "NEEDS_REVIEW"
                justifications = (
                    final_state.analysis.final_justifications
                    or (
                        final_state.analysis.last_evaluator_report.justifications
                        if final_state.analysis.last_evaluator_report
                        else []
                    )
                    or [
                        f"Investigation stopped: {final_state.stop_reason or 'unknown'}",
                    ]
                )

                per_issue.analysis_response = AnalysisResponse(
                    investigation_result=investigation_result,
                    is_final=(
                        FinalStatus.TRUE.value if final_state.is_final else FinalStatus.FALSE.value
                    ),
                    justifications=justifications,
                    prompt="agentic_investigation (ADR-0002)",
                )
                per_issue.source_code = final_state.context.fetched_files
                # PerIssueData expects a Set[str]
                per_issue.found_symbols = set(final_state.context.found_symbols or [])

                logger.info(
                    f"Investigation complete for {issue_id}: "
                    f"verdict={final_state.analysis.verdict}, "
                    f"iterations={final_state.iteration_count}"
                )

            except TimeoutError:
                logger.warning(f"Investigation timeout for issue {issue_id} after 5 minutes")
                # Force evaluation with available context
                # TODO: Implement fallback logic
                # For now, mark as needs review
                if per_issue.analysis_response:
                    per_issue.analysis_response.is_final = FinalStatus.TRUE.value
                    per_issue.analysis_response.justifications.append(
                        "Investigation timeout - NEEDS_REVIEW"
                    )

            except Exception as e:
                logger.error(f"Investigation failed for issue {issue_id}: {e}", exc_info=True)
                # Mark as needs review
                # TODO: Implement error handling logic
                if per_issue.analysis_response:
                    per_issue.analysis_response.is_final = FinalStatus.TRUE.value
                    per_issue.analysis_response.justifications.append(
                        f"Investigation error - NEEDS_REVIEW: {str(e)}"
                    )

        # Increment tracker iteration (for compatibility)
        tracker.iteration_count += 1

        logger.info("Investigate_Issue node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_investigate_issue_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker,
        )
    except GeneratorExit:
        logger.info("Investigate_Issue function exited early!")
    finally:
        logger.info("Cleaning up Investigate_Issue function.")
