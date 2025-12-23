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

from dto.LLMResponse import FinalStatus
from dto.SASTWorkflowModels import SASTWorkflowTracker
from sast_agent_workflow.agent.agent_graph import create_agent_graph
from sast_agent_workflow.agent.agent_state import SASTAgentState
from sast_agent_workflow.agent.project_context import initialize_project_context

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

        # Initialize project context ONCE (local to this function)
        project_context = initialize_project_context(tracker.config.REPO_LOCAL_PATH)

        # Get LLM
        llm = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)

        # Get tools from NAT
        tool_names = ["fetch_code", "analyze_issue", "comprehensive_evaluation"]
        tools = await builder.get_tools(
            tool_names=tool_names, wrapper_type=LLMFrameworkEnum.LANGCHAIN
        )
        if not tools:
            raise ValueError(
                f"Failed to load tools: {tool_names}. Ensure tools are registered with NAT."
            )
        logger.info(f"Loaded {len(tools)} tools: {[t.name for t in tools]}")

        # Build agent graph with NAT tools
        agent_graph = create_agent_graph(
            llm=llm,
            tools=tools,
            config=tracker.config,
            max_iterations=15,
            max_error_recovery_attempts=3,
            enable_duplicate_detection=True,
        )

        # Process each non-final issue
        for issue_id, per_issue in issues_to_investigate.items():

            logger.info("=" * 60)
            logger.info(f"Investigating issue: {issue_id}")
            logger.info("=" * 60)

            # Create per-issue agent state
            from sast_agent_workflow.agent.agent_state import InvestigationContext

            agent_state = SASTAgentState(
                issue_id=issue_id,
                issue=per_issue.issue,
                context=InvestigationContext(
                    fetched_files=dict(per_issue.source_code) if per_issue.source_code else {},
                    found_symbols=(
                        per_issue.found_symbols.copy() if per_issue.found_symbols else set()
                    ),
                ),
                project_context=project_context,  # Shared across all issues in this batch
            )

            # Run agent investigation with timeout and recursion limit
            try:
                final_state_dict = await agent_graph.ainvoke(
                    agent_state,
                    config={
                        "recursion_limit": 20,  # Max 20 nodes in path (15 tools + 5 buffer)
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
                # Defensive check: ensure investigation_result exists
                if final_state.analysis.investigation_result is None:
                    # Create fallback analysis if agent failed before producing one
                    from dto.LLMResponse import AnalysisResponse, CVEValidationStatus

                    logger.warning(f"[{issue_id}] No investigation result - creating fallback")
                    final_state.analysis.investigation_result = AnalysisResponse(
                        investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                        is_final=FinalStatus.TRUE.value,
                        justifications=["Investigation incomplete - needs manual review"],
                        prompt="Error - no analysis produced",
                    )

                per_issue.analysis_response = final_state.analysis.investigation_result
                per_issue.source_code = final_state.context.fetched_files
                per_issue.found_symbols = final_state.context.found_symbols
                per_issue.analysis_response.is_final = (
                    FinalStatus.TRUE.value if final_state.is_final else FinalStatus.FALSE.value
                )

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
                        "Investigation timeout - needs manual review"
                    )

            except Exception as e:
                logger.error(f"Investigation failed for issue {issue_id}: {e}", exc_info=True)
                # Mark as needs review
                # TODO: Implement error handling logic
                if per_issue.analysis_response:
                    per_issue.analysis_response.is_final = FinalStatus.TRUE.value
                    per_issue.analysis_response.justifications.append(
                        f"Investigation error: {str(e)}"
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
