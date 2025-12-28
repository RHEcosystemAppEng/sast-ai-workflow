"""
LangGraph workflow construction for SAST Investigation Agent.

This module builds the agent graph using NAT-provided tools. It creates wrapper
nodes that bridge between stateless NAT tools and the stateful SASTAgentState.

Architecture:
- Tools are LangChain BaseTool objects from NAT (stateless, reusable)
- Agent node uses tool-bound LLM for intelligent tool selection
- Tool wrapper nodes extract args from LLM calls, invoke NAT tools, and update state
- Specialized routing: agent → tools → agent (until evaluator verification)
- Circuit breakers: max iterations, duplicate detection, error recovery, stalled progress, guard rejection limit
"""

import logging
from datetime import datetime
from functools import partial
from typing import List

from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.messages import ToolMessage
from langchain_core.tools import BaseTool
from langgraph.graph import END, StateGraph
from langgraph.graph.state import CompiledStateGraph

from .agent_node import agent_decision_node
from .agent_state import SASTAgentState, ToolError
from .tools.state_updaters import ToolStateUpdater, get_state_updater

logger = logging.getLogger(__name__)


def route_next(
    state: SASTAgentState,
    *,
    max_iterations: int,
    max_error_recovery: int,
    enable_duplicate_detection: bool = True,
) -> str:
    """
    Generic router for the agent graph.

    This single routing function handles:
    - **Agent → Tool** routing: called after the agent decision node; routes based on
      the last AI message's tool_calls.
    - **Tool → Next** routing: called after tool wrapper nodes; routes back to agent,
      or terminates when `is_final` is set (e.g., after evaluator verification or a breaker).

    Why one router works:
    - After the agent node, the last message will (normally) be an AI message with tool_calls.
    - After a tool node, the last message will be a ToolMessage and tool_calls will be absent;
      we can instead consult `tool_call_history` to determine which tool just ran.

    Returns:
        Next node name: a tool name, "agent", "circuit_breaker", or END.
    """
    # ---- Case 1: Agent just ran and produced a tool call ----
    if state.memory.messages:
        last_message = state.memory.messages[-1]
        if hasattr(last_message, "tool_calls") and last_message.tool_calls:
            tool_call = last_message.tool_calls[0]
            tool_name = tool_call["name"]
            logger.info(f"[{state.issue_id}] Routing to tool: {tool_name}")
            return tool_name

    # ---- Case 2: A tool just ran (ToolMessage appended), decide what’s next ----
    if not state.memory.tool_call_history:
        # Fallback: if we somehow have no history, send back to agent.
        logger.debug(f"[{state.issue_id}] No tool_call_history yet, routing to agent")
        return "agent"

    last_tool_name = state.memory.tool_call_history[-1][0]

    # Apply circuit breakers after EVERY tool call
    # Circuit breaker 1: Max iterations
    if state.iteration_count >= max_iterations:
        logger.warning(
            f"[{state.issue_id}] Circuit breaker: Max iterations ({max_iterations}) reached"
        )
        return "circuit_breaker"

    # Circuit breaker 2: Duplicate calls
    if enable_duplicate_detection and is_duplicate_call(state):
        logger.warning(f"[{state.issue_id}] Circuit breaker: Duplicate tool call detected")
        return "circuit_breaker"

    # Circuit breaker 3: Error recovery limit
    if state.error_state.error_recovery_attempts >= max_error_recovery:
        logger.warning(
            f"[{state.issue_id}] Circuit breaker: Max error recovery ({max_error_recovery})"
        )
        return "circuit_breaker"

    # Circuit breaker 4: Stalled progress (3 consecutive retrieval calls without new evidence/claims)
    if state.no_progress_streak >= 3:
        logger.warning(f"[{state.issue_id}] Circuit breaker: Stalled progress (no_progress_streak)")
        return "circuit_breaker"

    # Circuit breaker 5: Guard (evaluator) rejection limit
    if state.guard_rejection_streak >= 3:
        logger.warning(
            f"[{state.issue_id}] Circuit breaker: Guard rejection limit (guard_rejection_streak)"
        )
        return "circuit_breaker"

    # Circuit breaker 6: Agent repeatedly fails to produce reasoning
    if state.error_state.last_error and state.error_state.last_error.tool_name == "agent_reasoning":
        if state.error_state.error_recovery_attempts >= 2:
            logger.warning(
                f"[{state.issue_id}] Circuit breaker: Agent failed to produce reasoning 2+ times"
            )
            return "circuit_breaker"

    # Normal termination: any tool can theoretically set is_final (today only evaluation does)
    if state.is_final:
        logger.info(f"[{state.issue_id}] Investigation complete (is_final=TRUE)")
        return END

    # All tools loop back to agent until is_final is set.
    if last_tool_name == "evaluator":
        logger.info(f"[{state.issue_id}] Evaluator did not verify (continuing)")
    return "agent"


def is_duplicate_call(state: SASTAgentState) -> bool:
    """
    Circuit breaker: Detect if agent is repeating the same tool call.

    Checks if the last 2 tool calls are identical (same tool + same args).
    """
    if len(state.memory.tool_call_history) < 2:
        return False

    last_call = state.memory.tool_call_history[-1]
    previous_call = state.memory.tool_call_history[-2]

    # Compare tool name and args
    if last_call[0] == previous_call[0] and last_call[1] == previous_call[1]:
        logger.warning(
            f"[{state.issue_id}] Duplicate call detected: {last_call[0]}({last_call[1]})"
        )
        return True

    return False


async def circuit_breaker_node(state: SASTAgentState) -> SASTAgentState:
    """
    Handle circuit breaker termination - set is_final and default verdict.

    This node is called when any circuit breaker condition is met.
    It properly updates state before termination, keeping routing functions pure.
    """
    logger.warning(f"[{state.issue_id}] Circuit breaker triggered, forcing termination")
    state.is_final = True
    if not state.analysis.verdict:
        state.analysis.verdict = "NEEDS_REVIEW"
    if not state.stop_reason:
        state.stop_reason = "circuit_breaker"
    return state


def create_tool_wrapper_node(tool: BaseTool, state_updater: ToolStateUpdater, tool_name: str):
    """
    Create a wrapper node that calls a NAT tool and updates agent state.

    This is a generic wrapper that handles all common logic (error handling,
    message management, iteration tracking). Tool-specific state updates are
    delegated to the injected state_updater via the Strategy pattern.

    Args:
        tool: The NAT-registered BaseTool
        state_updater: Strategy for updating state based on tool results
        tool_name: Name of the tool (for logging and messages)

    Returns:
        Node function that takes SASTAgentState and returns updated SASTAgentState
    """

    async def tool_node(state: SASTAgentState) -> SASTAgentState:
        """Generic wrapper node for NAT tools."""
        issue_id = state.issue_id
        logger.info(f"[{issue_id}] Executing tool: {tool_name}")

        # Increment iteration counter
        state.iteration_count += 1

        # Snapshot for stalled progress detection
        before_claims = len(state.analysis.claims)
        before_evidence = len(state.analysis.evidence)
        before_unknowns = len(state.analysis.unknowns)

        # Extract tool call from last message
        last_message = state.memory.messages[-1]
        if not hasattr(last_message, "tool_calls") or not last_message.tool_calls:
            error_msg = f"No tool_calls found in last message for tool {tool_name}"
            logger.error(f"[{issue_id}] {error_msg}")
            # Record error
            error = ToolError(
                tool_name=tool_name,
                error_message=error_msg,
                attempted_args={},
                timestamp=datetime.now().isoformat(),
            )
            state.error_state.errors.append(error)
            state.error_state.last_error = error
            state.error_state.error_recovery_attempts += 1
            return state

        tool_call = last_message.tool_calls[0]
        tool_args = tool_call.get("args", {})
        tool_call_id = tool_call.get("id", "unknown")

        logger.debug(f"[{issue_id}] Tool args: {tool_args}")

        # Inject state into tool_args for all tools
        # This allows tools to access context (fetched_files, found_symbols, etc.)
        # without the LLM having to construct or pass the state object
        tool_args["state"] = state
        logger.debug(f"[{issue_id}] Injected state into tool_args")

        try:
            # Call the NAT tool
            if hasattr(tool, "ainvoke"):
                result = await tool.ainvoke(tool_args)
            else:
                result = tool.invoke(tool_args)

            logger.info(f"[{issue_id}] Tool {tool_name} succeeded")

            # Delegate tool-specific state updates to injected strategy
            state_updater.update_state(state, tool_args, result)

            # Add tool result to messages as ToolMessage
            tool_message = ToolMessage(content=result, tool_call_id=tool_call_id, name=tool_name)
            state.memory.messages.append(tool_message)

            # Record tool call for duplicate detection
            state.memory.tool_call_history.append((tool_name, tool_args))
            if len(state.memory.tool_call_history) > 2:
                state.memory.tool_call_history = state.memory.tool_call_history[-2:]

            # Update stalled progress counters:
            # Progress = agent produced new reasoning artifacts OR resolved unknowns
            after_claims = len(state.analysis.claims)
            after_evidence = len(state.analysis.evidence)
            after_unknowns = len(state.analysis.unknowns)

            made_progress = (
                (after_claims > before_claims)
                or (after_evidence > before_evidence)
                or (after_unknowns < before_unknowns)  # Resolved unknowns counts as progress
            )

            if tool_name not in ["evaluator"]:  # Don't penalize evaluator for not adding reasoning
                state.no_progress_streak = 0 if made_progress else (state.no_progress_streak + 1)

                if made_progress:
                    logger.info(
                        f"[{issue_id}] Progress: claims +{after_claims-before_claims}, "
                        f"evidence +{after_evidence-before_evidence}, "
                        f"unknowns {before_unknowns}→{after_unknowns}"
                    )
                else:
                    logger.warning(
                        f"[{issue_id}] No reasoning progress (streak={state.no_progress_streak}/3)"
                    )
            else:
                # Evaluator doesn't produce claims/evidence directly
                state.no_progress_streak = 0 if made_progress else state.no_progress_streak

        except Exception as e:
            logger.error(f"[{issue_id}] Tool {tool_name} failed: {e}", exc_info=True)

            # Record error
            error = ToolError(
                tool_name=tool_name,
                error_message=str(e),
                attempted_args=tool_args,
                timestamp=datetime.now().isoformat(),
            )
            state.error_state.errors.append(error)
            state.error_state.last_error = error
            state.error_state.error_recovery_attempts += 1

            # Add error message to messages
            error_tool_message = ToolMessage(
                content=f"Error: {str(e)}", tool_call_id=tool_call_id, name=tool_name
            )
            state.memory.messages.append(error_tool_message)

        return state

    return tool_node


def create_agent_graph(
    llm: BaseChatModel,
    tools: List[BaseTool],
    config,
    max_iterations: int = 15,
    max_error_recovery_attempts: int = 3,
    enable_duplicate_detection: bool = True,
) -> CompiledStateGraph:
    """
    Build the per-issue agent investigation graph with NAT tools.

    Graph structure:
        agent (decide with tool-bound LLM) → tool wrapper nodes → agent → ... → evaluator → END

    Args:
        llm: Language model for agent decision making
        tools: List of NAT-registered BaseTool objects
        config: Configuration object (for repo handler, etc.)
        max_iterations: Maximum investigation iterations
        max_error_recovery_attempts: Maximum consecutive errors
        enable_duplicate_detection: Enable duplicate call circuit breaker

    Returns:
        Compiled LangGraph ready for invocation
    """
    logger.info("Building NAT-integrated agent investigation graph...")
    logger.info(f"Tools available: {[t.name for t in tools]}")

    # Create tool dictionary
    tools_dict = {tool.name: tool for tool in tools}

    # Bind tools to LLM for tool calling
    # NOTE: NIM endpoints may not support tool_choice="auto", so we pass tool_choice=None
    # to let the model decide without requiring the auto-tool-choice flag
    try:
        bound_llm = llm.bind_tools(tools, tool_choice=None)
        logger.info("Tools bound to LLM for tool calling (tool_choice=None)")
    except TypeError:
        # Fallback if tool_choice parameter not supported
        bound_llm = llm.bind_tools(tools)
        logger.info("Tools bound to LLM for tool calling (default behavior)")

    # Create graph
    graph = StateGraph(SASTAgentState)

    # Add agent node (uses tool-bound LLM)
    # NOTE: Must use async wrapper because agent_decision_node is async
    async def agent_node_wrapper(state: SASTAgentState) -> SASTAgentState:
        """Async wrapper for agent_decision_node to ensure proper await."""
        return await agent_decision_node(state, bound_llm)

    graph.add_node("agent", agent_node_wrapper)

    # Add circuit breaker node for handling forced termination
    graph.add_node("circuit_breaker", circuit_breaker_node)

    # Add tool wrapper nodes with injected state updaters (Strategy pattern)
    for tool_name, tool in tools_dict.items():
        state_updater = get_state_updater(tool_name)
        wrapper_node = create_tool_wrapper_node(tool, state_updater, tool_name)
        graph.add_node(tool_name, wrapper_node)
        logger.debug(f"Added tool wrapper node: {tool_name} with {type(state_updater).__name__}")

    # Set entry point
    graph.set_entry_point("agent")

    # Agent routes to tools based on LLM decision
    tool_routes = {tool_name: tool_name for tool_name in tools_dict.keys()}
    tool_routes["agent"] = "agent"  # Fallback
    tool_routes[END] = END

    route_fn = partial(
        route_next,
        max_iterations=max_iterations,
        max_error_recovery=max_error_recovery_attempts,
        enable_duplicate_detection=enable_duplicate_detection,
    )

    graph.add_conditional_edges("agent", route_fn, tool_routes)

    # Circuit breaker always routes to END after updating state
    graph.add_edge("circuit_breaker", END)

    # Tool routing logic
    for tool_name in tools_dict.keys():
        # All tools route via the same router so circuit breakers apply uniformly
        graph.add_conditional_edges(
            tool_name,
            route_fn,
            {"agent": "agent", "circuit_breaker": "circuit_breaker", END: END},
        )

    # Compile graph
    compiled = graph.compile()

    logger.info("NAT-integrated agent investigation graph compiled successfully")

    return compiled
