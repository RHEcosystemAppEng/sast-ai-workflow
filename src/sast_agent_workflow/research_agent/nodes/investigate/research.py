"""Research node - Agent for code gathering with middleware-based state updates.

Upgraded to LangChain v1 create_agent API with middleware pattern.

Design: STATELESS per model call - model sees only system prompt (instructions + CODE BANK),
NOT conversation history. All context is embedded in the dynamic prompt.
"""

import logging
from typing import Annotated, Any, Dict, List, Optional

from langchain.agents import AgentState, create_agent
from langchain.agents.middleware import (
    ModelRequest,
    wrap_model_call,
    wrap_tool_call,
)
from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.messages import AIMessage, AnyMessage, SystemMessage, ToolMessage
from langchain_core.tools import BaseTool
from langgraph.errors import GraphRecursionError
from langgraph.prebuilt.tool_node import ToolCallRequest
from langgraph.types import Command
from typing_extensions import NotRequired

from common.config import Config

from ...core import (
    CODE_GATHERING_TOOLS,
    ERROR_MSG_TRUNCATE_CHARS,
    FAILED_FETCH_SUMMARY_CHARS,
    FAILED_MSG_PREVIEW_CHARS,
    MAX_TOOL_RESULT_CHARS,
    RESEARCH_AGENT_RECURSION_LIMIT,
    build_code_bank,
    build_research_instructions,
)

from .schemas import InvestigationState

logger = logging.getLogger(__name__)


# ============================================================================
# State Reducers for Custom State Management
# ============================================================================

def merge_dicts(left: Optional[Dict], right: Optional[Dict]) -> Dict:
    """
    Merge two dictionaries, combining list values and overwriting other values.
    
    Used for fetched_files state field to accumulate tool results.
    """
    left = left or {}
    right = right or {}
    result = dict(left)
    for key, value in right.items():
        if value is None:
            continue
        if key in result and isinstance(result[key], list) and isinstance(value, list):
            # Merge lists
            result[key] = result[key] + value
        else:
            # Overwrite
            result[key] = value
    return result


def unique_list_add(left: Optional[List], right: Optional[List]) -> List:
    """
    Add items from right to left, avoiding duplicates.
    
    Used for failed_fetches to track unique failures.
    """
    left = left or []
    right = right or []
    result = list(left)
    for item in right:
        if item not in result:
            result.append(item)
    return result


def _truncate_message(msg: AnyMessage) -> AnyMessage:
    """
    Truncate a single message if it's too long.
    
    - ToolMessage: Summarize long results (full code is in prompt CODE BANK)
    - AIMessage: Strip reasoning_content (thinking model bloat)
    - SystemMessage: Skip (handled by dynamic prompt)
    """
    # Truncate long ToolMessage results
    if isinstance(msg, ToolMessage):
        content = msg.content
        
        # Skip tool definition messages (shouldn't be in history)
        if isinstance(content, dict) and content.get('type') == 'function':
            return None  # Signal to skip
        
        content_str = content if isinstance(content, str) else str(content)
        if len(content_str) > MAX_TOOL_RESULT_CHARS:
            tool_name = getattr(msg, 'name', 'tool')
            is_error = "Error:" in content_str or "not found" in content_str.lower()
            
            if is_error:
                summary = content_str[:MAX_TOOL_RESULT_CHARS] + "..."
            else:
                # Successful fetch - summarize (full code in CODE BANK)
                lines = content_str.count('\n')
                chars = len(content_str)
                summary = f"[{tool_name}: {lines} lines, {chars:,} chars - see CODE BANK in prompt]"
            
            return ToolMessage(
                content=summary,
                tool_call_id=msg.tool_call_id,
                name=getattr(msg, 'name', None),
            )
    
    # Strip reasoning from AIMessage
    if isinstance(msg, AIMessage):
        # Check for reasoning in additional_kwargs
        if hasattr(msg, 'additional_kwargs') and msg.additional_kwargs:
            cleaned_kwargs = {
                k: v for k, v in msg.additional_kwargs.items()
                if k not in ('reasoning_content', 'reasoning', 'thinking')
            }
            if len(cleaned_kwargs) < len(msg.additional_kwargs):
                return AIMessage(
                    content=msg.content,
                    tool_calls=getattr(msg, 'tool_calls', []),
                    additional_kwargs=cleaned_kwargs,
                    id=msg.id,
                )
        
        # Check for reasoning blocks in content (if content is a list)
        if isinstance(msg.content, list):
            cleaned_content = [
                block for block in msg.content
                if not (isinstance(block, dict) and block.get('type') in ('reasoning', 'thinking'))
            ]
            if len(cleaned_content) < len(msg.content):
                return AIMessage(
                    content=cleaned_content,
                    tool_calls=getattr(msg, 'tool_calls', []),
                    additional_kwargs=getattr(msg, 'additional_kwargs', {}),
                    id=msg.id,
                )
    
    return msg


def messages_reducer(left: Optional[List[AnyMessage]], right: Optional[List[AnyMessage]]) -> List[AnyMessage]:
    """
    Custom reducer for messages that truncates long content as messages are added.
    
    This ensures:
    - Tool results are summarized (full code is in prompt CODE BANK)
    - AI reasoning is stripped (thinking model bloat)
    - History stays bounded automatically
    """
    left = left or []
    right = right or []
    
    # Process new messages (right) before merging
    cleaned_right = []
    for msg in right:
        truncated = _truncate_message(msg)
        if truncated is not None:  # None means skip
            cleaned_right.append(truncated)
    
    return left + cleaned_right


# ============================================================================
# Research Agent State Schema
# ============================================================================

class ResearchAgentState(AgentState):
    """
    State schema for the research agent.
    
    Extends LangChain v1 AgentState with domain-specific fields
    for tracking code gathering progress. Uses NotRequired for optional fields.
    
    Custom messages reducer truncates long content automatically:
    - Tool results → summarized (full code in prompt CODE BANK)
    - AI reasoning → stripped (thinking model bloat)
    """
    # Override messages with custom reducer that truncates content
    messages: Annotated[List[AnyMessage], messages_reducer]
    
    # Domain-specific state with custom reducers
    fetched_files: Annotated[NotRequired[Dict[str, List[str]]], merge_dicts]
    failed_fetches: Annotated[NotRequired[List[str]], unique_list_add]
    
    # Investigation context (passed through from parent state)
    issue_id: NotRequired[str]
    iteration: NotRequired[int]
    issue_description: NotRequired[str]
    initial_code: NotRequired[str]
    evaluation_feedback: NotRequired[str]
    required_information: NotRequired[List[str]]


# ============================================================================
# Middleware (LangChain v1 Pattern)
# ============================================================================

@wrap_model_call
async def stateless_model_middleware(request: ModelRequest, handler):
    """
    Build model input with STATELESS design and separate SystemMessages.
    
    Original design: model sees ONLY:
    1. SystemMessage(instructions) - iteration-dependent research instructions
    2. SystemMessage(CODE BANK) - all previously fetched code
    3. Tool schemas (handled internally)
    
    NOT the conversation history. All context is embedded in the prompt.
    
    This is critical because:
    - CODE BANK already contains all fetched code
    - Instructions are iteration-aware
    - No need to replay tool call/response history
    """
    state = request.state
    
    # Build separate messages for instructions and CODE BANK
    # Using functions from core.prompts module
    instructions = build_research_instructions(state)
    code_bank = build_code_bank(state.get('fetched_files', {}))
    
    # Override: set messages to our SystemMessages, clear system_message
    # This gives us two separate SystemMessages like the original design
    request = request.override(
        system_message=None,
        messages=[
            SystemMessage(content=instructions),
            SystemMessage(content=code_bank),
        ]
    )
    return await handler(request)


@wrap_tool_call
async def code_gathering_middleware(request: ToolCallRequest, handler):
    """
    Track code-gathering tool results in state via Command.
    
    This middleware:
    1. Executes the tool via handler
    2. For code-gathering tools, returns Command with state updates
    3. Handles errors gracefully
    
    Returns Command to update fetched_files/failed_fetches state fields.
    """
    tool_name = request.tool_call.get('name', 'unknown')
    tool_call_id = request.tool_call["id"]
    
    # Execute the tool
    try:
        result = await handler(request)
    except Exception as e:
        # Handle execution errors
        error_msg = f"Tool error: {tool_name} failed - {str(e)[:ERROR_MSG_TRUNCATE_CHARS]}"
        
        if tool_name in CODE_GATHERING_TOOLS:
            # Track the failure in state
            return Command(update={
                'failed_fetches': [f"{tool_name}: {str(e)[:FAILED_FETCH_SUMMARY_CHARS]}..."],
                'messages': [ToolMessage(
                    content=error_msg,
                    tool_call_id=tool_call_id
                )]
            })
        else:
            # Non-code-gathering tool - just return error message
            return ToolMessage(content=error_msg, tool_call_id=tool_call_id)
    
    # For non-code-gathering tools, pass through the result unchanged
    if tool_name not in CODE_GATHERING_TOOLS:
        return result
    
    # Extract content from successful result
    content = result.content if isinstance(result, ToolMessage) else str(result)
    is_failure = "Error:" in content or "not found" in content.lower()
    
    if is_failure:
        # Tool executed but returned an error/not found
        return Command(update={
            'failed_fetches': [f"{tool_name}: {content[:FAILED_FETCH_SUMMARY_CHARS]}..."],
            'messages': [ToolMessage(
                content=f"Failed: {content[:FAILED_MSG_PREVIEW_CHARS]}",
                tool_call_id=tool_call_id
            )]
        })
    else:
        # Success - store full content in state, return summary to model
        return Command(update={
            'fetched_files': {tool_name: [content]},
            'messages': [ToolMessage(
                content=f"Success ({len(content):,} chars) - see CODE BANK in prompt",
                tool_call_id=tool_call_id
            )]
        })


# ============================================================================
# Research Node Constructor (LangChain v1 API)
# ============================================================================

def create_research_node(llm: BaseChatModel, tools: List[BaseTool], config: Config):
    """
    Create research node using LangChain v1 create_agent with middleware.
    
    This version uses:
    - LangChain v1 create_agent (replaces deprecated create_react_agent)
    - Custom state schema (ResearchAgentState) with domain-specific fields
    - Middleware pattern for dynamic prompts and state tracking
    - Command-returning tool middleware for state updates
    
    Design: STATELESS per model call
    - Model sees ONLY: SystemMessage(instructions) + SystemMessage(CODE BANK) + tool schemas
    - Model does NOT see: conversation history (state["messages"])
    - All context is embedded in the SystemMessages
    
    Middleware order (important):
    1. stateless_model_middleware: Builds [SystemMessage(instructions), SystemMessage(CODE BANK)]
    2. code_gathering_middleware: Tracks tool results, returns Command for state updates
    """
    # Create agent with LangChain v1 API
    # Note: Don't pre-bind tools - create_agent handles this internally
    research_agent = create_agent(
        model=llm,
        tools=tools,
        state_schema=ResearchAgentState,
        middleware=[
            stateless_model_middleware,   # Build SystemMessages (instructions + CODE BANK)
            code_gathering_middleware,    # Track tool results via Command
        ],
    )
    
    async def research(state: InvestigationState) -> InvestigationState:
        """Execute research phase using the agent."""
        logger.info(f"[{state['issue_id']}] Research phase (iteration {state['iteration']})")
        
        # Prepare agent state from investigation state
        agent_state: ResearchAgentState = {
            'messages': [],
            'fetched_files': state.get('fetched_files', {}),
            'failed_fetches': state.get('failed_fetches', []),
            'issue_id': state['issue_id'],
            'iteration': state['iteration'],
            'issue_description': state['issue_description'],
            'initial_code': state['initial_code'],
            'evaluation_feedback': state.get('evaluation_feedback', ''),
            'required_information': state.get('required_information', []),
        }
        
        # Run research agent
        # Note: Retry for transient API errors (500, 429, timeouts) is handled
        # at the LLM level via max_retries in llm_factory.py, preserving agent state
        try:
            result = await research_agent.ainvoke(
                agent_state,
                config={
                    "recursion_limit": RESEARCH_AGENT_RECURSION_LIMIT,
                    "run_name": f"research_{state['issue_id']}_iter{state['iteration']}",
                    "type": "actor",
                    "run_type": "agent",  # Langfuse recognizes this as agent span
                }
            )
        except GraphRecursionError:
            # Recursion limit hit - this is expected behavior, not an error
            # The agent made too many tool calls without completing
            logger.warning(
                f"[{state['issue_id']}] Research agent hit recursion limit "
                f"({RESEARCH_AGENT_RECURSION_LIMIT} steps) at iteration {state['iteration']}. "
                f"Proceeding with gathered code so far."
            )
            
            # Return state preserving what was gathered before this iteration
            # Note: We lose partial fetches from current iteration, but keep
            # accumulated data from previous iterations
            return {
                **state,
                'research_messages': [],
                'gathered_code': state.get('gathered_code', ''),
                'fetched_files': state.get('fetched_files', {}),
                'failed_fetches': state.get('failed_fetches', []),
                'stop_reason': f"research_recursion_limit_hit (limit={RESEARCH_AGENT_RECURSION_LIMIT})",
            }
        except Exception as e:
            logger.error(f"[{state['issue_id']}] Research agent error: {e}", exc_info=True)
            
            # Return state preserving accumulated data
            return {
                **state,
                'research_messages': [],
                'gathered_code': state.get('gathered_code', ''),
                'fetched_files': state.get('fetched_files', {}),
                'failed_fetches': state.get('failed_fetches', []),
                'stop_reason': f"research_error: {str(e)[:100]}",
            }
        
        # Extract results from agent state (middleware updates it)
        fetched_files_updated = result.get('fetched_files', {})
        failed_fetches_updated = result.get('failed_fetches', [])
        
        # Build gathered_code from state
        gathered_code = state.get('gathered_code', '')
        for tool_name, code_list in fetched_files_updated.items():
            for code in code_list:
                # Avoid duplicates
                if code not in gathered_code:
                    gathered_code += f"\n\n{code}\n"
        
        # Log summary
        logger.info(
            f"[{state['issue_id']}] Research complete. "
            f"Total gathered code: {len(gathered_code)} chars, "
            f"{len(fetched_files_updated)} tools used, "
            f"{len(failed_fetches_updated)} failed fetches"
        )
        
        return {
            **state,
            'research_messages': result['messages'],
            'gathered_code': gathered_code,
            'fetched_files': fetched_files_updated,
            'failed_fetches': failed_fetches_updated,
        }
    
    return research
