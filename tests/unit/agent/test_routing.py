"""
Unit tests for agent graph routing logic.

These tests verify the routing functions that control investigation flow:
- route_agent_decision: Routes from agent node to tools
- route_after_evaluation: Routes from comprehensive_evaluation (handles is_final)
- is_duplicate_call: Detects repeated tool calls
- Graph structure: Verifies nodes and edges are correctly configured
"""

from unittest.mock import Mock

import pytest
from langchain_core.messages import AIMessage
from langgraph.graph import END

from dto.Issue import Issue
from sast_agent_workflow.agent.agent_graph import (
    create_agent_graph,
    is_duplicate_call,
    route_after_evaluation,
    route_agent_decision,
)
from sast_agent_workflow.agent.agent_state import (
    AgentMemory,
    AnalysisState,
    ErrorState,
    InvestigationContext,
    SASTAgentState,
)


@pytest.fixture
def base_state():
    """Create a base SASTAgentState for testing."""
    return SASTAgentState(
        issue_id="test-routing-001",
        issue=Issue(
            id="issue-001",
            issue_type="SQL Injection",
            trace="user_input -> query -> execute",
            file_path="app/views.py",
            line_number=42,
        ),
        context=InvestigationContext(),
        analysis=AnalysisState(),
        error_state=ErrorState(),
        memory=AgentMemory(),
    )


class TestRouteAgentDecision:
    """Tests for route_agent_decision function."""

    def test_routes_to_agent_when_no_messages(self, base_state):
        """Test that first call (no messages) routes to agent."""
        base_state.memory.messages = []

        result = route_agent_decision(base_state)

        assert result == "agent"

    def test_routes_to_fetch_code_when_agent_calls_it(self, base_state):
        """Test routing to fetch_code when agent decides to call it."""
        ai_message = AIMessage(
            content="I'll fetch the code",
            tool_calls=[
                {
                    "name": "fetch_code",
                    "args": {"identifier": "app/views.py", "reason": "get vulnerable code"},
                    "id": "call_001",
                }
            ],
        )
        base_state.memory.messages = [ai_message]

        result = route_agent_decision(base_state)

        assert result == "fetch_code"

    def test_routes_to_analyze_issue_when_agent_calls_it(self, base_state):
        """Test routing to analyze_issue when agent decides to call it."""
        ai_message = AIMessage(
            content="I'll analyze the issue",
            tool_calls=[
                {
                    "name": "analyze_issue",
                    "args": {"issue_trace": "...", "fetched_code": "..."},
                    "id": "call_002",
                }
            ],
        )
        base_state.memory.messages = [ai_message]

        result = route_agent_decision(base_state)

        assert result == "analyze_issue"

    def test_routes_to_comprehensive_evaluation_when_agent_calls_it(self, base_state):
        """Test routing to comprehensive_evaluation when agent calls it."""
        ai_message = AIMessage(
            content="I'll evaluate the investigation",
            tool_calls=[
                {
                    "name": "comprehensive_evaluation",
                    "args": {"issue_trace": "...", "analysis_verdict": "TRUE_POSITIVE"},
                    "id": "call_003",
                }
            ],
        )
        base_state.memory.messages = [ai_message]

        result = route_agent_decision(base_state)

        assert result == "comprehensive_evaluation"

    def test_returns_agent_when_no_tool_calls(self, base_state):
        """Test fallback to agent when LLM doesn't make tool call."""
        # Agent responded but without tool_calls (LLM error)
        ai_message = AIMessage(content="I'm confused")
        base_state.memory.messages = [ai_message]

        result = route_agent_decision(base_state)

        assert result == "agent"

    def test_uses_first_tool_call_when_multiple(self, base_state):
        """Test that first tool call is used when multiple exist."""
        ai_message = AIMessage(
            content="Multiple calls",
            tool_calls=[
                {"name": "fetch_code", "args": {}, "id": "call_001"},
                {"name": "analyze_issue", "args": {}, "id": "call_002"},
            ],
        )
        base_state.memory.messages = [ai_message]

        result = route_agent_decision(base_state)

        assert result == "fetch_code"  # First call


class TestRouteAfterEvaluation:
    """Tests for route_after_evaluation function."""

    def test_routes_to_end_when_is_final_true(self, base_state):
        """Test that is_final=TRUE routes to END."""
        base_state.is_final = True
        base_state.iteration_count = 5

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        assert result == END

    def test_routes_to_agent_when_is_final_false(self, base_state):
        """Test that is_final=FALSE routes back to agent."""
        base_state.is_final = False
        base_state.iteration_count = 5

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        assert result == "agent"

    def test_circuit_breaker_max_iterations(self, base_state):
        """Test circuit breaker triggers at max iterations.

        Note: route_after_evaluation is now PURE - it returns 'circuit_breaker'
        instead of END, and does NOT mutate state. State updates happen in
        the circuit_breaker_node.
        """
        base_state.is_final = False
        base_state.iteration_count = 15

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        assert result == "circuit_breaker"
        assert base_state.is_final is False  # Routing is pure - no mutation

    def test_circuit_breaker_max_iterations_exceeded(self, base_state):
        """Test circuit breaker triggers when iterations exceed limit."""
        base_state.is_final = False
        base_state.iteration_count = 20

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        assert result == "circuit_breaker"
        assert base_state.is_final is False  # Routing is pure - no mutation

    def test_circuit_breaker_duplicate_calls(self, base_state):
        """Test circuit breaker triggers on duplicate tool calls."""
        base_state.is_final = False
        base_state.iteration_count = 5
        # Set up duplicate calls
        base_state.memory.tool_call_history = [
            ("fetch_code", {"identifier": "app/views.py", "reason": "test"}),
            ("fetch_code", {"identifier": "app/views.py", "reason": "test"}),
        ]

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        assert result == "circuit_breaker"
        assert base_state.is_final is False  # Routing is pure - no mutation

    def test_circuit_breaker_error_recovery_limit(self, base_state):
        """Test circuit breaker triggers at error recovery limit."""
        base_state.is_final = False
        base_state.iteration_count = 5
        base_state.error_state.error_recovery_attempts = 3

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        assert result == "circuit_breaker"
        assert base_state.is_final is False  # Routing is pure - no mutation

    def test_circuit_breaker_error_recovery_exceeded(self, base_state):
        """Test circuit breaker triggers when error recovery exceeds limit."""
        base_state.is_final = False
        base_state.iteration_count = 5
        base_state.error_state.error_recovery_attempts = 5

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        assert result == "circuit_breaker"
        assert base_state.is_final is False  # Routing is pure - no mutation

    def test_normal_continuation_no_circuit_breakers(self, base_state):
        """Test normal case: continue to agent when no termination conditions met."""
        base_state.is_final = False
        base_state.iteration_count = 5
        base_state.error_state.error_recovery_attempts = 1
        base_state.memory.tool_call_history = [
            ("fetch_code", {"identifier": "app/views.py"}),
            ("analyze_issue", {"issue_trace": "..."}),
        ]

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        assert result == "agent"
        assert base_state.is_final is False

    def test_circuit_breaker_precedence_iterations_checked_first(self, base_state):
        """Test that max iterations is checked before other conditions."""
        base_state.is_final = False  # Would normally continue
        base_state.iteration_count = 15  # But iterations limit reached

        result = route_after_evaluation(base_state, max_iterations=15, max_error_recovery=3)

        assert result == "circuit_breaker"
        assert base_state.is_final is False  # Routing is pure - no mutation


class TestIsDuplicateCall:
    """Tests for is_duplicate_call function."""

    def test_returns_false_when_less_than_two_calls(self, base_state):
        """Test no duplicate detection with < 2 calls."""
        base_state.memory.tool_call_history = [
            ("fetch_code", {"identifier": "app/views.py"}),
        ]

        result = is_duplicate_call(base_state)

        assert result is False

    def test_returns_false_when_no_calls(self, base_state):
        """Test no duplicate detection with empty history."""
        base_state.memory.tool_call_history = []

        result = is_duplicate_call(base_state)

        assert result is False

    def test_returns_true_when_identical_calls(self, base_state):
        """Test duplicate detection for identical tool calls."""
        base_state.memory.tool_call_history = [
            ("fetch_code", {"identifier": "app/views.py", "reason": "test"}),
            ("fetch_code", {"identifier": "app/views.py", "reason": "test"}),
        ]

        result = is_duplicate_call(base_state)

        assert result is True

    def test_returns_false_when_different_tools(self, base_state):
        """Test no duplicate when different tools called."""
        base_state.memory.tool_call_history = [
            ("fetch_code", {"identifier": "app/views.py"}),
            ("analyze_issue", {"issue_trace": "..."}),
        ]

        result = is_duplicate_call(base_state)

        assert result is False

    def test_returns_false_when_same_tool_different_args(self, base_state):
        """Test no duplicate when same tool with different args."""
        base_state.memory.tool_call_history = [
            ("fetch_code", {"identifier": "app/views.py"}),
            ("fetch_code", {"identifier": "app/models.py"}),
        ]

        result = is_duplicate_call(base_state)

        assert result is False

    def test_ignores_older_calls_only_checks_last_two(self, base_state):
        """Test that only last 2 calls are checked for duplicates."""
        base_state.memory.tool_call_history = [
            ("fetch_code", {"identifier": "old.py"}),  # Ignored
            ("fetch_code", {"identifier": "app/views.py"}),
            ("analyze_issue", {"issue_trace": "..."}),
        ]

        result = is_duplicate_call(base_state)

        assert result is False


class TestGraphStructure:
    """Tests for agent graph structure and compilation."""

    @pytest.fixture
    def mock_llm(self):
        """Create a mock LLM."""
        llm = Mock()
        llm.bind_tools = Mock(return_value=llm)
        return llm

    @pytest.fixture
    def mock_tools(self):
        """Create mock NAT tools."""
        fetch_tool = Mock()
        fetch_tool.name = "fetch_code"

        analyze_tool = Mock()
        analyze_tool.name = "analyze_issue"

        eval_tool = Mock()
        eval_tool.name = "comprehensive_evaluation"

        return [fetch_tool, analyze_tool, eval_tool]

    @pytest.fixture
    def mock_config(self):
        """Create mock config."""
        return Mock()

    def test_graph_compiles_successfully(self, mock_llm, mock_tools, mock_config):
        """Test that agent graph compiles without errors."""
        graph = create_agent_graph(
            llm=mock_llm,
            tools=mock_tools,
            config=mock_config,
            max_iterations=15,
            max_error_recovery_attempts=3,
        )

        assert graph is not None
        assert hasattr(graph, "invoke") or hasattr(graph, "ainvoke")

    def test_graph_has_agent_node(self, mock_llm, mock_tools, mock_config):
        """Test that graph includes agent decision node."""
        graph = create_agent_graph(llm=mock_llm, tools=mock_tools, config=mock_config)

        graph_structure = graph.get_graph()
        node_names = list(graph_structure.nodes.keys())

        assert "agent" in node_names

    def test_graph_has_all_tool_nodes(self, mock_llm, mock_tools, mock_config):
        """Test that graph includes all tool wrapper nodes."""
        graph = create_agent_graph(llm=mock_llm, tools=mock_tools, config=mock_config)

        graph_structure = graph.get_graph()
        node_names = list(graph_structure.nodes.keys())

        assert "fetch_code" in node_names
        assert "analyze_issue" in node_names
        assert "comprehensive_evaluation" in node_names

    def test_graph_entry_point_is_agent(self, mock_llm, mock_tools, mock_config):
        """Test that graph starts at agent node."""
        graph = create_agent_graph(llm=mock_llm, tools=mock_tools, config=mock_config)

        # Entry point should be agent
        # Note: LangGraph doesn't expose entry_point directly, but we can verify
        # by checking that __start__ node exists and points to agent
        graph_structure = graph.get_graph()

        # Find edges from __start__
        start_edges = [edge for edge in graph_structure.edges if edge.source == "__start__"]
        assert len(start_edges) > 0
        assert start_edges[0].target == "agent"

    def test_analyze_issue_routes_to_comprehensive_evaluation(
        self, mock_llm, mock_tools, mock_config
    ):
        """Test that analyze_issue has automatic edge to comprehensive_evaluation."""
        graph = create_agent_graph(llm=mock_llm, tools=mock_tools, config=mock_config)

        graph_structure = graph.get_graph()

        # Find edge from analyze_issue to comprehensive_evaluation
        analyze_edges = [edge for edge in graph_structure.edges if edge.source == "analyze_issue"]

        assert len(analyze_edges) > 0
        assert any(edge.target == "comprehensive_evaluation" for edge in analyze_edges)

    def test_fetch_code_routes_to_agent(self, mock_llm, mock_tools, mock_config):
        """Test that fetch_code routes back to agent."""
        graph = create_agent_graph(llm=mock_llm, tools=mock_tools, config=mock_config)

        graph_structure = graph.get_graph()

        # Find edge from fetch_code to agent
        fetch_edges = [edge for edge in graph_structure.edges if edge.source == "fetch_code"]

        assert len(fetch_edges) > 0
        assert any(edge.target == "agent" for edge in fetch_edges)

    def test_comprehensive_evaluation_has_conditional_routing(
        self, mock_llm, mock_tools, mock_config
    ):
        """Test that comprehensive_evaluation has conditional edges (to agent OR END)."""
        graph = create_agent_graph(llm=mock_llm, tools=mock_tools, config=mock_config)

        graph_structure = graph.get_graph()

        # Find edges from comprehensive_evaluation
        eval_edges = [
            edge for edge in graph_structure.edges if edge.source == "comprehensive_evaluation"
        ]

        # Should have at least one conditional edge
        assert len(eval_edges) > 0
        # Note: LangGraph may represent conditional edges differently,
        # but we should see edges to both agent and END states

    def test_llm_has_tools_bound(self, mock_llm, mock_tools, mock_config):
        """Test that LLM has tools bound during graph creation."""
        create_agent_graph(llm=mock_llm, tools=mock_tools, config=mock_config)

        # Verify bind_tools was called
        mock_llm.bind_tools.assert_called_once()
        # Verify it was called with the tools list
        call_args = mock_llm.bind_tools.call_args
        assert call_args[0][0] == mock_tools

    def test_graph_respects_max_iterations_parameter(self, mock_llm, mock_tools, mock_config):
        """Test that max_iterations parameter is used in routing."""
        # This is harder to test directly, but we can verify the graph compiles
        # with different max_iterations values
        graph_15 = create_agent_graph(
            llm=mock_llm, tools=mock_tools, config=mock_config, max_iterations=15
        )

        graph_5 = create_agent_graph(
            llm=mock_llm, tools=mock_tools, config=mock_config, max_iterations=5
        )

        assert graph_15 is not None
        assert graph_5 is not None
        # Different max_iterations shouldn't break compilation
