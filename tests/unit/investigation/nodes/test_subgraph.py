"""
Tests for investigation subgraph builder in investigation/nodes/subgraph.py.

Covers: graph compilation, node presence, edge wiring, entry point,
conditional routing from evaluate, and iteration increment behaviour.
"""

from unittest.mock import MagicMock, Mock, patch

import pytest

_MOD = "sast_agent_workflow.nodes.sub_agents.investigation.nodes.subgraph"

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def mock_llm():
    """Mock LangChain chat model."""
    return MagicMock()


@pytest.fixture
def mock_tools():
    """Mock list of LangChain tools."""
    return [MagicMock(), MagicMock()]


@pytest.fixture
def mock_config():
    """Mock configuration object."""
    config = Mock()
    config.MAX_ANALYSIS_ITERATIONS = 4
    return config


@pytest.fixture
def compiled_graph(mock_llm, mock_tools, mock_config):
    """Build and return the compiled investigation subgraph."""
    with (
        patch(f"{_MOD}.create_research_node", return_value=MagicMock()),
        patch(f"{_MOD}.create_analysis_node", return_value=MagicMock()),
        patch(f"{_MOD}.create_evaluation_node", return_value=MagicMock()),
        patch(f"{_MOD}.create_circuit_breaker_node", return_value=MagicMock()),
    ):
        from sast_agent_workflow.nodes.sub_agents.investigation.nodes.subgraph import (
            build_investigation_subgraph,
        )

        return build_investigation_subgraph(mock_llm, mock_tools, mock_config)


# ---------------------------------------------------------------------------
# Graph compilation
# ---------------------------------------------------------------------------


class TestBuildInvestigationSubgraph:
    """Tests for build_investigation_subgraph."""

    def test__compiles_without_error(self, compiled_graph):
        """Subgraph should compile successfully and return a runnable."""
        assert compiled_graph is not None

    def test__contains_expected_nodes(self, compiled_graph):
        """Compiled graph should contain all expected node names."""
        drawable = compiled_graph.get_graph()
        node_names = set(drawable.nodes.keys())

        expected = {"research", "analyze", "evaluate", "circuit_breaker", "increment"}
        assert expected.issubset(node_names), f"Missing nodes: {expected - node_names}"

    def test__entry_point_is_research(self, compiled_graph):
        """Graph entry point should be the research node."""
        drawable = compiled_graph.get_graph()
        # __start__ node should have an edge to research
        start_edges = [e.target for e in drawable.edges if e.source == "__start__"]
        assert "research" in start_edges

    def test__research_edges_to_analyze(self, compiled_graph):
        """research node should have an edge to analyze."""
        drawable = compiled_graph.get_graph()
        research_targets = [e.target for e in drawable.edges if e.source == "research"]
        assert "analyze" in research_targets

    def test__analyze_edges_to_evaluate(self, compiled_graph):
        """analyze node should have an edge to evaluate."""
        drawable = compiled_graph.get_graph()
        analyze_targets = [e.target for e in drawable.edges if e.source == "analyze"]
        assert "evaluate" in analyze_targets

    def test__circuit_breaker_edges_to_end(self, compiled_graph):
        """circuit_breaker should have an edge to __end__."""
        drawable = compiled_graph.get_graph()
        cb_targets = [e.target for e in drawable.edges if e.source == "circuit_breaker"]
        assert "__end__" in cb_targets

    def test__evaluate_has_conditional_edges(self, compiled_graph):
        """evaluate node should have conditional edges (not just a single target)."""
        drawable = compiled_graph.get_graph()
        evaluate_targets = [e.target for e in drawable.edges if e.source == "evaluate"]
        # Should reach increment (for research/reanalyze), circuit_breaker, and __end__
        assert "increment" in evaluate_targets
        assert "circuit_breaker" in evaluate_targets
        assert "__end__" in evaluate_targets

    def test__increment_has_conditional_edges(self, compiled_graph):
        """increment node should conditionally route to research or analyze."""
        drawable = compiled_graph.get_graph()
        increment_targets = [e.target for e in drawable.edges if e.source == "increment"]
        assert "research" in increment_targets
        assert "analyze" in increment_targets


# ---------------------------------------------------------------------------
# Increment iteration logic
# ---------------------------------------------------------------------------


class TestIncrementIteration:
    """Tests for the increment_iteration inline function."""

    def test__increments_iteration_counter(self, mock_llm, mock_tools, mock_config):
        """Increment node should bump iteration by 1."""
        with (
            patch(f"{_MOD}.create_research_node", return_value=MagicMock()),
            patch(f"{_MOD}.create_analysis_node", return_value=MagicMock()),
            patch(f"{_MOD}.create_evaluation_node", return_value=MagicMock()),
            patch(f"{_MOD}.create_circuit_breaker_node", return_value=MagicMock()),
            patch(f"{_MOD}.StateGraph") as mock_state_graph,
        ):
            # Capture the increment function passed to add_node
            added_nodes = {}

            def capture_add_node(name, fn):
                added_nodes[name] = fn

            mock_graph = MagicMock()
            mock_graph.add_node = capture_add_node
            mock_graph.compile.return_value = MagicMock()
            mock_state_graph.return_value = mock_graph

            from sast_agent_workflow.nodes.sub_agents.investigation.nodes.subgraph import (
                build_investigation_subgraph,
            )

            build_investigation_subgraph(mock_llm, mock_tools, mock_config)

            assert "increment" in added_nodes
            increment_fn = added_nodes["increment"]

            state = {"iteration": 2, "other_field": "value"}
            result = increment_fn(state)

            assert result["iteration"] == 3
            assert result["other_field"] == "value"


# ---------------------------------------------------------------------------
# Route after increment
# ---------------------------------------------------------------------------


class TestRouteAfterIncrement:
    """Tests for the route_after_increment inline function."""

    def test__routes_to_research_by_default(self, mock_llm, mock_tools, mock_config):
        """When needs_reanalysis is False, should route to research."""
        route_fn = self._get_route_fn(mock_llm, mock_tools, mock_config)

        assert route_fn({"needs_reanalysis": False}) == "research"

    def test__routes_to_analyze_when_reanalysis_needed(self, mock_llm, mock_tools, mock_config):
        """When needs_reanalysis is True, should route to analyze."""
        route_fn = self._get_route_fn(mock_llm, mock_tools, mock_config)

        assert route_fn({"needs_reanalysis": True}) == "analyze"

    def test__routes_to_research_when_flag_missing(self, mock_llm, mock_tools, mock_config):
        """When needs_reanalysis is absent from state, should default to research."""
        route_fn = self._get_route_fn(mock_llm, mock_tools, mock_config)

        assert route_fn({}) == "research"

    @staticmethod
    def _get_route_fn(mock_llm, mock_tools, mock_config):
        """Extract the route_after_increment function from graph building."""
        with (
            patch(f"{_MOD}.create_research_node", return_value=MagicMock()),
            patch(f"{_MOD}.create_analysis_node", return_value=MagicMock()),
            patch(f"{_MOD}.create_evaluation_node", return_value=MagicMock()),
            patch(f"{_MOD}.create_circuit_breaker_node", return_value=MagicMock()),
            patch(f"{_MOD}.StateGraph") as mock_state_graph,
        ):
            conditional_edges = []

            def capture_conditional_edges(source, fn, path_map):
                conditional_edges.append((source, fn, path_map))

            mock_graph = MagicMock()
            mock_graph.add_conditional_edges = capture_conditional_edges
            mock_graph.compile.return_value = MagicMock()
            mock_state_graph.return_value = mock_graph

            from sast_agent_workflow.nodes.sub_agents.investigation.nodes.subgraph import (
                build_investigation_subgraph,
            )

            build_investigation_subgraph(mock_llm, mock_tools, mock_config)

            # The second add_conditional_edges call is from "increment"
            for source, fn, _ in conditional_edges:
                if source == "increment":
                    return fn

            raise AssertionError("route_after_increment not found in conditional edges")
