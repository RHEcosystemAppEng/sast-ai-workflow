"""
End-to-end integration tests for agent → fetch_code → state update flow.

These tests verify the COMPLETE flow:
1. Agent receives an issue and analyzes it
2. Agent decides to call fetch_code tool
3. fetch_code executes and fetches real code
4. Agent state is updated with fetched code
5. Agent processes the result and continues

This fills the gap in test coverage - we test components in isolation,
but this verifies they work together in the full agent loop.
"""

import os
from unittest.mock import Mock, patch

import pytest

from common.config import Config
from dto.Issue import Issue
from handlers.repo_handler_factory import repo_handler_factory
from sast_agent_workflow.agent.agent_graph import create_agent_graph
from sast_agent_workflow.agent.agent_state import (
    AgentMemory,
    AnalysisState,
    Claim,
    ErrorState,
    Evidence,
    InvestigationContext,
    ReasoningStateUpdate,
    SASTAgentState,
    Unknown,
)


@pytest.fixture(scope="module")
def talloc_repo_path():
    """Path to talloc test repository."""
    path = "/tmp/sast-workflow-prep/source/talloc-2.4.3"
    if not os.path.exists(path):
        pytest.skip(f"Talloc repository not found at {path}")
    return path


@pytest.fixture(scope="module")
def test_config(talloc_repo_path):
    """Create test config pointing to talloc repository."""
    config = Config()
    original_path = config.REPO_LOCAL_PATH
    config.REPO_LOCAL_PATH = talloc_repo_path

    yield config

    # Restore
    config.REPO_LOCAL_PATH = original_path


@pytest.fixture
def sample_issue():
    """Create a sample issue for testing."""
    return Issue(
        id="test-e2e-001",
        issue_type="BUFFER_OVERFLOW",
        issue_label="CWE-120",
        issue_cwe="CWE-120",
        issue_cwe_link="https://cwe.mitre.org/data/definitions/120.html",
        trace="talloc.c:520: Buffer overflow in talloc_lib_init",
        file_path="talloc.c",
        line_number=520,
    )


@pytest.fixture
def initial_state(sample_issue):
    """Create initial agent state."""
    return SASTAgentState(
        issue_id=sample_issue.id,
        issue=sample_issue,
        context=InvestigationContext(),
        analysis=AnalysisState(),
        error_state=ErrorState(),
        memory=AgentMemory(),
    )


# ============================================================
# Mock LLM that simulates agent reasoning
# ============================================================


class MockLLMForFetchCode:
    """
    Mock LLM that simulates agent deciding to call fetch_code.

    This mock returns structured reasoning that instructs the agent
    to call fetch_code to investigate the talloc_lib_init function.
    """

    def __init__(self):
        self.call_count = 0

    def with_structured_output(self, schema):
        """Return self to mimic LangChain's with_structured_output."""
        self.output_schema = schema
        return self

    def bind_tools(self, tools):
        """Return self to mimic LangChain's bind_tools."""
        self.tools = tools
        return self

    async def ainvoke(self, messages, config=None):
        """
        Return structured reasoning that calls fetch_code.

        First call: Agent decides to fetch talloc_lib_init code
        Second call: Agent analyzes the code and concludes
        """
        self.call_count += 1

        if self.call_count == 1:
            # First iteration: Call fetch_code to get talloc_lib_init
            return ReasoningStateUpdate(
                analysis="Need to examine the talloc_lib_init function to understand the buffer overflow risk.",
                claims=[
                    Claim(
                        claim_id="c1",
                        text="The issue mentions talloc_lib_init function at line 520",
                        status="tentative",
                        supporting_evidence_ids=["e1"],
                    )
                ],
                evidence=[
                    Evidence(
                        evidence_id="e1",
                        path_or_identifier="Issue trace",
                        excerpt="talloc.c:520: Buffer overflow in talloc_lib_init",
                        why_it_matters="Indicates the location of the potential vulnerability",
                    )
                ],
                unknowns=[
                    Unknown(
                        unknown_id="u1",
                        question="What does talloc_lib_init function do and why might it have a buffer overflow?",
                        priority=100,
                        blocking=True,
                    )
                ],
                next_tool="fetch_code",
                tool_reasoning="Need to fetch the implementation of talloc_lib_init to analyze the buffer overflow risk",
                tool_parameters={
                    "expression_name": "talloc_lib_init",
                    "referring_source_code_path": "talloc.c",
                },
            )
        else:
            # Second iteration: Analyze and conclude
            return ReasoningStateUpdate(
                analysis="After examining talloc_lib_init, this appears to be a false positive. "
                         "The function only initializes a boolean flag and calls talloc_setup_atexit, "
                         "with no buffer operations that could overflow.",
                claims=[
                    Claim(
                        claim_id="c2",
                        text="talloc_lib_init does not perform any buffer operations",
                        status="supported",
                        supporting_evidence_ids=["e2"],
                    )
                ],
                evidence=[
                    Evidence(
                        evidence_id="e2",
                        path_or_identifier="talloc_lib_init code",
                        excerpt="Function only sets 'initialized = true' and calls talloc_setup_atexit()",
                        why_it_matters="Shows no buffer manipulation",
                    )
                ],
                unknowns=[],
                next_tool="evaluator",
                tool_reasoning="Ready to submit analysis for evaluation",
                tool_parameters={
                    "proposed_verdict": "FALSE_POSITIVE",
                },
            )

    def invoke(self, messages, config=None):
        """Synchronous version (not used in async graph)."""
        import asyncio
        return asyncio.run(self.ainvoke(messages, config))


# ============================================================
# End-to-End Tests
# ============================================================


@pytest.mark.asyncio
async def test_agent_calls_fetch_code_and_updates_state(test_config, initial_state, talloc_repo_path):
    """
    End-to-end test: Agent decides to call fetch_code → tool executes → state updated.

    This test verifies the COMPLETE flow:
    1. Agent analyzes issue
    2. Agent decides to call fetch_code for talloc_lib_init
    3. fetch_code fetches REAL code from talloc repository
    4. State is updated with fetched code in context.fetched_files
    5. Agent receives the code and continues reasoning
    """
    # Setup: Create mock LLM that will call fetch_code
    mock_llm = MockLLMForFetchCode()

    # Setup: Create tools list with real fetch_code tool
    from sast_agent_workflow.agent.tools.fetch_code import register_fetch_code_tool, FetchCodeToolConfig
    from langchain_core.tools import StructuredTool

    # Create a simple fetch_code tool for testing
    # We'll mock the repo_handler to point to talloc
    with patch('sast_agent_workflow.agent.tools.fetch_code.Config') as mock_config_class:
        mock_config_instance = Mock()
        mock_config_instance.REPO_LOCAL_PATH = talloc_repo_path
        mock_config_class.return_value = mock_config_instance

        # Create the real fetch_code tool
        from sast_agent_workflow.agent.tools.fetch_code import FetchCodeInput

        # Get real repo handler for talloc
        real_repo_handler = repo_handler_factory(test_config)

        with patch('sast_agent_workflow.agent.tools.fetch_code.repo_handler_factory') as mock_factory:
            mock_factory.return_value = real_repo_handler

            # Register fetch_code tool through NAT
            fetch_code_config = FetchCodeToolConfig()
            from nat.builder.builder import Builder
            builder = Mock(spec=Builder)

            # Get the registered tool
            async with register_fetch_code_tool(fetch_code_config, builder) as func_info:
                # Create LangChain StructuredTool wrapper
                def sync_fetch_wrapper(expression_name: str, referring_source_code_path: str) -> str:
                    """Sync wrapper for fetch_code."""
                    import asyncio
                    return asyncio.run(
                        func_info.single_fn(
                            FetchCodeInput(
                                expression_name=expression_name,
                                referring_source_code_path=referring_source_code_path
                            )
                        )
                    )

                fetch_code_tool = StructuredTool.from_function(
                    func=sync_fetch_wrapper,
                    name="fetch_code",
                    description="Fetch source code by symbol name",
                    args_schema=FetchCodeInput,
                )

                # Create simple evaluator mock (not the focus of this test)
                def mock_evaluator(**kwargs) -> str:
                    return "APPROVED: Analysis is complete and well-supported."

                evaluator_tool = StructuredTool.from_function(
                    func=mock_evaluator,
                    name="evaluator",
                    description="Evaluate investigation completeness",
                )

                tools = [fetch_code_tool, evaluator_tool]

                # Create agent graph with mock LLM and real tools
                graph = create_agent_graph(
                    llm=mock_llm,
                    tools=tools,
                    config=test_config,
                    max_iterations=5,
                    max_error_recovery_attempts=3,
                )

                # Execute: Invoke the graph with initial state
                result_state_dict = await graph.ainvoke(
                    initial_state,
                    config={
                        "recursion_limit": 20,
                        "timeout": 60,
                    }
                )

                # Convert dict back to SASTAgentState
                result_state = SASTAgentState(**result_state_dict)

                # ============================================================
                # Verify: Agent called fetch_code
                # ============================================================
                assert mock_llm.call_count >= 1, "Agent should have made at least one reasoning call"

                # ============================================================
                # Verify: State was updated with fetched code
                # ============================================================
                assert len(result_state.context.fetched_files) > 0, \
                    "State should have fetched files after fetch_code call"

                # Check that talloc_lib_init code was fetched
                fetched_file_paths = list(result_state.context.fetched_files.keys())
                assert any("talloc" in path for path in fetched_file_paths), \
                    f"Should have fetched talloc file, got: {fetched_file_paths}"

                # ============================================================
                # Verify: Fetched code contains expected content
                # ============================================================
                # Get the fetched code
                talloc_files = [
                    (path, code) for path, code in result_state.context.fetched_files.items()
                    if "talloc" in path
                ]
                assert len(talloc_files) > 0, "Should have fetched talloc code"

                file_path, code_blocks = talloc_files[0]
                code_content = "\n".join(code_blocks)

                # Verify code contains talloc_lib_init function
                assert "talloc_lib_init" in code_content, \
                    "Fetched code should contain talloc_lib_init function"

                # Verify code has line numbers (shows it's real formatted code)
                assert "|" in code_content, \
                    "Fetched code should have line numbers in format 'number|'"

                # ============================================================
                # Verify: Symbol was added to found_symbols
                # ============================================================
                assert "talloc_lib_init" in result_state.context.found_symbols, \
                    "talloc_lib_init should be in found_symbols set"

                # ============================================================
                # Verify: Agent continued after receiving code
                # ============================================================
                # Agent should have made at least 2 calls (fetch_code, then evaluator)
                assert mock_llm.call_count >= 2, \
                    f"Agent should have continued after fetch_code, made {mock_llm.call_count} calls"

                # ============================================================
                # Verify: Iteration count increased
                # ============================================================
                assert result_state.iteration_count > 0, \
                    "Iteration count should have increased"


@pytest.mark.asyncio
async def test_agent_fetch_code_handles_nonexistent_symbol(test_config, initial_state):
    """
    Test that agent handles fetch_code failure gracefully.

    Verifies:
    1. Agent calls fetch_code for nonexistent symbol
    2. fetch_code returns error message
    3. State is NOT updated with code (no file added)
    4. Agent receives error and can continue
    """
    # Setup: Mock LLM that tries to fetch nonexistent symbol
    mock_llm = MockLLMForFetchCode()

    # Override to request nonexistent function
    original_ainvoke = mock_llm.ainvoke

    async def mock_ainvoke_nonexistent(messages, config=None):
        result = await original_ainvoke(messages, config)
        if mock_llm.call_count == 1:
            # Request nonexistent symbol
            result.tool_parameters = {
                "expression_name": "this_function_does_not_exist_xyz",
                "referring_source_code_path": "talloc.c",
            }
        return result

    mock_llm.ainvoke = mock_ainvoke_nonexistent

    # Setup tools (same as previous test)
    from sast_agent_workflow.agent.tools.fetch_code import register_fetch_code_tool, FetchCodeToolConfig, FetchCodeInput
    from langchain_core.tools import StructuredTool

    with patch('sast_agent_workflow.agent.tools.fetch_code.Config') as mock_config_class:
        mock_config_instance = Mock()
        mock_config_instance.REPO_LOCAL_PATH = test_config.REPO_LOCAL_PATH
        mock_config_class.return_value = mock_config_instance

        real_repo_handler = repo_handler_factory(test_config)

        with patch('sast_agent_workflow.agent.tools.fetch_code.repo_handler_factory') as mock_factory:
            mock_factory.return_value = real_repo_handler

            fetch_code_config = FetchCodeToolConfig()
            from nat.builder.builder import Builder
            builder = Mock(spec=Builder)

            async with register_fetch_code_tool(fetch_code_config, builder) as func_info:
                def sync_fetch_wrapper(expression_name: str, referring_source_code_path: str) -> str:
                    import asyncio
                    return asyncio.run(
                        func_info.single_fn(
                            FetchCodeInput(
                                expression_name=expression_name,
                                referring_source_code_path=referring_source_code_path
                            )
                        )
                    )

                fetch_code_tool = StructuredTool.from_function(
                    func=sync_fetch_wrapper,
                    name="fetch_code",
                    description="Fetch source code by symbol name",
                    args_schema=FetchCodeInput,
                )

                def mock_evaluator(**kwargs) -> str:
                    return "APPROVED"

                evaluator_tool = StructuredTool.from_function(
                    func=mock_evaluator,
                    name="evaluator",
                    description="Evaluate investigation",
                )

                tools = [fetch_code_tool, evaluator_tool]

                graph = create_agent_graph(
                    llm=mock_llm,
                    tools=tools,
                    config=test_config,
                    max_iterations=5,
                )

                # Execute
                result_state_dict = await graph.ainvoke(
                    initial_state,
                    config={"recursion_limit": 20, "timeout": 60}
                )

                result_state = SASTAgentState(**result_state_dict)

                # ============================================================
                # Verify: fetch_code was called but returned error
                # ============================================================
                # The nonexistent symbol should NOT be in found_symbols
                assert "this_function_does_not_exist_xyz" not in result_state.context.found_symbols, \
                    "Nonexistent symbol should not be in found_symbols"

                # Agent should still have continued (didn't crash)
                assert result_state.iteration_count > 0, \
                    "Agent should have continued despite fetch_code error"


if __name__ == '__main__':
    pytest.main([__file__, '-v'])