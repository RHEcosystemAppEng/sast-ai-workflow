"""
NAT tests for fetch_code tool.

Tests the actual _fetch_code() function through NAT framework registration.
This completes the test coverage gap from unit/integration tests.
"""

import unittest
from unittest.mock import Mock, patch

from nat.builder.builder import Builder
from sast_agent_workflow.agent.tools.fetch_code import (
    FetchCodeInput,
    FetchCodeToolConfig,
    register_fetch_code_tool,
)


class TestFetchCodeNAT(unittest.IsolatedAsyncioTestCase):
    """NAT-based tests for fetch_code tool."""

    def setUp(self):
        """Set up test fixtures."""
        self.config = FetchCodeToolConfig()
        self.builder = Mock(spec=Builder)

    @patch('sast_agent_workflow.agent.tools.fetch_code.repo_handler_factory')
    @patch('sast_agent_workflow.agent.tools.fetch_code.Config')
    async def test_fetch_code_returns_formatted_result_for_valid_symbol(
        self, mock_config_class, mock_repo_factory
    ):
        """Test that _fetch_code returns properly formatted result for valid symbol."""
        # Setup mocks
        mock_config = Mock()
        mock_config.REPO_LOCAL_PATH = "/tmp/test/repo"
        mock_config_class.return_value = mock_config

        mock_repo_handler = Mock()
        mock_repo_factory.return_value = mock_repo_handler

        # Mock successful symbol fetch
        sample_code = """code of src/utils/sanitize.c file:
15| /**
16|  * Sanitize user input by removing special characters
17|  */
18| char* sanitize_input(const char* input) {
19|     static char buffer[256];
20|     // ... sanitization logic
21|     return buffer;
22| }"""

        mock_repo_handler.extract_missing_functions_or_macros.return_value = (
            sample_code,  # missing_code
            {"sanitize_input"},  # new_symbols
        )

        async with register_fetch_code_tool(self.config, self.builder) as func_info:
            input_data = FetchCodeInput(
                expression_name="sanitize_input",
                referring_source_code_path="src/auth/login.c"
            )
            result = await func_info.single_fn(input_data)

        # Verify result format
        self.assertIsInstance(result, str, "Result should be a string")
        self.assertIn("=== Fetched Code: sanitize_input ===", result, "Should have header")
        self.assertIn("File Path: src/utils/sanitize.c", result, "Should include file path")
        self.assertIn("Line Range: 15-22", result, "Should include line range")
        self.assertIn("Type: Symbol", result, "Should include type")
        self.assertIn("Referring File: src/auth/login.c", result, "Should include referring file")
        self.assertIn("char* sanitize_input", result, "Should include actual code")
        self.assertIn("=== End of sanitize_input ===", result, "Should have footer")

        # Verify repo_handler was called correctly
        mock_repo_handler.extract_missing_functions_or_macros.assert_called_once()
        call_args = mock_repo_handler.extract_missing_functions_or_macros.call_args
        instructions = call_args[0][0]
        self.assertEqual(len(instructions), 1, "Should call with one instruction")
        self.assertEqual(instructions[0].expression_name, "sanitize_input")
        self.assertEqual(instructions[0].referring_source_code_path, "src/auth/login.c")

    @patch('sast_agent_workflow.agent.tools.fetch_code.repo_handler_factory')
    @patch('sast_agent_workflow.agent.tools.fetch_code.Config')
    async def test_fetch_code_returns_error_message_for_nonexistent_symbol(
        self, mock_config_class, mock_repo_factory
    ):
        """Test that _fetch_code returns error message when symbol not found."""
        mock_config = Mock()
        mock_config.REPO_LOCAL_PATH = "/tmp/test/repo"
        mock_config_class.return_value = mock_config

        mock_repo_handler = Mock()
        mock_repo_factory.return_value = mock_repo_handler

        # Mock symbol not found scenario
        mock_repo_handler.extract_missing_functions_or_macros.return_value = (
            "",
            set(),
        )

        async with register_fetch_code_tool(self.config, self.builder) as func_info:
            input_data = FetchCodeInput(
                expression_name="nonexistent_function",
                referring_source_code_path="src/test.c"
            )
            result = await func_info.single_fn(input_data)

        self.assertIsInstance(result, str, "Result should be a string")
        self.assertIn("Error:", result, "Should contain error message")
        self.assertIn("nonexistent_function", result, "Should mention the symbol name")
        self.assertIn("not found", result, "Should indicate symbol not found")
        self.assertIn("src/test.c", result, "Should mention referring file")
        self.assertIn("search_codebase", result, "Should suggest alternative tool")

    @patch('sast_agent_workflow.agent.tools.fetch_code.repo_handler_factory')
    @patch('sast_agent_workflow.agent.tools.fetch_code.Config')
    async def test_fetch_code_handles_repo_handler_exception(
        self, mock_config_class, mock_repo_factory
    ):
        """Test that _fetch_code handles exceptions from repo_handler gracefully."""
        mock_config = Mock()
        mock_config.REPO_LOCAL_PATH = "/tmp/test/repo"
        mock_config_class.return_value = mock_config

        mock_repo_handler = Mock()
        mock_repo_factory.return_value = mock_repo_handler

        # Mock repo_handler raising exception
        mock_repo_handler.extract_missing_functions_or_macros.side_effect = Exception(
            "Repository access failed"
        )

        async with register_fetch_code_tool(self.config, self.builder) as func_info:
            input_data = FetchCodeInput(
                expression_name="test_function",
                referring_source_code_path="src/test.c"
            )
            result = await func_info.single_fn(input_data)

        self.assertIsInstance(result, str, "Result should be a string")
        self.assertIn("Error:", result, "Should contain error message")
        self.assertIn("Unexpected failure", result, "Should indicate unexpected error")
        self.assertIn("test_function", result, "Should mention the symbol")
        self.assertIn("Repository access failed", result, "Should include exception message")

    @patch('sast_agent_workflow.agent.tools.fetch_code.repo_handler_factory')
    @patch('sast_agent_workflow.agent.tools.fetch_code.Config')
    async def test_fetch_code_extracts_metadata_from_code_format(
        self, mock_config_class, mock_repo_factory
    ):
        """Test that _fetch_code correctly extracts file path and line range from code."""
        mock_config = Mock()
        mock_config.REPO_LOCAL_PATH = "/tmp/test/repo"
        mock_config_class.return_value = mock_config

        mock_repo_handler = Mock()
        mock_repo_factory.return_value = mock_repo_handler

        # Mock code with specific format
        code_with_metadata = """code of lib/talloc/talloc.c file:
520| void talloc_lib_init(void)
521| {
522|     static bool initialized = false;
523|     if (!initialized) {
524|         talloc_setup_atexit();
525|         initialized = true;
526|     }
527| }"""

        mock_repo_handler.extract_missing_functions_or_macros.return_value = (
            code_with_metadata,
            {"talloc_lib_init"},
        )

        async with register_fetch_code_tool(self.config, self.builder) as func_info:
            input_data = FetchCodeInput(
                expression_name="talloc_lib_init",
                referring_source_code_path="talloc.c"
            )
            result = await func_info.single_fn(input_data)

        # Verify metadata extraction
        self.assertIn("File Path: lib/talloc/talloc.c", result, "Should extract correct file path")
        self.assertIn("Line Range: 520-527", result, "Should extract correct line range")

    @patch('sast_agent_workflow.agent.tools.fetch_code.repo_handler_factory')
    @patch('sast_agent_workflow.agent.tools.fetch_code.Config')
    async def test_fetch_code_handles_code_without_line_numbers(
        self, mock_config_class, mock_repo_factory
    ):
        """Test that _fetch_code handles code without line numbers gracefully."""
        mock_config = Mock()
        mock_config.REPO_LOCAL_PATH = "/tmp/test/repo"
        mock_config_class.return_value = mock_config

        mock_repo_handler = Mock()
        mock_repo_factory.return_value = mock_repo_handler

        # Mock code without line numbers
        code_without_lines = """code of src/helper.c file:
void helper_function() {
    // Some code
}"""

        mock_repo_handler.extract_missing_functions_or_macros.return_value = (
            code_without_lines,
            {"helper_function"},
        )

        async with register_fetch_code_tool(self.config, self.builder) as func_info:
            input_data = FetchCodeInput(
                expression_name="helper_function",
                referring_source_code_path="main.c"
            )
            result = await func_info.single_fn(input_data)

        # Verify - should default to "unknown" for line range
        self.assertIn("Line Range: unknown", result, "Should use 'unknown' when no line numbers")
        self.assertIn("File Path: src/helper.c", result, "Should still extract file path")


if __name__ == '__main__':
    unittest.main()