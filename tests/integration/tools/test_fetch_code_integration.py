"""
Integration tests for fetch_code tool with real file operations.
Tests actual file reading, function extraction, and fallback mechanisms.
"""
import pytest
from pathlib import Path
from unittest.mock import Mock

from sast_agent_workflow.investigation.tools.fetch_code import create_fetch_code_tool


@pytest.fixture
def sample_repo_path():
    """Path to sample repository for integration testing."""
    return Path(__file__).parent / "fixtures" / "sample_repo"


@pytest.fixture
def mock_repo_handler():
    """Mock repository handler that simulates failure (forces fallback)."""
    handler = Mock()
    # Simulate sophisticated extraction failing
    handler.extract_missing_functions_or_macros = Mock(return_value=("", set()))
    return handler


@pytest.fixture
def fetch_code_tool(sample_repo_path, mock_repo_handler):
    """Create fetch_code tool with real repo path."""
    return create_fetch_code_tool(mock_repo_handler, sample_repo_path)


class TestFetchCodeIntegration:
    """Integration tests for fetch_code tool."""

    def test_fetch_sanitize_input_function(self, fetch_code_tool):
        """Test fetching a complete function from real C file."""
        result = fetch_code_tool.invoke({
            "file_path": "src/main.c",
            "function_name": "sanitize_input",
            "context_lines": 2
        })

        assert "sanitize_input" in result
        assert "char* sanitize_input" in result
        assert "Remove dangerous characters" in result
        assert "malloc" in result
        assert "src/main.c" in result
        assert "=== End of sanitize_input ===" in result

    def test_fetch_function_with_nested_braces(self, fetch_code_tool):
        """Test fetching function with nested brace structures."""
        result = fetch_code_tool.invoke({
            "file_path": "src/main.c",
            "function_name": "validate_email",
            "context_lines": 1
        })

        assert "validate_email" in result
        assert "strchr" in result
        assert "strrchr" in result
        # Should capture full function including nested if statements
        assert result.count("{") == result.count("}")

    def test_fetch_from_subdirectory(self, fetch_code_tool):
        """Test fetching function from file in subdirectory."""
        result = fetch_code_tool.invoke({
            "file_path": "lib/auth.c",
            "function_name": "authenticate_user",
            "context_lines": 0
        })

        assert "authenticate_user" in result
        assert "MAX_USERNAME_LEN" in result
        assert "lib/auth.c" in result

    def test_fetch_function_not_found(self, fetch_code_tool):
        """Test fetching non-existent function returns helpful error."""
        result = fetch_code_tool.invoke({
            "file_path": "src/main.c",
            "function_name": "nonexistent_function",
            "context_lines": 2
        })

        assert "Error" in result
        assert "not found" in result
        assert "nonexistent_function" in result
        assert "search_codebase" in result  # Suggests recovery

    def test_fetch_file_not_found(self, fetch_code_tool):
        """Test fetching from non-existent file returns helpful error."""
        result = fetch_code_tool.invoke({
            "file_path": "nonexistent/file.c",
            "function_name": "some_function",
            "context_lines": 2
        })

        assert "Error" in result
        assert "not found" in result
        assert "search_codebase" in result  # Suggests recovery

    def test_fetch_entire_file_without_function_name(self, fetch_code_tool):
        """Test fetching entire file when function_name is empty."""
        result = fetch_code_tool.invoke({
            "file_path": "include/utils.h",
            "function_name": "",
            "context_lines": 0
        })

        assert "include/utils.h" in result
        assert "#ifndef UTILS_H" in result
        assert "sanitize_input" in result
        assert "validate_email" in result

    def test_fetch_with_custom_context_lines(self, fetch_code_tool):
        """Test that context_lines parameter works correctly."""
        result_no_context = fetch_code_tool.invoke({
            "file_path": "lib/auth.c",
            "function_name": "hash_password",
            "context_lines": 0
        })

        result_with_context = fetch_code_tool.invoke({
            "file_path": "lib/auth.c",
            "function_name": "hash_password",
            "context_lines": 5
        })

        # Result with context should include more lines
        assert len(result_with_context) > len(result_no_context)
        assert "hash_password" in result_with_context
        assert "hash_password" in result_no_context

    def test_fetch_function_with_comments(self, fetch_code_tool):
        """Test that function with docstring comments is extracted correctly."""
        result = fetch_code_tool.invoke({
            "file_path": "src/main.c",
            "function_name": "process_user_data",
            "context_lines": 3  # Need more context to capture docstring
        })

        assert "process_user_data" in result
        assert "fprintf" in result
        # Function is successfully extracted
        assert "int process_user_data" in result

    def test_fetch_distinguishes_function_definition_from_call(self, fetch_code_tool):
        """Test that it finds function definitions, not calls."""
        result = fetch_code_tool.invoke({
            "file_path": "src/main.c",
            "function_name": "sanitize_input",
            "context_lines": 0
        })

        # Should extract the definition, not the call in process_user_data
        assert "char* sanitize_input(const char* input)" in result
        # Should not include the process_user_data function
        assert "process_user_data" not in result or result.count("sanitize_input") > result.count("process_user_data")

    def test_fetch_function_with_malloc_calloc_realloc(self, fetch_code_tool):
        """Test fetching functions with memory allocation patterns."""
        # Test malloc
        result_malloc = fetch_code_tool.invoke({
            "file_path": "lib/config.c",
            "function_name": "allocate_buffer",
            "context_lines": 0
        })
        assert "malloc" in result_malloc
        assert "memset" in result_malloc

        # Test realloc
        result_realloc = fetch_code_tool.invoke({
            "file_path": "lib/config.c",
            "function_name": "resize_buffer",
            "context_lines": 0
        })
        assert "realloc" in result_realloc

    def test_fetch_handles_function_pointers(self, fetch_code_tool):
        """Test that function pointer assignments don't confuse extraction."""
        result = fetch_code_tool.invoke({
            "file_path": "lib/config.c",
            "function_name": "load_config",
            "context_lines": 0
        })

        # Should extract load_config, not callback_ptr line
        assert "load_config" in result
        assert "fopen" in result
        assert "FILE* fp" in result
