"""
Unit tests for investigation tool schemas.
Tests all Pydantic models with 100% coverage.
"""
import pytest
from pydantic import ValidationError

from sast_agent_workflow.investigation.tools.schemas import (
    FetchCodeInput,
    SearchCodebaseInput,
    ReadFileInput,
)
from sast_agent_workflow.investigation.constants import (
    DEFAULT_CONTEXT_LINES,
    DEFAULT_MAX_SEARCH_RESULTS,
)


class TestFetchCodeInput:
    """Test FetchCodeInput schema."""

    def test_valid_input_with_defaults(self):
        """Test valid input with default context_lines."""
        input_data = FetchCodeInput(
            file_path="src/main.c",
            function_name="validate_input"
        )
        assert input_data.file_path == "src/main.c"
        assert input_data.function_name == "validate_input"
        assert input_data.context_lines == DEFAULT_CONTEXT_LINES

    def test_valid_input_with_custom_context(self):
        """Test valid input with custom context_lines."""
        input_data = FetchCodeInput(
            file_path="lib/auth.c",
            function_name="sanitize_data",
            context_lines=5
        )
        assert input_data.file_path == "lib/auth.c"
        assert input_data.function_name == "sanitize_data"
        assert input_data.context_lines == 5

    def test_valid_input_with_zero_context(self):
        """Test valid input with zero context lines."""
        input_data = FetchCodeInput(
            file_path="src/parser.c",
            function_name="parse",
            context_lines=0
        )
        assert input_data.context_lines == 0

    def test_missing_file_path_raises_error(self):
        """Test that missing file_path raises ValidationError."""
        with pytest.raises(ValidationError) as exc_info:
            FetchCodeInput(function_name="test_func")
        assert "file_path" in str(exc_info.value)

    def test_missing_function_name_raises_error(self):
        """Test that missing function_name raises ValidationError."""
        with pytest.raises(ValidationError) as exc_info:
            FetchCodeInput(file_path="src/test.c")
        assert "function_name" in str(exc_info.value)

    def test_empty_file_path_accepted(self):
        """Test that empty string file_path is accepted (validation happens in tool)."""
        input_data = FetchCodeInput(
            file_path="",
            function_name="func"
        )
        assert input_data.file_path == ""

    def test_empty_function_name_accepted(self):
        """Test that empty string function_name is accepted."""
        input_data = FetchCodeInput(
            file_path="src/test.c",
            function_name=""
        )
        assert input_data.function_name == ""

    def test_negative_context_lines_accepted(self):
        """Test that negative context_lines is accepted (handled by tool logic)."""
        input_data = FetchCodeInput(
            file_path="src/test.c",
            function_name="func",
            context_lines=-1
        )
        assert input_data.context_lines == -1


class TestSearchCodebaseInput:
    """Test SearchCodebaseInput schema."""

    def test_valid_input_with_defaults(self):
        """Test valid input with default file_pattern and max_results."""
        input_data = SearchCodebaseInput(pattern="malloc")
        assert input_data.pattern == "malloc"
        assert input_data.file_pattern == "*.c"
        assert input_data.max_results == DEFAULT_MAX_SEARCH_RESULTS

    def test_valid_input_with_custom_file_pattern(self):
        """Test valid input with custom file_pattern."""
        input_data = SearchCodebaseInput(
            pattern="validate_.*",
            file_pattern="*.py"
        )
        assert input_data.pattern == "validate_.*"
        assert input_data.file_pattern == "*.py"

    def test_valid_input_with_custom_max_results(self):
        """Test valid input with custom max_results."""
        input_data = SearchCodebaseInput(
            pattern="sanitize",
            max_results=50
        )
        assert input_data.max_results == 50

    def test_valid_input_all_custom_values(self):
        """Test valid input with all custom values."""
        input_data = SearchCodebaseInput(
            pattern="buffer.*overflow",
            file_pattern="*.h",
            max_results=100
        )
        assert input_data.pattern == "buffer.*overflow"
        assert input_data.file_pattern == "*.h"
        assert input_data.max_results == 100

    def test_missing_pattern_raises_error(self):
        """Test that missing pattern raises ValidationError."""
        with pytest.raises(ValidationError) as exc_info:
            SearchCodebaseInput(file_pattern="*.c")
        assert "pattern" in str(exc_info.value)

    def test_empty_pattern_accepted(self):
        """Test that empty pattern is accepted (validation happens in tool)."""
        input_data = SearchCodebaseInput(pattern="")
        assert input_data.pattern == ""

    def test_zero_max_results_accepted(self):
        """Test that zero max_results is accepted."""
        input_data = SearchCodebaseInput(pattern="test", max_results=0)
        assert input_data.max_results == 0

    def test_negative_max_results_accepted(self):
        """Test that negative max_results is accepted (handled by tool logic)."""
        input_data = SearchCodebaseInput(pattern="test", max_results=-1)
        assert input_data.max_results == -1

    def test_regex_pattern_accepted(self):
        """Test that complex regex patterns are accepted."""
        input_data = SearchCodebaseInput(pattern=r"(malloc|calloc|realloc)\s*\(")
        assert input_data.pattern == r"(malloc|calloc|realloc)\s*\("


class TestReadFileInput:
    """Test ReadFileInput schema."""

    def test_valid_input_file_path_only(self):
        """Test valid input with only file_path."""
        input_data = ReadFileInput(file_path="src/main.c")
        assert input_data.file_path == "src/main.c"
        assert input_data.start_line is None
        assert input_data.end_line is None

    def test_valid_input_with_start_line(self):
        """Test valid input with start_line."""
        input_data = ReadFileInput(
            file_path="src/auth.c",
            start_line=10
        )
        assert input_data.start_line == 10
        assert input_data.end_line is None

    def test_valid_input_with_end_line(self):
        """Test valid input with end_line."""
        input_data = ReadFileInput(
            file_path="src/parser.c",
            end_line=50
        )
        assert input_data.start_line is None
        assert input_data.end_line == 50

    def test_valid_input_with_line_range(self):
        """Test valid input with both start_line and end_line."""
        input_data = ReadFileInput(
            file_path="lib/utils.c",
            start_line=100,
            end_line=200
        )
        assert input_data.start_line == 100
        assert input_data.end_line == 200

    def test_valid_input_start_equals_end(self):
        """Test valid input when start_line equals end_line."""
        input_data = ReadFileInput(
            file_path="src/test.c",
            start_line=42,
            end_line=42
        )
        assert input_data.start_line == 42
        assert input_data.end_line == 42

    def test_valid_input_end_before_start(self):
        """Test that end_line before start_line is accepted (handled by tool)."""
        input_data = ReadFileInput(
            file_path="src/test.c",
            start_line=100,
            end_line=50
        )
        assert input_data.start_line == 100
        assert input_data.end_line == 50

    def test_missing_file_path_raises_error(self):
        """Test that missing file_path raises ValidationError."""
        with pytest.raises(ValidationError) as exc_info:
            ReadFileInput(start_line=1, end_line=10)
        assert "file_path" in str(exc_info.value)

    def test_empty_file_path_accepted(self):
        """Test that empty file_path is accepted (validation happens in tool)."""
        input_data = ReadFileInput(file_path="")
        assert input_data.file_path == ""

    def test_zero_start_line_accepted(self):
        """Test that zero start_line is accepted."""
        input_data = ReadFileInput(file_path="src/test.c", start_line=0)
        assert input_data.start_line == 0

    def test_zero_end_line_accepted(self):
        """Test that zero end_line is accepted."""
        input_data = ReadFileInput(file_path="src/test.c", end_line=0)
        assert input_data.end_line == 0

    def test_negative_start_line_accepted(self):
        """Test that negative start_line is accepted (handled by tool)."""
        input_data = ReadFileInput(file_path="src/test.c", start_line=-1)
        assert input_data.start_line == -1

    def test_negative_end_line_accepted(self):
        """Test that negative end_line is accepted (handled by tool)."""
        input_data = ReadFileInput(file_path="src/test.c", end_line=-1)
        assert input_data.end_line == -1

    def test_large_line_numbers_accepted(self):
        """Test that very large line numbers are accepted."""
        input_data = ReadFileInput(
            file_path="src/test.c",
            start_line=1000000,
            end_line=2000000
        )
        assert input_data.start_line == 1000000
        assert input_data.end_line == 2000000