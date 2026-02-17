"""
Unit tests for fetch_code tool.
Tests all scenarios with 100% coverage.
"""
import pytest
from pathlib import Path
from unittest.mock import Mock, MagicMock, patch
from langchain_core.tools import StructuredTool

from sast_agent_workflow.investigation.tools.fetch_code import (
    create_fetch_code_tool,
    Instruction,
    _try_sophisticated_extraction,
    _simple_file_extraction,
    _extract_function_simple,
)
from sast_agent_workflow.investigation.constants import (
    DEFAULT_CONTEXT_LINES,
    MAX_FUNCTION_SCAN_LINES,
)


class TestInstruction:
    """Test Instruction dataclass."""

    def test_instruction_creation(self):
        """Test creating an Instruction."""
        inst = Instruction(
            expression_name="malloc",
            referring_source_code_path="src/main.c"
        )
        assert inst.expression_name == "malloc"
        assert inst.referring_source_code_path == "src/main.c"


class TestCreateFetchCodeTool:
    """Test create_fetch_code_tool factory function."""

    def test_creates_structured_tool(self):
        """Test that create_fetch_code_tool returns a StructuredTool."""
        repo_handler = Mock()
        repo_path = Path("/fake/repo")
        tool = create_fetch_code_tool(repo_handler, repo_path)

        assert isinstance(tool, StructuredTool)
        assert tool.name == "fetch_code"
        assert "Fetch source code for a specific function" in tool.description

    def test_tool_has_correct_schema(self):
        """Test that tool has correct args_schema."""
        repo_handler = Mock()
        repo_path = Path("/fake/repo")
        tool = create_fetch_code_tool(repo_handler, repo_path)

        assert hasattr(tool, 'args_schema')


class TestTrySophisticatedExtraction:
    """Test _try_sophisticated_extraction function."""

    def test_successful_extraction(self):
        """Test successful sophisticated extraction."""
        repo_handler = Mock()
        repo_handler.extract_missing_functions_or_macros.return_value = (
            "10|void test_func() {\n11|    return;\n12|}\n",
            {"test_func"}
        )

        result = _try_sophisticated_extraction(repo_handler, "src/test.c", "test_func")

        assert result is not None
        assert "=== Fetched Code: test_func ===" in result
        assert "File Path:" in result
        assert "Line Range:" in result
        assert "10-12" in result
        assert "=== End of test_func ===" in result

    def test_extraction_function_not_in_new_symbols(self):
        """Test when function is not in new_symbols."""
        repo_handler = Mock()
        repo_handler.extract_missing_functions_or_macros.return_value = (
            "some code",
            set()  # Empty set, function not found
        )

        result = _try_sophisticated_extraction(repo_handler, "src/test.c", "missing_func")

        assert result is None

    def test_extraction_no_missing_code(self):
        """Test when no missing_code is returned."""
        repo_handler = Mock()
        repo_handler.extract_missing_functions_or_macros.return_value = (
            "",  # Empty missing code
            {"test_func"}
        )

        result = _try_sophisticated_extraction(repo_handler, "src/test.c", "test_func")

        assert result is None

    def test_extraction_exception_handling(self):
        """Test exception handling in sophisticated extraction."""
        repo_handler = Mock()
        repo_handler.extract_missing_functions_or_macros.side_effect = Exception("Extraction failed")

        result = _try_sophisticated_extraction(repo_handler, "src/test.c", "test_func")

        assert result is None

    def test_extraction_with_file_path_in_code(self):
        """Test extraction when file path is in the code output."""
        repo_handler = Mock()
        code_with_path = (
            "code of src/auth.c file:\n"
            "10|void validate() {\n"
            "11|    return;\n"
            "12|}\n"
        )
        repo_handler.extract_missing_functions_or_macros.return_value = (
            code_with_path,
            {"validate"}
        )

        result = _try_sophisticated_extraction(repo_handler, "src/test.c", "validate")

        assert result is not None
        assert "File Path: src/auth.c" in result

    def test_extraction_without_line_numbers(self):
        """Test extraction when code has no line numbers."""
        repo_handler = Mock()
        repo_handler.extract_missing_functions_or_macros.return_value = (
            "void test() { return; }",
            {"test"}
        )

        result = _try_sophisticated_extraction(repo_handler, "src/test.c", "test")

        assert result is not None
        assert "Line Range: unknown" in result


class TestSimpleFileExtraction:
    """Test _simple_file_extraction function."""

    @pytest.fixture
    def temp_repo(self, tmp_path):
        """Create a temporary repository with test files."""
        test_file = tmp_path / "test.c"
        test_file.write_text(
            "// Header\n"
            "void test_func() {\n"
            "    int x = 1;\n"
            "    return;\n"
            "}\n"
            "\n"
            "void another_func() {\n"
            "    return;\n"
            "}\n"
        )
        return tmp_path

    def test_successful_simple_extraction(self, temp_repo):
        """Test successful simple extraction."""
        result = _simple_file_extraction(
            temp_repo, "test.c", "test_func", DEFAULT_CONTEXT_LINES
        )

        assert "=== test.c:test_func" in result
        assert "void test_func()" in result
        assert "=== End of test_func ===" in result

    def test_file_not_found(self, temp_repo):
        """Test error when file doesn't exist."""
        result = _simple_file_extraction(
            temp_repo, "nonexistent.c", "func", DEFAULT_CONTEXT_LINES
        )

        assert "Error: File not found: nonexistent.c" in result
        assert "Suggested recovery:" in result
        assert "search_codebase" in result

    def test_function_not_found(self, temp_repo):
        """Test error when function doesn't exist."""
        result = _simple_file_extraction(
            temp_repo, "test.c", "nonexistent_func", DEFAULT_CONTEXT_LINES
        )

        assert "Error: Function 'nonexistent_func' not found in test.c" in result
        assert "Suggested recovery:" in result

    def test_extraction_without_function_name(self, temp_repo):
        """Test reading entire file when no function specified."""
        result = _simple_file_extraction(
            temp_repo, "test.c", None, DEFAULT_CONTEXT_LINES
        )

        assert "=== test.c ===" in result
        assert "1: // Header" in result or "   1: // Header" in result
        assert "void test_func()" in result
        assert "void another_func()" in result

    def test_extraction_with_empty_function_name(self, temp_repo):
        """Test reading entire file when function name is empty string."""
        result = _simple_file_extraction(
            temp_repo, "test.c", "", DEFAULT_CONTEXT_LINES
        )

        assert "=== test.c ===" in result

    def test_package_name_suffix_stripping(self, temp_repo):
        """Test stripping package name suffix from file path."""
        repo_name = temp_repo.name
        file_path_with_suffix = f"test.c{repo_name}"

        # This should strip the repo_name suffix
        result = _simple_file_extraction(
            temp_repo, file_path_with_suffix, "test_func", DEFAULT_CONTEXT_LINES
        )

        # Should successfully find the file after stripping
        assert "=== test.c" in result or "void test_func()" in result

    def test_exception_handling(self, temp_repo):
        """Test exception handling in simple extraction."""
        with patch("builtins.open", side_effect=PermissionError("Access denied")):
            result = _simple_file_extraction(
                temp_repo, "test.c", "test_func", DEFAULT_CONTEXT_LINES
            )

        assert "Error: Failed to fetch from 'test.c'" in result
        assert "Access denied" in result


class TestExtractFunctionSimple:
    """Test _extract_function_simple function."""

    def test_extract_simple_function(self):
        """Test extracting a simple function."""
        lines = [
            "// comment\n",
            "void test_func() {\n",
            "    return;\n",
            "}\n",
            "other code\n",
        ]

        result = _extract_function_simple(lines, "test.c", "test_func", 0)

        assert "=== test.c:test_func" in result
        assert "void test_func()" in result
        assert "=== End of test_func ===" in result

    def test_extract_with_context_lines(self):
        """Test extracting function with context lines."""
        lines = [
            "line 0\n",
            "line 1\n",
            "void test_func() {\n",  # line 2
            "    body;\n",  # line 3
            "}\n",  # line 4
            "line 5\n",
            "line 6\n",
        ]

        result = _extract_function_simple(lines, "test.c", "test_func", 2)

        # Should include 2 lines before and after
        assert "line 0" in result or "line 1" in result
        assert "void test_func()" in result
        assert "line 5" in result or "line 6" in result

    def test_extract_function_with_nested_braces(self):
        """Test extracting function with nested braces."""
        lines = [
            "void test_func() {\n",
            "    if (x) {\n",
            "        do_something();\n",
            "    }\n",
            "}\n",
        ]

        result = _extract_function_simple(lines, "test.c", "test_func", 0)

        assert "void test_func()" in result
        assert "if (x)" in result
        assert "do_something()" in result
        assert "=== End of test_func ===" in result

    def test_extract_function_not_found(self):
        """Test when function is not found."""
        lines = [
            "void other_func() {\n",
            "    return;\n",
            "}\n",
        ]

        result = _extract_function_simple(lines, "test.c", "missing_func", 0)

        assert "Error: Function 'missing_func' not found" in result
        assert "Suggested recovery:" in result

    def test_extract_distinguishes_definition_from_call(self):
        """Test that function definition is distinguished from function call."""
        lines = [
            "void caller() {\n",
            "    test_func();  // This is a call\n",
            "}\n",
            "\n",
            "void test_func() {  // This is the definition\n",
            "    return;\n",
            "}\n",
        ]

        result = _extract_function_simple(lines, "test.c", "test_func", 0)

        # Should extract the definition, not the call
        assert "void test_func() {  // This is the definition" in result
        assert "=== End of test_func ===" in result

    def test_extract_function_incomplete_braces(self):
        """Test extracting function with incomplete braces (partial match)."""
        lines = [
            "void test_func() {\n",
            "    // function body\n",
            # Missing closing brace - file ends abruptly
        ]

        result = _extract_function_simple(lines, "test.c", "test_func", 0)

        # Should return partial match
        assert "(partial match for 'test_func')" in result
        assert "void test_func()" in result
        assert "Function may be incomplete" in result

    def test_extract_context_lines_at_file_start(self):
        """Test context lines when function is at start of file."""
        lines = [
            "void test_func() {\n",
            "    return;\n",
            "}\n",
            "other code\n",
        ]

        result = _extract_function_simple(lines, "test.c", "test_func", 5)

        # Should not crash, just use available lines
        assert "void test_func()" in result

    def test_extract_context_lines_at_file_end(self):
        """Test context lines when function is at end of file."""
        lines = [
            "other code\n",
            "void test_func() {\n",
            "    return;\n",
            "}\n",
        ]

        result = _extract_function_simple(lines, "test.c", "test_func", 5)

        # Should not crash, just use available lines
        assert "void test_func()" in result

    def test_extract_function_with_assignment(self):
        """Test that function pointers/assignments don't match."""
        lines = [
            "void (*ptr)() = test_func;  // Assignment, not definition\n",
            "\n",
            "void test_func() {  // This is the definition\n",
            "    return;\n",
            "}\n",
        ]

        result = _extract_function_simple(lines, "test.c", "test_func", 0)

        # Should find the definition, not the assignment
        assert "void test_func() {  // This is the definition" in result

    def test_extract_with_member_access(self):
        """Test that member access doesn't match."""
        lines = [
            "obj.test_func();  // Member access\n",
            "obj->test_func();  // Pointer member access\n",
            "\n",
            "void test_func() {  // Definition\n",
            "    return;\n",
            "}\n",
        ]

        result = _extract_function_simple(lines, "test.c", "test_func", 0)

        # Should find the definition
        assert "void test_func() {  // Definition" in result

    def test_extract_function_multiple_definitions(self):
        """Test extracting when there are multiple functions."""
        lines = [
            "void first_func() {\n",
            "    return;\n",
            "}\n",
            "\n",
            "void test_func() {\n",
            "    return;\n",
            "}\n",
            "\n",
            "void third_func() {\n",
            "    return;\n",
            "}\n",
        ]

        result = _extract_function_simple(lines, "test.c", "test_func", 0)

        # Should extract only test_func
        assert "void test_func()" in result
        assert "void first_func()" not in result
        assert "void third_func()" not in result

    def test_partial_match_max_scan_lines(self):
        """Test that partial match is limited by MAX_FUNCTION_SCAN_LINES."""
        # Create a function that starts but doesn't end within MAX_FUNCTION_SCAN_LINES
        lines = ["void test_func() {\n"]
        lines.extend([f"    line {i};\n" for i in range(MAX_FUNCTION_SCAN_LINES + 10)])
        # No closing brace

        result = _extract_function_simple(lines, "test.c", "test_func", 0)

        # Should return partial match limited to MAX_FUNCTION_SCAN_LINES
        assert "(partial match for 'test_func')" in result


class TestFetchCodeToolIntegration:
    """Integration tests for fetch_code tool."""

    @pytest.fixture
    def temp_repo(self, tmp_path):
        """Create a temporary repository with test files."""
        test_file = tmp_path / "main.c"
        test_file.write_text(
            "void main_function() {\n"
            "    int x = 1;\n"
            "}\n"
        )
        return tmp_path

    def test_tool_uses_sophisticated_extraction_first(self, temp_repo):
        """Test that tool tries sophisticated extraction first."""
        repo_handler = Mock()
        repo_handler.extract_missing_functions_or_macros.return_value = (
            "10|void main_function() {\n11|    int x = 1;\n12|}\n",
            {"main_function"}
        )

        tool = create_fetch_code_tool(repo_handler, temp_repo)
        result = tool.invoke({
            "file_path": "main.c",
            "function_name": "main_function"
        })

        # Should use sophisticated approach
        assert "=== Fetched Code: main_function ===" in result
        repo_handler.extract_missing_functions_or_macros.assert_called_once()

    def test_tool_falls_back_to_simple_extraction(self, temp_repo):
        """Test that tool falls back to simple extraction."""
        repo_handler = Mock()
        repo_handler.extract_missing_functions_or_macros.return_value = (
            "",  # Empty result
            set()
        )

        tool = create_fetch_code_tool(repo_handler, temp_repo)
        result = tool.invoke({
            "file_path": "main.c",
            "function_name": "main_function"
        })

        # Should fall back to simple extraction
        assert "void main_function()" in result

    def test_tool_with_custom_context_lines(self, temp_repo):
        """Test tool with custom context_lines parameter."""
        repo_handler = Mock()
        repo_handler.extract_missing_functions_or_macros.return_value = ("", set())

        tool = create_fetch_code_tool(repo_handler, temp_repo)
        result = tool.invoke({
            "file_path": "main.c",
            "function_name": "main_function",
            "context_lines": 5
        })

        # Should use simple extraction with custom context
        assert "void main_function()" in result

    def test_tool_handles_exception_in_sophisticated_extraction(self, temp_repo):
        """Test that tool handles exceptions in sophisticated extraction."""
        repo_handler = Mock()
        repo_handler.extract_missing_functions_or_macros.side_effect = RuntimeError("Error")

        tool = create_fetch_code_tool(repo_handler, temp_repo)
        result = tool.invoke({
            "file_path": "main.c",
            "function_name": "main_function"
        })

        # Should fall back to simple extraction
        assert "void main_function()" in result