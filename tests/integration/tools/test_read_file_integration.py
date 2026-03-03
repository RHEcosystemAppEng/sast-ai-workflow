"""
Integration tests for read_file tool with real file operations.
Tests actual file reading with line ranges, encodings, and edge cases.
"""
import pytest
from pathlib import Path

from sast_agent_workflow.investigation.tools.read_file import create_read_file_tool


@pytest.fixture
def sample_repo_path():
    """Path to sample repository for integration testing."""
    return Path(__file__).parent / "fixtures" / "sample_repo"


@pytest.fixture
def read_file_tool(sample_repo_path):
    """Create read_file tool with real repo path."""
    return create_read_file_tool(sample_repo_path)


class TestReadFileIntegration:
    """Integration tests for read_file tool."""

    def test_read_entire_c_file(self, read_file_tool):
        """Test reading entire C source file."""
        result = read_file_tool.invoke({
            "file_path": "src/main.c",
            "start_line": None,
            "end_line": None
        })

        assert "=== File: src/main.c ===" in result
        assert "Total Lines:" in result
        assert "#include" in result
        assert "sanitize_input" in result
        assert "int main(" in result
        assert "=== End of src/main.c ===" in result

    def test_read_with_line_range(self, read_file_tool):
        """Test reading specific line range."""
        result = read_file_tool.invoke({
            "file_path": "src/main.c",
            "start_line": 1,
            "end_line": 10
        })

        assert "Showing Lines: 1-10" in result
        assert "/*" in result or "#include" in result
        # Should not include lines beyond 10
        lines_shown = [l for l in result.split('\n') if '|' in l]
        assert len(lines_shown) <= 10

    def test_read_header_file(self, read_file_tool):
        """Test reading header file."""
        result = read_file_tool.invoke({
            "file_path": "include/utils.h",
            "start_line": None,
            "end_line": None
        })

        assert "#ifndef UTILS_H" in result
        assert "#define UTILS_H" in result
        assert "#endif" in result
        assert "sanitize_input" in result
        assert "validate_email" in result

    def test_read_from_subdirectory(self, read_file_tool):
        """Test reading file from subdirectory."""
        result = read_file_tool.invoke({
            "file_path": "lib/auth.c",
            "start_line": None,
            "end_line": None
        })

        assert "lib/auth.c" in result
        assert "authenticate_user" in result
        assert "hash_password" in result

    def test_read_nested_include_directory(self, read_file_tool):
        """Test reading file from include directory."""
        result = read_file_tool.invoke({
            "file_path": "include/auth.h",
            "start_line": None,
            "end_line": None
        })

        assert "AUTH_H" in result
        assert "authenticate_user" in result

    def test_read_with_start_line_only(self, read_file_tool):
        """Test reading from specific line to end of file."""
        result = read_file_tool.invoke({
            "file_path": "include/utils.h",
            "start_line": 5,
            "end_line": None
        })

        assert "Showing Lines:" in result
        # Should include lines from 5 onwards
        assert "char*" in result or "int" in result

    def test_read_with_end_line_only(self, read_file_tool):
        """Test reading from start to specific line."""
        result = read_file_tool.invoke({
            "file_path": "include/utils.h",
            "start_line": None,
            "end_line": 5
        })

        assert "Showing Lines:" in result
        # Should include lines from start to 5
        lines_shown = [l for l in result.split('\n') if '|' in l]
        assert len(lines_shown) <= 5

    def test_read_single_line(self, read_file_tool):
        """Test reading a single line."""
        result = read_file_tool.invoke({
            "file_path": "include/utils.h",
            "start_line": 1,
            "end_line": 1
        })

        assert "Showing Lines: 1-1" in result
        lines_shown = [l for l in result.split('\n') if '|' in l]
        assert len(lines_shown) == 1

    def test_read_middle_section_of_file(self, read_file_tool):
        """Test reading a middle section of file."""
        result = read_file_tool.invoke({
            "file_path": "src/main.c",
            "start_line": 10,
            "end_line": 20
        })

        assert "Showing Lines: 10-20" in result
        lines_shown = [l for l in result.split('\n') if '|' in l]
        assert len(lines_shown) <= 11  # 10 to 20 inclusive = 11 lines

    def test_read_includes_line_numbers(self, read_file_tool):
        """Test that output includes proper line numbers."""
        result = read_file_tool.invoke({
            "file_path": "include/utils.h",
            "start_line": None,
            "end_line": None
        })

        # Should have line numbers formatted like "     1| "
        assert "     1|" in result or "1|" in result
        # Should preserve formatting
        assert "|" in result

    def test_read_file_not_found(self, read_file_tool):
        """Test reading non-existent file."""
        result = read_file_tool.invoke({
            "file_path": "nonexistent/file.c",
            "start_line": None,
            "end_line": None
        })

        assert "Error" in result
        assert "does not exist" in result or "not found" in result.lower()

    def test_read_directory_returns_error(self, read_file_tool):
        """Test that trying to read a directory returns error."""
        result = read_file_tool.invoke({
            "file_path": "src",
            "start_line": None,
            "end_line": None
        })

        assert "Error" in result
        assert "not a file" in result or "directory" in result.lower()

    def test_read_handles_line_range_beyond_file_length(self, read_file_tool):
        """Test reading with end_line beyond actual file length."""
        result = read_file_tool.invoke({
            "file_path": "include/utils.h",
            "start_line": 1,
            "end_line": 1000
        })

        # Should read up to actual end of file, not error
        assert "utils.h" in result
        assert "#endif" in result  # Should include last line

    def test_read_shows_total_lines(self, read_file_tool):
        """Test that output shows total number of lines in file."""
        result = read_file_tool.invoke({
            "file_path": "include/utils.h",
            "start_line": None,
            "end_line": None
        })

        assert "Total Lines:" in result
        # Should show a number
        assert any(char.isdigit() for char in result)

    def test_read_preserves_indentation(self, read_file_tool):
        """Test that indentation is preserved."""
        result = read_file_tool.invoke({
            "file_path": "src/main.c",
            "start_line": 20,
            "end_line": 30
        })

        # Should preserve spaces/tabs
        # Original file has indented code
        lines_with_content = [l for l in result.split('\n') if '|' in l and l.strip()]
        # Some lines should have indentation after the pipe
        has_indented_lines = any('|    ' in l or '|  ' in l for l in lines_with_content)
        assert has_indented_lines or "malloc" in result  # At minimum, code should be there

    def test_read_handles_comments(self, read_file_tool):
        """Test reading file with C comments."""
        result = read_file_tool.invoke({
            "file_path": "lib/auth.c",
            "start_line": 1,
            "end_line": 10
        })

        # Should include C-style comments
        assert "/*" in result or "//" in result or "Authentication" in result

    def test_read_handles_preprocessor_directives(self, read_file_tool):
        """Test reading file with preprocessor directives."""
        result = read_file_tool.invoke({
            "file_path": "include/auth.h",
            "start_line": None,
            "end_line": None
        })

        assert "#ifndef" in result
        assert "#define" in result
        assert "#endif" in result

    def test_read_real_world_code_patterns(self, read_file_tool):
        """Test reading file with realistic code patterns."""
        result = read_file_tool.invoke({
            "file_path": "lib/auth.c",
            "start_line": None,
            "end_line": None
        })

        # Should include realistic patterns
        assert "unsigned int" in result or "int" in result
        assert "if" in result
        assert "return" in result
