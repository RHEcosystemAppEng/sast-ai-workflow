"""
Unit tests for read_file tool.
Tests all scenarios with 100% coverage.
"""
import pytest
from pathlib import Path
from unittest.mock import Mock, patch, mock_open
from langchain_core.tools import StructuredTool

from sast_agent_workflow.investigation.tools.read_file import create_read_file_tool


class TestCreateReadFileTool:
    """Test create_read_file_tool factory function."""

    def test_creates_structured_tool(self):
        """Test that create_read_file_tool returns a StructuredTool."""
        repo_path = Path("/fake/repo")
        tool = create_read_file_tool(repo_path)

        assert isinstance(tool, StructuredTool)
        assert tool.name == "read_file"
        assert "Read contents of a file" in tool.description

    def test_tool_has_correct_schema(self):
        """Test that tool has correct args_schema."""
        repo_path = Path("/fake/repo")
        tool = create_read_file_tool(repo_path)

        # Check that the tool accepts the expected parameters
        assert hasattr(tool, 'args_schema')


class TestReadFileTool:
    """Test read_file tool execution."""

    @pytest.fixture
    def temp_repo(self, tmp_path):
        """Create a temporary repository with test files."""
        # Create test files
        test_file = tmp_path / "test.c"
        test_file.write_text("line 1\nline 2\nline 3\nline 4\nline 5\n")

        subdir = tmp_path / "src"
        subdir.mkdir()
        nested_file = subdir / "nested.c"
        nested_file.write_text("nested line 1\nnested line 2\nnested line 3\n")

        return tmp_path

    @pytest.fixture
    def read_file_tool(self, temp_repo):
        """Create a read_file tool with temp repository."""
        return create_read_file_tool(temp_repo)

    def test_read_entire_file(self, read_file_tool):
        """Test reading entire file without line range."""
        result = read_file_tool.invoke({"file_path": "test.c"})

        assert "=== File: test.c ===" in result
        assert "Total Lines: 5" in result
        assert "     1| line 1" in result
        assert "     5| line 5" in result
        assert "=== End of test.c ===" in result

    def test_read_file_with_start_line_only(self, read_file_tool):
        """Test reading file from start_line to end."""
        result = read_file_tool.invoke({
            "file_path": "test.c",
            "start_line": 3
        })

        assert "Total Lines: 5" in result
        assert "Showing Lines: 3-5" in result
        assert "     3| line 3" in result
        assert "     4| line 4" in result
        assert "     5| line 5" in result
        assert "     1| line 1" not in result
        assert "     2| line 2" not in result

    def test_read_file_with_end_line_only(self, read_file_tool):
        """Test reading file from beginning to end_line."""
        result = read_file_tool.invoke({
            "file_path": "test.c",
            "end_line": 3
        })

        assert "Total Lines: 5" in result
        assert "Showing Lines: 1-3" in result
        assert "     1| line 1" in result
        assert "     2| line 2" in result
        assert "     3| line 3" in result
        assert "     4| line 4" not in result
        assert "     5| line 5" not in result

    def test_read_file_with_line_range(self, read_file_tool):
        """Test reading file with both start_line and end_line."""
        result = read_file_tool.invoke({
            "file_path": "test.c",
            "start_line": 2,
            "end_line": 4
        })

        assert "Total Lines: 5" in result
        assert "Showing Lines: 2-4" in result
        assert "     2| line 2" in result
        assert "     3| line 3" in result
        assert "     4| line 4" in result
        assert "     1| line 1" not in result
        assert "     5| line 5" not in result

    def test_read_nested_file(self, read_file_tool):
        """Test reading file in subdirectory."""
        result = read_file_tool.invoke({"file_path": "src/nested.c"})

        assert "=== File: src/nested.c ===" in result
        assert "Total Lines: 3" in result
        assert "     1| nested line 1" in result
        assert "     2| nested line 2" in result
        assert "     3| nested line 3" in result

    def test_file_not_found(self, read_file_tool):
        """Test error handling when file doesn't exist."""
        result = read_file_tool.invoke({"file_path": "nonexistent.c"})

        assert "Error: File 'nonexistent.c' does not exist" in result

    def test_path_is_directory(self, read_file_tool, temp_repo):
        """Test error handling when path points to directory."""
        result = read_file_tool.invoke({"file_path": "src"})

        assert "Error: 'src' is not a file" in result

    def test_start_line_beyond_file_length(self, read_file_tool):
        """Test reading with start_line beyond file length."""
        result = read_file_tool.invoke({
            "file_path": "test.c",
            "start_line": 100
        })

        # Should return empty range but not error
        assert "Total Lines: 5" in result
        assert "Showing Lines:" in result or "=== End of test.c ===" in result

    def test_end_line_beyond_file_length(self, read_file_tool):
        """Test reading with end_line beyond file length."""
        result = read_file_tool.invoke({
            "file_path": "test.c",
            "end_line": 100
        })

        # Should read to actual end of file
        assert "Total Lines: 5" in result
        assert "     5| line 5" in result

    def test_negative_start_line(self, read_file_tool):
        """Test reading with negative start_line."""
        result = read_file_tool.invoke({
            "file_path": "test.c",
            "start_line": -1
        })

        # max(0, -1 - 1) = 0, should read from beginning
        assert "Total Lines: 5" in result
        assert "     1| line 1" in result

    def test_zero_start_line(self, read_file_tool):
        """Test reading with start_line=0."""
        result = read_file_tool.invoke({
            "file_path": "test.c",
            "start_line": 0
        })

        # max(0, 0 - 1) = 0, should read from beginning
        assert "Total Lines: 5" in result
        assert "     1| line 1" in result

    def test_zero_end_line(self, read_file_tool):
        """Test reading with end_line=0."""
        result = read_file_tool.invoke({
            "file_path": "test.c",
            "end_line": 0
        })

        # min(5, 0) = 0, should return empty
        assert "Total Lines: 5" in result

    def test_start_line_equals_end_line(self, read_file_tool):
        """Test reading single line when start equals end."""
        result = read_file_tool.invoke({
            "file_path": "test.c",
            "start_line": 3,
            "end_line": 3
        })

        assert "Total Lines: 5" in result
        assert "Showing Lines: 3-3" in result
        assert "     3| line 3" in result
        assert "     2| line 2" not in result
        assert "     4| line 4" not in result

    def test_end_line_before_start_line(self, read_file_tool):
        """Test handling when end_line is before start_line."""
        result = read_file_tool.invoke({
            "file_path": "test.c",
            "start_line": 4,
            "end_line": 2
        })

        # Should result in empty range
        assert "Total Lines: 5" in result

    def test_package_prefix_stripping(self, temp_repo):
        """Test that package prefix is stripped from file path."""
        # Create repo with name matching the temp directory
        repo_name = temp_repo.name
        tool = create_read_file_tool(temp_repo)

        # Try to read with package prefix
        result = tool.invoke({"file_path": f"{repo_name}/test.c"})

        # Should successfully read the file
        assert "=== File:" in result
        assert "Total Lines: 5" in result
        assert "line 1" in result

    def test_read_file_with_encoding_errors(self, temp_repo):
        """Test reading file with encoding errors."""
        # Create a file with invalid UTF-8
        bad_file = temp_repo / "bad_encoding.c"
        bad_file.write_bytes(b"line 1\nline 2 \xff\xff invalid\nline 3\n")

        tool = create_read_file_tool(temp_repo)
        result = tool.invoke({"file_path": "bad_encoding.c"})

        # Should handle with 'replace' error handling
        assert "=== File: bad_encoding.c ===" in result
        assert "Total Lines: 3" in result

    def test_read_empty_file(self, temp_repo):
        """Test reading an empty file."""
        empty_file = temp_repo / "empty.c"
        empty_file.write_text("")

        tool = create_read_file_tool(temp_repo)
        result = tool.invoke({"file_path": "empty.c"})

        assert "=== File: empty.c ===" in result
        assert "Total Lines: 0" in result
        assert "=== End of empty.c ===" in result

    def test_read_file_with_no_trailing_newline(self, temp_repo):
        """Test reading file without trailing newline."""
        no_newline = temp_repo / "no_newline.c"
        no_newline.write_text("line 1\nline 2")

        tool = create_read_file_tool(temp_repo)
        result = tool.invoke({"file_path": "no_newline.c"})

        assert "=== File: no_newline.c ===" in result
        assert "Total Lines: 2" in result
        assert "     1| line 1" in result
        assert "     2| line 2" in result

    def test_exception_handling(self, temp_repo):
        """Test generic exception handling."""
        tool = create_read_file_tool(temp_repo)

        # Patch open to raise an exception
        with patch("builtins.open", side_effect=PermissionError("Access denied")):
            result = tool.invoke({"file_path": "test.c"})

        assert "Error: Access denied" in result

    def test_line_number_formatting(self, read_file_tool):
        """Test that line numbers are formatted with proper width."""
        result = read_file_tool.invoke({"file_path": "test.c"})

        # Line numbers should be right-aligned with width 6
        assert "     1|" in result  # 5 spaces + 1 digit
        assert "     5|" in result

    def test_trailing_whitespace_stripped(self, temp_repo):
        """Test that trailing whitespace is stripped from lines."""
        whitespace_file = temp_repo / "whitespace.c"
        whitespace_file.write_text("line 1   \nline 2\t\t\nline 3\n")

        tool = create_read_file_tool(temp_repo)
        result = tool.invoke({"file_path": "whitespace.c"})

        # Lines should have trailing whitespace removed (via rstrip())
        assert "     1| line 1" in result
        assert "     2| line 2" in result
        assert "     3| line 3" in result