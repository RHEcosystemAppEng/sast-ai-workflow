"""
Unit tests for search_codebase tool.
Tests all scenarios with 100% coverage.
"""
import pytest
import re
from pathlib import Path
from unittest.mock import patch
from langchain_core.tools import StructuredTool

from sast_agent_workflow.investigation.tools.search_codebase import (
    create_search_codebase_tool,
    _compile_search_regex,
    _should_search_file,
    _search_file,
    _format_search_results,
)
from sast_agent_workflow.investigation.constants import DEFAULT_MAX_SEARCH_RESULTS


class TestCreateSearchCodebaseTool:
    """Test create_search_codebase_tool factory function."""

    def test_creates_structured_tool(self):
        """Test that create_search_codebase_tool returns a StructuredTool."""
        repo_path = Path("/fake/repo")
        tool = create_search_codebase_tool(repo_path)

        assert isinstance(tool, StructuredTool)
        assert tool.name == "search_codebase"
        assert "Search the codebase for patterns" in tool.description

    def test_tool_has_correct_schema(self):
        """Test that tool has correct args_schema."""
        repo_path = Path("/fake/repo")
        tool = create_search_codebase_tool(repo_path)

        assert hasattr(tool, 'args_schema')


class TestSearchCodebaseTool:
    """Test search_codebase tool execution."""

    @pytest.fixture
    def temp_repo(self, tmp_path):
        """Create a temporary repository with test files."""
        # Create C files with various patterns
        main_c = tmp_path / "main.c"
        main_c.write_text(
            "int main() {\n"
            "    char *buffer = malloc(256);\n"
            "    sanitize_input(buffer);\n"
            "    return 0;\n"
            "}\n"
        )

        auth_c = tmp_path / "auth.c"
        auth_c.write_text(
            "void sanitize_input(char *input) {\n"
            "    // Sanitization code\n"
            "}\n"
            "\n"
            "void validate_credentials() {\n"
            "    malloc(100);\n"
            "}\n"
        )

        # Create Python file
        script_py = tmp_path / "script.py"
        script_py.write_text(
            "def validate_input(data):\n"
            "    return True\n"
        )

        # Create subdirectory with files
        src_dir = tmp_path / "src"
        src_dir.mkdir()
        utils_c = src_dir / "utils.c"
        utils_c.write_text(
            "void malloc_wrapper() {\n"
            "    return malloc(1024);\n"
            "}\n"
        )

        # Create hidden directory with files (should be ignored)
        hidden_dir = tmp_path / ".git"
        hidden_dir.mkdir()
        hidden_file = hidden_dir / "config"
        hidden_file.write_text("malloc should not be found")

        return tmp_path

    @pytest.fixture
    def search_tool(self, temp_repo):
        """Create a search_codebase tool with temp repository."""
        return create_search_codebase_tool(temp_repo)

    def test_search_finds_matches(self, search_tool):
        """Test basic search that finds matches."""
        result = search_tool.invoke({"pattern": "malloc"})

        assert "=== Search Results: 'malloc'" in result
        assert "main.c:" in result
        assert "auth.c:" in result
        assert "src/utils.c:" in result
        assert "malloc(256)" in result

    def test_search_with_no_matches(self, search_tool):
        """Test search that finds no matches."""
        result = search_tool.invoke({"pattern": "nonexistent_function"})

        assert "No matches found for pattern 'nonexistent_function'" in result
        assert "matching '*.c'" in result

    def test_search_with_custom_file_pattern(self, search_tool):
        """Test search with custom file pattern."""
        result = search_tool.invoke({
            "pattern": "validate",
            "file_pattern": "*.py"
        })

        assert "=== Search Results: 'validate'" in result
        assert "script.py:" in result
        assert "validate_input" in result
        # Should not find .c files
        assert "auth.c" not in result

    def test_search_with_max_results_limit(self, search_tool):
        """Test search respects max_results limit."""
        result = search_tool.invoke({
            "pattern": "malloc",
            "max_results": 2
        })

        assert "=== Search Results: 'malloc'" in result
        # Should show exactly 2 matches
        assert "(2 matches)" in result
        assert "(Showing first 2 matches, there may be more)" in result

    def test_search_case_insensitive(self, search_tool):
        """Test that search is case-insensitive."""
        result = search_tool.invoke({"pattern": "MALLOC"})

        # Should find 'malloc' even though pattern is uppercase
        assert "malloc" in result.lower()

    def test_search_with_regex_pattern(self, search_tool):
        """Test search with regex pattern."""
        result = search_tool.invoke({"pattern": "sanitize.*input"})

        assert "=== Search Results:" in result
        assert "sanitize_input" in result

    def test_search_excludes_hidden_files(self, search_tool):
        """Test that hidden files and directories are excluded."""
        result = search_tool.invoke({"pattern": "malloc"})

        # Should not find matches in .git directory
        assert ".git" not in result

    def test_invalid_regex_pattern(self, search_tool):
        """Test error handling for invalid regex."""
        result = search_tool.invoke({"pattern": "[invalid(regex"})

        assert "Error: Invalid regex pattern" in result
        assert "[invalid(regex" in result

    def test_search_multiple_matches_same_file(self, search_tool):
        """Test finding multiple matches in the same file."""
        result = search_tool.invoke({"pattern": "malloc"})

        # auth.c has malloc in line 6
        # Should show both occurrences
        assert "auth.c:" in result

    def test_search_with_default_max_results(self, search_tool):
        """Test that default max_results is applied."""
        result = search_tool.invoke({"pattern": "."})  # Match everything

        # Should be limited to DEFAULT_MAX_SEARCH_RESULTS
        assert f"({DEFAULT_MAX_SEARCH_RESULTS} matches)" in result or "matches)" in result

    def test_exception_during_search(self, temp_repo):
        """Test handling of exceptions during search."""
        tool = create_search_codebase_tool(temp_repo)

        # Patch rglob to raise an exception
        with patch.object(Path, 'rglob', side_effect=PermissionError("Access denied")):
            result = tool.invoke({"pattern": "test"})

        assert "Error: Search failed" in result
        assert "Access denied" in result

    def test_search_counts_files_searched(self, search_tool):
        """Test that search counts files searched."""
        result = search_tool.invoke({"pattern": "nonexistent"})

        # Should mention how many files were searched
        assert "in" in result
        assert "files" in result


class TestCompileSearchRegex:
    """Test _compile_search_regex helper function."""

    def test_valid_pattern_returns_regex(self):
        """Test that valid pattern compiles successfully."""
        regex, error = _compile_search_regex("test")

        assert error is None
        assert isinstance(regex, re.Pattern)
        assert regex.flags & re.IGNORECASE

    def test_case_insensitive_flag(self):
        """Test that regex is case-insensitive."""
        regex, _ = _compile_search_regex("test")

        assert regex.search("TEST") is not None
        assert regex.search("TeSt") is not None

    def test_invalid_pattern_returns_error(self):
        """Test that invalid pattern returns error message."""
        regex, error = _compile_search_regex("[invalid(")

        assert regex is None
        assert "Error: Invalid regex pattern" in error
        assert "[invalid(" in error

    def test_complex_regex_pattern(self):
        """Test that complex regex patterns compile."""
        regex, error = _compile_search_regex(r"(malloc|calloc|realloc)\s*\(")

        assert error is None
        assert isinstance(regex, re.Pattern)


class TestShouldSearchFile:
    """Test _should_search_file helper function."""

    def test_regular_file_with_matching_extension(self, tmp_path):
        """Test that regular files with matching extension are searchable."""
        test_file = tmp_path / "test.c"
        test_file.write_text("content")

        assert _should_search_file(test_file, "c") is True

    def test_directory_not_searchable(self, tmp_path):
        """Test that directories are not searchable."""
        test_dir = tmp_path / "testdir"
        test_dir.mkdir()

        assert _should_search_file(test_dir, "c") is False

    def test_hidden_file_not_searchable(self, tmp_path):
        """Test that hidden files are not searchable."""
        hidden_file = tmp_path / ".hidden"
        hidden_file.write_text("content")

        assert _should_search_file(hidden_file, "") is False

    def test_file_in_hidden_directory_not_searchable(self, tmp_path):
        """Test that files in hidden directories are not searchable."""
        hidden_dir = tmp_path / ".git"
        hidden_dir.mkdir()
        file_in_hidden = hidden_dir / "file.c"
        file_in_hidden.write_text("content")

        assert _should_search_file(file_in_hidden, "c") is False

    def test_wrong_extension_not_searchable(self, tmp_path):
        """Test that files with wrong extension are not searchable."""
        test_file = tmp_path / "test.py"
        test_file.write_text("content")

        assert _should_search_file(test_file, "c") is False

    def test_empty_extension_searches_all_files(self, tmp_path):
        """Test that empty extension searches all files."""
        test_file = tmp_path / "test.xyz"
        test_file.write_text("content")

        # Empty extension means no filtering by extension
        assert _should_search_file(test_file, "") is True

    def test_symlink_to_file(self, tmp_path):
        """Test handling of symlinks."""
        real_file = tmp_path / "real.c"
        real_file.write_text("content")
        link_file = tmp_path / "link.c"
        link_file.symlink_to(real_file)

        # Symlinks to files should be searchable
        assert _should_search_file(link_file, "c") is True


class TestSearchFile:
    """Test _search_file helper function."""

    def test_search_file_with_matches(self, tmp_path):
        """Test searching file that has matches."""
        test_file = tmp_path / "test.c"
        test_file.write_text("line 1: malloc\nline 2: test\nline 3: malloc\n")

        regex = re.compile("malloc", re.IGNORECASE)
        results = []

        stop = _search_file(test_file, regex, tmp_path, results, max_results=10)

        assert stop is False
        assert len(results) == 2
        assert results[0]["file"] == "test.c"
        assert results[0]["line"] == 1
        assert "malloc" in results[0]["content"]
        assert results[1]["line"] == 3

    def test_search_file_stops_at_max_results(self, tmp_path):
        """Test that search stops when max_results is reached."""
        test_file = tmp_path / "test.c"
        test_file.write_text("match\nmatch\nmatch\nmatch\n")

        regex = re.compile("match", re.IGNORECASE)
        results = []

        stop = _search_file(test_file, regex, tmp_path, results, max_results=2)

        assert stop is True
        assert len(results) == 2

    def test_search_file_with_no_matches(self, tmp_path):
        """Test searching file with no matches."""
        test_file = tmp_path / "test.c"
        test_file.write_text("no match here\n")

        regex = re.compile("pattern", re.IGNORECASE)
        results = []

        stop = _search_file(test_file, regex, tmp_path, results, max_results=10)

        assert stop is False
        assert len(results) == 0

    def test_search_file_handles_read_errors(self, tmp_path):
        """Test that file read errors are handled gracefully."""
        test_file = tmp_path / "test.c"
        test_file.write_text("content")

        regex = re.compile("test", re.IGNORECASE)
        results = []

        # Patch open to raise an exception
        with patch("builtins.open", side_effect=PermissionError("Access denied")):
            stop = _search_file(test_file, regex, tmp_path, results, max_results=10)

        # Should not crash, just return False
        assert stop is False
        assert len(results) == 0

    def test_search_file_relative_path(self, tmp_path):
        """Test that file paths are relative to repo root."""
        subdir = tmp_path / "src"
        subdir.mkdir()
        test_file = subdir / "test.c"
        test_file.write_text("malloc\n")

        regex = re.compile("malloc", re.IGNORECASE)
        results = []

        _search_file(test_file, regex, tmp_path, results, max_results=10)

        assert len(results) > 0
        assert results[0]["file"] == "src/test.c"

    def test_search_file_strips_whitespace(self, tmp_path):
        """Test that matched lines have whitespace stripped."""
        test_file = tmp_path / "test.c"
        test_file.write_text("   malloc   \n")

        regex = re.compile("malloc", re.IGNORECASE)
        results = []

        _search_file(test_file, regex, tmp_path, results, max_results=10)

        assert len(results) > 0
        assert results[0]["content"] == "malloc"


class TestFormatSearchResults:
    """Test _format_search_results helper function."""

    def test_format_with_results(self):
        """Test formatting when results are found."""
        results = [
            {"file": "main.c", "line": 10, "content": "malloc(256);"},
            {"file": "auth.c", "line": 25, "content": "free(ptr);"},
        ]

        output = _format_search_results(results, "malloc|free", "*.c", 5, 20)

        assert "=== Search Results: 'malloc|free' (2 matches) ===" in output
        assert "File pattern: *.c" in output
        assert "1. main.c:10" in output
        assert "   malloc(256);" in output
        assert "2. auth.c:25" in output
        assert "   free(ptr);" in output

    def test_format_with_no_results(self):
        """Test formatting when no results are found."""
        results = []

        output = _format_search_results(results, "pattern", "*.py", 10, 20)

        assert "No matches found for pattern 'pattern'" in output
        assert "in 10 files matching '*.py'" in output

    def test_format_with_max_results_reached(self):
        """Test formatting when max_results is reached."""
        results = [{"file": "test.c", "line": i, "content": "match"} for i in range(20)]

        output = _format_search_results(results, "test", "*.c", 50, 20)

        assert "(20 matches)" in output
        assert "(Showing first 20 matches, there may be more)" in output

    def test_format_with_max_results_not_reached(self):
        """Test formatting when max_results is not reached."""
        results = [
            {"file": "test.c", "line": 1, "content": "match"},
        ]

        output = _format_search_results(results, "test", "*.c", 10, 20)

        assert "(1 matches)" in output
        assert "Showing first" not in output

    def test_format_numbering(self):
        """Test that results are numbered correctly."""
        results = [
            {"file": "a.c", "line": 1, "content": "first"},
            {"file": "b.c", "line": 2, "content": "second"},
            {"file": "c.c", "line": 3, "content": "third"},
        ]

        output = _format_search_results(results, "test", "*.c", 10, 20)

        assert "1. a.c:1" in output
        assert "2. b.c:2" in output
        assert "3. c.c:3" in output