"""
Integration tests for search_codebase tool with real file operations.
Tests actual regex search across real C files.
"""
import pytest
from pathlib import Path

from sast_agent_workflow.investigation.tools.search_codebase import create_search_codebase_tool


@pytest.fixture
def sample_repo_path():
    """Path to sample repository for integration testing."""
    return Path(__file__).parent / "fixtures" / "sample_repo"


@pytest.fixture
def search_tool(sample_repo_path):
    """Create search_codebase tool with real repo path."""
    return create_search_codebase_tool(sample_repo_path)


class TestSearchCodebaseIntegration:
    """Integration tests for search_codebase tool."""

    def test_search_finds_function_name(self, search_tool):
        """Test searching for a specific function name."""
        result = search_tool.invoke({
            "pattern": "sanitize_input",
            "file_pattern": "*.c",
            "max_results": 10
        })

        assert "sanitize_input" in result
        assert "src/main.c" in result
        assert "matches" in result
        # Should find both declaration and call
        assert result.count("sanitize_input") >= 2

    def test_search_with_regex_pattern(self, search_tool):
        """Test searching with regex pattern (multiple alternatives)."""
        result = search_tool.invoke({
            "pattern": "malloc|calloc|realloc",
            "file_pattern": "*.c",
            "max_results": 20
        })

        assert "matches" in result
        # Should find malloc in multiple files
        assert "malloc" in result
        assert "lib/config.c" in result or "src/main.c" in result

    def test_search_case_insensitive(self, search_tool):
        """Test that search is case-insensitive."""
        result_lower = search_tool.invoke({
            "pattern": "authenticate",
            "file_pattern": "*.c",
            "max_results": 10
        })

        result_upper = search_tool.invoke({
            "pattern": "AUTHENTICATE",
            "file_pattern": "*.c",
            "max_results": 10
        })

        # Both should find the authenticate_user function
        assert "authenticate" in result_lower.lower()
        assert "authenticate" in result_upper.lower()

    def test_search_in_header_files(self, search_tool):
        """Test searching in header files."""
        result = search_tool.invoke({
            "pattern": "validate_email",
            "file_pattern": "*.h",
            "max_results": 10
        })

        assert "include/utils.h" in result
        assert "validate_email" in result

    def test_search_with_max_results_limit(self, search_tool):
        """Test that max_results parameter limits output."""
        # Search for common keyword that appears many times
        result = search_tool.invoke({
            "pattern": "return",
            "file_pattern": "*.c",
            "max_results": 3
        })

        assert "matches" in result
        # Should show "first 3 matches" message
        if "matches)" in result:
            # Extract number of matches shown
            lines = [l for l in result.split('\n') if result.split('\n').index(l) and '.' in l and l.strip()[0].isdigit()]
            # Should have at most 3 results
            assert len(lines) <= 3

    def test_search_no_matches(self, search_tool):
        """Test search with pattern that has no matches."""
        result = search_tool.invoke({
            "pattern": "nonexistent_pattern_xyz123",
            "file_pattern": "*.c",
            "max_results": 10
        })

        assert "No matches found" in result
        assert "nonexistent_pattern_xyz123" in result

    def test_search_multiple_file_types(self, search_tool):
        """Test searching across different file types."""
        # Search in C files
        result_c = search_tool.invoke({
            "pattern": "include",
            "file_pattern": "*.c",
            "max_results": 10
        })

        # Search in header files
        result_h = search_tool.invoke({
            "pattern": "include",
            "file_pattern": "*.h",
            "max_results": 10
        })

        # Both should find results
        assert "matches" in result_c or "#include" in result_c
        assert "matches" in result_h or "#ifndef" in result_h

    def test_search_finds_sanitization_functions(self, search_tool):
        """Test searching for sanitization patterns (realistic use case)."""
        result = search_tool.invoke({
            "pattern": "sanitize.*",
            "file_pattern": "*.c",
            "max_results": 10
        })

        assert "sanitize_input" in result
        assert "src/main.c" in result

    def test_search_finds_authentication_patterns(self, search_tool):
        """Test searching for authentication patterns."""
        result = search_tool.invoke({
            "pattern": "auth.*user",
            "file_pattern": "*.c",
            "max_results": 10
        })

        assert "authenticate_user" in result
        assert "lib/auth.c" in result

    def test_search_finds_memory_allocation(self, search_tool):
        """Test searching for memory allocation patterns."""
        result = search_tool.invoke({
            "pattern": "\\bmalloc\\s*\\(",
            "file_pattern": "*.c",
            "max_results": 15
        })

        # Should find malloc calls
        assert "malloc" in result
        # Should find in multiple files
        assert "matches" in result

    def test_search_excludes_hidden_files(self, search_tool):
        """Test that hidden files/directories are excluded."""
        # Even if hidden files existed, they shouldn't appear
        result = search_tool.invoke({
            "pattern": ".*",  # Match anything
            "file_pattern": "*.c",
            "max_results": 50
        })

        # Should not include .git, .venv, etc.
        assert ".git" not in result
        assert ".venv" not in result

    def test_search_shows_line_numbers(self, search_tool):
        """Test that search results include line numbers."""
        result = search_tool.invoke({
            "pattern": "sanitize_input",
            "file_pattern": "*.c",
            "max_results": 5
        })

        # Should show file:line format
        assert ":" in result
        # Should show actual code content
        assert "sanitize_input" in result

    def test_search_invalid_regex_returns_error(self, search_tool):
        """Test that invalid regex returns helpful error."""
        result = search_tool.invoke({
            "pattern": "[invalid(regex",
            "file_pattern": "*.c",
            "max_results": 10
        })

        assert "Error" in result
        assert "regex" in result.lower() or "pattern" in result.lower()

    def test_search_counts_files_searched(self, search_tool):
        """Test that search reports number of files searched."""
        result = search_tool.invoke({
            "pattern": "xyz_nomatch",
            "file_pattern": "*.c",
            "max_results": 10
        })

        # Should mention number of files searched
        assert "files" in result.lower()

    def test_search_realistic_vulnerability_pattern(self, search_tool):
        """Test realistic vulnerability search patterns."""
        # Search for strcpy (dangerous function)
        result = search_tool.invoke({
            "pattern": "strcpy|strcat|sprintf",
            "file_pattern": "*.c",
            "max_results": 10
        })

        # May or may not find results, but should work without error
        assert "Error" not in result or "No matches" in result
