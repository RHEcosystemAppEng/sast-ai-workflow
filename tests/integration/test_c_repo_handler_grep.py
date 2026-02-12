"""
Integration tests for CRepoHandler grep-based lookup methods.

Verifies that _get_macro_definition_file_location and _get_function_definition_file_location
run the real grep command correctly against a temporary C source tree.
"""

from unittest.mock import patch

import pytest

from src.handlers.c_repo_handler import CRepoHandler

# Minimal C source used as grep target
_SAMPLE_C = """
#define MY_MACRO 42
#define ANOTHER_MACRO(x) ((x) + 1)

void my_func(void) {
    return;
}
"""


@pytest.fixture
def repo_handler(tmp_path):
    """CRepoHandler with repo_local_path pointing at tmp_path (no real init)."""
    with patch.object(CRepoHandler, "__init__", return_value=None):
        handler = CRepoHandler(None)
    handler.repo_local_path = str(tmp_path)
    return handler


@pytest.fixture
def sample_repo(tmp_path):
    """Create a minimal C file in tmp_path for grep to find."""
    src = tmp_path / "src"
    src.mkdir()
    (src / "sample.c").write_text(_SAMPLE_C)
    return tmp_path


def test_get_macro_definition_file_location_finds_define(repo_handler, sample_repo):
    """Grep for #define MY_MACRO returns correct file and line."""
    repo_handler.repo_local_path = str(sample_repo)
    file_path, code_line_number = repo_handler._get_macro_definition_file_location("MY_MACRO")

    assert "sample.c" in file_path
    assert code_line_number == "2"  # #define MY_MACRO 42 is on line 2 of _SAMPLE_C


def test_get_macro_definition_file_location_unknown_macro_returns_empty(repo_handler, sample_repo):
    """Grep for non-existent macro returns empty strings."""
    repo_handler.repo_local_path = str(sample_repo)
    file_path, code_line_number = repo_handler._get_macro_definition_file_location(
        "NONEXISTENT_MACRO"
    )

    assert file_path == ""
    assert code_line_number == ""


def test_get_function_definition_file_location_finds_function(repo_handler, sample_repo):
    """Grep for function definition returns correct file and line."""
    repo_handler.repo_local_path = str(sample_repo)
    file_path, code_line_number = repo_handler._get_function_definition_file_location("my_func")

    assert "sample.c" in file_path
    assert code_line_number == "5"  # void my_func(void) { is on line 5 of _SAMPLE_C


def test_get_function_definition_file_location_unknown_function_returns_empty(
    repo_handler, sample_repo
):
    """Grep for non-existent function returns empty strings."""
    repo_handler.repo_local_path = str(sample_repo)
    file_path, code_line_number = repo_handler._get_function_definition_file_location(
        "nonexistent_func"
    )

    assert file_path == ""
    assert code_line_number == ""
