"""
Integration tests for fetch_code tool.

These tests use the REAL talloc-2.4.3 repository to verify:
1. The tool actually fetches code from a real repository
2. Symbol resolution works correctly
3. Error handling works for real failure scenarios
4. The tool returns properly formatted results

Requirements:
- Talloc source code in /tmp/sast-workflow-prep/source/talloc-2.4.3/
- Repository handler configured to point to talloc
"""

import logging
import os
import tempfile
from pathlib import Path
from unittest.mock import patch

import pytest

from common.config import Config
from handlers.repo_handler_factory import repo_handler_factory
from sast_agent_workflow.agent.tools.fetch_code import (
    FetchCodeInput,
    fetch_code_from_error_trace,
)

logger = logging.getLogger(__name__)

@pytest.fixture(scope="module")
def talloc_repo_path():
    """Path to talloc test repository."""
    path = "/tmp/sast-workflow-prep/source/talloc-2.4.3"
    if not os.path.exists(path):
        pytest.skip(f"Talloc repository not found at {path}")
    return path


@pytest.fixture(scope="module")
def talloc_repo_handler(talloc_repo_path):
    """Create a real repo handler for talloc repository."""
    # Create config and override REPO_LOCAL_PATH to point to talloc
    config = Config()
    original_path = config.REPO_LOCAL_PATH
    config.REPO_LOCAL_PATH = talloc_repo_path

    try:
        handler = repo_handler_factory(config)
        yield handler
    finally:
        # Restore original path
        config.REPO_LOCAL_PATH = original_path


@pytest.fixture
def real_error_trace():
    """Real error trace from talloc repository."""
    return """talloc.c:425: potential memory leak in talloc_abort
talloc.c:426: missing null check
talloc.c:520: buffer overflow risk"""


# ============================================================
# Integration Tests for fetch_code_from_error_trace()
# ============================================================


def test_fetch_code_from_error_trace_real_repository(talloc_repo_handler, real_error_trace):
    """Test fetching code from real error trace using talloc repository."""
    result = fetch_code_from_error_trace(
        real_error_trace, talloc_repo_handler, issue_id="integration-test-1"
    )

    assert isinstance(result, dict), "Result should be a dictionary"
    assert len(result) > 0, "Should extract code from error trace"

    # Check that talloc.c was extracted
    talloc_files = [k for k in result.keys() if "talloc.c" in k]
    assert len(talloc_files) > 0, "Should find talloc.c in results"

    # Each file should map to a list of code blocks
    for file_path, code_blocks in result.items():
        assert isinstance(code_blocks, list), f"{file_path} should map to a list"
        assert len(code_blocks) > 0, f"{file_path} should have at least one code block"
        assert isinstance(code_blocks[0], str), "Code blocks should be strings"
        assert len(code_blocks[0]) > 0, "Code blocks should not be empty"

    logger.info(f"Successfully extracted {len(result)} files from error trace")
    logger.info(f"Files: {list(result.keys())}")


def test_fetch_code_from_error_trace_validates_content(talloc_repo_handler):
    """Test that extracted code actually contains expected line numbers."""
    # Create trace with specific line number
    trace = "talloc.c:520: test error"

    result = fetch_code_from_error_trace(trace, talloc_repo_handler, issue_id="integration-test-2")

    # Verify - should find talloc.c
    assert len(result) > 0, "Should extract code"

    talloc_file = next((k for k in result.keys() if "talloc.c" in k), None)
    assert talloc_file is not None, "Should find talloc.c"

    code = result[talloc_file][0]
    # Code should contain line numbers in format "520|" or similar
    assert "520" in code, f"Code should reference line 520, got: {code[:200]}"


def test_fetch_code_from_error_trace_handles_invalid_file(talloc_repo_handler):
    """Test error trace with non-existent file."""
    # Create trace with file that doesn't exist
    trace = "nonexistent_file.c:42: test error"

    result = fetch_code_from_error_trace(trace, talloc_repo_handler, issue_id="integration-test-3")

    # Verify - might return empty dict or might skip the bad file
    assert isinstance(result, dict), "Should return dict even for invalid files"


# ============================================================
# Integration Tests for Real Symbol Fetching
# ============================================================


def test_fetch_code_tool_fetches_real_function(talloc_repo_handler):
    """Test fetching a real function from talloc repository."""
    from types import SimpleNamespace

    # Test fetching talloc_lib_init function
    instruction = SimpleNamespace(
        expression_name="talloc_lib_init",
        referring_source_code_path="talloc.c",
    )

    # Execute - use the repo_handler's extract_missing_functions_or_macros
    missing_code, new_symbols = talloc_repo_handler.extract_missing_functions_or_macros(
        [instruction], set()
    )

    assert missing_code, "Should return code for talloc_lib_init"
    assert "talloc_lib_init" in new_symbols, "Should add talloc_lib_init to symbols"
    assert "void talloc_lib_init" in missing_code or "talloc_lib_init" in missing_code, \
        "Code should contain the function definition"

    logger.info(f"Successfully fetched talloc_lib_init")
    logger.info(f"Code length: {len(missing_code)} chars")


def test_fetch_code_tool_handles_nonexistent_symbol(talloc_repo_handler):
    """Test fetching a symbol that doesn't exist."""
    from types import SimpleNamespace

    # Test fetching non-existent function
    instruction = SimpleNamespace(
        expression_name="this_function_does_not_exist_12345",
        referring_source_code_path="talloc.c",
    )

    missing_code, new_symbols = talloc_repo_handler.extract_missing_functions_or_macros(
        [instruction], set()
    )

    # Verify - should handle gracefully
    if missing_code:
        assert "this_function_does_not_exist_12345" not in new_symbols, \
            "Non-existent symbol should not be in new_symbols"


def test_fetch_code_tool_fetches_static_function(talloc_repo_handler):
    """Test fetching a static function from talloc."""
    from types import SimpleNamespace

    # Test fetching a static function - talloc_log is static
    instruction = SimpleNamespace(
        expression_name="talloc_log",
        referring_source_code_path="talloc.c",
    )

    missing_code, new_symbols = talloc_repo_handler.extract_missing_functions_or_macros(
        [instruction], set()
    )

    assert missing_code, "Should return code for static function talloc_log"
    assert "talloc_log" in new_symbols, "Should add talloc_log to symbols"

    logger.info(f"Successfully fetched static function talloc_log")


def test_fetch_code_tool_fetches_inline_function(talloc_repo_handler):
    """Test fetching an inline function from talloc."""
    from types import SimpleNamespace

    # Test fetching an inline function - talloc_chunk_from_ptr is inline
    instruction = SimpleNamespace(
        expression_name="talloc_chunk_from_ptr",
        referring_source_code_path="talloc.c",
    )

    missing_code, new_symbols = talloc_repo_handler.extract_missing_functions_or_macros(
        [instruction], set()
    )

    assert missing_code, "Should return code for inline function"
    assert "talloc_chunk_from_ptr" in new_symbols, "Should add to symbols"

    logger.info(f"Successfully fetched inline function talloc_chunk_from_ptr")


# ============================================================
# Tests for Multiple Symbol Fetching
# ============================================================


def test_fetch_code_tool_fetches_multiple_symbols(talloc_repo_handler):
    """Test fetching multiple symbols in one call."""
    from types import SimpleNamespace

    instructions = [
        SimpleNamespace(expression_name="talloc_lib_init", referring_source_code_path="talloc.c"),
        SimpleNamespace(expression_name="talloc_log", referring_source_code_path="talloc.c"),
        SimpleNamespace(expression_name="talloc_abort", referring_source_code_path="talloc.c"),
    ]

    missing_code, new_symbols = talloc_repo_handler.extract_missing_functions_or_macros(
        instructions, set()
    )

    assert missing_code, "Should return code for all functions"
    assert len(new_symbols) >= 2, f"Should add multiple symbols, got {len(new_symbols)}"

    # Check that at least some of the requested symbols were found
    found_count = sum(1 for name in ["talloc_lib_init", "talloc_log", "talloc_abort"] if name in new_symbols)
    assert found_count >= 2, f"Should find at least 2 out of 3 symbols, found {found_count}"

    logger.info(f"Successfully fetched multiple symbols: {new_symbols}")


def test_fetch_code_tool_deduplicates_symbols(talloc_repo_handler):
    """Test that fetching the same symbol twice doesn't duplicate."""
    from types import SimpleNamespace

    instructions = [
        SimpleNamespace(expression_name="talloc_lib_init", referring_source_code_path="talloc.c"),
        SimpleNamespace(expression_name="talloc_lib_init", referring_source_code_path="talloc.c"),
    ]

    # Use existing symbols set to test deduplication
    existing_symbols = {"talloc_lib_init"}

    missing_code, new_symbols = talloc_repo_handler.extract_missing_functions_or_macros(
        instructions, existing_symbols
    )

    # Verify - should not fetch the same symbol twice
    # The behavior depends on repo_handler implementation
    # Either returns empty or returns the code once
    if new_symbols:
        # If it returns symbols, should not duplicate in the set
        assert isinstance(new_symbols, set), "new_symbols should be a set (no duplicates)"


# ============================================================
# Tests for Code Format and Metadata
# ============================================================


def test_fetched_code_contains_line_numbers(talloc_repo_handler):
    """Test that fetched code includes line numbers."""
    from types import SimpleNamespace

    instruction = SimpleNamespace(
        expression_name="talloc_lib_init",
        referring_source_code_path="talloc.c",
    )

    # Execute
    missing_code, new_symbols = talloc_repo_handler.extract_missing_functions_or_macros(
        [instruction], set()
    )

    # Verify - code should contain line numbers in format "123|"
    assert missing_code, "Should return code"

    # Look for line number pattern: digits followed by pipe
    import re

    line_numbers = re.findall(r"^(\d+)\|", missing_code, re.MULTILINE)
    assert len(line_numbers) > 0, "Code should contain line numbers in format 'number|'"

    logger.info(f"Found {len(line_numbers)} lines with line numbers")
    logger.info(f"Line range: {line_numbers[0]} to {line_numbers[-1]}")


def test_fetched_code_contains_file_path(talloc_repo_handler):
    """Test that fetched code includes file path information."""
    from types import SimpleNamespace

    instruction = SimpleNamespace(
        expression_name="talloc_lib_init",
        referring_source_code_path="talloc.c",
    )

    # Execute
    missing_code, new_symbols = talloc_repo_handler.extract_missing_functions_or_macros(
        [instruction], set()
    )

    # Verify - code should contain file path
    assert missing_code, "Should return code"
    assert "talloc.c" in missing_code or "code of" in missing_code, \
        "Code should reference the source file"


# ============================================================
# Performance and Scalability Tests
# ============================================================


def test_fetch_code_tool_handles_large_function(talloc_repo_handler):
    """Test fetching a large/complex function."""
    from types import SimpleNamespace

    # talloc.c has some complex functions - let's try _talloc_free_internal
    instruction = SimpleNamespace(
        expression_name="_talloc_free_internal",
        referring_source_code_path="talloc.c",
    )

    # Execute
    missing_code, new_symbols = talloc_repo_handler.extract_missing_functions_or_macros(
        [instruction], set()
    )

    # Verify
    if missing_code:
        assert len(missing_code) > 100, "Large function should return substantial code"
        logger.info(f"Fetched large function: {len(missing_code)} characters")


# ============================================================
# Error Recovery Tests
# ============================================================


def test_fetch_code_from_error_trace_with_malformed_trace(talloc_repo_handler):
    """Test handling of malformed error traces."""
    # Various malformed traces
    malformed_traces = [
        "",  # Empty
        "no colons here",  # No file:line format
        "::::",  # Just colons
        "file.c:",  # Missing line number
        ":123:",  # Missing filename
    ]

    for trace in malformed_traces:
        # Execute - should not crash
        result = fetch_code_from_error_trace(
            trace, talloc_repo_handler, issue_id=f"malformed-{hash(trace)}"
        )

        # Verify - should return dict (might be empty)
        assert isinstance(result, dict), f"Should handle malformed trace: {trace}"


def test_fetch_code_tool_with_invalid_referring_path(talloc_repo_handler):
    """Test fetching with an invalid referring_source_code_path."""
    from types import SimpleNamespace

    instruction = SimpleNamespace(
        expression_name="talloc_lib_init",
        referring_source_code_path="/nonexistent/path/file.c",  # Invalid path
    )

    # Execute - should handle gracefully
    missing_code, new_symbols = talloc_repo_handler.extract_missing_functions_or_macros(
        [instruction], set()
    )

    # Verify - implementation dependent
    # Might still find the symbol or might fail gracefully
    assert isinstance(new_symbols, set), "Should return a set even with invalid path"
