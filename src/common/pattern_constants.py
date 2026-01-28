"""
Constants for SAST false positive pattern integration.
"""

import os
from pathlib import Path

# Default patterns directory location
# In Tekton pipelines, this will be overridden by PATTERNS_DIR env var
DEFAULT_PATTERNS_DIR = os.getenv("PATTERNS_DIR", "/shared-data/patterns")

# Issue type keywords for pattern file matching
# Maps issue types to filename patterns
ISSUE_TYPE_KEYWORDS = {
    'RESOURCE_LEAK': ['resource_leak', 'resourceleak', '_resource_leak_'],
    'UNINIT': ['uninit', '_uninit_'],
    'OVERRUN': ['overrun', '_overrun_'],
    'INTEGER_OVERFLOW': ['integer_overflow', 'integeroverflow', '_integer_overflow_'],
    'USE_AFTER_FREE': ['use_after_free', 'useafterfree', '_use_after_free_'],
    'CPPCHECK_WARNING': ['cppcheck_warning', 'cppcheck', '_cppcheck_warning_'],
    'BUFFER_SIZE': ['buffer_size', 'buffersize', '_buffer_size_'],
    'COMPILER_WARNING': ['compiler_warning', 'compilerwarning', '_compiler_warning_'],
    'COPY_PASTE_ERROR': ['copy_paste_error', 'copypasteerror', '_copy_paste_error_'],
}


def get_patterns_directory() -> Path:
    """
    Get the patterns directory path.

    Returns:
        Path to patterns directory
    """
    patterns_dir = Path(DEFAULT_PATTERNS_DIR)
    return patterns_dir


def is_patterns_available() -> bool:
    """
    Check if patterns directory exists and is accessible.

    Returns:
        True if patterns are available, False otherwise
    """
    patterns_dir = get_patterns_directory()
    return patterns_dir.exists() and patterns_dir.is_dir()