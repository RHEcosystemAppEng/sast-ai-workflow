"""
SAST Agent Workflow Tools

This package contains individual function implementations for the SAST agent workflow.
Each function should be in its own file for better organization and maintainability.
"""

# ruff: noqa: E402
import os
import sys

# Calculate project root once at module level for use across all tools
PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

# Add project root to sys.path once at module level to avoid redundant manipulations
if PROJECT_ROOT not in sys.path:
    sys.path.insert(0, PROJECT_ROOT)

from .calculate_metrics import calculate_metrics  # noqa: E402
from .data_fetcher import data_fetcher  # noqa: E402
from .evaluate_analysis import evaluate_analysis  # noqa: E402
from .filter import filter  # noqa: E402
from .investigate_issue import investigate_issue  # noqa: E402
from .judge_llm_analysis import judge_llm_analysis  # noqa: E402
from .pre_process import pre_process  # noqa: E402
from .summarize_justifications import summarize_justifications  # noqa: E402
from .write_results import write_results  # noqa: E402

__all__ = [
    "pre_process",
    "filter",
    "investigate_issue",
    "summarize_justifications",
    "calculate_metrics",
    "write_results",
    # Old functions (backward compatibility)
    "data_fetcher",
    "judge_llm_analysis",
    "evaluate_analysis",
]
