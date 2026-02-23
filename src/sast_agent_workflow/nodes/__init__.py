"""
SAST Agent Workflow Nodes

This package contains the workflow node implementations:
- NAT-registered functions (pre_process, filter, etc.) at the top level
- Sub-agents (investigation) under sub_agents/
"""

import os
import sys

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

if PROJECT_ROOT not in sys.path:
    sys.path.insert(0, PROJECT_ROOT)

from .calculate_metrics import calculate_metrics  # noqa: E402
from .filter import filter  # noqa: E402
from .investigate import investigate  # noqa: E402
from .pre_process import pre_process  # noqa: E402
from .summarize_justifications import summarize_justifications  # noqa: E402
from .write_results import write_results  # noqa: E402

__all__ = [
    "pre_process",
    "filter",
    "investigate",
    "summarize_justifications",
    "calculate_metrics",
    "write_results",
]
