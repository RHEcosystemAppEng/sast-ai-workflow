"""
Common utilities for SAST workflow tracker operations.

This module provides reusable functions for converting tracker data
to different formats for use across multiple workflow nodes.
Used by both Calculate_Metrics and Write_Results nodes.
"""

import logging
from typing import List, Tuple

from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.SummaryInfo import SummaryInfo
from dto.Issue import Issue

logger = logging.getLogger(__name__)


def convert_tracker_to_summary_data(tracker: SASTWorkflowTracker, include_non_final: bool = False) -> List[Tuple[Issue, SummaryInfo]]:
    """
    Convert SASTWorkflowTracker to summary_data format expected by ExcelWriter and EvaluationSummary.
    
    Args:
        tracker: SASTWorkflowTracker containing issues and analysis results
        include_non_final: Whether to include issues with is_final="FALSE" (default: False)
        
    Returns:
        List of (Issue, SummaryInfo) tuples ready for evaluation and output writing
    """
    summary_data = []
    
    for issue_id, per_issue_data in tracker.issues.items():
        if per_issue_data.analysis_response:
            # Include based on is_final flag
            if include_non_final or per_issue_data.analysis_response.is_final == "TRUE":
                
                summary_info = SummaryInfo(
                    response=per_issue_data.analysis_response,
                    metrics={},
                    critique_response=per_issue_data.analysis_response,
                    context=""
                )
                
                summary_data.append((per_issue_data.issue, summary_info))
    
    return summary_data