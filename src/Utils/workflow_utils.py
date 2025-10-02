"""
Common utilities for NAT workflow.
"""

from enum import Enum
import logging
from typing import List, Tuple

from dto.SASTWorkflowModels import PerIssueData, SASTWorkflowTracker
from dto.SummaryInfo import SummaryInfo
from dto.Issue import Issue
from Utils.output_utils import filter_items_for_evaluation
from dto.LLMResponse import FinalStatus

logger = logging.getLogger(__name__)


def build_analysis_context(per_issue: PerIssueData) -> str:
    """
    Build full analysis context by combining source_code, similar_known_issues, and other relevant data.
    """
    # Build source code context
    source_code_context = ""
    if per_issue.source_code:
        source_code_parts = []
        for file_path, code_snippets in per_issue.source_code.items():
            for snippet in code_snippets:
                source_code_parts.append(f"\ncode of {file_path} file:\n{snippet}")
        source_code_context = "".join(source_code_parts)
    
    # Combine source code and examples in structured format
    context = (
        f"*** Source Code Context ***\n{source_code_context}\n\n"
        f"*** Examples ***\n{per_issue.similar_known_issues}"
    )
    
    return context


def convert_tracker_to_summary_data(tracker: SASTWorkflowTracker, include_non_final: bool = True, filter_failed: bool = True) -> List[Tuple[Issue, SummaryInfo]]:
    """
    Convert SASTWorkflowTracker to summary_data format expected by ExcelWriter and EvaluationSummary.
    
    Args:
        tracker: SASTWorkflowTracker containing issues and analysis results
        include_non_final: Whether to include issues with is_final="FALSE" (default: True)
        filter_failed: Whether to filter out failed issues using output_utils (default: True)
        
    Returns:
        List of (Issue, SummaryInfo) tuples ready for evaluation and output writing
    """
    summary_data = []
    
    for issue_id, per_issue_data in tracker.issues.items():
        if per_issue_data.analysis_response:
            # Include based on is_final flag
            if include_non_final or per_issue_data.analysis_response.is_final == FinalStatus.TRUE.value:
                
                summary_info = SummaryInfo(
                    response=per_issue_data.analysis_response,
                    metrics={},
                    critique_response=per_issue_data.analysis_response,
                    context=build_analysis_context(per_issue_data)
                )
                
                summary_data.append((per_issue_data.issue, summary_info))
    
    # Filter out failed items if requested
    if filter_failed and summary_data:
        summary_data, failed_item_ids = filter_items_for_evaluation(summary_data)
        if failed_item_ids:
            logger.warning(f"Filtered out {len(failed_item_ids)} failed items: {failed_item_ids}")
    
    return summary_data

def get_linear_edges() -> List[Tuple[str, str]]:
        """
        Get all linear (non-conditional) edges in the workflow.
        
        Returns:
            List of (source, target) tuples representing direct edges
        """
        from langgraph.graph import START, END
        
        return [
            (START, WorkflowNode.PRE_PROCESS.value),
            (WorkflowNode.PRE_PROCESS.value, WorkflowNode.FILTER.value),
            (WorkflowNode.FILTER.value, WorkflowNode.DATA_FETCHER.value),
            (WorkflowNode.DATA_FETCHER.value, WorkflowNode.JUDGE_LLM_ANALYSIS.value),
            (WorkflowNode.JUDGE_LLM_ANALYSIS.value, WorkflowNode.EVALUATE_ANALYSIS.value),
            (WorkflowNode.SUMMARIZE_JUSTIFICATIONS.value, WorkflowNode.CALCULATE_METRICS.value),
            (WorkflowNode.CALCULATE_METRICS.value, WorkflowNode.WRITE_RESULTS.value),
            (WorkflowNode.WRITE_RESULTS.value, END)
        ]

def count_issues_needing_second_analysis(issues: dict[str, PerIssueData]):
    """
    Count the number of issues that need a second analysis.
    """
    return sum(1 for per_issue_data in issues.values() 
                 if per_issue_data.analysis_response and per_issue_data.analysis_response.is_second_analysis_needed())

class WorkflowNode(Enum):
    """
    Enumeration of all workflow nodes in the SAST analysis pipeline.
    
    The value corresponds to the string identifier used in LangGraph.
    """
    PRE_PROCESS = "pre_process"
    FILTER = "filter"
    DATA_FETCHER = "data_fetcher"
    JUDGE_LLM_ANALYSIS = "judge_llm_analysis"
    EVALUATE_ANALYSIS = "evaluate_analysis"
    SUMMARIZE_JUSTIFICATIONS = "summarize_justifications"
    CALCULATE_METRICS = "calculate_metrics"
    WRITE_RESULTS = "write_results"

    @classmethod
    def get_all_node_names(cls):
        return [member.value for member in cls]
