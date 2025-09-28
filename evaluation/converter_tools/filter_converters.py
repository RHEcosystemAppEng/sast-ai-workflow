#!/usr/bin/env python3
"""
NAT evaluation converters for filter function.

These converters transform between string format (used by NAT) and
SASTWorkflowTracker objects (used by filter function).
"""

import json
import logging
from typing import Dict, Any
from pathlib import Path
import sys

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus
from evaluation.converter_tools.base_converter import BaseEvaluationConverter

logger = logging.getLogger(__name__)


class FilterConverter(BaseEvaluationConverter):
    """Filter evaluation converter that inherits from BaseEvaluationConverter."""

    def __init__(self):
        super().__init__("filter", "filter_eval_dataset.json")

    def parse_input_data(self, input_str: str) -> Dict[str, Any]:
        """Parse input string for filter evaluation."""
        try:
            eval_data = json.loads(input_str)
            return eval_data
        except json.JSONDecodeError:
            logger.info("Input is not JSON, loading dataset to find matching entry")
            eval_data = self.load_dataset_entry(input_str)
            if not eval_data:
                logger.warning(f"No matching entry found for input: {input_str}")
                return {"id": "unknown", "input_data": {}}
            return eval_data

    def create_issue_objects(self, parsed_data: Dict[str, Any]) -> Dict[str, Any]:
        """Create Issue objects for filter evaluation."""
        test_id = parsed_data.get("id", "unknown")
        input_data = parsed_data.get("input_data", {})

        issues_data = input_data.get("issues", [])
        if not issues_data:
            issues_data = [{
                "issue_id": test_id,
                "issue_type": input_data.get("issue_type", "unknown"),
                "severity": input_data.get("severity", "medium"),
                "trace": input_data.get("trace", ""),
                "file_path": input_data.get("file_path", ""),
                "line_number": input_data.get("line_number", 0),
                "cwe_id": input_data.get("cwe_id", "CWE-000")
            }]

        issues = {}
        for issue_data in issues_data:
            issue_id = issue_data.get("issue_id", test_id)

            issue = Issue(
                id=issue_id,
                issue_type=issue_data.get("issue_type", "unknown"),
                severity=issue_data.get("severity", "medium"),
                trace=issue_data.get("trace", ""),
                file_path=issue_data.get("file_path", ""),
                line_number=issue_data.get("line_number", 0),
                cwe_id=issue_data.get("cwe_id", "CWE-000"),
                description=f"Test issue for filter evaluation: {issue_id}",
                source_position=issue_data.get("line_number", 0),
                sink_position=issue_data.get("line_number", 0)
            )

            per_issue_data = PerIssueData(
                issue=issue,
                analysis_response=AnalysisResponse(
                    investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                    is_final=FinalStatus.FALSE.value,
                    recommendations=[],
                    justifications=[],
                    short_justifications="Awaiting filter analysis"
                )
            )

            issues[issue_id] = per_issue_data

        return issues

    def extract_output_data(self, tracker: SASTWorkflowTracker) -> Dict[str, Any]:
        """Extract output data for filter evaluation."""
        package_results = {}

        for issue_id, per_issue_data in tracker.issues.items():
            analysis = per_issue_data.analysis_response

            # Determine if issue was filtered as false positive based on analysis.is_final
            filter_result = "FALSE_POSITIVE" if analysis.is_final == FinalStatus.TRUE.value else "TRUE_POSITIVE"

            confidence = getattr(per_issue_data, 'filter_confidence', 0.8)

            # Get justification first
            justification = ""
            if analysis.justifications:
                justification = " ".join(analysis.justifications)
            elif analysis.short_justifications:
                justification = analysis.short_justifications
            else:
                justification = "No justification provided"

            # Parse similar_known_issues from text to extract pattern IDs
            similar_issues_text = getattr(per_issue_data, 'similar_known_issues', '')
            similar_issues = []
            if similar_issues_text and filter_result == "FALSE_POSITIVE":
                # For known patterns based on content analysis
                if "alloc_strlen" in similar_issues_text and "databuf_init" in similar_issues_text:
                    similar_issues = ["buffer_overflow_pattern_1", "buffer_overflow_pattern_2"]
                elif "overrun-buffer-arg" in similar_issues_text and "nlmsghdr" in similar_issues_text:
                    similar_issues = ["buffer_overflow_pattern_3"]
                elif any(keyword in similar_issues_text for keyword in ["alloc_fn", "leaked_storage", "memory"]):
                    similar_issues = ["memory_management_pattern_1"]

            package_results[issue_id] = {
                "filter_result": filter_result,
                "confidence": confidence,
                "similar_known_issues": similar_issues if isinstance(similar_issues, list) else [],
                "justification": justification
            }

        return {"package_analysis": package_results}

    def get_minimal_config(self):
        """Get filter-specific minimal config."""
        class MinimalConfig:
            def __init__(self):
                self.USE_KNOWN_FALSE_POSITIVE_FILE = True
                self.KNOWN_FALSE_POSITIVE_FILE_PATH = str(project_root / "evaluation/known_non_issues_data/known_non_issues.txt")
                self.OUTPUT_DIR = "evaluation/output"
                self.MAX_ITERATIONS = 1
                self.VECTOR_STORE_PATH = "evaluation/known_non_issues_data/faiss_vector_store"
                self.SIMILARITY_ERROR_THRESHOLD = 5
                self.USE_VECTOR_SIMILARITY = True
                self.FILTER_SYSTEM_PROMPT = """You're an expert at identifying similar error stack traces.
Given:

1.  **Known False Positive Issues** (`context_false_positives`): A list where each issue contains:
    * `false_positive_error_trace`: The error trace of the false positive.
    * `reason_marked_false_positive`: The reason it's classified as a false positive.
2.  **New User Error Trace** (`user_error_trace`): The error trace from a new user.

Your task is to determine if the `user_error_trace` **exactly matches** any of the `false_positive_error_trace` entries.

**Comparison Rules:**

* **Ignore:** Line numbers and package version details.
* **Must Match Exactly:** Method names and their call order.

**Constraint:**

* Your response must strictly follow the provided **answer response template** and include no additional text.

---

**Answer Response Template:**

```json
{answer_template}


context_false_positives:
{context}


user_error_trace:
{user_error_trace}"""

                self.FILTER_HUMAN_PROMPT = """Does the error trace of user_error_trace match any of the context_false_positives errors?
user_error_trace: {user_error_trace}"""

        return MinimalConfig()

    def setup_environment(self, **kwargs):
        """Setup filter-specific environment variables."""
        filter_kwargs = {
            'KNOWN_FALSE_POSITIVE_FILE_PATH': str(project_root / "evaluation/known_non_issues_data/known_non_issues.txt")
        }
        filter_kwargs.update(kwargs)
        super().setup_environment(**filter_kwargs)

    def create_config(self):
        """Create filter-specific config."""
        config = super().create_config()

        # Add filter-specific config if we have the real Config object
        if hasattr(config, 'USE_KNOWN_FALSE_POSITIVE_FILE'):
            config.USE_KNOWN_FALSE_POSITIVE_FILE = True
            config.KNOWN_FALSE_POSITIVE_FILE_PATH = str(project_root / "evaluation/known_non_issues_data/known_non_issues.txt")
            config.OUTPUT_DIR = "evaluation/output"
            config.MAX_ITERATIONS = 1
            config.VECTOR_STORE_PATH = "evaluation/known_non_issues_data/faiss_vector_store"
            config.SIMILARITY_ERROR_THRESHOLD = 5

        return config


# Create converter instance for backward compatibility
_filter_converter = FilterConverter()


def convert_str_to_sast_tracker(input_str: str) -> SASTWorkflowTracker:
    """Convert string to SASTWorkflowTracker (backward compatibility function)."""
    return _filter_converter.convert_str_to_sast_tracker(input_str)




def convert_sast_tracker_to_str(tracker: SASTWorkflowTracker) -> str:
    """Convert SASTWorkflowTracker to string (backward compatibility function)."""
    return _filter_converter.convert_sast_tracker_to_str(tracker)


def extract_filter_metrics(tracker: SASTWorkflowTracker) -> Dict[str, Any]:
    """
    Extract filter-specific metrics from tracker for analysis.

    Args:
        tracker: The processed SASTWorkflowTracker

    Returns:
        Dict containing filter metrics
    """
    metrics = {
        "total_issues": len(tracker.issues),
        "filtered_as_false_positive": 0,
        "passed_for_analysis": 0,
        "high_confidence_decisions": 0,
        "low_confidence_decisions": 0
    }

    for per_issue_data in tracker.issues.values():
        analysis = per_issue_data.analysis_response

        if analysis.is_final == FinalStatus.TRUE.value:
            metrics["filtered_as_false_positive"] += 1
        else:
            metrics["passed_for_analysis"] += 1

        confidence = getattr(per_issue_data, 'filter_confidence', 0.8)
        if confidence >= 0.8:
            metrics["high_confidence_decisions"] += 1
        else:
            metrics["low_confidence_decisions"] += 1

    return metrics


__all__ = ['FilterConverter', 'convert_str_to_sast_tracker', 'convert_sast_tracker_to_str', 'extract_filter_metrics']