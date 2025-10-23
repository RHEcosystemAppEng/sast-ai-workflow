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

from evaluation.constants import (
    MAX_FILTER_ITERATIONS,
    FILTER_DATASET_FILENAME,
    OUTPUT_DIR,
    KNOWN_NON_ISSUES_DIR,
    KNOWN_NON_ISSUES_FILENAME,
    KNOWN_NON_ISSUES_VECTOR_STORE_DIR,
    CLASSIFICATION_TRUE_POSITIVE,
    CLASSIFICATION_FALSE_POSITIVE,
)

from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus
from evaluation.converter_tools.base_converter import BaseEvaluationConverter

logger = logging.getLogger(__name__)


class FilterConverter(BaseEvaluationConverter):
    """Filter evaluation converter that inherits from BaseEvaluationConverter."""

    def __init__(self):
        super().__init__("filter", FILTER_DATASET_FILENAME)

    def parse_input_data(self, input_str: str) -> Dict[str, Any]:
        """Parse input string for filter evaluation."""
        try:
            # Try to parse the question field as JSON (individual issue data)
            data = json.loads(input_str)
            return data
        except json.JSONDecodeError:
            logger.info("Input is not JSON, loading dataset to find matching entry")
            eval_data = self.load_dataset_entry(input_str)
            if not eval_data:
                logger.warning(f"No matching entry found for input: {input_str}")
                return {"id": "unknown", "issue_id": "unknown", "issue_type": "unknown"}

            # Extract the JSON from the question field
            question = eval_data.get("question", "{}")
            try:
                parsed_question = json.loads(question)
                return parsed_question
            except json.JSONDecodeError:
                logger.warning(f"Question field is not valid JSON: {question}")
                return eval_data

    def create_issue_objects(self, parsed_data: Dict[str, Any]) -> Dict[str, Any]:
        """Create Issue objects for filter evaluation."""
        # Create a single issue from the individual issue data (like judge converter)
        issue_id = parsed_data.get("issue_id", parsed_data.get("id", "unknown"))

        issue = Issue(
            id=issue_id,
            issue_type=parsed_data.get("issue_type", "unknown"),
            severity=parsed_data.get("severity", "medium"),
            trace=parsed_data.get("trace", ""),
            file_path=parsed_data.get("file_path", ""),
            line_number=parsed_data.get("line_number", 0),
            cwe_id=parsed_data.get("cwe_id", "CWE-000"),
            description=f"Filter evaluation for issue: {issue_id}",
            source_position=parsed_data.get("line_number", 0),
            sink_position=parsed_data.get("line_number", 0)
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

        # Debug logging
        logger.info(f"Created issue: {issue_id} with trace: {parsed_data.get('trace', '')[:100]}...")

        return {issue_id: per_issue_data}

    def extract_output_data(self, tracker: SASTWorkflowTracker) -> Dict[str, Any]:
        """Extract output data for filter evaluation."""
        # For individual issue evaluation, we return flat results (like judge converter)
        # Since we only have one issue per tracker, return the results for that single issue

        if not tracker.issues:
            return {
                "filter_result": "",
                "confidence": None,
                "similar_known_issues": [],
                "justification": ""
            }

        # Get the single issue (there should only be one for individual evaluation)
        issue_id, per_issue_data = next(iter(tracker.issues.items()))
        analysis = per_issue_data.analysis_response

        # Determine if issue was filtered as false positive based on analysis.is_final
        filter_result = (
            CLASSIFICATION_FALSE_POSITIVE
            if analysis.is_final == FinalStatus.TRUE.value
            else CLASSIFICATION_TRUE_POSITIVE
        )

        # Use default confidence - let the evaluation system compare with expected values
        confidence = None

        # Get justification
        justification = ""
        if analysis.justifications:
            justification = " ".join(analysis.justifications)
        elif analysis.short_justifications:
            justification = analysis.short_justifications
        else:
            justification = "No justification provided"

        # Get similar_known_issues from the per_issue_data if available
        similar_issues_context = getattr(per_issue_data, 'similar_known_issues', '')

        # Debug logging to see what we're getting
        logger.info(f"Similar issues context for {issue_id}: {similar_issues_context}")
        logger.info(f"Type of similar_issues_context: {type(similar_issues_context)}")

        # Extract real similar issue identifiers ONLY if the issue was identified as a known false positive
        similar_issues = []
        if (filter_result == "FALSE_POSITIVE" and
            similar_issues_context and isinstance(similar_issues_context, str)):
            # Parse the context to extract actual file paths from the error traces
            lines = similar_issues_context.split('\n')
            for line in lines:
                line = line.strip()
                # Look for lines with file paths in format "audit-4.0/path/file.c:number:"
                if line and '/' in line and '.c:' in line and ':' in line:
                    # Extract the file path portion before the colon
                    colon_idx = line.find(':')
                    if colon_idx > 0:
                        potential_path = line[:colon_idx]
                        # Verify it's a valid file path format
                        if '/' in potential_path and '.c' in potential_path:
                            similar_issues.append(potential_path)

            # Remove duplicates while preserving order
            similar_issues = list(dict.fromkeys(similar_issues))

        # If filter_result is TRUE_POSITIVE, leave similar_issues empty (no matches found)

        return {
            "filter_result": filter_result,
            "confidence": confidence,
            "similar_known_issues": similar_issues,
            "justification": justification
        }

    def get_minimal_config(self):
        """Get filter-specific minimal config."""
        class MinimalConfig:
            def __init__(self):
                self.USE_KNOWN_FALSE_POSITIVE_FILE = True
                self.KNOWN_FALSE_POSITIVE_FILE_PATH = str(project_root / KNOWN_NON_ISSUES_DIR / KNOWN_NON_ISSUES_FILENAME)
                self.OUTPUT_DIR = OUTPUT_DIR
                self.MAX_ITERATIONS = MAX_FILTER_ITERATIONS
                self.VECTOR_STORE_PATH = KNOWN_NON_ISSUES_VECTOR_STORE_DIR
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
            'KNOWN_FALSE_POSITIVE_FILE_PATH': str(project_root / KNOWN_NON_ISSUES_DIR / KNOWN_NON_ISSUES_FILENAME)
        }
        filter_kwargs.update(kwargs)
        super().setup_environment(**filter_kwargs)

    def create_config(self):
        """Create filter-specific config."""
        config = super().create_config()

        # Add filter-specific config if we have the real Config object
        if hasattr(config, 'USE_KNOWN_FALSE_POSITIVE_FILE'):
            config.USE_KNOWN_FALSE_POSITIVE_FILE = True
            config.KNOWN_FALSE_POSITIVE_FILE_PATH = str(project_root / KNOWN_NON_ISSUES_DIR / KNOWN_NON_ISSUES_FILENAME)
            config.OUTPUT_DIR = OUTPUT_DIR
            config.MAX_ITERATIONS = MAX_FILTER_ITERATIONS
            config.VECTOR_STORE_PATH = KNOWN_NON_ISSUES_VECTOR_STORE_DIR
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