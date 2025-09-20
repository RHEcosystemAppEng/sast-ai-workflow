#!/usr/bin/env python3
"""
NAT evaluation converters for filter function.

These converters transform between string format (used by NAT) and
SASTWorkflowTracker objects (used by filter function).
"""

import json
import logging
import os
from typing import Dict, Any
from pathlib import Path
import sys

# Add project root to path for imports
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus

try:
    from common.config import Config
except ImportError:
    try:
        from Config import Config
    except ImportError:
        Config = None

logger = logging.getLogger(__name__)


def convert_str_to_sast_tracker(input_str: str) -> SASTWorkflowTracker:
    """
    Convert string input to SASTWorkflowTracker for filter evaluation.

    Args:
        input_str: The evaluation input (typically the question string from NAT)

    Returns:
        SASTWorkflowTracker: Configured tracker for filter evaluation
    """
    logger.info(f"Converting evaluation input to SASTWorkflowTracker: {input_str}")

    try:
        # DEBUG: Print the actual input string we're receiving
        print(f"[DEBUG] Raw input_str received: {repr(input_str)}")
        print(f"[DEBUG] Input string type: {type(input_str)}")
        print(f"[DEBUG] Input string length: {len(input_str)}")

        # First try to parse as JSON (full dataset entry)
        try:
            eval_data = json.loads(input_str)
            print(f"[DEBUG] Successfully parsed as JSON: {eval_data}")
            test_id = eval_data.get("id", "unknown")
            input_data = eval_data.get("input_data", {})
        except json.JSONDecodeError:
            # NAT is passing just the question string, need to load the dataset entry
            logger.info("Input is not JSON, loading dataset to find matching entry")
            from evaluation.tools.eval_utils import load_evaluation_dataset, find_dataset_entry

            dataset = load_evaluation_dataset("filter_eval_dataset.json")
            eval_data = find_dataset_entry(dataset, input_str)

            if not eval_data:
                logger.warning(f"No matching entry found for input: {input_str}")
                return _create_empty_tracker()

            test_id = eval_data.get("id", "unknown")
            input_data = eval_data.get("input_data", {})

        # Handle both old single-issue format and new package-based format
        issues_data = input_data.get("issues", [])
        if not issues_data:
            # Fallback to old single-issue format
            issues_data = [{
                "issue_id": test_id,
                "issue_type": input_data.get("issue_type", "unknown"),
                "severity": input_data.get("severity", "medium"),
                "trace": input_data.get("trace", ""),
                "file_path": input_data.get("file_path", ""),
                "line_number": input_data.get("line_number", 0),
                "cwe_id": input_data.get("cwe_id", "CWE-000")
            }]

        # Create tracker with all issues from the package
        tracker = SASTWorkflowTracker()
        tracker.issues = {}

        for issue_data in issues_data:
            issue_id = issue_data.get("issue_id", test_id)

            # Create a SAST issue from the issue data
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

            # Create PerIssueData with empty analysis response
            per_issue_data = PerIssueData(
                issue=issue,
                analysis_response=AnalysisResponse(
                    investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,  # Default to TRUE_POSITIVE for filtering
                    is_final=FinalStatus.FALSE.value,
                    recommendations=[],
                    justifications=[],
                    short_justifications="Awaiting filter analysis"
                )
            )

            tracker.issues[issue_id] = per_issue_data

        # Create realistic config with filter enabled and proper FAISS paths
        try:
            # Set environment variables that Config needs
            os.environ.setdefault('PROJECT_NAME', 'filter-eval')
            os.environ.setdefault('PROJECT_VERSION', '1.0.0')
            os.environ.setdefault('INPUT_REPORT_FILE_PATH', '/dev/null')
            os.environ.setdefault('OUTPUT_FILE_PATH', '/dev/null')
            os.environ.setdefault('REPO_LOCAL_PATH', str(project_root))

            tracker.config = Config()
            tracker.config.USE_KNOWN_FALSE_POSITIVE_FILE = True
            tracker.config.KNOWN_FALSE_POSITIVE_FILE_PATH = str(project_root / "evaluation/data/known_false_positives.txt")
            tracker.config.OUTPUT_DIR = "evaluation/output"
            tracker.config.MAX_ITERATIONS = 1
            # Vector store configuration for real FAISS similarity
            tracker.config.VECTOR_STORE_PATH = "evaluation/data/faiss_vector_store"
            tracker.config.SIMILARITY_ERROR_THRESHOLD = 5  # Number of similar issues to retrieve for comparison
            logger.info("Successfully created real Config object for filter evaluation")
        except Exception as e:
            logger.error(f"Main Config failed ({e}), using minimal config for evaluation")
            tracker.config = create_minimal_config()

        logger.info(f"Created SASTWorkflowTracker for filter evaluation with {len(tracker.issues)} issues: {list(tracker.issues.keys())}")
        return tracker

    except Exception as e:
        logger.error(f"Failed to convert input to SASTWorkflowTracker: {e}")
        # Return empty tracker as fallback
        return _create_empty_tracker()


def create_minimal_config():
    """Create a minimal configuration object for fallback scenarios."""
    class MinimalConfig:
        def __init__(self):
            self.USE_KNOWN_FALSE_POSITIVE_FILE = True
            self.KNOWN_FALSE_POSITIVE_FILE_PATH = str(project_root / "evaluation/data/known_false_positives.txt")
            self.OUTPUT_DIR = "evaluation/output"
            self.MAX_ITERATIONS = 1
            self.VECTOR_STORE_PATH = "evaluation/data/faiss_vector_store"
            self.SIMILARITY_ERROR_THRESHOLD = 5
            self.USE_VECTOR_SIMILARITY = True
            # Add the filter system prompt for LLM analysis (from original YAML template)
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

            # Add the filter human prompt (from original YAML template)
            self.FILTER_HUMAN_PROMPT = """Does the error trace of user_error_trace match any of the context_false_positives errors?
user_error_trace: {user_error_trace}"""

    return MinimalConfig()


def convert_sast_tracker_to_str(tracker: SASTWorkflowTracker) -> str:
    """
    Convert SASTWorkflowTracker to string output for filter evaluation.

    Args:
        tracker: The processed SASTWorkflowTracker from filter function

    Returns:
        str: JSON string containing filter results
    """
    print(f"[DEBUG] convert_sast_tracker_to_str called with tracker: {type(tracker)}")
    print(f"[DEBUG] tracker.issues count: {len(tracker.issues) if tracker.issues else 0}")
    try:
        # For package-based evaluation, wrap results in package_analysis structure
        package_results = {}

        for issue_id, per_issue_data in tracker.issues.items():
            # Extract filter results
            analysis = per_issue_data.analysis_response

            # Determine if issue was filtered as false positive based on analysis.is_final
            # When is_final == FinalStatus.FALSE.value, it means "not final" = needs investigation = TRUE_POSITIVE
            # When is_final == FinalStatus.TRUE.value, it means "is final" = false positive = FALSE_POSITIVE
            filter_result = "FALSE_POSITIVE" if analysis.is_final == FinalStatus.TRUE.value else "TRUE_POSITIVE"

            # Extract confidence and similar issues information
            confidence = getattr(per_issue_data, 'filter_confidence', 0.8)  # Default confidence
            similar_issues = getattr(per_issue_data, 'similar_known_issues', [])

            # Get justification from analysis response
            justification = ""
            if analysis.justifications:
                justification = " ".join(analysis.justifications)
            elif analysis.short_justifications:
                justification = analysis.short_justifications
            else:
                justification = "No justification provided"

            # Only include the essential fields for NAT evaluation (remove problematic extras)
            package_results[issue_id] = {
                "filter_result": filter_result,
                "confidence": confidence,
                "similar_known_issues": similar_issues if isinstance(similar_issues, list) else [],
                "justification": justification
            }

        # Wrap in package_analysis structure to match expected output format
        results = {"package_analysis": package_results}

        # Convert to JSON string
        output = json.dumps(results, indent=2)
        logger.debug(f"Converted SASTWorkflowTracker to filter results: {output}")
        return output

    except Exception as e:
        logger.error(f"Failed to convert SASTWorkflowTracker to string: {e}")
        return json.dumps({"error": f"Conversion failed: {str(e)}"}, indent=2)


def _create_empty_tracker():
    """Create an empty SASTWorkflowTracker for error cases."""
    try:
        import os
        os.environ.setdefault('INPUT_REPORT_FILE_PATH', '/dev/null')
        os.environ.setdefault('OUTPUT_FILE_PATH', '/dev/null')
        os.environ.setdefault('KNOWN_FALSE_POSITIVE_FILE_PATH', '/dev/null')
        os.environ.setdefault('PROJECT_NAME', 'filter-eval-empty')
        os.environ.setdefault('PROJECT_VERSION', '1.0.0')
        os.environ.setdefault('REPO_LOCAL_PATH', str(project_root))

        try:
            from common.config import Config
            config = Config()
        except Exception:
            config = create_minimal_config()

        return SASTWorkflowTracker(issues={}, config=config)
    except Exception as e:
        logger.error(f"Failed to create empty tracker: {e}")
        return None


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

        # Check confidence if available
        confidence = getattr(per_issue_data, 'filter_confidence', 0.8)
        if confidence >= 0.8:
            metrics["high_confidence_decisions"] += 1
        else:
            metrics["low_confidence_decisions"] += 1

    return metrics