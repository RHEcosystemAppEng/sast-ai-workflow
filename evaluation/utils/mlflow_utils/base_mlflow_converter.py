#!/usr/bin/env python3
"""
Base MLflow Converter for SAST AI Workflow Evaluation Dashboard

This module provides the base class for converting SAST AI evaluation reports
into MLflow format with common functionality shared across all node types.

APPENG-3747: Evaluation Dashboard Creation
"""

import json
import os
import re
import sys
import tempfile
from abc import ABC, abstractmethod
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import mlflow
import mlflow.tracking

# Add src directory to path for importing Utils
sys.path.insert(0, str(Path(__file__).resolve().parents[3] / "src"))
from Utils.file_utils import load_json_file

# Re-export load_json_file for use by child classes
__all__ = ['BaseMLflowConverter', 'load_json_file', 'PackageMetrics']

from .mlflow_constants import (
    ACCURACY_KEY,
    AVG_TIME_PER_REQUEST_KEY,
    COMPLETION_TOKENS_KEY,
    EVALUATION_METRICS_FILE,
    EXPECTED_OUTPUT_PARAM,
    F1_SCORE_KEY,
    FALSE_NEGATIVES_KEY,
    FALSE_POSITIVES_KEY,
    FILTER_VALIDATION_FILE,
    INFERENCE_OPTIMIZATION_FILE,
    INPUT_QUESTION_PARAM,
    ISSUE_COUNT_PARAM,
    ISSUES_PARAM,
    JUSTIFICATION_QUALITY_FILE,
    LEVEL_EVALUATION_RUN,
    LEVEL_ISSUE,
    LEVEL_PACKAGE,
    LLM_CALL_COUNT_KEY,
    METRICS_KEY,
    PACKAGE_NAME_PARAM,
    PACKAGES_PARAM,
    PRECISION_KEY,
    PROFILER_TRACES_FILE,
    PROMPT_TOKENS_KEY,
    RECALL_KEY,
    RUN_DIR_PREFIX,
    RUN_ID_PREFIX,
    SIMILAR_ISSUES_COUNT_KEY,
    SUMMARIZATION_QUALITY_FILE,
    TAG_EVALUATION_RUN,
    TAG_ISSUE_COUNT,
    TAG_ISSUE_ID,
    TAG_LEVEL,
    TAG_NODE_TYPE,
    TAG_ORIGINAL_ISSUE_ID,
    TAG_PACKAGE,
    TAG_PACKAGE_VERSION,
    TAG_PARENT_RUN_ID,
    TAG_RUN_DATE,
    TAG_RUN_TIME,
    TAG_RUN_TIMESTAMP,
    TAG_TOTAL_ISSUES,
    TAG_TOTAL_PACKAGES,
    TAG_VERSION,
    TOTAL_ITEMS_KEY,
    TOTAL_PACKAGES_PARAM,
    TOTAL_ISSUES_PARAM,
    TOTAL_TOKENS_KEY,
    TOTAL_TIME_SECONDS_KEY,
    TRUE_NEGATIVES_KEY,
    TRUE_POSITIVES_KEY,
    VERSION_PARAM,
    WORKFLOW_OUTPUT_FILE,
)


# ============================================================================
# Data Classes
# ============================================================================
@dataclass
class PackageMetrics:
    """Standard metrics structure for package-level aggregation."""
    total_packages: int
    total_issues: int
    similar_issues_count: int
    precision: float
    recall: float
    total_tokens: int
    avg_time_per_request: float
    llm_call_count: int

    def to_dict_with_prefix(self, prefix: str) -> dict:
        """Convert to dictionary with node-specific precision/recall naming."""
        return {
            "total_packages": self.total_packages,
            "total_issues": self.total_issues,
            SIMILAR_ISSUES_COUNT_KEY: self.similar_issues_count,
            f"{prefix}_precision": self.precision,
            f"{prefix}_recall": self.recall,
            TOTAL_TOKENS_KEY: self.total_tokens,
            AVG_TIME_PER_REQUEST_KEY: self.avg_time_per_request,
            LLM_CALL_COUNT_KEY: self.llm_call_count,
        }


class BaseMLflowConverter(ABC):
    """Base class for converting SAST AI evaluation reports to MLflow format."""

    def __init__(self, reports_dir: str, mlruns_dir: str):
        self.reports_dir = Path(reports_dir)
        self.mlruns_dir = Path(mlruns_dir)

    @property
    @abstractmethod
    def node_type(self) -> str:
        """Return the node type this converter handles."""
        pass

    @property
    @abstractmethod
    def experiment_name(self) -> str:
        """Return the experiment name for this node type."""
        pass

    def convert_reports(self):
        """Convert all reports for this node type."""
        node_dir = self.reports_dir / self.node_type
        if not node_dir.exists():
            print(f"Warning: No reports found for {self.node_type}")
            return 0

        # Process each run in the node directory
        run_dirs = [d for d in node_dir.iterdir() if d.is_dir() and d.name.startswith(RUN_DIR_PREFIX)]
        print(f"Found {len(run_dirs)} runs for {self.node_type}")

        processed_runs = 0
        for run_dir in sorted(run_dirs):
            try:
                self._process_single_run(run_dir)
                processed_runs += 1
            except Exception as e:
                print(f"Error processing {self.node_type}/{run_dir.name}: {e}")

        return processed_runs

    def _process_single_run(self, run_dir: Path):
        """Process a single evaluation run for this node type."""
        run_name = run_dir.name
        print(f"Processing {self.node_type}/{run_name}")

        # Load workflow output to get individual issues
        workflow_file = run_dir / WORKFLOW_OUTPUT_FILE
        if not workflow_file.exists():
            print(f"  No {WORKFLOW_OUTPUT_FILE} found for {run_name}")
            return

        workflow_data = load_json_file(str(workflow_file))
        if not workflow_data or not isinstance(workflow_data, list):
            print(f"  Invalid workflow data for {run_name}")
            return

        # Filter valid issues and group by package
        filtered_issues = self._filter_valid_issues(workflow_data)
        package_versions = self._group_issues_by_package(filtered_issues)

        # Load run-specific metrics
        run_metrics = self._load_run_metrics(run_dir)

        # Process the run data
        self._process_single_run_data(run_name, filtered_issues, package_versions, run_dir, run_metrics)

        print(f"✓ Logged {self.node_type}/{run_name} with {len(package_versions)} packages and {len(filtered_issues)} issues")

    def _filter_valid_issues(self, workflow_data: List[Dict]) -> List[Dict]:
        """Filter out invalid issues (like run IDs) from workflow data."""
        filtered_issues = []
        for issue_data in workflow_data:
            if not isinstance(issue_data, dict) or "id" not in issue_data:
                continue

            issue_id = issue_data["id"]
            # Skip run IDs (they start with RUN_ID_PREFIX)
            if issue_id.startswith(RUN_ID_PREFIX):
                continue

            filtered_issues.append(issue_data)
        return filtered_issues

    def _group_issues_by_package(self, issues: List[Dict]) -> Dict[str, Dict]:
        """Group issues by package-version."""
        package_versions = {}
        for issue_data in issues:
            issue_id = issue_data["id"]
            package_name, version, clean_issue_id = self._extract_package_info(issue_id)
            package_version_key = f"{package_name}-{version}"

            if package_version_key not in package_versions:
                package_versions[package_version_key] = {
                    "package": package_name,
                    "version": version,
                    "issues": []
                }
            package_versions[package_version_key]["issues"].append({
                "clean_issue_id": clean_issue_id,
                "original_id": issue_id,
                "data": issue_data
            })
        return package_versions

    @abstractmethod
    def _load_run_metrics(self, run_dir: Path) -> Dict[str, Any]:
        """Load metrics files specific to this node type."""
        pass

    def _process_evaluation_metrics(self, metrics_file: Path) -> Dict[str, float]:
        """Process evaluation_metrics.json file."""
        metrics_data = load_json_file(str(metrics_file))
        if not metrics_data or METRICS_KEY not in metrics_data:
            return {}

        metrics = metrics_data[METRICS_KEY]
        return {
            PRECISION_KEY: metrics.get(PRECISION_KEY, 0.0),
            RECALL_KEY: metrics.get(RECALL_KEY, 0.0),
            F1_SCORE_KEY: metrics.get(F1_SCORE_KEY, 0.0),
            ACCURACY_KEY: metrics.get(ACCURACY_KEY, 0.0),
            TOTAL_ITEMS_KEY: metrics.get(TOTAL_ITEMS_KEY, 0),
            TRUE_POSITIVES_KEY: metrics.get(TRUE_POSITIVES_KEY, 0),
            FALSE_POSITIVES_KEY: metrics.get(FALSE_POSITIVES_KEY, 0),
            TRUE_NEGATIVES_KEY: metrics.get(TRUE_NEGATIVES_KEY, 0),
            FALSE_NEGATIVES_KEY: metrics.get(FALSE_NEGATIVES_KEY, 0),
        }

    def _process_inference_optimization(self, inference_file: Path) -> Dict[str, float]:
        """Process inference_optimization.json file for token usage and timing."""
        inference_data = load_json_file(str(inference_file))
        if not inference_data:
            return {}

        metrics = {}

        # Extract token usage metrics
        if TOTAL_TOKENS_KEY in inference_data:
            metrics[TOTAL_TOKENS_KEY] = inference_data[TOTAL_TOKENS_KEY]
        if PROMPT_TOKENS_KEY in inference_data:
            metrics[PROMPT_TOKENS_KEY] = inference_data[PROMPT_TOKENS_KEY]
        if COMPLETION_TOKENS_KEY in inference_data:
            metrics[COMPLETION_TOKENS_KEY] = inference_data[COMPLETION_TOKENS_KEY]

        # Extract timing metrics
        if TOTAL_TIME_SECONDS_KEY in inference_data:
            metrics[TOTAL_TIME_SECONDS_KEY] = inference_data[TOTAL_TIME_SECONDS_KEY]
        if AVG_TIME_PER_REQUEST_KEY in inference_data:
            metrics[AVG_TIME_PER_REQUEST_KEY] = inference_data[AVG_TIME_PER_REQUEST_KEY]

        return metrics

    def _process_single_run_data(self, run_name: str, filtered_issues: List[dict],
                               package_versions: Dict, run_dir: Path, run_metrics: Dict):
        """Process a single evaluation run with hierarchical structure."""
        # Get or create experiment
        experiment_id = self._get_or_create_experiment()

        # Extract run timestamp
        run_timestamp = self._extract_run_timestamp(run_name)

        # Level 1: Create evaluation run
        with mlflow.start_run(experiment_id=experiment_id, run_name=run_name) as parent_run:
            mlflow.set_tag(TAG_NODE_TYPE, self.node_type)
            mlflow.set_tag(TAG_LEVEL, LEVEL_EVALUATION_RUN)
            mlflow.set_tag(TAG_EVALUATION_RUN, run_name)
            mlflow.set_tag(TAG_TOTAL_PACKAGES, len(package_versions))
            mlflow.set_tag(TAG_TOTAL_ISSUES, len(filtered_issues))

            if run_timestamp:
                mlflow.set_tag(TAG_RUN_TIMESTAMP, run_timestamp.isoformat())
                mlflow.set_tag(TAG_RUN_DATE, run_timestamp.strftime("%Y-%m-%d"))
                mlflow.set_tag(TAG_RUN_TIME, run_timestamp.strftime("%H:%M:%S"))

            # Calculate and log run-level metrics
            similar_issues_count, precision, recall = self._calculate_run_level_metrics(filtered_issues, run_metrics)

            # Calculate run-level performance metrics
            run_total_tokens = 0
            run_total_time = 0
            run_llm_call_count = 0

            for issue_data in filtered_issues:
                tokens, time_per_req, calls = self._extract_performance_metrics_from_issue(issue_data)
                run_total_tokens += tokens
                run_total_time += time_per_req * calls
                run_llm_call_count += calls

            run_avg_time_per_request = run_total_time / run_llm_call_count if run_llm_call_count > 0 else 0.0

            # Log run-level metrics
            self._log_standard_metrics(
                len(package_versions), len(filtered_issues), similar_issues_count,
                precision, recall, run_total_tokens, run_avg_time_per_request, run_llm_call_count
            )

            # Log additional node-specific run metrics
            self._log_additional_run_metrics(filtered_issues, run_metrics)

            # Log run parameters
            self._log_param_truncated(TOTAL_PACKAGES_PARAM, len(package_versions))
            self._log_param_truncated(TOTAL_ISSUES_PARAM, len(filtered_issues))
            self._log_param_truncated(PACKAGES_PARAM, ", ".join(list(package_versions.keys())))

            # Log run artifacts
            self._log_run_artifacts(run_dir)

            # Level 2: Create package-level nested runs
            self._process_package_level_runs(experiment_id, package_versions, run_name, run_timestamp, parent_run, run_metrics)

    def _get_or_create_experiment(self) -> str:
        """Get existing experiment or create new one for this node type."""
        experiment = mlflow.get_experiment_by_name(self.experiment_name)
        if not experiment:
            experiment_id = mlflow.create_experiment(self.experiment_name)
        else:
            experiment_id = experiment.experiment_id
        return experiment_id

    def _extract_run_timestamp(self, run_name: str) -> Optional[datetime]:
        """Extract timestamp from run name (e.g., run_20250917_145832)."""
        match = re.search(r'run_(\d{8})_(\d{6})', run_name)
        if match:
            date_str, time_str = match.groups()
            return datetime.strptime(f"{date_str}_{time_str}", "%Y%m%d_%H%M%S")
        return None

    @abstractmethod
    def _calculate_run_level_metrics(self, filtered_issues: List[Dict], run_metrics: Dict) -> Tuple[int, float, float]:
        """Calculate run-level metrics specific to this node type."""
        pass

    def _extract_performance_metrics_from_issue(self, issue_data: Dict) -> Tuple[int, float, int]:
        """
        Extract performance metrics from issue's intermediate_steps.

        Time units: Timing is calculated from payload timestamps
        (event_timestamp - span_event_timestamp). These timestamps are
        derived from the tracing framework and represent time in seconds.
        The calculation assumes both timestamps use the same time base and unit.

        IMPORTANT: If the framework timestamps are not in seconds, this method
        will produce incorrect timing values. The returned avg_time_per_request
        is always in seconds based on the assumption that input timestamps are
        in seconds.

        Returns:
            Tuple of (total_tokens, avg_time_per_request_in_seconds, llm_call_count)
        """
        total_tokens = 0
        avg_time_per_request = 0.0
        llm_call_count = 0

        if "intermediate_steps" in issue_data:
            intermediate_steps = issue_data["intermediate_steps"]

            if isinstance(intermediate_steps, list) and len(intermediate_steps) > 0:
                llm_call_count = len(intermediate_steps)
                total_time = 0
                total_tokens_sum = 0

                for step in intermediate_steps:
                    if isinstance(step, dict) and "payload" in step:
                        payload = step["payload"]

                        # Extract timing information
                        if "event_timestamp" in payload and "span_event_timestamp" in payload:
                            duration = payload["event_timestamp"] - payload["span_event_timestamp"]
                            total_time += duration

                        # Extract token usage from metadata
                        if "usage_info" in payload and "token_usage" in payload["usage_info"]:
                            token_usage = payload["usage_info"]["token_usage"]
                            total_tokens_sum += token_usage.get("total_tokens", 0)

                # Calculate averages
                if llm_call_count > 0:
                    avg_time_per_request = total_time / llm_call_count
                    total_tokens = total_tokens_sum

        return total_tokens, avg_time_per_request, llm_call_count

    def _log_standard_metrics(self, total_packages: int, total_issues: int, similar_issues_count: int,
                           precision: float, recall: float, total_tokens: int,
                           avg_time_per_request: float, llm_call_count: int):
        """Log the standardized 8-column metrics structure."""
        mlflow.log_metric("total_packages", total_packages)
        mlflow.log_metric("total_issues", total_issues)
        mlflow.log_metric("similar_issues_count", similar_issues_count)
        mlflow.log_metric(f"{self.node_type}_precision", precision)
        mlflow.log_metric(f"{self.node_type}_recall", recall)
        mlflow.log_metric("total_tokens", total_tokens)
        mlflow.log_metric("avg_time_per_request", avg_time_per_request)
        mlflow.log_metric("llm_call_count", llm_call_count)

    def _log_param_truncated(self, param_name: str, param_value: Any, max_length: int = None):
        """
        Log parameter with automatic truncation.

        Args:
            param_name: Name of the parameter
            param_value: Value to log (will be converted to string if needed)
            max_length: Maximum length (defaults to MAX_PARAM_LENGTH)
        """
        from .mlflow_constants import MAX_PARAM_LENGTH
        if max_length is None:
            max_length = MAX_PARAM_LENGTH

        if isinstance(param_value, str):
            truncated_value = param_value[:max_length]
        else:
            truncated_value = str(param_value)[:max_length]

        mlflow.log_param(param_name, truncated_value)

    @abstractmethod
    def _log_additional_run_metrics(self, filtered_issues: List[Dict], run_metrics: Dict):
        """Log additional run-level metrics specific to this node type."""
        pass

    def _log_run_artifacts(self, run_dir: Path, artifact_prefix: str = ""):
        """Log run artifacts (evaluation files) to MLflow."""
        try:
            # Log key evaluation files as artifacts
            artifact_files = [
                (EVALUATION_METRICS_FILE, "metrics"),
                (INFERENCE_OPTIMIZATION_FILE, "optimization"),
                (WORKFLOW_OUTPUT_FILE, "workflow"),
                (FILTER_VALIDATION_FILE, "validation"),
                (JUSTIFICATION_QUALITY_FILE, "quality"),
                (SUMMARIZATION_QUALITY_FILE, "quality"),
                (PROFILER_TRACES_FILE, "profiler")
            ]

            for filename, artifact_type in artifact_files:
                file_path = run_dir / filename
                if file_path.exists():
                    artifact_path = f"{artifact_type}/{filename}"
                    if artifact_prefix:
                        artifact_path = f"{artifact_prefix}/{artifact_type}/{filename}"
                    mlflow.log_artifact(str(file_path), artifact_path)

        except Exception as e:
            print(f"Warning: Failed to log artifacts for {run_dir}: {e}")

    def _process_package_level_runs(self, experiment_id: str, package_versions: Dict, run_name: str,
                                 run_timestamp: Optional[datetime], parent_run, run_metrics: Dict):
        """Process package-level nested runs."""
        for package_version_key, package_info in package_versions.items():
            package_name = package_info["package"]
            version = package_info["version"]
            issues = package_info["issues"]

            with mlflow.start_run(experiment_id=experiment_id,
                                run_name=f"{package_name} {version}",
                                nested=True) as package_run:

                mlflow.set_tag(TAG_NODE_TYPE, self.node_type)
                mlflow.set_tag(TAG_LEVEL, LEVEL_PACKAGE)
                mlflow.set_tag(TAG_EVALUATION_RUN, run_name)
                mlflow.set_tag(TAG_PARENT_RUN_ID, parent_run.info.run_id)
                mlflow.set_tag(TAG_PACKAGE, package_name)
                mlflow.set_tag(TAG_VERSION, version)
                mlflow.set_tag(TAG_PACKAGE_VERSION, package_version_key)
                mlflow.set_tag(TAG_ISSUE_COUNT, len(issues))

                if run_timestamp:
                    mlflow.set_tag(TAG_RUN_TIMESTAMP, run_timestamp.isoformat())
                    mlflow.set_tag(TAG_RUN_DATE, run_timestamp.strftime("%Y-%m-%d"))
                    mlflow.set_tag(TAG_RUN_TIME, run_timestamp.strftime("%H:%M:%S"))

                # Log package-level aggregated metrics
                package_metrics = self._aggregate_package_metrics(issues, run_metrics)
                for metric_name, metric_value in package_metrics.items():
                    mlflow.log_metric(metric_name, metric_value)

                # Log package parameters
                self._log_param_truncated(PACKAGE_NAME_PARAM, package_name)
                self._log_param_truncated(VERSION_PARAM, version)
                self._log_param_truncated(ISSUE_COUNT_PARAM, len(issues))
                self._log_param_truncated(ISSUES_PARAM, ", ".join([issue["clean_issue_id"] for issue in issues]))

                # Level 3: Create issue-level nested runs
                self._process_issue_level_runs(experiment_id, issues, package_info, package_run,
                                            run_name, run_timestamp, run_metrics)

    @abstractmethod
    def _aggregate_package_metrics(self, issues: List[dict], run_metrics: Dict) -> Dict[str, float]:
        """Aggregate metrics across all issues in a package for this node type."""
        pass

    # ============================================================================
    # Issue-Level Processing (called by process_package_level_runs)
    # ============================================================================

    def _process_issue_level_runs(self, experiment_id: str, issues: List[dict], package_info: Dict,
                               package_run, run_name: str, run_timestamp: Optional[datetime], run_metrics: Dict):
        """Process issue-level nested runs."""
        for issue_info in issues:
            issue_id = issue_info["original_id"]
            clean_issue_id = issue_info["clean_issue_id"]
            issue_data = issue_info["data"]

            with mlflow.start_run(experiment_id=experiment_id,
                                run_name=clean_issue_id,
                                nested=True):

                mlflow.set_tag(TAG_NODE_TYPE, self.node_type)
                mlflow.set_tag(TAG_LEVEL, LEVEL_ISSUE)
                mlflow.set_tag(TAG_EVALUATION_RUN, run_name)
                mlflow.set_tag(TAG_PARENT_RUN_ID, package_run.info.run_id)
                mlflow.set_tag(TAG_PACKAGE, package_info["package"])
                mlflow.set_tag(TAG_VERSION, package_info["version"])
                mlflow.set_tag(TAG_PACKAGE_VERSION, f"{package_info['package']}-{package_info['version']}")
                mlflow.set_tag(TAG_ISSUE_ID, clean_issue_id)
                mlflow.set_tag(TAG_ORIGINAL_ISSUE_ID, issue_id)

                if run_timestamp:
                    mlflow.set_tag(TAG_RUN_TIMESTAMP, run_timestamp.isoformat())
                    mlflow.set_tag(TAG_RUN_DATE, run_timestamp.strftime("%Y-%m-%d"))
                    mlflow.set_tag(TAG_RUN_TIME, run_timestamp.strftime("%H:%M:%S"))

                # Log issue-specific metrics
                self._log_issue_metrics(issue_data, run_metrics)

                # Log issue-specific parameters and results
                self._log_issue_results(issue_data)

                # Log issue data as artifact
                self._log_issue_artifact(issue_id, run_name, package_info, issue_data)

    @abstractmethod
    def _log_issue_metrics(self, issue_data: Dict, run_metrics: Dict):
        """Log issue-specific metrics for this node type."""
        pass

    def _log_issue_results(self, issue_data: Dict):
        """Log issue-specific parameters and expected results."""
        # Log expected output if available
        if "expected_output_obj" in issue_data:
            expected = issue_data["expected_output_obj"]
            self._log_param_truncated(EXPECTED_OUTPUT_PARAM, expected)

        # Log question/input if available
        if "question" in issue_data:
            question = issue_data["question"]
            self._log_param_truncated(INPUT_QUESTION_PARAM, question)

    def _log_issue_artifact(self, issue_id: str, run_name: str, package_info: Dict, issue_data: Dict):
        """Log issue data as MLflow artifact."""
        # Create artifact with full issue context
        issue_artifact = {
            "issue_id": issue_id,
            "node_type": self.node_type,
            "run_name": run_name,
            "package": package_info["package"],
            "version": package_info["version"],
            "data": issue_data
        }

        # Create temporary file for issue data
        with tempfile.NamedTemporaryFile(mode='w', suffix=f'_{issue_id.replace("/", "_")}.json', delete=False) as f:
            json.dump(issue_artifact, f, indent=2)
            temp_path = f.name

        try:
            mlflow.log_artifact(temp_path, f"issue_data/{issue_id.replace('/', '_')}.json")
        finally:
            # Clean up temp file
            os.unlink(temp_path)

    def _extract_package_info(self, issue_id: str) -> Tuple[str, str, str]:
        """
        Extract package name, version, and issue ID from naming convention.

        Format: {package}-{version}-{issue_id}
        Examples:
        - audit-4_0-buffer_overflow_known_fp -> ("audit", "4.0", "buffer_overflow_known_fp")
        - glibc-2_8-memory_leak_borderline -> ("glibc", "2.8", "memory_leak_borderline")

        Returns:
            Tuple of (package_name, version, clean_issue_id)
        """
        parts = issue_id.split("-")

        if len(parts) >= 3:
            package_name = parts[0]
            version_part = parts[1]
            issue_part = "-".join(parts[2:])  # Join remaining parts as issue ID

            # Convert version format: 4_0 -> 4.0, 2_8 -> 2.8
            version = version_part.replace("_", ".")

            return package_name, version, issue_part
        else:
            # Fallback for malformed IDs
            return "unknown", "unknown", issue_id
