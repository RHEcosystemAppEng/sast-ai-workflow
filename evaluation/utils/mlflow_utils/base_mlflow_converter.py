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
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import mlflow
import mlflow.tracking

# Add src directory to path for importing Utils
sys.path.insert(0, str(Path(__file__).resolve().parents[3] / "src"))
from Utils.file_utils import load_json_file


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
        run_dirs = [d for d in node_dir.iterdir() if d.is_dir() and d.name.startswith("run_")]
        print(f"Found {len(run_dirs)} runs for {self.node_type}")

        processed_runs = 0
        for run_dir in sorted(run_dirs):
            try:
                self._process_single_run(run_dir)
                processed_runs += 1
            except Exception as e:
                print(f"Error processing {self.node_type}/{run_dir.name}: {e}")

        return processed_runs

    def setup_mlflow_tracking(self):
        """Set up MLflow tracking URI and create mlruns directory."""
        self.mlruns_dir.mkdir(exist_ok=True)
        mlflow.set_tracking_uri(f"file://{self.mlruns_dir.absolute()}")
        print(f"MLflow tracking URI set to: {mlflow.get_tracking_uri()}")

    def _process_single_run(self, run_dir: Path):
        """Process a single evaluation run for this node type."""
        run_name = run_dir.name
        print(f"Processing {self.node_type}/{run_name}")

        # Load workflow output to get individual issues
        workflow_file = run_dir / "workflow_output.json"
        if not workflow_file.exists():
            print(f"  No workflow_output.json found for {run_name}")
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
            # Skip run IDs (they start with "run--")
            if issue_id.startswith("run--"):
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
    def _load_run_metrics(self, run_dir: Path) -> Dict[str, any]:
        """Load metrics files specific to this node type."""
        pass

    def _process_single_run_data(self, run_name: str, filtered_issues: List[dict],
                               package_versions: Dict, run_dir: Path, run_metrics: Dict):
        """Process a single evaluation run with hierarchical structure."""
        # Get or create experiment
        experiment_id = self._get_or_create_experiment()

        # Extract run timestamp
        run_timestamp = self._extract_run_timestamp(run_name)

        # Level 1: Create evaluation run
        with mlflow.start_run(experiment_id=experiment_id, run_name=run_name) as parent_run:
            mlflow.set_tag("node_type", self.node_type)
            mlflow.set_tag("level", "evaluation_run")
            mlflow.set_tag("evaluation_run", run_name)
            mlflow.set_tag("total_packages", len(package_versions))
            mlflow.set_tag("total_issues", len(filtered_issues))

            if run_timestamp:
                mlflow.set_tag("run_timestamp", run_timestamp.isoformat())
                mlflow.set_tag("run_date", run_timestamp.strftime("%Y-%m-%d"))
                mlflow.set_tag("run_time", run_timestamp.strftime("%H:%M:%S"))

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
            mlflow.log_param("total_packages", len(package_versions))
            mlflow.log_param("total_issues", len(filtered_issues))
            mlflow.log_param("packages", ", ".join(list(package_versions.keys())))

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

        Returns:
            Tuple of (total_tokens, avg_time_per_request, llm_call_count)
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

    @abstractmethod
    def _log_additional_run_metrics(self, filtered_issues: List[Dict], run_metrics: Dict):
        """Log additional run-level metrics specific to this node type."""
        pass

    def _log_run_artifacts(self, run_dir: Path, artifact_prefix: str = ""):
        """Log run artifacts (evaluation files) to MLflow."""
        try:
            # Log key evaluation files as artifacts
            artifact_files = [
                ("evaluation_metrics.json", "metrics"),
                ("inference_optimization.json", "optimization"),
                ("workflow_output.json", "workflow"),
                ("filter_validation_report.json", "validation"),
                ("justification_quality_eval_output.json", "quality"),
                ("summarization_quality_eval_output.json", "quality"),
                ("all_requests_profiler_traces.json", "profiler")
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
                                run_name=f"📦 {package_name} {version}",
                                nested=True) as package_run:

                mlflow.set_tag("node_type", self.node_type)
                mlflow.set_tag("level", "package")
                mlflow.set_tag("evaluation_run", run_name)
                mlflow.set_tag("parent_run_id", parent_run.info.run_id)
                mlflow.set_tag("package", package_name)
                mlflow.set_tag("version", version)
                mlflow.set_tag("package_version", package_version_key)
                mlflow.set_tag("issue_count", len(issues))

                if run_timestamp:
                    mlflow.set_tag("run_timestamp", run_timestamp.isoformat())
                    mlflow.set_tag("run_date", run_timestamp.strftime("%Y-%m-%d"))
                    mlflow.set_tag("run_time", run_timestamp.strftime("%H:%M:%S"))

                # Log package-level aggregated metrics
                package_metrics = self._aggregate_package_metrics(issues, run_metrics)
                for metric_name, metric_value in package_metrics.items():
                    mlflow.log_metric(metric_name, metric_value)

                # Log package parameters
                mlflow.log_param("package_name", package_name)
                mlflow.log_param("version", version)
                mlflow.log_param("issue_count", len(issues))
                mlflow.log_param("issues", ", ".join([issue["clean_issue_id"] for issue in issues]))

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
                                run_name=f"🐛 {clean_issue_id}",
                                nested=True):

                mlflow.set_tag("node_type", self.node_type)
                mlflow.set_tag("level", "issue")
                mlflow.set_tag("evaluation_run", run_name)
                mlflow.set_tag("parent_run_id", package_run.info.run_id)
                mlflow.set_tag("package", package_info["package"])
                mlflow.set_tag("version", package_info["version"])
                mlflow.set_tag("package_version", f"{package_info['package']}-{package_info['version']}")
                mlflow.set_tag("issue_id", clean_issue_id)
                mlflow.set_tag("original_issue_id", issue_id)

                if run_timestamp:
                    mlflow.set_tag("run_timestamp", run_timestamp.isoformat())
                    mlflow.set_tag("run_date", run_timestamp.strftime("%Y-%m-%d"))
                    mlflow.set_tag("run_time", run_timestamp.strftime("%H:%M:%S"))

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
            if isinstance(expected, str):
                mlflow.log_param("expected_output", expected[:1000])  # Truncate long outputs
            else:
                mlflow.log_param("expected_output", str(expected)[:1000])

        # Log question/input if available
        if "question" in issue_data:
            question = issue_data["question"]
            if isinstance(question, str):
                mlflow.log_param("input_question", question[:1000])

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
