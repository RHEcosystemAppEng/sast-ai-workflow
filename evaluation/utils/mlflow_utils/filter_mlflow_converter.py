#!/usr/bin/env python3
"""
Filter Node MLflow Converter for SAST AI Workflow Evaluation Dashboard

This module handles the conversion of filter node evaluation reports
into MLflow format with FAISS similarity matching and classification metrics.

APPENG-3747: Evaluation Dashboard Creation
"""

import json
from pathlib import Path
from typing import Any, Dict, List, Tuple

import mlflow

from .base_mlflow_converter import BaseMLflowConverter, load_json_file, PackageMetrics
from .mlflow_constants import (
    AVG_TIME_PER_REQUEST_KEY,
    DEFAULT_COUNT_VALUE,
    DEFAULT_METRIC_VALUE,
    EVALUATION_KEY,
    EVALUATION_METRICS_FILE,
    FILTER_RESULT_PARAM,
    FILTER_VALIDATION_FILE,
    INFERENCE_KEY,
    INFERENCE_OPTIMIZATION_FILE,
    LLM_CALL_COUNT_KEY,
    PROFILER_TRACES_FILE,
    SIMILAR_ISSUES_COUNT_KEY,
    SINGLE_ISSUE_COUNT,
    SINGLE_PACKAGE_COUNT,
    TOTAL_TOKENS_KEY,
)


class FilterNodeConverter(BaseMLflowConverter):
    """Converter for filter node evaluation reports."""

    @property
    def node_type(self) -> str:
        return "filter"

    @property
    def experiment_name(self) -> str:
        return "Filter"

    def _load_run_metrics(self, run_dir: Path) -> Dict[str, Any]:
        """Load filter-specific metrics files."""
        metrics = {}

        # Load evaluation metrics
        eval_metrics_file = run_dir / EVALUATION_METRICS_FILE
        if eval_metrics_file.exists():
            metrics[EVALUATION_KEY] = self._process_evaluation_metrics(eval_metrics_file)

        # Load inference optimization (tokens and timing)
        inference_file = run_dir / INFERENCE_OPTIMIZATION_FILE
        if inference_file.exists():
            metrics[INFERENCE_KEY] = self._process_inference_optimization(inference_file)

        # Load filter validation report
        filter_file = run_dir / FILTER_VALIDATION_FILE
        if filter_file.exists():
            metrics["filter"] = self._process_filter_validation_report(filter_file)
            # Also store the full validation data for detailed metrics
            metrics["filter_validation"] = load_json_file(str(filter_file))

        # Load profiler traces for timing data
        profiler_file = run_dir / PROFILER_TRACES_FILE
        if profiler_file.exists():
            metrics["profiler"] = load_json_file(str(profiler_file))

        return metrics

    def _process_filter_validation_report(self, filter_file: Path) -> Dict[str, float]:
        """Process filter_validation_report.json file."""
        filter_data = load_json_file(str(filter_file))
        if not filter_data:
            return {}

        metrics = {}

        # Extract summary metrics
        summary = filter_data.get("summary", {})
        if "classification_accuracy" in summary:
            metrics["classification_accuracy"] = summary["classification_accuracy"]
        if "faiss_matching_accuracy" in summary and summary["faiss_matching_accuracy"] is not None:
            metrics["faiss_matching_accuracy"] = summary["faiss_matching_accuracy"]

        # Extract classification metrics
        classification_metrics = filter_data.get("classification_metrics", {})
        for key in ["precision", "recall", "f1_score", "true_positives", "false_positives", "true_negatives", "false_negatives"]:
            if key in classification_metrics:
                metrics[f"filter_{key}"] = classification_metrics[key]

        return metrics

    def _log_issue_metrics(self, issue_data: Dict, run_metrics: Dict):
        """Log filter-specific issue metrics."""
        if "generated_answer" not in issue_data:
            return

        try:
            answer_str = issue_data["generated_answer"]
            if isinstance(answer_str, str):
                answer_data = json.loads(answer_str)
            else:
                answer_data = answer_str

            # Get filter validation data
            filter_validation_data = run_metrics.get("filter_validation")
            self._log_filter_issue_metrics(answer_data, issue_data["id"], filter_validation_data)

        except (json.JSONDecodeError, KeyError, TypeError) as e:
            print(f"Warning: Could not parse generated_answer for issue {issue_data.get('id', 'unknown')}: {e}")

    def _log_filter_issue_metrics(self, answer_data: Dict, issue_id: str, filter_validation_data: Dict = None):
        """Log filter-specific metrics optimized for 8-column structure."""
        # Initialize 8-column metrics with defaults
        total_packages = SINGLE_PACKAGE_COUNT
        total_issues = SINGLE_ISSUE_COUNT
        similar_issues_count = DEFAULT_COUNT_VALUE
        filter_precision = DEFAULT_METRIC_VALUE
        filter_recall = DEFAULT_METRIC_VALUE
        total_tokens = DEFAULT_COUNT_VALUE
        avg_time_per_request = DEFAULT_METRIC_VALUE
        llm_call_count = DEFAULT_COUNT_VALUE

        # Extract similar issues count from FAISS validation data (authoritative source)
        if filter_validation_data and "detailed_results" in filter_validation_data:
            issue_validation = filter_validation_data["detailed_results"].get(issue_id, {})
            issues_data = issue_validation.get("issues", {})

            # Handle single-entry invariant defensively
            if len(issues_data) > 1:
                print(f"Warning: Expected at most 1 nested issue entry, found {len(issues_data)} for {issue_id}. Using first entry.")

            for issue_name, issue_detail in issues_data.items():
                faiss_matching = issue_detail.get("faiss_matching", {})
                if faiss_matching:
                    actual_matches = faiss_matching.get("actual_matches", [])
                    similar_issues_count = len(actual_matches)

                # Calculate issue-level precision/recall (1.0 if correct, 0.0 if not)
                classification = issue_detail.get("classification", {})
                if classification:
                    is_correct = classification.get("correct", False)
                    filter_precision = 1.0 if is_correct else 0.0
                    filter_recall = 1.0 if is_correct else 0.0
                break  # Use first (and only) issue's data

        # Fallback to workflow output if no FAISS data
        if similar_issues_count == 0 and "similar_known_issues" in answer_data:
            similar_issues = answer_data["similar_known_issues"]
            similar_issues_count = len(similar_issues) if isinstance(similar_issues, list) else 0

        # Log the 8 standardized metrics
        self._log_standard_metrics(
            total_packages, total_issues, similar_issues_count,
            filter_precision, filter_recall, total_tokens,
            avg_time_per_request, llm_call_count
        )

        # Keep essential classification info as parameters
        if "filter_result" in answer_data:
            self._log_param_truncated(FILTER_RESULT_PARAM, answer_data["filter_result"])

    def _aggregate_package_metrics(self, issues: List[dict], run_metrics: Dict) -> Dict[str, float]:
        """Aggregate filter metrics across all issues in a package."""
        # Initialize 8-column metrics
        total_packages = SINGLE_PACKAGE_COUNT
        total_issues = len(issues)
        similar_issues_count = DEFAULT_COUNT_VALUE
        filter_precision = DEFAULT_METRIC_VALUE
        filter_recall = DEFAULT_METRIC_VALUE
        total_tokens = DEFAULT_COUNT_VALUE
        avg_time_per_request = DEFAULT_METRIC_VALUE
        llm_call_count = DEFAULT_COUNT_VALUE

        # Get filter validation data for detailed metrics
        filter_validation_data = run_metrics.get("filter_validation", {})

        # Collect metrics from all issues in this package
        correct_classifications = DEFAULT_COUNT_VALUE
        total_similar_matches = DEFAULT_COUNT_VALUE

        for issue_info in issues:
            issue_data = issue_info["data"]
            issue_id = issue_info["original_id"]

            # Extract accuracy from validation report (authoritative source)
            if filter_validation_data and "detailed_results" in filter_validation_data:
                issue_validation = filter_validation_data["detailed_results"].get(issue_id, {})
                issues_data = issue_validation.get("issues", {})

                # Handle single-entry invariant defensively
                if len(issues_data) > 1:
                    print(f"Warning: Expected at most 1 nested issue entry, found {len(issues_data)} for {issue_id}. Using first entry.")

                for issue_name, issue_detail in issues_data.items():
                    # Count correct classifications
                    classification = issue_detail.get("classification", {})
                    if classification.get("correct", False):
                        correct_classifications += 1

                    # Use FAISS matches count as authoritative source
                    faiss_matching = issue_detail.get("faiss_matching", {})
                    if faiss_matching:
                        actual_matches = faiss_matching.get("actual_matches", [])
                        total_similar_matches += len(actual_matches)
                    break  # Use first (and only) issue's data

            # Fallback to workflow output if no FAISS data for this issue
            elif "generated_answer" in issue_data:
                try:
                    answer_str = issue_data["generated_answer"]
                    if isinstance(answer_str, str):
                        answer_data = json.loads(answer_str)
                    else:
                        answer_data = answer_str

                    if "similar_known_issues" in answer_data:
                        similar_issues = answer_data["similar_known_issues"]
                        if isinstance(similar_issues, list):
                            total_similar_matches += len(similar_issues)
                except:
                    pass

        # Calculate package-level precision and recall
        if total_issues > 0:
            filter_precision = correct_classifications / total_issues
            filter_recall = correct_classifications / total_issues  # For this context, precision = recall

        # Sum all similar issues from this package
        similar_issues_count = total_similar_matches

        # Aggregate performance metrics for this package
        total_time = DEFAULT_COUNT_VALUE
        total_calls = DEFAULT_COUNT_VALUE
        package_tokens = DEFAULT_COUNT_VALUE

        # Extract performance data directly from workflow output for each issue in package
        for issue_info in issues:
            issue_data = issue_info["data"]
            tokens, time_per_req, calls = self._extract_performance_metrics_from_issue(issue_data)
            package_tokens += tokens
            total_time += time_per_req * calls
            total_calls += calls

        # Calculate package averages
        avg_time_per_request = total_time / total_calls if total_calls > 0 else DEFAULT_METRIC_VALUE
        llm_call_count = total_calls
        total_tokens = package_tokens

        # Create base metrics using PackageMetrics dataclass
        package_metrics = PackageMetrics(
            total_packages=total_packages,
            total_issues=total_issues,
            similar_issues_count=similar_issues_count,
            precision=filter_precision,
            recall=filter_recall,
            total_tokens=total_tokens,
            avg_time_per_request=avg_time_per_request,
            llm_call_count=llm_call_count
        )

        # Convert to dictionary with filter-specific prefix
        return package_metrics.to_dict_with_prefix("filter")

    def _calculate_run_level_metrics(self, filtered_issues: List[Dict], run_metrics: Dict) -> Tuple[int, float, float]:
        """Calculate run-level filter metrics."""
        run_similar_issues_count = 0
        run_filter_precision = 0.0
        run_filter_recall = 0.0

        if "filter_validation" in run_metrics and run_metrics["filter_validation"]:
            filter_validation_data = run_metrics["filter_validation"]

            # Count actual similar issues from all issues in this run
            for issue_data in filtered_issues:
                issue_id = issue_data["id"]
                if "detailed_results" in filter_validation_data:
                    issue_validation = filter_validation_data["detailed_results"].get(issue_id, {})
                    issues_data = issue_validation.get("issues", {})

                    # Handle single-entry invariant defensively
                    if len(issues_data) > 1:
                        print(f"Warning: Expected at most 1 nested issue entry, found {len(issues_data)} for {issue_id}. Using first entry.")

                    for issue_name, issue_detail in issues_data.items():
                        faiss_matching = issue_detail.get("faiss_matching", {})
                        if faiss_matching:
                            actual_matches = faiss_matching.get("actual_matches", [])
                            run_similar_issues_count += len(actual_matches)
                        break  # Use first (and only) issue's data

            # Use classification metrics for precision/recall
            classification_metrics = filter_validation_data.get("classification_metrics", {})
            if classification_metrics:
                run_filter_precision = classification_metrics.get("precision", 0.0)
                run_filter_recall = classification_metrics.get("recall", 0.0)

        return run_similar_issues_count, run_filter_precision, run_filter_recall

    def _log_additional_run_metrics(self, filtered_issues: List[Dict], run_metrics: Dict):
        """Log additional filter-specific run metrics."""
        # Base class already logs filter_precision/filter_recall via _log_standard_metrics
        # No additional metrics needed for filter node
        pass