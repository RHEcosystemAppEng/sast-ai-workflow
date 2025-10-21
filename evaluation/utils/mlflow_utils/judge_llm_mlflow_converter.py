#!/usr/bin/env python3
"""
Judge LLM Node MLflow Converter for SAST AI Workflow Evaluation Dashboard

This module handles the conversion of judge LLM analysis evaluation reports
into MLflow format with justification quality evaluation and classification metrics.

APPENG-3747: Evaluation Dashboard Creation
"""

import json
from pathlib import Path
from typing import Any, Dict, List, Tuple

import mlflow

from .base_mlflow_converter import BaseMLflowConverter, load_json_file, PackageMetrics
from .mlflow_constants import (
    AVG_TIME_PER_REQUEST_KEY,
    CLARITY_KEY,
    COMPLETENESS_KEY,
    DEFAULT_COUNT_VALUE,
    DEFAULT_METRIC_VALUE,
    EVAL_OUTPUT_ITEMS_KEY,
    EVALUATION_KEY,
    EVALUATION_METRICS_FILE,
    INFERENCE_KEY,
    INFERENCE_OPTIMIZATION_FILE,
    INVESTIGATION_RESULT_PARAM,
    IS_FINAL_PARAM,
    JUSTIFICATION_QUALITY_FILE,
    LOGICAL_FLOW_KEY,
    LLM_CALL_COUNT_KEY,
    OVERALL_SCORE_KEY,
    PROFILER_TRACES_FILE,
    REASONING_KEY,
    SHORT_JUSTIFICATIONS_PARAM,
    SIMILAR_ISSUES_COUNT_KEY,
    SINGLE_ISSUE_COUNT,
    SINGLE_PACKAGE_COUNT,
    TECHNICAL_ACCURACY_KEY,
    TOTAL_TOKENS_KEY,
)


class JudgeLLMNodeConverter(BaseMLflowConverter):
    """Converter for judge LLM analysis evaluation reports."""

    @property
    def node_type(self) -> str:
        return "judge_llm_analysis"

    @property
    def experiment_name(self) -> str:
        return "Judge Llm Analysis"

    def _load_run_metrics(self, run_dir: Path) -> Dict[str, Any]:
        """Load judge LLM-specific metrics files."""
        metrics = {}

        # Load evaluation metrics
        eval_metrics_file = run_dir / EVALUATION_METRICS_FILE
        if eval_metrics_file.exists():
            metrics[EVALUATION_KEY] = self._process_evaluation_metrics(eval_metrics_file)

        # Load inference optimization (tokens and timing)
        inference_file = run_dir / INFERENCE_OPTIMIZATION_FILE
        if inference_file.exists():
            metrics[INFERENCE_KEY] = self._process_inference_optimization(inference_file)

        # Load judge LLM justification quality evaluation
        justification_quality_file = run_dir / JUSTIFICATION_QUALITY_FILE
        if justification_quality_file.exists():
            metrics["justification_quality"] = load_json_file(str(justification_quality_file))

        # Load profiler traces for timing data
        profiler_file = run_dir / PROFILER_TRACES_FILE
        if profiler_file.exists():
            metrics["profiler"] = load_json_file(str(profiler_file))

        return metrics

    def _log_issue_metrics(self, issue_data: Dict, run_metrics: Dict):
        """Log judge LLM-specific issue metrics."""
        if "generated_answer" not in issue_data:
            return

        try:
            answer_str = issue_data["generated_answer"]
            if isinstance(answer_str, str):
                answer_data = json.loads(answer_str)
            else:
                answer_data = answer_str

            # Get justification quality evaluation data
            justification_quality_data = run_metrics.get("justification_quality")
            self._log_judge_issue_metrics(answer_data, issue_data["id"], justification_quality_data)

        except (json.JSONDecodeError, KeyError, TypeError) as e:
            self.logger.warning(f"Could not parse generated_answer for issue {issue_data.get('id', 'unknown')}: {e}")

    def _log_judge_issue_metrics(self, answer_data: Dict, issue_id: str, justification_quality_data: Dict = None):
        """Log judge LLM specific metrics optimized for 8-column structure."""
        # Initialize 8-column metrics with defaults
        total_packages = SINGLE_PACKAGE_COUNT
        total_issues = SINGLE_ISSUE_COUNT
        similar_issues_count = DEFAULT_COUNT_VALUE
        judge_precision = DEFAULT_METRIC_VALUE
        judge_recall = DEFAULT_METRIC_VALUE
        total_tokens = DEFAULT_COUNT_VALUE
        avg_time_per_request = DEFAULT_METRIC_VALUE
        llm_call_count = DEFAULT_COUNT_VALUE

        # Handle nested issue format: {issue_id: {investigation_result: ...}}
        issue_analysis = answer_data.get(issue_id, answer_data)

        # Extract investigation result metrics
        investigation_result = issue_analysis.get("investigation_result", "")
        is_true_positive = 1 if investigation_result == "TRUE_POSITIVE" else 0
        is_final = issue_analysis.get("is_final", "FALSE")
        is_final_decision = 1 if is_final == "TRUE" else 0

        # Count justifications
        justifications = issue_analysis.get("justifications", [])
        justification_count = len(justifications) if isinstance(justifications, list) else 0

        # Extract quality metrics from justification quality evaluation data (authoritative source)
        clarity = DEFAULT_METRIC_VALUE
        completeness = DEFAULT_METRIC_VALUE
        technical_accuracy = DEFAULT_METRIC_VALUE
        logical_flow = DEFAULT_METRIC_VALUE
        overall_score = DEFAULT_METRIC_VALUE

        if justification_quality_data and EVAL_OUTPUT_ITEMS_KEY in justification_quality_data:
            for eval_item in justification_quality_data[EVAL_OUTPUT_ITEMS_KEY]:
                if eval_item.get("id") == issue_id:
                    overall_score = eval_item.get("score", DEFAULT_METRIC_VALUE)
                    reasoning = eval_item.get(REASONING_KEY, {}).get(REASONING_KEY, {})
                    if isinstance(reasoning, dict):
                        clarity = reasoning.get("CLARITY", DEFAULT_METRIC_VALUE)
                        completeness = reasoning.get("COMPLETENESS", DEFAULT_METRIC_VALUE)
                        technical_accuracy = reasoning.get("TECHNICAL_ACCURACY", DEFAULT_METRIC_VALUE)
                        logical_flow = reasoning.get("LOGICAL_FLOW", DEFAULT_METRIC_VALUE)
                    break

        # Use quality metrics for precision/recall in 8-column structure
        judge_precision = overall_score  # Overall quality score as precision
        judge_recall = technical_accuracy  # Technical accuracy as recall
        similar_issues_count = justification_count  # Number of justifications as "similar issues"

        # Log the 8 standardized metrics
        self._log_standard_metrics(
            total_packages, total_issues, similar_issues_count,
            judge_precision, judge_recall, total_tokens,
            avg_time_per_request, llm_call_count
        )

        # Log judge-specific quality metrics
        mlflow.log_metric(CLARITY_KEY, clarity)
        mlflow.log_metric(COMPLETENESS_KEY, completeness)
        mlflow.log_metric(TECHNICAL_ACCURACY_KEY, technical_accuracy)
        mlflow.log_metric(LOGICAL_FLOW_KEY, logical_flow)
        mlflow.log_metric(OVERALL_SCORE_KEY, overall_score)

        # Log classification metrics
        mlflow.log_metric("is_true_positive", is_true_positive)
        mlflow.log_metric("is_final_decision", is_final_decision)
        mlflow.log_metric("justification_count", justification_count)

        # Keep essential parameters
        self._log_param_truncated(INVESTIGATION_RESULT_PARAM, investigation_result)
        self._log_param_truncated(IS_FINAL_PARAM, is_final)

        # Log justification text length if available
        if "short_justifications" in issue_analysis:
            justification_text = issue_analysis["short_justifications"]
            if isinstance(justification_text, str):
                mlflow.log_metric("justification_length", len(justification_text))
                self._log_param_truncated(SHORT_JUSTIFICATIONS_PARAM, justification_text)

    def _aggregate_package_metrics(self, issues: List[dict], run_metrics: Dict) -> Dict[str, float]:
        """Aggregate judge LLM metrics across all issues in a package."""
        # Initialize 8-column metrics
        total_packages = SINGLE_PACKAGE_COUNT
        total_issues = len(issues)
        similar_issues_count = DEFAULT_COUNT_VALUE
        judge_precision = DEFAULT_METRIC_VALUE
        judge_recall = DEFAULT_METRIC_VALUE
        total_tokens = DEFAULT_COUNT_VALUE
        avg_time_per_request = DEFAULT_METRIC_VALUE
        llm_call_count = DEFAULT_COUNT_VALUE

        # For judge analysis, aggregate quality metrics and classification results
        justification_quality_data = run_metrics.get("justification_quality", {})

        true_positive_count = DEFAULT_COUNT_VALUE
        total_justification_count = DEFAULT_COUNT_VALUE
        total_clarity = DEFAULT_METRIC_VALUE
        total_completeness = DEFAULT_METRIC_VALUE
        total_technical_accuracy = DEFAULT_METRIC_VALUE
        total_logical_flow = DEFAULT_METRIC_VALUE
        total_overall_score = DEFAULT_METRIC_VALUE
        valid_quality_scores = DEFAULT_COUNT_VALUE

        for issue_info in issues:
            issue_data = issue_info["data"]
            issue_id = issue_info["original_id"]

            # Extract classification results from workflow data
            if "generated_answer" in issue_data:
                try:
                    answer_str = issue_data["generated_answer"]
                    if isinstance(answer_str, str):
                        answer_data = json.loads(answer_str)
                    else:
                        answer_data = answer_str

                    issue_analysis = answer_data.get(issue_id, answer_data)

                    if issue_analysis.get("investigation_result") == "TRUE_POSITIVE":
                        true_positive_count += 1

                    # Count justifications
                    justifications = issue_analysis.get("justifications", [])
                    if isinstance(justifications, list):
                        total_justification_count += len(justifications)
                except:
                    pass

            # Extract quality metrics from justification quality evaluation (authoritative source)
            if justification_quality_data and "eval_output_items" in justification_quality_data:
                for eval_item in justification_quality_data["eval_output_items"]:
                    if eval_item.get("id") == issue_id:
                        total_overall_score += eval_item.get("score", DEFAULT_METRIC_VALUE)
                        reasoning = eval_item.get("reasoning", {}).get("reasoning", {})
                        if isinstance(reasoning, dict):
                            total_clarity += reasoning.get("CLARITY", DEFAULT_METRIC_VALUE)
                            total_completeness += reasoning.get("COMPLETENESS", DEFAULT_METRIC_VALUE)
                            total_technical_accuracy += reasoning.get("TECHNICAL_ACCURACY", DEFAULT_METRIC_VALUE)
                            total_logical_flow += reasoning.get("LOGICAL_FLOW", DEFAULT_METRIC_VALUE)
                        valid_quality_scores += 1
                        break

        # Calculate package averages for judge analysis
        if total_issues > 0:
            # Use quality metrics for precision/recall (following 8-column structure)
            if valid_quality_scores > 0:
                judge_precision = total_overall_score / valid_quality_scores  # Overall quality as precision
                judge_recall = total_technical_accuracy / valid_quality_scores  # Technical accuracy as recall
            else:
                # Fallback to classification-based metrics
                judge_precision = true_positive_count / total_issues
                judge_recall = true_positive_count / total_issues

            # Use justification count as "similar issues"
            similar_issues_count = total_justification_count

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
            precision=judge_precision,
            recall=judge_recall,
            total_tokens=total_tokens,
            avg_time_per_request=avg_time_per_request,
            llm_call_count=llm_call_count
        )

        # Convert to dictionary with judge-specific prefix
        base_dict = package_metrics.to_dict_with_prefix("judge")

        # Add judge-specific quality metrics
        base_dict.update({
            CLARITY_KEY: total_clarity / valid_quality_scores if valid_quality_scores > 0 else DEFAULT_METRIC_VALUE,
            COMPLETENESS_KEY: total_completeness / valid_quality_scores if valid_quality_scores > 0 else DEFAULT_METRIC_VALUE,
            TECHNICAL_ACCURACY_KEY: judge_recall,  # Already calculated as average
            LOGICAL_FLOW_KEY: total_logical_flow / valid_quality_scores if valid_quality_scores > 0 else DEFAULT_METRIC_VALUE,
            OVERALL_SCORE_KEY: judge_precision,  # Already calculated as average
            "true_positive_rate": judge_precision,
            "accuracy": judge_recall
        })

        return base_dict

    def _calculate_run_level_metrics(self, filtered_issues: List[Dict], run_metrics: Dict) -> Tuple[int, float, float]:
        """Calculate run-level judge LLM analysis metrics."""
        run_similar_issues_count = 0
        run_judge_precision = 0.0
        run_judge_recall = 0.0

        if "justification_quality" in run_metrics:
            # Calculate run-level judge LLM analysis quality metrics
            justification_quality_data = run_metrics["justification_quality"]

            total_true_positives = 0
            total_justification_count = 0
            total_overall_score = 0.0
            total_technical_accuracy = 0.0
            valid_scores = 0

            for issue_data in filtered_issues:
                issue_id = issue_data["id"]

                # Extract classification results from workflow data
                if "generated_answer" in issue_data:
                    try:
                        answer_str = issue_data["generated_answer"]
                        if isinstance(answer_str, str):
                            answer_data = json.loads(answer_str)
                        else:
                            answer_data = answer_str

                        issue_analysis = answer_data.get(issue_id, answer_data)

                        if issue_analysis.get("investigation_result") == "TRUE_POSITIVE":
                            total_true_positives += 1

                        # Count justifications
                        justifications = issue_analysis.get("justifications", [])
                        if isinstance(justifications, list):
                            total_justification_count += len(justifications)
                    except:
                        pass

                # Find quality evaluation for this issue
                if justification_quality_data and "eval_output_items" in justification_quality_data:
                    for eval_item in justification_quality_data["eval_output_items"]:
                        if eval_item.get("id") == issue_id:
                            total_overall_score += eval_item.get("score", 0.0)
                            reasoning = eval_item.get("reasoning", {}).get("reasoning", {})
                            if isinstance(reasoning, dict):
                                total_technical_accuracy += reasoning.get("TECHNICAL_ACCURACY", 0.0)
                            valid_scores += 1
                            break

            # Calculate run averages for judge quality metrics
            if len(filtered_issues) > 0:
                if valid_scores > 0:
                    run_judge_precision = total_overall_score / valid_scores  # Overall quality as precision
                    run_judge_recall = total_technical_accuracy / valid_scores  # Technical accuracy as recall
                else:
                    # Fallback to classification-based metrics
                    run_judge_precision = total_true_positives / len(filtered_issues)
                    run_judge_recall = total_true_positives / len(filtered_issues)

                run_similar_issues_count = total_justification_count

        return run_similar_issues_count, run_judge_precision, run_judge_recall

    def _log_additional_run_metrics(self, filtered_issues: List[Dict], run_metrics: Dict):
        """Log additional judge LLM-specific run metrics."""
        # Calculate run-level judge quality metrics
        justification_quality_data = run_metrics.get("justification_quality", {})
        run_clarity = 0.0
        run_completeness = 0.0
        run_logical_flow = 0.0

        if justification_quality_data and "eval_output_items" in justification_quality_data:
            total_clarity = 0.0
            total_completeness = 0.0
            total_logical_flow = 0.0
            valid_scores = 0

            for issue_data in filtered_issues:
                issue_id = issue_data["id"]
                for eval_item in justification_quality_data["eval_output_items"]:
                    if eval_item.get("id") == issue_id:
                        reasoning = eval_item.get("reasoning", {}).get("reasoning", {})
                        if isinstance(reasoning, dict):
                            total_clarity += reasoning.get("CLARITY", 0.0)
                            total_completeness += reasoning.get("COMPLETENESS", 0.0)
                            total_logical_flow += reasoning.get("LOGICAL_FLOW", 0.0)
                        valid_scores += 1
                        break

            if valid_scores > 0:
                run_clarity = total_clarity / valid_scores
                run_completeness = total_completeness / valid_scores
                run_logical_flow = total_logical_flow / valid_scores

        # Log only additional quality metrics (base class logs judge_llm_analysis_precision/recall)
        mlflow.log_metric(CLARITY_KEY, run_clarity)
        mlflow.log_metric(COMPLETENESS_KEY, run_completeness)
        mlflow.log_metric(LOGICAL_FLOW_KEY, run_logical_flow)