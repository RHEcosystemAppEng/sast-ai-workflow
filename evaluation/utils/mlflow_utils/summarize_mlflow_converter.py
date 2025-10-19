#!/usr/bin/env python3
"""
Summarize Node MLflow Converter for SAST AI Workflow Evaluation Dashboard

This module handles the conversion of summarize justifications evaluation reports
into MLflow format with summarization quality evaluation metrics.

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
    CONCISENESS_KEY,
    DEFAULT_COUNT_VALUE,
    DEFAULT_METRIC_VALUE,
    EVAL_OUTPUT_ITEMS_KEY,
    EVALUATION_KEY,
    EVALUATION_METRICS_FILE,
    EXPECTED_OUTPUT_PARAM,
    FACTUAL_ACCURACY_KEY,
    GENERATED_SUMMARY_PARAM,
    INFERENCE_KEY,
    INFERENCE_OPTIMIZATION_FILE,
    LLM_CALL_COUNT_KEY,
    LOGICAL_FLOW_KEY,
    MAX_PARAM_LENGTH,
    OVERALL_SCORE_KEY,
    PROFESSIONAL_TONE_KEY,
    PROFILER_TRACES_FILE,
    REASONING_KEY,
    SEMANTIC_SIMILARITY_KEY,
    SEMANTIC_SIMILARITY_SCALE_FACTOR,
    SIMILAR_ISSUES_COUNT_KEY,
    SINGLE_ISSUE_COUNT,
    SINGLE_PACKAGE_COUNT,
    SUMMARIZATION_QUALITY_FILE,
    TECHNICAL_ACCURACY_KEY,
    TOTAL_TOKENS_KEY,
)


class SummarizeNodeConverter(BaseMLflowConverter):
    """Converter for summarize justifications evaluation reports."""

    @property
    def node_type(self) -> str:
        return "summarize_justifications"

    @property
    def experiment_name(self) -> str:
        return "Summarize Justifications"

    def _load_run_metrics(self, run_dir: Path) -> Dict[str, Any]:
        """Load summarize-specific metrics files."""
        metrics = {}

        # Load evaluation metrics
        eval_metrics_file = run_dir / EVALUATION_METRICS_FILE
        if eval_metrics_file.exists():
            metrics[EVALUATION_KEY] = self._process_evaluation_metrics(eval_metrics_file)

        # Load inference optimization (tokens and timing)
        inference_file = run_dir / INFERENCE_OPTIMIZATION_FILE
        if inference_file.exists():
            metrics[INFERENCE_KEY] = self._process_inference_optimization(inference_file)

        # Load summarization quality evaluation
        summarization_quality_file = run_dir / SUMMARIZATION_QUALITY_FILE
        if summarization_quality_file.exists():
            metrics["summarization_quality"] = load_json_file(str(summarization_quality_file))

        # Load profiler traces for timing data
        profiler_file = run_dir / PROFILER_TRACES_FILE
        if profiler_file.exists():
            metrics["profiler"] = load_json_file(str(profiler_file))

        return metrics

    def _log_issue_metrics(self, issue_data: Dict, run_metrics: Dict):
        """Log summarize-specific issue metrics."""
        # For summarization, generated_answer is often a plain text string, not JSON
        answer_data = issue_data.get("generated_answer", "")
        quality_eval_data = run_metrics.get("summarization_quality")
        self._log_summarize_issue_metrics(answer_data, issue_data, quality_eval_data)

    def _log_summarize_issue_metrics(self, answer_data: Dict, issue_data: Dict, quality_eval_data: Dict = None):
        """Log summarize justifications specific metrics optimized for 8-column structure."""
        # Initialize 8-column metrics with defaults
        total_packages = SINGLE_PACKAGE_COUNT
        total_issues = SINGLE_ISSUE_COUNT
        total_tokens = DEFAULT_COUNT_VALUE
        avg_time_per_request = DEFAULT_METRIC_VALUE
        llm_call_count = DEFAULT_COUNT_VALUE

        # Quality evaluation defaults
        overall_score = DEFAULT_METRIC_VALUE
        semantic_similarity = DEFAULT_METRIC_VALUE
        factual_accuracy = DEFAULT_METRIC_VALUE
        conciseness = DEFAULT_METRIC_VALUE
        professional_tone = DEFAULT_METRIC_VALUE

        # Extract quality metrics from quality evaluation data (authoritative source)
        issue_id = issue_data.get("id", "")
        if quality_eval_data and EVAL_OUTPUT_ITEMS_KEY in quality_eval_data:
            for eval_item in quality_eval_data[EVAL_OUTPUT_ITEMS_KEY]:
                if eval_item.get("id") == issue_id:
                    overall_score = eval_item.get("score", DEFAULT_METRIC_VALUE)
                    reasoning = eval_item.get(REASONING_KEY, {}).get(REASONING_KEY, {})
                    if isinstance(reasoning, dict):
                        semantic_similarity = reasoning.get("SEMANTIC_SIMILARITY", DEFAULT_METRIC_VALUE)
                        factual_accuracy = reasoning.get("FACTUAL_ACCURACY", DEFAULT_METRIC_VALUE)
                        conciseness = reasoning.get("CONCISENESS", DEFAULT_METRIC_VALUE)
                        professional_tone = reasoning.get("PROFESSIONAL_TONE", DEFAULT_METRIC_VALUE)
                    break

        # Extract performance metrics from issue data
        total_tokens, avg_time_per_request, llm_call_count = self._extract_performance_metrics_from_issue(issue_data)

        # Log core metrics (consistent across all nodes)
        self._log_standard_metrics(
            total_packages, total_issues, DEFAULT_COUNT_VALUE,  # similar_issues_count = 0 for summarization
            overall_score, factual_accuracy, total_tokens,
            avg_time_per_request, llm_call_count
        )

        # Log summarization-specific quality metrics
        mlflow.log_metric(OVERALL_SCORE_KEY, overall_score)
        mlflow.log_metric(SEMANTIC_SIMILARITY_KEY, semantic_similarity)
        mlflow.log_metric(FACTUAL_ACCURACY_KEY, factual_accuracy)
        mlflow.log_metric(CONCISENESS_KEY, conciseness)
        mlflow.log_metric(PROFESSIONAL_TONE_KEY, professional_tone)

        # Keep essential parameters
        generated = answer_data if isinstance(answer_data, str) else str(answer_data)
        expected = issue_data.get("expected_output_obj", "")
        self._log_param_truncated(GENERATED_SUMMARY_PARAM, generated)
        self._log_param_truncated(EXPECTED_OUTPUT_PARAM, expected if isinstance(expected, str) else str(expected))

    def _aggregate_package_metrics(self, issues: List[dict], run_metrics: Dict) -> Dict[str, float]:
        """Aggregate summarize metrics across all issues in a package."""
        # Initialize 8-column metrics
        total_packages = SINGLE_PACKAGE_COUNT
        total_issues = len(issues)
        similar_issues_count = DEFAULT_COUNT_VALUE
        summarize_precision = DEFAULT_METRIC_VALUE
        summarize_recall = DEFAULT_METRIC_VALUE
        total_tokens = DEFAULT_COUNT_VALUE
        avg_time_per_request = DEFAULT_METRIC_VALUE
        llm_call_count = DEFAULT_COUNT_VALUE

        # For summarization, aggregate quality metrics
        quality_eval_data = run_metrics.get("summarization_quality", {})

        total_overall_score = DEFAULT_METRIC_VALUE
        total_semantic_similarity = DEFAULT_METRIC_VALUE
        total_factual_accuracy = DEFAULT_METRIC_VALUE
        total_conciseness = DEFAULT_METRIC_VALUE
        total_professional_tone = DEFAULT_METRIC_VALUE
        valid_scores = DEFAULT_COUNT_VALUE

        if quality_eval_data and EVAL_OUTPUT_ITEMS_KEY in quality_eval_data:
            for issue_info in issues:
                issue_id = issue_info["original_id"]

                # Find quality evaluation for this issue
                for eval_item in quality_eval_data[EVAL_OUTPUT_ITEMS_KEY]:
                    if eval_item.get("id") == issue_id:
                        total_overall_score += eval_item.get("score", DEFAULT_METRIC_VALUE)
                        reasoning = eval_item.get(REASONING_KEY, {}).get(REASONING_KEY, {})
                        if isinstance(reasoning, dict):
                            total_semantic_similarity += reasoning.get("SEMANTIC_SIMILARITY", DEFAULT_METRIC_VALUE)
                            total_factual_accuracy += reasoning.get("FACTUAL_ACCURACY", DEFAULT_METRIC_VALUE)
                            total_conciseness += reasoning.get("CONCISENESS", DEFAULT_METRIC_VALUE)
                            total_professional_tone += reasoning.get("PROFESSIONAL_TONE", DEFAULT_METRIC_VALUE)
                        valid_scores += 1
                        break

            # Calculate package averages for quality metrics (use as precision/recall for 8-column compatibility)
            if valid_scores > 0:
                summarize_precision = total_overall_score / valid_scores
                summarize_recall = total_factual_accuracy / valid_scores
                similar_issues_count = int(total_semantic_similarity / valid_scores * SEMANTIC_SIMILARITY_SCALE_FACTOR)

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
            precision=summarize_precision,
            recall=summarize_recall,
            total_tokens=total_tokens,
            avg_time_per_request=avg_time_per_request,
            llm_call_count=llm_call_count
        )

        # Convert to dictionary with summarize-specific prefix
        base_dict = package_metrics.to_dict_with_prefix("summarize")

        # Add summarization-specific quality metrics
        base_dict.update({
            SEMANTIC_SIMILARITY_KEY: similar_issues_count / SEMANTIC_SIMILARITY_SCALE_FACTOR if similar_issues_count > 0 else DEFAULT_METRIC_VALUE,
            OVERALL_SCORE_KEY: summarize_precision,
            FACTUAL_ACCURACY_KEY: summarize_recall,
            CONCISENESS_KEY: total_conciseness / valid_scores if valid_scores > 0 else DEFAULT_METRIC_VALUE,
            PROFESSIONAL_TONE_KEY: total_professional_tone / valid_scores if valid_scores > 0 else DEFAULT_METRIC_VALUE
        })

        return base_dict

    def _calculate_run_level_metrics(self, filtered_issues: List[Dict], run_metrics: Dict) -> Tuple[int, float, float]:
        """Calculate run-level summarization metrics."""
        run_similar_issues_count = 0
        run_summarize_precision = 0.0
        run_summarize_recall = 0.0

        if "summarization_quality" in run_metrics:
            # Calculate run-level summarization quality metrics
            quality_eval_data = run_metrics["summarization_quality"]

            if quality_eval_data and "eval_output_items" in quality_eval_data:
                total_overall_score = 0.0
                total_semantic_similarity = 0.0
                total_factual_accuracy = 0.0
                valid_scores = 0

                for issue_data in filtered_issues:
                    issue_id = issue_data["id"]

                    # Find quality evaluation for this issue
                    for eval_item in quality_eval_data["eval_output_items"]:
                        if eval_item.get("id") == issue_id:
                            total_overall_score += eval_item.get("score", 0.0)
                            reasoning = eval_item.get("reasoning", {}).get("reasoning", {})
                            if isinstance(reasoning, dict):
                                total_semantic_similarity += reasoning.get("SEMANTIC_SIMILARITY", 0.0)
                                total_factual_accuracy += reasoning.get("FACTUAL_ACCURACY", 0.0)
                            valid_scores += 1
                            break

                # Calculate run averages for quality metrics
                if valid_scores > 0:
                    run_summarize_precision = total_overall_score / valid_scores  # Use overall score as precision
                    run_summarize_recall = total_factual_accuracy / valid_scores  # Use factual accuracy as recall
                    run_similar_issues_count = int(total_semantic_similarity / valid_scores * 10)  # Scale semantic similarity

        return run_similar_issues_count, run_summarize_precision, run_summarize_recall

    def _log_additional_run_metrics(self, filtered_issues: List[Dict], run_metrics: Dict):
        """Log additional summarization-specific run metrics."""
        similar_issues_count, precision, recall = self._calculate_run_level_metrics(filtered_issues, run_metrics)

        # Calculate run-level quality metrics
        quality_eval_data = run_metrics.get("summarization_quality", {})
        run_conciseness = 0.0
        run_professional_tone = 0.0

        if quality_eval_data and "eval_output_items" in quality_eval_data:
            total_conciseness = 0.0
            total_professional_tone = 0.0
            valid_scores = 0

            for issue_data in filtered_issues:
                issue_id = issue_data["id"]
                for eval_item in quality_eval_data["eval_output_items"]:
                    if eval_item.get("id") == issue_id:
                        reasoning = eval_item.get("reasoning", {}).get("reasoning", {})
                        if isinstance(reasoning, dict):
                            total_conciseness += reasoning.get("CONCISENESS", 0.0)
                            total_professional_tone += reasoning.get("PROFESSIONAL_TONE", 0.0)
                        valid_scores += 1
                        break

            if valid_scores > 0:
                run_conciseness = total_conciseness / valid_scores
                run_professional_tone = total_professional_tone / valid_scores

        mlflow.log_metric(SEMANTIC_SIMILARITY_KEY, similar_issues_count / SEMANTIC_SIMILARITY_SCALE_FACTOR if similar_issues_count > 0 else DEFAULT_METRIC_VALUE)
        mlflow.log_metric(OVERALL_SCORE_KEY, precision)
        mlflow.log_metric(FACTUAL_ACCURACY_KEY, recall)
        mlflow.log_metric(CONCISENESS_KEY, run_conciseness)
        mlflow.log_metric(PROFESSIONAL_TONE_KEY, run_professional_tone)