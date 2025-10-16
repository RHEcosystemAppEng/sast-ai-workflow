#!/usr/bin/env python3
"""
Summarize Node MLflow Converter for SAST AI Workflow Evaluation Dashboard

This module handles the conversion of summarize justifications evaluation reports
into MLflow format with summarization quality evaluation metrics.

APPENG-3747: Evaluation Dashboard Creation
"""

import json
from pathlib import Path
from typing import Dict, List, Tuple

import mlflow

from .base_mlflow_converter import BaseMLflowConverter


class SummarizeNodeConverter(BaseMLflowConverter):
    """Converter for summarize justifications evaluation reports."""

    @property
    def node_type(self) -> str:
        return "summarize_justifications"

    @property
    def experiment_name(self) -> str:
        return "Summarize Justifications"

    def _load_run_metrics(self, run_dir: Path) -> Dict[str, any]:
        """Load summarize-specific metrics files."""
        metrics = {}

        # Load evaluation metrics
        eval_metrics_file = run_dir / "evaluation_metrics.json"
        if eval_metrics_file.exists():
            metrics["evaluation"] = self._process_evaluation_metrics(eval_metrics_file)

        # Load inference optimization (tokens and timing)
        inference_file = run_dir / "inference_optimization.json"
        if inference_file.exists():
            metrics["inference"] = self._process_inference_optimization(inference_file)

        # Load summarization quality evaluation
        summarization_quality_file = run_dir / "summarization_quality_eval_output.json"
        if summarization_quality_file.exists():
            metrics["summarization_quality"] = self._load_json_file(summarization_quality_file)

        # Load profiler traces for timing data
        profiler_file = run_dir / "all_requests_profiler_traces.json"
        if profiler_file.exists():
            metrics["profiler"] = self._load_json_file(profiler_file)

        return metrics

    def _process_evaluation_metrics(self, metrics_file: Path) -> Dict[str, float]:
        """Process evaluation_metrics.json file."""
        metrics_data = self._load_json_file(metrics_file)
        if not metrics_data or "metrics" not in metrics_data:
            return {}

        metrics = metrics_data["metrics"]
        return {
            "precision": metrics.get("precision", 0.0),
            "recall": metrics.get("recall", 0.0),
            "f1_score": metrics.get("f1_score", 0.0),
            "accuracy": metrics.get("accuracy", 0.0),
            "total_items": metrics.get("total_items", 0),
            "true_positives": metrics.get("true_positives", 0),
            "false_positives": metrics.get("false_positives", 0),
            "true_negatives": metrics.get("true_negatives", 0),
            "false_negatives": metrics.get("false_negatives", 0),
        }

    def _process_inference_optimization(self, inference_file: Path) -> Dict[str, float]:
        """Process inference_optimization.json file for token usage and timing."""
        inference_data = self._load_json_file(inference_file)
        if not inference_data:
            return {}

        metrics = {}

        # Extract token usage metrics
        if "total_tokens" in inference_data:
            metrics["total_tokens"] = inference_data["total_tokens"]
        if "prompt_tokens" in inference_data:
            metrics["prompt_tokens"] = inference_data["prompt_tokens"]
        if "completion_tokens" in inference_data:
            metrics["completion_tokens"] = inference_data["completion_tokens"]

        # Extract timing metrics
        if "total_time_seconds" in inference_data:
            metrics["total_time_seconds"] = inference_data["total_time_seconds"]
        if "average_time_per_request" in inference_data:
            metrics["average_time_per_request"] = inference_data["average_time_per_request"]

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
        total_packages = 1  # Issue belongs to 1 package
        total_issues = 1    # This is 1 issue
        total_tokens = 0
        avg_time_per_request = 0.0
        llm_call_count = 0

        # Quality evaluation defaults
        overall_score = 0.0
        semantic_similarity = 0.0
        factual_accuracy = 0.0
        conciseness = 0.0
        professional_tone = 0.0

        # Extract quality metrics from quality evaluation data (authoritative source)
        issue_id = issue_data.get("id", "")
        if quality_eval_data and "eval_output_items" in quality_eval_data:
            for eval_item in quality_eval_data["eval_output_items"]:
                if eval_item.get("id") == issue_id:
                    overall_score = eval_item.get("score", 0.0)
                    reasoning = eval_item.get("reasoning", {}).get("reasoning", {})
                    if isinstance(reasoning, dict):
                        semantic_similarity = reasoning.get("SEMANTIC_SIMILARITY", 0.0)
                        factual_accuracy = reasoning.get("FACTUAL_ACCURACY", 0.0)
                        conciseness = reasoning.get("CONCISENESS", 0.0)
                        professional_tone = reasoning.get("PROFESSIONAL_TONE", 0.0)
                    break

        # Extract performance metrics from issue data
        total_tokens, avg_time_per_request, llm_call_count = self.extract_performance_metrics_from_issue(issue_data)

        # Log core metrics (consistent across all nodes)
        self.log_standard_metrics(
            total_packages, total_issues, 0,  # similar_issues_count = 0 for summarization
            overall_score, factual_accuracy, total_tokens,
            avg_time_per_request, llm_call_count
        )

        # Log summarization-specific quality metrics
        mlflow.log_metric("overall_score", overall_score)
        mlflow.log_metric("semantic_similarity", semantic_similarity)
        mlflow.log_metric("factual_accuracy", factual_accuracy)
        mlflow.log_metric("conciseness", conciseness)
        mlflow.log_metric("professional_tone", professional_tone)

        # Keep essential parameters
        generated = answer_data if isinstance(answer_data, str) else str(answer_data)
        expected = issue_data.get("expected_output_obj", "")
        mlflow.log_param("generated_summary", generated[:500])  # Truncate long outputs
        mlflow.log_param("expected_output", expected[:500] if isinstance(expected, str) else str(expected)[:500])

    def _aggregate_package_metrics(self, issues: List[dict], run_metrics: Dict) -> Dict[str, float]:
        """Aggregate summarize metrics across all issues in a package."""
        # Initialize 8-column metrics
        total_packages = 1  # This package
        total_issues = len(issues)
        similar_issues_count = 0
        filter_precision = 0.0
        filter_recall = 0.0
        total_tokens = 0
        avg_time_per_request = 0.0
        llm_call_count = 0

        # For summarization, aggregate quality metrics
        quality_eval_data = run_metrics.get("summarization_quality", {})

        total_overall_score = 0.0
        total_semantic_similarity = 0.0
        total_factual_accuracy = 0.0
        total_conciseness = 0.0
        total_professional_tone = 0.0

        if quality_eval_data and "eval_output_items" in quality_eval_data:
            valid_scores = 0
            for issue_info in issues:
                issue_id = issue_info["original_id"]

                # Find quality evaluation for this issue
                for eval_item in quality_eval_data["eval_output_items"]:
                    if eval_item.get("id") == issue_id:
                        total_overall_score += eval_item.get("score", 0.0)
                        reasoning = eval_item.get("reasoning", {}).get("reasoning", {})
                        if isinstance(reasoning, dict):
                            total_semantic_similarity += reasoning.get("SEMANTIC_SIMILARITY", 0.0)
                            total_factual_accuracy += reasoning.get("FACTUAL_ACCURACY", 0.0)
                            total_conciseness += reasoning.get("CONCISENESS", 0.0)
                            total_professional_tone += reasoning.get("PROFESSIONAL_TONE", 0.0)
                        valid_scores += 1
                        break

            # Calculate package averages for quality metrics (use as precision/recall for 8-column compatibility)
            if valid_scores > 0:
                filter_precision = total_overall_score / valid_scores
                filter_recall = total_factual_accuracy / valid_scores
                similar_issues_count = int(total_semantic_similarity / valid_scores * 10)  # Scale for visibility

        # Aggregate performance metrics for this package
        total_time = 0
        total_calls = 0
        package_tokens = 0

        # Extract performance data directly from workflow output for each issue in package
        for issue_info in issues:
            issue_data = issue_info["data"]
            tokens, time_per_req, calls = self.extract_performance_metrics_from_issue(issue_data)
            package_tokens += tokens
            total_time += time_per_req * calls
            total_calls += calls

        # Calculate package averages
        avg_time_per_request = total_time / total_calls if total_calls > 0 else 0.0
        llm_call_count = total_calls
        total_tokens = package_tokens

        # Return metrics with summarization-specific names
        return {
            "total_packages": total_packages,
            "total_issues": total_issues,
            "similar_issues_count": similar_issues_count,
            "summarize_precision": filter_precision,
            "summarize_recall": filter_recall,
            "total_tokens": total_tokens,
            "avg_time_per_request": avg_time_per_request,
            "llm_call_count": llm_call_count,
            "semantic_similarity": similar_issues_count / 10.0 if similar_issues_count > 0 else 0.0,  # Unscale back
            "overall_score": filter_precision,
            "factual_accuracy": filter_recall,
            "conciseness": total_conciseness / valid_scores if 'valid_scores' in locals() and valid_scores > 0 else 0.0,
            "professional_tone": total_professional_tone / valid_scores if 'valid_scores' in locals() and valid_scores > 0 else 0.0
        }

    def _calculate_run_level_metrics(self, filtered_issues: List[Dict], run_metrics: Dict) -> Tuple[int, float, float]:
        """Calculate run-level summarization metrics."""
        run_similar_issues_count = 0
        run_filter_precision = 0.0
        run_filter_recall = 0.0

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
                    run_filter_precision = total_overall_score / valid_scores  # Use overall score as precision
                    run_filter_recall = total_factual_accuracy / valid_scores  # Use factual accuracy as recall
                    run_similar_issues_count = int(total_semantic_similarity / valid_scores * 10)  # Scale semantic similarity

        return run_similar_issues_count, run_filter_precision, run_filter_recall

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

        mlflow.log_metric("semantic_similarity", similar_issues_count / 10.0 if similar_issues_count > 0 else 0.0)  # Unscale back
        mlflow.log_metric("overall_score", precision)
        mlflow.log_metric("factual_accuracy", recall)
        mlflow.log_metric("conciseness", run_conciseness)
        mlflow.log_metric("professional_tone", run_professional_tone)