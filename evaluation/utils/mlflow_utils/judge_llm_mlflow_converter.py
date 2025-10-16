#!/usr/bin/env python3
"""
Judge LLM Node MLflow Converter for SAST AI Workflow Evaluation Dashboard

This module handles the conversion of judge LLM analysis evaluation reports
into MLflow format with justification quality evaluation and classification metrics.

APPENG-3747: Evaluation Dashboard Creation
"""

import json
from pathlib import Path
from typing import Dict, List, Tuple

import mlflow

from .base_mlflow_converter import BaseMLflowConverter


class JudgeLLMNodeConverter(BaseMLflowConverter):
    """Converter for judge LLM analysis evaluation reports."""

    @property
    def node_type(self) -> str:
        return "judge_llm_analysis"

    @property
    def experiment_name(self) -> str:
        return "Judge Llm Analysis"

    def _load_run_metrics(self, run_dir: Path) -> Dict[str, any]:
        """Load judge LLM-specific metrics files."""
        metrics = {}

        # Load evaluation metrics
        eval_metrics_file = run_dir / "evaluation_metrics.json"
        if eval_metrics_file.exists():
            metrics["evaluation"] = self._process_evaluation_metrics(eval_metrics_file)

        # Load inference optimization (tokens and timing)
        inference_file = run_dir / "inference_optimization.json"
        if inference_file.exists():
            metrics["inference"] = self._process_inference_optimization(inference_file)

        # Load judge LLM justification quality evaluation
        justification_quality_file = run_dir / "justification_quality_eval_output.json"
        if justification_quality_file.exists():
            metrics["justification_quality"] = self._load_json_file(justification_quality_file)

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
            print(f"Warning: Could not parse generated_answer for issue {issue_data.get('id', 'unknown')}: {e}")

    def _log_judge_issue_metrics(self, answer_data: Dict, issue_id: str, justification_quality_data: Dict = None):
        """Log judge LLM specific metrics optimized for 8-column structure."""
        # Initialize 8-column metrics with defaults
        total_packages = 1  # Issue belongs to 1 package
        total_issues = 1    # This is 1 issue
        similar_issues_count = 0
        judge_precision = 0.0
        judge_recall = 0.0
        total_tokens = 0
        avg_time_per_request = 0.0
        llm_call_count = 0

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
        clarity = 0.0
        completeness = 0.0
        technical_accuracy = 0.0
        logical_flow = 0.0
        overall_score = 0.0

        if justification_quality_data and "eval_output_items" in justification_quality_data:
            for eval_item in justification_quality_data["eval_output_items"]:
                if eval_item.get("id") == issue_id:
                    overall_score = eval_item.get("score", 0.0)
                    reasoning = eval_item.get("reasoning", {}).get("reasoning", {})
                    if isinstance(reasoning, dict):
                        clarity = reasoning.get("CLARITY", 0.0)
                        completeness = reasoning.get("COMPLETENESS", 0.0)
                        technical_accuracy = reasoning.get("TECHNICAL_ACCURACY", 0.0)
                        logical_flow = reasoning.get("LOGICAL_FLOW", 0.0)
                    break

        # Use quality metrics for precision/recall in 8-column structure
        judge_precision = overall_score  # Overall quality score as precision
        judge_recall = technical_accuracy  # Technical accuracy as recall
        similar_issues_count = justification_count  # Number of justifications as "similar issues"

        # Log the 8 standardized metrics
        self.log_standard_metrics(
            total_packages, total_issues, similar_issues_count,
            judge_precision, judge_recall, total_tokens,
            avg_time_per_request, llm_call_count
        )

        # Log judge-specific quality metrics
        mlflow.log_metric("clarity", clarity)
        mlflow.log_metric("completeness", completeness)
        mlflow.log_metric("technical_accuracy", technical_accuracy)
        mlflow.log_metric("logical_flow", logical_flow)
        mlflow.log_metric("overall_score", overall_score)

        # Log classification metrics
        mlflow.log_metric("is_true_positive", is_true_positive)
        mlflow.log_metric("is_final_decision", is_final_decision)
        mlflow.log_metric("justification_count", justification_count)

        # Keep essential parameters
        mlflow.log_param("investigation_result", investigation_result)
        mlflow.log_param("is_final", is_final)

        # Log justification text length if available
        if "short_justifications" in issue_analysis:
            justification_text = issue_analysis["short_justifications"]
            if isinstance(justification_text, str):
                mlflow.log_metric("justification_length", len(justification_text))
                mlflow.log_param("short_justifications", justification_text[:500])  # Truncate long outputs

    def _aggregate_package_metrics(self, issues: List[dict], run_metrics: Dict) -> Dict[str, float]:
        """Aggregate judge LLM metrics across all issues in a package."""
        # Initialize 8-column metrics
        total_packages = 1  # This package
        total_issues = len(issues)
        similar_issues_count = 0
        filter_precision = 0.0
        filter_recall = 0.0
        total_tokens = 0
        avg_time_per_request = 0.0
        llm_call_count = 0

        # For judge analysis, aggregate quality metrics and classification results
        justification_quality_data = run_metrics.get("justification_quality", {})

        true_positive_count = 0
        total_justification_count = 0
        total_clarity = 0.0
        total_completeness = 0.0
        total_technical_accuracy = 0.0
        total_logical_flow = 0.0
        total_overall_score = 0.0
        valid_quality_scores = 0

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
                        total_overall_score += eval_item.get("score", 0.0)
                        reasoning = eval_item.get("reasoning", {}).get("reasoning", {})
                        if isinstance(reasoning, dict):
                            total_clarity += reasoning.get("CLARITY", 0.0)
                            total_completeness += reasoning.get("COMPLETENESS", 0.0)
                            total_technical_accuracy += reasoning.get("TECHNICAL_ACCURACY", 0.0)
                            total_logical_flow += reasoning.get("LOGICAL_FLOW", 0.0)
                        valid_quality_scores += 1
                        break

        # Calculate package averages for judge analysis
        if total_issues > 0:
            # Use quality metrics for precision/recall (following 8-column structure)
            if valid_quality_scores > 0:
                filter_precision = total_overall_score / valid_quality_scores  # Overall quality as precision
                filter_recall = total_technical_accuracy / valid_quality_scores  # Technical accuracy as recall
            else:
                # Fallback to classification-based metrics
                filter_precision = true_positive_count / total_issues
                filter_recall = true_positive_count / total_issues

            # Use justification count as "similar issues"
            similar_issues_count = total_justification_count

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

        # Extract additional quality metrics for package level
        return {
            "total_packages": total_packages,
            "total_issues": total_issues,
            "similar_issues_count": similar_issues_count,
            "judge_precision": filter_precision,
            "judge_recall": filter_recall,
            "total_tokens": total_tokens,
            "avg_time_per_request": avg_time_per_request,
            "llm_call_count": llm_call_count,
            "clarity": total_clarity / valid_quality_scores if valid_quality_scores > 0 else 0.0,
            "completeness": total_completeness / valid_quality_scores if valid_quality_scores > 0 else 0.0,
            "technical_accuracy": filter_recall,  # Already calculated as average
            "logical_flow": total_logical_flow / valid_quality_scores if valid_quality_scores > 0 else 0.0,
            "overall_score": filter_precision,  # Already calculated as average
            "true_positive_rate": filter_precision,
            "accuracy": filter_recall
        }

    def _calculate_run_level_metrics(self, filtered_issues: List[Dict], run_metrics: Dict) -> Tuple[int, float, float]:
        """Calculate run-level judge LLM analysis metrics."""
        run_similar_issues_count = 0
        run_filter_precision = 0.0
        run_filter_recall = 0.0

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
                    run_filter_precision = total_overall_score / valid_scores  # Overall quality as precision
                    run_filter_recall = total_technical_accuracy / valid_scores  # Technical accuracy as recall
                else:
                    # Fallback to classification-based metrics
                    run_filter_precision = total_true_positives / len(filtered_issues)
                    run_filter_recall = total_true_positives / len(filtered_issues)

                run_similar_issues_count = total_justification_count

        return run_similar_issues_count, run_filter_precision, run_filter_recall

    def _log_additional_run_metrics(self, filtered_issues: List[Dict], run_metrics: Dict):
        """Log additional judge LLM-specific run metrics."""
        similar_issues_count, precision, recall = self._calculate_run_level_metrics(filtered_issues, run_metrics)

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

        mlflow.log_metric("similar_issues_count", similar_issues_count)
        mlflow.log_metric("judge_precision", precision)
        mlflow.log_metric("judge_recall", recall)
        mlflow.log_metric("clarity", run_clarity)
        mlflow.log_metric("completeness", run_completeness)
        mlflow.log_metric("technical_accuracy", recall)
        mlflow.log_metric("logical_flow", run_logical_flow)
        mlflow.log_metric("overall_score", precision)
        mlflow.log_metric("true_positive_rate", precision)
        mlflow.log_metric("accuracy", recall)