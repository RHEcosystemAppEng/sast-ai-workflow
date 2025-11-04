#!/usr/bin/env python3
"""
Generate orchestrator-compatible JSON output from NAT evaluation results.

This module provides base and specialized classes for converting evaluation
results into JSON format for storage in the orchestrator database.
"""

import json
import logging
import sys
from abc import ABC, abstractmethod
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Optional

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from evaluation.constants import WORKFLOW_OUTPUT_FILENAME, PROFILER_TRACES_FILENAME

logger = logging.getLogger(__name__)

# NAT evaluation output keys
EVAL_OUTPUT_ITEMS_KEY = "eval_output_items"
REASONING_KEY = "reasoning"


class BaseEvaluationJsonGenerator(ABC):
    """Base class for generating evaluation JSON output."""

    def __init__(self, reports_dir: Path, dataset_filename: str):
        self.reports_dir = reports_dir
        self.dataset_filename = dataset_filename
        self.workflow_data = []
        self.quality_data = {}
        self.profiler_data = {}

    def generate_json(self) -> None:
        """Main method to generate and output JSON to stdout."""
        self._load_result_files()
        package_info = self._extract_package_info()
        issues = self._extract_issues()
        aggregated = self._calculate_aggregated_metrics(issues)
        result = self._build_output_structure(package_info, issues, aggregated)
        self._output_json(result)

    def generate_json_to_file(self, output_path: str) -> None:
        """Generate and write JSON directly to file."""
        self._load_result_files()
        package_info = self._extract_package_info()
        issues = self._extract_issues()
        aggregated = self._calculate_aggregated_metrics(issues)
        result = self._build_output_structure(package_info, issues, aggregated)

        with open(output_path, 'w') as f:
            json.dump(result, f, separators=(',', ':'))

    def _load_result_files(self) -> None:
        """Load workflow, quality, and profiler JSON files."""
        workflow_file = self.reports_dir / WORKFLOW_OUTPUT_FILENAME
        quality_file = self.reports_dir / self._get_quality_filename()
        profiler_file = self.reports_dir / PROFILER_TRACES_FILENAME

        self.workflow_data = self._load_json_file(workflow_file)
        self.quality_data = self._load_json_file(quality_file)
        self.profiler_data = self._load_json_file(profiler_file)

    def _load_json_file(self, file_path: Path) -> Any:
        """Load JSON file and return as Python object."""
        try:
            with open(file_path, 'r') as f:
                return json.load(f)
        except Exception as e:
            logger.warning(f"Could not load {file_path}: {e}")
            # workflow_output.json is a list, all other files are dicts
            if file_path.name == 'workflow_output.json':
                return []
            else:
                return {}

    @abstractmethod
    def _get_quality_filename(self) -> str:
        """Get the quality evaluation filename for this node type."""
        pass

    @abstractmethod
    def _get_node_type(self) -> str:
        """Get the node type identifier."""
        pass

    def _extract_package_info(self) -> Dict[str, Any]:
        """Extract package name and version from workflow data."""
        package_name = "unknown"
        package_version = "unknown"

        if self.workflow_data and len(self.workflow_data) > 0:
            first_id = self.workflow_data[0].get("id", "")
            parts = first_id.rsplit("_", 3)
            if parts:
                package_name = parts[0]
                name_parts = package_name.rsplit("-", 2)
                if len(name_parts) >= 2:
                    package_version = "-".join(name_parts[-2:])

        return {
            "name": package_name,
            "version": package_version,
            "total_issues": len(self.workflow_data)
        }

    @abstractmethod
    def _extract_issues(self) -> List[Dict[str, Any]]:
        """Extract issue-level metrics. Node-specific implementation."""
        pass

    def _calculate_aggregated_metrics(self, issues: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Calculate package-level aggregated metrics from issues."""
        if not issues:
            return self._get_default_aggregated_metrics()

        quality_metrics = self._aggregate_quality_metrics(issues)
        performance_metrics = self._aggregate_performance_metrics(issues)

        return {
            "quality_metrics": quality_metrics,
            "performance_metrics": performance_metrics
        }

    @abstractmethod
    def _get_default_aggregated_metrics(self) -> Dict[str, Any]:
        """Get default aggregated metrics structure."""
        pass

    @abstractmethod
    def _aggregate_quality_metrics(self, issues: List[Dict[str, Any]]) -> Dict[str, float]:
        """Aggregate quality metrics from issues."""
        pass

    def _aggregate_performance_metrics(self, issues: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Aggregate performance metrics from issues."""
        total_tokens = sum(i["performance_metrics"]["tokens"] for i in issues)
        total_llm_calls = sum(i["performance_metrics"]["llm_calls"] for i in issues)

        return {
            "total_tokens": total_tokens,
            "avg_time_per_request": 0.0,
            "llm_call_count": total_llm_calls
        }

    def _build_output_structure(self, package_info: Dict[str, Any],
                                issues: List[Dict[str, Any]],
                                aggregated: Dict[str, Any]) -> Dict[str, Any]:
        """Build final JSON output structure."""
        return {
            "node_type": self._get_node_type(),
            "package_info": package_info,
            "aggregated_metrics": aggregated,
            "issues": issues,
            "metadata": {
                "timestamp": datetime.utcnow().isoformat() + "Z",
                "evaluation_dataset": self.dataset_filename
            }
        }

    def _output_json(self, result: Dict[str, Any]) -> None:
        """Output JSON with markers for orchestrator parsing."""
        logger.info("\n=== EVALUATION_RESULTS_JSON ===")
        logger.info(json.dumps(result, separators=(',', ':')))
        logger.info("=== END_EVALUATION_RESULTS_JSON ===\n")


class SummarizeJsonGenerator(BaseEvaluationJsonGenerator):
    """JSON generator for summarize_justifications evaluation."""

    def _get_quality_filename(self) -> str:
        return "summarization_quality_eval_output.json"

    def _get_node_type(self) -> str:
        return "summarize_justifications"

    def _extract_issues(self) -> List[Dict[str, Any]]:
        """Extract summarization-specific issue metrics."""
        issues = []
        quality_items = self.quality_data.get(EVAL_OUTPUT_ITEMS_KEY, [])

        for issue in self.workflow_data:
            issue_id = issue.get("id", "")
            quality_metrics = self._extract_quality_metrics_for_issue(issue_id, quality_items)
            performance_metrics = self._extract_performance_metrics_for_issue(issue)

            issues.append({
                "id": issue_id,
                "quality_metrics": quality_metrics,
                "performance_metrics": performance_metrics,
                "generated_answer": issue.get("generated_answer", ""),
                "expected_output": issue.get("expected_output", "")
            })

        return issues

    def _extract_quality_metrics_for_issue(self, issue_id: str,
                                          quality_items: List[Dict]) -> Dict[str, float]:
        """Extract quality metrics from judge LLM evaluation for a single issue."""
        default_metrics = {
            "overall_score": 0.0,
            "semantic_similarity": 0.0,
            "factual_accuracy": 0.0,
            "conciseness": 0.0,
            "professional_tone": 0.0
        }

        for qual_item in quality_items:
            if qual_item.get("id") == issue_id:
                default_metrics["overall_score"] = qual_item.get("score", 0.0)
                reasoning = qual_item.get(REASONING_KEY, {}).get(REASONING_KEY, {})
                if isinstance(reasoning, dict):
                    default_metrics["semantic_similarity"] = reasoning.get("SEMANTIC_SIMILARITY", 0.0)
                    default_metrics["factual_accuracy"] = reasoning.get("FACTUAL_ACCURACY", 0.0)
                    default_metrics["conciseness"] = reasoning.get("CONCISENESS", 0.0)
                    default_metrics["professional_tone"] = reasoning.get("PROFESSIONAL_TONE", 0.0)
                break

        return default_metrics

    def _extract_performance_metrics_for_issue(self, issue: Dict[str, Any]) -> Dict[str, Any]:
        """Extract performance metrics from intermediate steps."""
        total_tokens = 0
        llm_calls = 0

        for step in issue.get("intermediate_steps", []):
            payload = step.get("payload", {})
            usage_info = payload.get("usage_info", {})
            token_usage = usage_info.get("token_usage", {})
            total_tokens += token_usage.get("total_tokens", 0)
            llm_calls += 1

        return {
            "tokens": total_tokens,
            "time": 0.0,
            "llm_calls": llm_calls
        }

    def _get_default_aggregated_metrics(self) -> Dict[str, Any]:
        """Get default aggregated metrics for summarization."""
        return {
            "quality_metrics": {
                "overall_score": 0.0,
                "semantic_similarity": 0.0,
                "factual_accuracy": 0.0,
                "conciseness": 0.0,
                "professional_tone": 0.0
            },
            "performance_metrics": {
                "total_tokens": 0,
                "avg_time_per_request": 0.0,
                "llm_call_count": 0
            }
        }

    def _aggregate_quality_metrics(self, issues: List[Dict[str, Any]]) -> Dict[str, float]:
        """Aggregate summarization quality metrics."""
        num_issues = len(issues)

        total_score = sum(i["quality_metrics"]["overall_score"] for i in issues)
        total_sem_sim = sum(i["quality_metrics"]["semantic_similarity"] for i in issues)
        total_fact_acc = sum(i["quality_metrics"]["factual_accuracy"] for i in issues)
        total_concise = sum(i["quality_metrics"]["conciseness"] for i in issues)
        total_prof = sum(i["quality_metrics"]["professional_tone"] for i in issues)

        return {
            "overall_score": total_score / num_issues,
            "semantic_similarity": total_sem_sim / num_issues,
            "factual_accuracy": total_fact_acc / num_issues,
            "conciseness": total_concise / num_issues,
            "professional_tone": total_prof / num_issues
        }


class FilterJsonGenerator(BaseEvaluationJsonGenerator):
    """JSON generator for filter evaluation."""

    def _get_quality_filename(self) -> str:
        return "filter_validation_report.json"

    def _get_node_type(self) -> str:
        return "filter"

    def _extract_issues(self) -> List[Dict[str, Any]]:
        """Extract filter-specific issue metrics."""
        issues = []

        for issue in self.workflow_data:
            issue_id = issue.get("id", "")
            filter_result, similar_issues_count = self._extract_filter_metrics_for_issue(issue_id, issue)
            performance_metrics = self._extract_performance_metrics_for_issue(issue)

            issues.append({
                "id": issue_id,
                "filter_result": filter_result,
                "similar_issues_count": similar_issues_count,
                "performance_metrics": performance_metrics,
                "generated_answer": issue.get("generated_answer", ""),
                "expected_output": issue.get("expected_output", "")
            })

        return issues

    def _extract_filter_metrics_for_issue(self, issue_id: str, issue: Dict) -> tuple:
        """Extract filter result and similar issues count."""
        filter_result = "UNKNOWN"
        similar_issues_count = 0

        try:
            answer_str = issue.get("generated_answer", "")
            if isinstance(answer_str, str) and answer_str:
                answer_data = json.loads(answer_str)
            else:
                answer_data = answer_str

            filter_result = answer_data.get("filter_result", "UNKNOWN")

            if self.quality_data and "detailed_results" in self.quality_data:
                issue_validation = self.quality_data["detailed_results"].get(issue_id, {})
                issues_data = issue_validation.get("issues", {})

                for issue_name, issue_detail in issues_data.items():
                    faiss_matching = issue_detail.get("faiss_matching", {})
                    if faiss_matching:
                        actual_matches = faiss_matching.get("actual_matches", [])
                        similar_issues_count = len(actual_matches)
                    break

            if similar_issues_count == 0 and "similar_known_issues" in answer_data:
                similar_issues = answer_data["similar_known_issues"]
                similar_issues_count = len(similar_issues) if isinstance(similar_issues, list) else 0

        except (json.JSONDecodeError, KeyError, TypeError):
            pass

        return filter_result, similar_issues_count

    def _extract_performance_metrics_for_issue(self, issue: Dict[str, Any]) -> Dict[str, Any]:
        """Extract performance metrics from intermediate steps."""
        total_tokens = 0
        llm_calls = 0

        for step in issue.get("intermediate_steps", []):
            payload = step.get("payload", {})
            usage_info = payload.get("usage_info", {})
            token_usage = usage_info.get("token_usage", {})
            total_tokens += token_usage.get("total_tokens", 0)
            llm_calls += 1

        return {
            "tokens": total_tokens,
            "time": 0.0,
            "llm_calls": llm_calls
        }

    def _get_default_aggregated_metrics(self) -> Dict[str, Any]:
        """Get default aggregated metrics for filter."""
        return {
            "quality_metrics": {
                "faiss_matching_accuracy": 0.0
            },
            "performance_metrics": {
                "total_tokens": 0,
                "avg_time_per_request": 0.0,
                "llm_call_count": 0
            }
        }

    def _aggregate_quality_metrics(self, issues: List[Dict[str, Any]]) -> Dict[str, float]:
        """Aggregate filter quality metrics."""
        if not self.quality_data or "summary" not in self.quality_data:
            return {"faiss_matching_accuracy": 0.0}

        summary = self.quality_data.get("summary", {})
        faiss_accuracy = summary.get("faiss_matching_accuracy")

        if faiss_accuracy is not None:
            return {"faiss_matching_accuracy": faiss_accuracy}

        return {"faiss_matching_accuracy": 0.0}


class JudgeLLMJsonGenerator(BaseEvaluationJsonGenerator):
    """JSON generator for judge_llm_analysis evaluation."""

    def _get_quality_filename(self) -> str:
        return "justification_quality_eval_output.json"

    def _get_node_type(self) -> str:
        return "judge_llm_analysis"

    def _extract_issues(self) -> List[Dict[str, Any]]:
        """Extract judge LLM analysis-specific issue metrics."""
        issues = []
        quality_items = self.quality_data.get(EVAL_OUTPUT_ITEMS_KEY, [])

        for issue in self.workflow_data:
            issue_id = issue.get("id", "")
            quality_metrics = self._extract_quality_metrics_for_issue(issue_id, quality_items)
            performance_metrics = self._extract_performance_metrics_for_issue(issue)

            issues.append({
                "id": issue_id,
                "quality_metrics": quality_metrics,
                "performance_metrics": performance_metrics,
                "generated_answer": issue.get("generated_answer", ""),
                "expected_output": issue.get("expected_output", "")
            })

        return issues

    def _extract_quality_metrics_for_issue(self, issue_id: str,
                                          quality_items: List[Dict]) -> Dict[str, float]:
        """Extract quality metrics from judge LLM quality evaluator for a single issue."""
        default_metrics = {
            "overall_score": 0.0,
            "clarity": 0.0,
            "completeness": 0.0,
            "technical_accuracy": 0.0,
            "logical_flow": 0.0
        }

        for qual_item in quality_items:
            if qual_item.get("id") == issue_id:
                default_metrics["overall_score"] = qual_item.get("score", 0.0)
                reasoning = qual_item.get(REASONING_KEY, {}).get(REASONING_KEY, {})
                if isinstance(reasoning, dict):
                    default_metrics["clarity"] = reasoning.get("CLARITY", 0.0)
                    default_metrics["completeness"] = reasoning.get("COMPLETENESS", 0.0)
                    default_metrics["technical_accuracy"] = reasoning.get("TECHNICAL_ACCURACY", 0.0)
                    default_metrics["logical_flow"] = reasoning.get("LOGICAL_FLOW", 0.0)
                break

        return default_metrics

    def _extract_performance_metrics_for_issue(self, issue: Dict[str, Any]) -> Dict[str, Any]:
        """Extract performance metrics from intermediate steps."""
        total_tokens = 0
        llm_calls = 0

        for step in issue.get("intermediate_steps", []):
            payload = step.get("payload", {})
            usage_info = payload.get("usage_info", {})
            token_usage = usage_info.get("token_usage", {})
            total_tokens += token_usage.get("total_tokens", 0)
            llm_calls += 1

        return {
            "tokens": total_tokens,
            "time": 0.0,
            "llm_calls": llm_calls
        }

    def _get_default_aggregated_metrics(self) -> Dict[str, Any]:
        """Get default aggregated metrics for judge LLM analysis."""
        return {
            "quality_metrics": {
                "overall_score": 0.0,
                "clarity": 0.0,
                "completeness": 0.0,
                "technical_accuracy": 0.0,
                "logical_flow": 0.0
            },
            "performance_metrics": {
                "total_tokens": 0,
                "avg_time_per_request": 0.0,
                "llm_call_count": 0
            }
        }

    def _aggregate_quality_metrics(self, issues: List[Dict[str, Any]]) -> Dict[str, float]:
        """Aggregate judge LLM quality metrics."""
        num_issues = len(issues)

        total_score = sum(i["quality_metrics"]["overall_score"] for i in issues)
        total_clarity = sum(i["quality_metrics"]["clarity"] for i in issues)
        total_completeness = sum(i["quality_metrics"]["completeness"] for i in issues)
        total_technical = sum(i["quality_metrics"]["technical_accuracy"] for i in issues)
        total_logical = sum(i["quality_metrics"]["logical_flow"] for i in issues)

        return {
            "overall_score": total_score / num_issues,
            "clarity": total_clarity / num_issues,
            "completeness": total_completeness / num_issues,
            "technical_accuracy": total_technical / num_issues,
            "logical_flow": total_logical / num_issues
        }


__all__ = ['BaseEvaluationJsonGenerator', 'SummarizeJsonGenerator', 'FilterJsonGenerator', 'JudgeLLMJsonGenerator']