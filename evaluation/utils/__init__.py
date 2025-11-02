"""
Evaluation utilities for SAST-AI-Workflow.

This package provides utilities for converting SAST AI evaluation reports
into MLflow format for dashboard visualization.

MLflow Components (located in mlflow_utils subpackage):
- BaseMLflowConverter: Abstract base class with common functionality
- FilterNodeConverter: Converter for filter node evaluation reports
- JudgeLLMNodeConverter: Converter for judge LLM analysis reports
- SummarizeNodeConverter: Converter for summarization reports
- MLflowEvaluationRunner: Main orchestrator for all converters

APPENG-3747: Evaluation Dashboard Creation
"""

# Import archive_results utility (always available)
from .archive_results import archive_evaluation_results

# Make MLflow imports optional - only needed for MLflow dashboard conversion
# Runners can work without MLflow when using PostgreSQL + Grafana
try:
    from .mlflow_utils import (
        BaseMLflowConverter,
        FilterNodeConverter,
        JudgeLLMNodeConverter,
        SummarizeNodeConverter,
        MLflowEvaluationRunner
    )

    __all__ = [
        'archive_evaluation_results',
        'BaseMLflowConverter',
        'FilterNodeConverter',
        'JudgeLLMNodeConverter',
        'SummarizeNodeConverter',
        'MLflowEvaluationRunner'
    ]
except ImportError:
    # MLflow not installed - evaluation runners will still work
    # MLflow converters just won't be available
    __all__ = [
        'archive_evaluation_results'
    ]