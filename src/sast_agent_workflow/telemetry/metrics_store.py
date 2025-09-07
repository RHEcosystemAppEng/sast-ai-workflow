"""
Workflow metrics storage and processing for Phoenix telemetry.

This module provides a centralized store for workflow metrics, handling
configuration, storage, and formatting for Phoenix evaluation logging.
"""

import logging
from typing import Optional

from .metrics_config import MetricDefinition, WorkflowMetricsConfig

logger = logging.getLogger(__name__)


class MetricsStore:
    """
    Central store for workflow metrics and telemetry operations.

    Handles configuration, storage, and processing of workflow metrics for
    Phoenix telemetry integration. Provides methods to store metrics from
    workflow execution and format them for evaluation logging.
    """

    def __init__(self):
        self._metrics_config: Optional[WorkflowMetricsConfig] = None
        self._workflow_metrics: dict = {}

    def get_metrics_config(self) -> WorkflowMetricsConfig:
        """Get or create the metrics configuration."""
        if self._metrics_config is None:
            self._metrics_config = WorkflowMetricsConfig()
        return self._metrics_config

    def set_metrics_config(self, config: WorkflowMetricsConfig):
        """Set the metrics configuration."""
        self._metrics_config = config

    def store_workflow_metrics(self, metrics: dict):
        """Store workflow metrics for later retrieval."""
        self._workflow_metrics.update(metrics)

    def get_workflow_metrics(self) -> dict:
        """Get a copy of stored workflow metrics."""
        return self._workflow_metrics.copy()

    def clear_workflow_metrics(self):
        """Clear stored workflow metrics (typically called at workflow start)."""
        self._workflow_metrics.clear()

    def _convert_metric_value(self, value, metric_def: MetricDefinition):
        """Convert metric value to appropriate type for evaluations."""
        if metric_def.metric_type == "numeric":
            return float(value)
        elif metric_def.metric_type == "count":
            return int(value)
        elif metric_def.metric_type == "boolean":
            return bool(value)
        else:  # string
            return str(value)

    def get_metrics_for_evaluation(self, tracker_metrics: dict) -> dict:
        """Get metrics that should be logged as Phoenix evaluations."""
        evaluation_metrics = {}

        config = self.get_metrics_config()
        for metric_def in config.get_evaluation_metrics():
            if metric_def.key in tracker_metrics and tracker_metrics[metric_def.key] is not None:
                try:
                    value = self._convert_metric_value(tracker_metrics[metric_def.key], metric_def)
                    evaluation_metrics[metric_def.evaluation_name] = {"value": value}
                except Exception as e:
                    logger.warning(f"Failed to prepare evaluation metric {metric_def.key}: {e}")

        return evaluation_metrics


# Shared metrics store instance for the workflow
metrics_store = MetricsStore()
