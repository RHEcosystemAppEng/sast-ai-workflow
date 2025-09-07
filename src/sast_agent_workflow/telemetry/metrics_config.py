from enum import Enum
from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


class MetricType(str, Enum):
    """Types of metrics that can be extracted."""

    NUMERIC = "numeric"
    COUNT = "count"
    BOOLEAN = "boolean"
    STRING = "string"


class MetricDefinition(BaseModel):
    """Definition of a single metric to extract and expose."""

    key: str = Field(description="Key in tracker.metrics")
    metric_type: MetricType = Field(description="Type of metric")
    evaluation_name: Optional[str] = Field(
        default=None, description="Phoenix evaluation name (if should be logged as evaluation)"
    )
    enabled: bool = Field(default=True, description="Whether to extract this metric")
    description: str = Field(default="", description="Description of the metric")


class WorkflowMetricsConfig(BaseModel):
    """Configuration for workflow metrics extraction and telemetry."""

    # Workflow span identification
    workflow_span_indicators: List[str] = Field(
        default_factory=lambda: ["workflow"],
        description="List of strings to identify workflow root spans",
    )

    # Metrics definitions - loaded from YAML configuration (NAT framework pattern)
    metrics: List[MetricDefinition] = Field(
        default_factory=list,
        description="List of metrics to extract and expose (populated by NAT from YAML config)",
    )

    def get_enabled_metrics(self) -> List[MetricDefinition]:
        """Get only enabled metrics."""
        return [m for m in self.metrics if m.enabled]

    def get_evaluation_metrics(self) -> List[MetricDefinition]:
        """Get metrics that should be logged as Phoenix evaluations."""
        return [m for m in self.get_enabled_metrics() if m.evaluation_name is not None]
