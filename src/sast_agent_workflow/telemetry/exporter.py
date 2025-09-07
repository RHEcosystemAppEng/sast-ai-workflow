import logging

from nat.builder.builder import Builder
from nat.cli.register_workflow import register_telemetry_exporter
from nat.data_models.span import Span
from nat.data_models.telemetry_exporter import TelemetryExporterBaseConfig
from nat.observability.mixin.batch_config_mixin import BatchConfigMixin
from nat.observability.mixin.collector_config_mixin import CollectorConfigMixin
from nat.observability.processor.processor import Processor
from nat.plugins.phoenix.phoenix_exporter import PhoenixOtelExporter
from pydantic import Field

from .metrics_config import WorkflowMetricsConfig

logger = logging.getLogger(__name__)


class CustomAttributeProcessor(Processor[Span, Span]):
    """Processor to add custom attributes to NAT spans."""

    def __init__(self, custom_attributes: dict[str, str] | None = None):
        self.custom_attributes = custom_attributes or {}

    async def process(self, item: Span) -> Span:
        """Add custom attributes to the span."""
        logger.info(f"CustomAttributeProcessor processing span: {item.name}")

        # Add static attributes
        item.set_attribute("custom.environment", "local")
        item.set_attribute("custom.version", "1.0.0")

        # Add configured attributes
        for key, value in self.custom_attributes.items():
            item.set_attribute(f"custom.{key}", value)

        logger.debug(f"Added custom attributes to span: {item.name}")
        return item


class WorkflowEvaluationProcessor(Processor[Span, Span]):
    """Processor to add configurable workflow evaluations to spans."""

    def __init__(self, config: "CustomPhoenixTelemetryExporter"):
        from .metrics_store import metrics_store

        self.config = config

        # Clear any previous workflow metrics to ensure fresh start
        metrics_store.clear_workflow_metrics()
        logger.debug("Cleared previous workflow metrics from metrics store")

        # Set the configuration in metrics store so it's shared across the workflow
        metrics_store.set_metrics_config(config.workflow_metrics)
        self.processed_workflows = set()

    async def process(self, item: Span) -> Span:
        """Add workflow evaluations based on configuration."""

        # Debug logging
        logger.info(f"WorkflowEvaluationProcessor processing span: {item.name}")
        logger.debug(f"Span attributes: {dict(item.attributes)}")

        is_workflow_span = self._is_workflow_root_span(item)
        logger.info(f"Is workflow root span: {is_workflow_span}")

        if is_workflow_span and item.context.span_id not in self.processed_workflows:
            logger.info(f"Processing workflow span: {item.name}")

            # Get metrics from metrics store
            try:
                from .metrics_store import metrics_store

                workflow_metrics = metrics_store.get_workflow_metrics()
                if workflow_metrics:
                    logger.info(f"Found metrics in metrics store: {list(workflow_metrics.keys())}")
                else:
                    logger.debug("No metrics found in metrics store")
            except Exception as e:
                logger.debug(f"Failed to get metrics store metrics: {e}")
                workflow_metrics = {}

            if workflow_metrics:
                logger.info(
                    f"Logging evaluations for span {item.name} with metrics: {list(workflow_metrics.keys())}"
                )
                await self._log_configured_evaluations(item, workflow_metrics)
                self.processed_workflows.add(item.context.span_id)
            else:
                logger.warning(f"No workflow metrics found for span: {item.name}")
                logger.warning(f"Available span attributes: {list(item.attributes.keys())}")

                # Check if this span has any metrics-related attributes at all
                metrics_attrs = [
                    k
                    for k in item.attributes.keys()
                    if "metric" in k.lower() or "workflow" in k.lower()
                ]
                if metrics_attrs:
                    logger.info(f"Found potential metrics attributes: {metrics_attrs}")
                else:
                    logger.warning("No metrics-related attributes found at all")

        return item

    def _is_workflow_root_span(self, span: Span) -> bool:
        """Check if span matches configured workflow indicators."""
        span_name_lower = span.name.lower()
        indicators = self.config.workflow_metrics.workflow_span_indicators

        logger.debug(f"Checking span '{span.name}' against indicators: {indicators}")

        for indicator in indicators:
            if indicator in span_name_lower:
                logger.info(f"Span '{span.name}' matches indicator '{indicator}'")
                return True

        logger.debug(f"Span '{span.name}' does not match any workflow indicators")
        return False

    async def _log_configured_evaluations(self, span: Span, metrics: dict):
        """Log evaluations based on configuration."""
        try:
            import pandas as pd
            from phoenix import Client
            from phoenix.trace import SpanEvaluations

            client = Client()
            span_id_hex = f"{span.context.span_id:016x}"

            # Use metrics store to get evaluation metrics
            from .metrics_store import metrics_store

            evaluation_metrics = metrics_store.get_metrics_for_evaluation(metrics)

            for eval_name, eval_data in evaluation_metrics.items():
                eval_df = pd.DataFrame(
                    {"score": [eval_data["value"]]},
                    index=pd.Index([span_id_hex], name="context.span_id"),
                )

                client.log_evaluations(SpanEvaluations(eval_name=eval_name, dataframe=eval_df))
                logger.info(f"Logged evaluation '{eval_name}': {eval_data['value']}")

        except Exception as e:
            logger.error(f"Failed to log workflow evaluations: {e}")


class CustomPhoenixTelemetryExporter(
    BatchConfigMixin, CollectorConfigMixin, TelemetryExporterBaseConfig, name="custom_phoenix"
):
    """Custom Phoenix telemetry exporter with configurable workflow metrics."""

    custom_attributes: dict[str, str] = Field(
        default_factory=dict, description="Custom attributes to add to all spans"
    )

    # Single source of truth for workflow metrics
    workflow_metrics: WorkflowMetricsConfig = Field(
        default_factory=WorkflowMetricsConfig, description="Workflow metrics configuration"
    )


@register_telemetry_exporter(config_type=CustomPhoenixTelemetryExporter)
async def custom_phoenix_telemetry_exporter(
    config: CustomPhoenixTelemetryExporter, builder: Builder
):
    """Create a custom Phoenix telemetry exporter with configurable workflow evaluations."""

    try:
        # Create the base Phoenix exporter
        exporter = PhoenixOtelExporter(
            endpoint=config.endpoint,
            project=config.project,
            batch_size=config.batch_size,
            flush_interval=config.flush_interval,
            max_queue_size=config.max_queue_size,
            drop_on_overflow=config.drop_on_overflow,
            shutdown_timeout=config.shutdown_timeout,
        )

        # Clear existing processors
        processors = exporter._processors.copy()
        exporter.clear_processors()

        # Add workflow evaluation processor first
        workflow_processor = WorkflowEvaluationProcessor(config)
        exporter.add_processor(workflow_processor)

        # Add custom attribute processor
        custom_processor = CustomAttributeProcessor(custom_attributes=config.custom_attributes)
        exporter.add_processor(custom_processor)

        # Re-add original processors
        for processor in processors:
            exporter.add_processor(processor)

        logger.info(
            f"Custom Phoenix exporter created with workflow evaluation processor. Processors: {len(exporter._processors)}"
        )
        logger.info(f"Workflow span indicators: {config.workflow_metrics.workflow_span_indicators}")
        logger.info(
            f"Metrics configuration: {len(config.workflow_metrics.metrics)} metrics defined"
        )

        yield exporter

    except Exception as ex:
        logger.error("Unable to create custom Phoenix exporter: %s", ex, exc_info=True)
        raise
