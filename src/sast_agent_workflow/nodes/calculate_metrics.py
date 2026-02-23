import logging

from nat.builder.builder import Builder
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.function import FunctionBaseConfig
from pydantic import Field

from common.constants import (
    METRICS_ERROR_CALCULATION_FAILED,
    METRICS_ERROR_IMPORT,
    METRICS_ERROR_NO_ISSUES,
    METRICS_ERROR_UNEXPECTED,
)
from dto.EvaluationSummary import EvaluationSummary
from dto.SASTWorkflowModels import SASTWorkflowTracker
from Utils.confidence_scoring import (
    calculate_aggregate_confidence_metrics,
    calculate_final_confidence,
    inject_mock_confidence_data,
)
from Utils.file_utils import get_human_verified_results
from Utils.workflow_utils import convert_tracker_to_summary_data

logger = logging.getLogger(__name__)

# Constants for metrics extraction
EVALUATION_SUMMARY_EXCLUDED_ATTRS = {
    "summary_data",
    "config",
    "ground_truth",
    "predicted_summary",
    "predicted_true_positives",
    "predicted_false_positives",
    "actual_true_positives",
    "actual_false_positives",
    "tp",
    "tn",
    "fp",
    "fn",
}


class CalculateMetricsConfig(FunctionBaseConfig, name="calculate_metrics"):
    description: str = Field(
        default="Calculate metrics function that calculates performance metrics for SAST analysis",
        description="Function description",
    )


@register_function(config_type=CalculateMetricsConfig)
async def calculate_metrics(config: CalculateMetricsConfig, builder: Builder):
    logger.info("Initializing Calculate_Metrics function...")

    async def _calculate_metrics_fn(  # NOSONAR - async required by NAT framework
        tracker: SASTWorkflowTracker,
    ) -> SASTWorkflowTracker:
        logger.info("Running Calculate_Metrics node - calculating metrics")
        logger.info(f"Calculate_Metrics node processing tracker with {len(tracker.issues)} issues")

        if not tracker.config:
            logger.warning("No config found in tracker - skipping metrics calculation")
            return tracker

        try:
            include_non_final = getattr(tracker.config, "WRITE_RESULTS_INCLUDE_NON_FINAL", True)
            summary_data = convert_tracker_to_summary_data(
                tracker, include_non_final=include_non_final
            )

            if not summary_data:
                logger.warning("No completed issues found for metrics calculation")
                tracker.metrics = {"error": METRICS_ERROR_NO_ISSUES}
                return tracker

            ground_truth = get_human_verified_results(tracker.config)

            evaluation_summary = EvaluationSummary(summary_data, tracker.config, ground_truth)

            tracker.metrics = _extract_metrics_from_evaluation_summary(evaluation_summary)

            # Calculate confidence scores for all issues
            confidence_scores = _calculate_confidence_scores(tracker)
            tracker.metrics['confidence_scores'] = confidence_scores

            logger.info(f"Successfully calculated metrics for {len(summary_data)} issues")

        except (AttributeError, ValueError, KeyError) as e:
            logger.error(f"Failed to calculate metrics due to data issue: {e}")
            tracker.metrics = {"error": f"{METRICS_ERROR_CALCULATION_FAILED}: {str(e)}"}
        except ImportError as e:
            logger.error(f"Failed to calculate metrics due to missing dependency: {e}")
            tracker.metrics = {"error": f"{METRICS_ERROR_IMPORT}: {str(e)}"}
        except Exception as e:
            logger.error(f"Unexpected error calculating metrics: {e}")
            tracker.metrics = {"error": f"{METRICS_ERROR_UNEXPECTED}: {str(e)}"}

        logger.info("Calculate_Metrics node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_calculate_metrics_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker,
        )
    except GeneratorExit:
        logger.info("Calculate_Metrics function exited early!")
    finally:
        logger.info("Cleaning up Calculate_Metrics function.")


def _extract_metrics_from_evaluation_summary(evaluation_summary: EvaluationSummary) -> dict:
    """
    Extract metrics from EvaluationSummary object, excluding internal attributes.

    Args:
        evaluation_summary: EvaluationSummary object containing calculated metrics

    Returns:
        Dictionary of metrics ready for storage in tracker
    """

    metrics = {
        "total_issues": len(evaluation_summary.predicted_summary),
        "predicted_true_positives": evaluation_summary.predicted_true_positives,
        "predicted_true_positives_count": len(evaluation_summary.predicted_true_positives),
        "predicted_false_positives": evaluation_summary.predicted_false_positives,
        "predicted_false_positives_count": len(evaluation_summary.predicted_false_positives),
        "has_ground_truth": evaluation_summary.ground_truth is not None,
    }

    if evaluation_summary.ground_truth is None:
        logger.info("No ground truth data available - calculating basic statistics only")
        _add_dynamic_metrics(evaluation_summary, metrics, EVALUATION_SUMMARY_EXCLUDED_ATTRS)
        metrics["confusion_matrix"] = None
    else:
        logger.info("Ground truth available - calculating full performance metrics")
        metrics.update(
            {
                "actual_true_positives": evaluation_summary.actual_true_positives,
                "actual_true_positives_count": len(evaluation_summary.actual_true_positives),
                "actual_false_positives": evaluation_summary.actual_false_positives,
                "actual_false_positives_count": len(evaluation_summary.actual_false_positives),
                "confusion_matrix": {
                    "true_positives": evaluation_summary.tp,
                    "true_negatives": evaluation_summary.tn,
                    "false_positives": evaluation_summary.fp,
                    "false_negatives": evaluation_summary.fn,
                },
            }
        )
        _add_dynamic_metrics(evaluation_summary, metrics, EVALUATION_SUMMARY_EXCLUDED_ATTRS)

    return metrics


def _add_dynamic_metrics(evaluation_summary: EvaluationSummary, metrics: dict, excluded_attrs: set):
    """Add dynamic metrics from evaluation_summary attributes to metrics dict."""
    for attr_name in dir(evaluation_summary):
        if not attr_name.startswith("_") and attr_name not in excluded_attrs:
            if hasattr(evaluation_summary, attr_name):
                attr_value = getattr(evaluation_summary, attr_name)
                if not callable(attr_value):
                    metrics[attr_name] = attr_value


def _calculate_confidence_scores(tracker: SASTWorkflowTracker) -> dict:
    """
    Calculate confidence scores for all issues in the tracker.

    Stores final_confidence_score directly in each PerIssueData object.

    Returns:
        Dictionary with only aggregate statistics
    """
    logger.info("Calculating confidence scores for all issues")

    confidence_breakdowns = {}

    for issue_id, per_issue_data in tracker.issues.items():
        try:
            # TEMPORARY: Inject mock data for missing components
            # TODO: Remove this once all nodes properly populate confidence data
            inject_mock_confidence_data(per_issue_data)

            # Calculate confidence score
            breakdown = calculate_final_confidence(per_issue_data)
            confidence_breakdowns[issue_id] = breakdown

            # Inject final score directly into PerIssueData
            per_issue_data.final_confidence_score = breakdown.final_confidence

            logger.debug(
                f"Confidence for {issue_id}: {breakdown.final_confidence:.3f} "
                f"(filter={breakdown.filter_confidence:.2f}, agent={breakdown.agent_confidence:.2f}, "
                f"evidence={breakdown.evidence_strength:.2f}, depth={breakdown.investigation_depth:.2f})"
            )

        except Exception as e:
            logger.error(f"Failed to calculate confidence for issue {issue_id}: {e}")
            # Leave final_confidence_score as None on error

    # Calculate aggregate statistics
    aggregate_stats = calculate_aggregate_confidence_metrics(confidence_breakdowns)

    logger.info(
        f"Confidence scoring complete: mean={aggregate_stats['mean_confidence']:.3f}, "
        f"high={aggregate_stats['high_confidence_count']}, "
        f"medium={aggregate_stats['medium_confidence_count']}, "
        f"low={aggregate_stats['low_confidence_count']}"
    )

    # Return only aggregate - per-issue scores are in tracker.issues[].final_confidence_score
    return aggregate_stats
