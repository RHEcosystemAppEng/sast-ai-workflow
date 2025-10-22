import logging

from nat.builder.builder import Builder
from nat.builder.function_info import FunctionInfo
from nat.cli.register_workflow import register_function
from nat.data_models.function import FunctionBaseConfig
from pydantic import Field

from common.constants import (
    METRICS_FIELD_ACTUAL_FALSE_POSITIVES,
    METRICS_FIELD_ACTUAL_TRUE_POSITIVES,
    METRICS_FIELD_CONFUSION_MATRIX,
    METRICS_FIELD_COUNT_SUFFIX,
    METRICS_FIELD_ERROR,
    METRICS_FIELD_FALSE_NEGATIVES,
    METRICS_FIELD_FALSE_POSITIVES,
    METRICS_FIELD_PREDICTED_FALSE_POSITIVES,
    METRICS_FIELD_PREDICTED_TRUE_POSITIVES,
    METRICS_FIELD_TRUE_NEGATIVES,
    METRICS_FIELD_TRUE_POSITIVES,
    WRITE_RESULTS_CONFIG_MISSING,
    WRITE_RESULTS_FAILURE,
    WRITE_RESULTS_NO_VALID_METRICS,
    WRITE_RESULTS_SUCCESS,
    WRITE_RESULTS_SUMMARY_FALLBACK,
    WRITE_RESULTS_SUMMARY_FALLBACK_FAILED,
    WRITE_RESULTS_SUMMARY_SUCCESS,
)
from dto.EvaluationSummary import EvaluationSummary
from dto.SASTWorkflowModels import SASTWorkflowTracker
from report_writers import write_analysis_results
from Utils.file_utils import get_human_verified_results
from Utils.output_utils import print_conclusion
from Utils.workflow_utils import convert_tracker_to_summary_data

logger = logging.getLogger(__name__)


class WriteResultsConfig(FunctionBaseConfig, name="write_results"):
    """
    Write results function for SAST workflow.
    """

    description: str = Field(
        default="Write results function that writes the final SAST analysis results",
        description="Function description",
    )


@register_function(config_type=WriteResultsConfig)
async def write_results(config: WriteResultsConfig, builder: Builder):
    """Register the Write_Results function."""

    logger.info("Initializing Write_Results function...")

    async def _write_results_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Write results function for SAST workflow.

        This is a terminal node that writes final analysis results to destinations
        specified in config (Google Sheets, CSV files, etc.) without modifying
        the tracker state.
        """
        logger.info("Running Write_Results node - writing results")
        logger.info(f"Write_Results node processing tracker with {len(tracker.issues)} issues")

        if not tracker.config:
            logger.warning(WRITE_RESULTS_CONFIG_MISSING)
            return tracker

        try:
            # Convert tracker to summary_data format expected by ExcelWriter
            include_non_final = getattr(tracker.config, "WRITE_RESULTS_INCLUDE_NON_FINAL", True)
            summary_data = convert_tracker_to_summary_data(
                tracker, include_non_final=include_non_final
            )

            logger.info(f"Converted {len(summary_data)} issues for results writing")

            # Create EvaluationSummary from already calculated metrics to avoid duplication
            evaluation_summary = _create_evaluation_summary_from_metrics(
                summary_data, tracker.config, tracker.metrics
            )

            # Write results with automatic dual output for SARIF inputs
            write_analysis_results(summary_data, evaluation_summary, tracker.config)

            logger.info(WRITE_RESULTS_SUCCESS)

        except Exception as e:
            logger.error(WRITE_RESULTS_FAILURE.format(e))
            # Continue execution - don't fail the workflow for output writing issues

        try:
            # Write conclusion to logger
            print_conclusion(evaluation_summary)
        except Exception as e:
            logger.error(f"Failed to print conclusion: {e}")

        logger.info("Write_Results node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_write_results_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker,
        )
    except GeneratorExit:
        logger.info("Write_Results function exited early!")
    finally:
        logger.info("Cleaning up Write_Results function.")


def _create_evaluation_summary_from_metrics(summary_data, config, metrics):
    """
    Create EvaluationSummary for ExcelWriter using pre-calculated metrics.

    Uses existing metrics from Calculate_Metrics node when available,
    otherwise creates fresh EvaluationSummary from raw data.
    """
    if not metrics or isinstance(metrics, dict) and METRICS_FIELD_ERROR in metrics:
        logger.warning(WRITE_RESULTS_NO_VALID_METRICS)
        try:
            ground_truth = get_human_verified_results(config)
            return EvaluationSummary(summary_data, config, ground_truth)
        except Exception as e:
            logger.error(f"Failed to create EvaluationSummary: {e}")
            return None

    try:
        # Build EvaluationSummary-like object from existing metrics
        evaluation_summary = _create_mock_evaluation_summary(summary_data, config, metrics)
        logger.info(WRITE_RESULTS_SUMMARY_SUCCESS)
        return evaluation_summary

    except Exception as e:
        logger.warning(WRITE_RESULTS_SUMMARY_FALLBACK.format(e))
        try:
            ground_truth = get_human_verified_results(config)
            return EvaluationSummary(summary_data, config, ground_truth)
        except Exception as fallback_error:
            logger.error(WRITE_RESULTS_SUMMARY_FALLBACK_FAILED.format(fallback_error))
            return None


def _create_mock_evaluation_summary(summary_data, config, metrics):
    """
    Create a mock EvaluationSummary object from pre-calculated metrics.

    This maps the metrics field names back to EvaluationSummary attribute names
    to avoid duplicate calculations.
    """
    from types import SimpleNamespace

    # Create a mock object that behaves like EvaluationSummary for ExcelWriter
    mock_summary = SimpleNamespace()

    # Map metrics back to EvaluationSummary attributes
    confusion_matrix = metrics.get(METRICS_FIELD_CONFUSION_MATRIX, {})
    if confusion_matrix:
        mock_summary.tp = confusion_matrix.get(METRICS_FIELD_TRUE_POSITIVES, 0)
        mock_summary.tn = confusion_matrix.get(METRICS_FIELD_TRUE_NEGATIVES, 0)
        mock_summary.fp = confusion_matrix.get(METRICS_FIELD_FALSE_POSITIVES, 0)
        mock_summary.fn = confusion_matrix.get(METRICS_FIELD_FALSE_NEGATIVES, 0)
    else:
        mock_summary.tp = mock_summary.tn = mock_summary.fp = mock_summary.fn = 0

    # ExcelWriter expects these to be collections that support len()
    # Map from our stored sets/lists back to the expected attributes
    if METRICS_FIELD_ACTUAL_TRUE_POSITIVES in metrics:
        mock_summary.actual_true_positives = metrics[METRICS_FIELD_ACTUAL_TRUE_POSITIVES]
    if METRICS_FIELD_ACTUAL_FALSE_POSITIVES in metrics:
        mock_summary.actual_false_positives = metrics[METRICS_FIELD_ACTUAL_FALSE_POSITIVES]
    if METRICS_FIELD_PREDICTED_TRUE_POSITIVES in metrics:
        mock_summary.predicted_true_positives = metrics[METRICS_FIELD_PREDICTED_TRUE_POSITIVES]
    if METRICS_FIELD_PREDICTED_FALSE_POSITIVES in metrics:
        mock_summary.predicted_false_positives = metrics[METRICS_FIELD_PREDICTED_FALSE_POSITIVES]

    # Add other metrics that ExcelWriter might need (excluding the collections and counts)
    for key, value in metrics.items():
        if (
            key
            not in [
                METRICS_FIELD_CONFUSION_MATRIX,
                METRICS_FIELD_ACTUAL_TRUE_POSITIVES,
                METRICS_FIELD_ACTUAL_FALSE_POSITIVES,
                METRICS_FIELD_PREDICTED_TRUE_POSITIVES,
                METRICS_FIELD_PREDICTED_FALSE_POSITIVES,
            ]
            and not key.startswith("_")
            and not key.endswith(METRICS_FIELD_COUNT_SUFFIX)
        ):
            setattr(mock_summary, key, value)

    return mock_summary
