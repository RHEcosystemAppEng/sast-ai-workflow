import logging

from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.EvaluationSummary import EvaluationSummary
from Utils.file_utils import get_human_verified_results
from Utils.workflow_utils import convert_tracker_to_summary_data
from ExcelWriter import write_to_excel_file

logger = logging.getLogger(__name__)


class WriteResultsConfig(FunctionBaseConfig, name="write_results"):
    """
    Write results function for SAST workflow.
    """
    description: str = Field(
        default="Write results function that writes the final SAST analysis results",
        description="Function description"
    )


@register_function(config_type=WriteResultsConfig)
async def write_results(
    config: WriteResultsConfig, builder: Builder
):
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
            logger.warning("No config found in tracker - skipping results writing")
            return tracker
            
        if not getattr(tracker.config, 'WRITE_RESULTS', True):
            logger.info("WRITE_RESULTS is disabled in config - skipping results writing")
            return tracker
        
        try:
            # Convert tracker to summary_data format expected by ExcelWriter
            include_non_final = getattr(tracker.config, 'WRITE_RESULTS_INCLUDE_NON_FINAL', False)
            summary_data = convert_tracker_to_summary_data(tracker, include_non_final=include_non_final)
            
            logger.info(f"Converted {len(summary_data)} issues for results writing")
            
            # Create EvaluationSummary from already calculated metrics to avoid duplication
            evaluation_summary = _create_evaluation_summary_from_metrics(summary_data, tracker.config, tracker.metrics)
            
            # Write results to configured destinations (Google Sheets, CSV, etc.)
            write_to_excel_file(summary_data, evaluation_summary, tracker.config)
            
            logger.info("Successfully wrote results to configured destinations")
            
        except Exception as e:
            logger.error(f"Failed to write results: {e}")
            # Continue execution - don't fail the workflow for output writing issues
        
        logger.info("Write_Results node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_write_results_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker
        )
    except GeneratorExit:
        logger.info("Write_Results function exited early!")
    finally:
        logger.info("Cleaning up Write_Results function.")


def _create_evaluation_summary_from_metrics(summary_data, config, metrics):
    """
    Create EvaluationSummary from already calculated metrics to avoid duplication.
    
    This reuses the metrics calculated by Calculate_Metrics node instead of
    recreating the EvaluationSummary from scratch.
    """
    if not metrics or isinstance(metrics, dict) and "error" in metrics:
        logger.warning("No valid metrics available - creating EvaluationSummary from scratch")
        try:
            ground_truth = get_human_verified_results(config)
            return EvaluationSummary(summary_data, config, ground_truth)
        except Exception as e:
            logger.error(f"Failed to create EvaluationSummary: {e}")
            return None
    
    try:
        # Create a mock EvaluationSummary object with the pre-calculated metrics
        # This avoids recalculating the same data that was already computed
        evaluation_summary = _create_mock_evaluation_summary(summary_data, config, metrics)
        logger.info("Successfully created EvaluationSummary from existing metrics")
        return evaluation_summary
        
    except Exception as e:
        logger.warning(f"Failed to create EvaluationSummary from metrics: {e}. Falling back to fresh calculation.")
        try:
            ground_truth = get_human_verified_results(config)
            return EvaluationSummary(summary_data, config, ground_truth)
        except Exception as fallback_error:
            logger.error(f"Fallback EvaluationSummary creation also failed: {fallback_error}")
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
    confusion_matrix = metrics.get("confusion_matrix", {})
    if confusion_matrix:
        mock_summary.tp = confusion_matrix.get("true_positives", 0)
        mock_summary.tn = confusion_matrix.get("true_negatives", 0) 
        mock_summary.fp = confusion_matrix.get("false_positives", 0)
        mock_summary.fn = confusion_matrix.get("false_negatives", 0)
    else:
        mock_summary.tp = mock_summary.tn = mock_summary.fp = mock_summary.fn = 0
    
    # Add other metrics that ExcelWriter might need
    for key, value in metrics.items():
        if key != "confusion_matrix" and not key.startswith("_"):
            setattr(mock_summary, key, value)
    
    return mock_summary
