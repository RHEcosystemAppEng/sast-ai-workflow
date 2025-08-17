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

logger = logging.getLogger(__name__)


class CalculateMetricsConfig(FunctionBaseConfig, name="calculate_metrics"):
    description: str = Field(
        default="Calculate metrics function that calculates performance metrics for SAST analysis",
        description="Function description"
    )


@register_function(config_type=CalculateMetricsConfig)
async def calculate_metrics(
    config: CalculateMetricsConfig, builder: Builder
):
    logger.info("Initializing Calculate_Metrics function...")
    
    async def _calculate_metrics_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        logger.info("Running Calculate_Metrics node - calculating metrics")
        logger.info(f"Calculate_Metrics node processing tracker with {len(tracker.issues)} issues")
        
        if not tracker.config:
            logger.warning("No config found in tracker - skipping metrics calculation")
            return tracker
            
        if not tracker.config.CALCULATE_METRICS:
            logger.info("CALCULATE_METRICS is disabled in config - skipping metrics calculation")
            return tracker
        
        try:
            summary_data = convert_tracker_to_summary_data(tracker, include_non_final=False)
            
            if not summary_data:
                logger.warning("No completed issues found for metrics calculation")
                tracker.metrics = {"error": "No completed issues found"}
                return tracker
            
            ground_truth = get_human_verified_results(tracker.config)
            
            evaluation_summary = EvaluationSummary(summary_data, tracker.config, ground_truth)
            
            tracker.metrics = _extract_metrics_from_evaluation_summary(evaluation_summary)
            
            logger.info(f"Successfully calculated metrics for {len(summary_data)} issues")
            
        except Exception as e:
            logger.error(f"Failed to calculate metrics: {e}")
            tracker.metrics = {"error": f"Metrics calculation failed: {str(e)}"}
        
        logger.info("Calculate_Metrics node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_calculate_metrics_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker
        )
    except GeneratorExit:
        logger.info("Calculate_Metrics function exited early!")
    finally:
        logger.info("Cleaning up Calculate_Metrics function.")


def _extract_metrics_from_evaluation_summary(evaluation_summary: EvaluationSummary) -> dict:
    excluded_attrs = {
        'summary_data', 'config', 'ground_truth', 'predicted_summary', 
        'predicted_true_positives', 'predicted_false_positives', 
        'actual_true_positives', 'actual_false_positives', 
        'tp', 'tn', 'fp', 'fn'
    }
    
    metrics = {
        "total_issues": len(evaluation_summary.predicted_summary),
        "predicted_true_positives_count": len(evaluation_summary.predicted_true_positives),
        "predicted_false_positives_count": len(evaluation_summary.predicted_false_positives),
        "has_ground_truth": evaluation_summary.ground_truth is not None
    }
    
    if evaluation_summary.ground_truth is None:
        logger.info("No ground truth data available - calculating basic statistics only")
        _add_dynamic_metrics(evaluation_summary, metrics, excluded_attrs, None)
        metrics["confusion_matrix"] = None
    else:
        logger.info("Ground truth available - calculating full performance metrics")
        metrics.update({
            "actual_true_positives_count": len(evaluation_summary.actual_true_positives),
            "actual_false_positives_count": len(evaluation_summary.actual_false_positives),
            "confusion_matrix": {
                "true_positives": evaluation_summary.tp,
                "true_negatives": evaluation_summary.tn,
                "false_positives": evaluation_summary.fp,
                "false_negatives": evaluation_summary.fn
            }
        })
        _add_dynamic_metrics(evaluation_summary, metrics, excluded_attrs, None)
    
    return metrics


def _add_dynamic_metrics(evaluation_summary: EvaluationSummary, metrics: dict, excluded_attrs: set, default_value):
    for attr_name in dir(evaluation_summary):
        if not attr_name.startswith('_') and attr_name not in excluded_attrs:
            if hasattr(evaluation_summary, attr_name):
                attr_value = getattr(evaluation_summary, attr_name)
                if not callable(attr_value):
                    metrics[attr_name] = default_value if default_value is not None else attr_value
