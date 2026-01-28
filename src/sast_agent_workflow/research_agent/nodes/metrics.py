"""
Metrics Node - Calculate performance metrics.

Extracts logic from tools/calculate_metrics.py without NAT dependency.
"""

import logging

from common.config import Config
from dto.EvaluationSummary import EvaluationSummary
from dto.SASTWorkflowModels import SASTWorkflowTracker
from Utils.file_utils import get_human_verified_results
from Utils.workflow_utils import convert_tracker_to_summary_data

logger = logging.getLogger(__name__)


def create_metrics_node(config: Config):
    """
    Create metrics calculation node function.
    
    Args:
        config: Configuration instance
    
    Returns:
        Node function that calculates metrics
    """
    
    def calculate_metrics(state: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Calculate performance metrics for investigated issues.
        
        Core logic extracted from tools/calculate_metrics.py lines 46-81.
        
        Args:
            state: Tracker with investigated issues
        
        Returns:
            Tracker with calculated metrics
        """
        logger.info("Calculating metrics...")
        
        try:
            # Convert tracker to summary data
            include_non_final = getattr(config, 'WRITE_RESULTS_INCLUDE_NON_FINAL', True)
            summary_data = convert_tracker_to_summary_data(state, include_non_final=include_non_final)
            
            if not summary_data:
                logger.warning("No completed issues for metrics calculation")
                state.metrics = {"error": "No completed issues"}
                return state
            
            # Load ground truth
            ground_truth = get_human_verified_results(config)
            
            # Calculate evaluation metrics
            evaluation_summary = EvaluationSummary(summary_data, config, ground_truth)
            
            # Extract metrics
            state.metrics = {
                "total_issues": len(evaluation_summary.predicted_summary),
                "predicted_true_positives_count": len(evaluation_summary.predicted_true_positives),
                "predicted_false_positives_count": len(evaluation_summary.predicted_false_positives),
                "has_ground_truth": evaluation_summary.ground_truth is not None,
            }
            
            # Add confusion matrix if ground truth available
            if evaluation_summary.ground_truth:
                state.metrics["accuracy"] = getattr(evaluation_summary, 'accuracy', None)
                state.metrics["precision"] = getattr(evaluation_summary, 'precision', None)
                state.metrics["recall"] = getattr(evaluation_summary, 'recall', None)
                state.metrics["f1_score"] = getattr(evaluation_summary, 'f1_score', None)
            
            logger.info(f"Calculated metrics for {len(summary_data)} issues")
            
        except Exception as e:
            logger.error(f"Metrics calculation failed: {e}", exc_info=True)
            state.metrics = {"error": f"Calculation failed: {str(e)}"}
        
        return state
    
    return calculate_metrics

