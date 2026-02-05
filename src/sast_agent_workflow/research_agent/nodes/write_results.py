"""
Write Results Node - Write final analysis results.

Extracts logic from tools/write_results.py without NAT dependency.
"""

import logging
import os

from common.config import Config
from dto.EvaluationSummary import EvaluationSummary
from dto.SASTWorkflowModels import SASTWorkflowTracker
from report_writers import write_to_excel_file
from Utils.file_utils import get_human_verified_results
from Utils.workflow_utils import convert_tracker_to_summary_data
from Utils.output_utils import print_conclusion

logger = logging.getLogger(__name__)


def create_write_results_node(config: Config):
    """
    Create write results node function.
    
    Args:
        config: Configuration instance
    
    Returns:
        Node function that writes results
    """
    
    def write_results(state: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Write final analysis results to output files.
        
        Core logic extracted from tools/write_results.py lines 57-110.
        
        Args:
            state: Tracker with completed investigation
        
        Returns:
            Tracker (unchanged - terminal node)
        """
        logger.info("Writing results...")
        
        try:
            # Convert tracker to summary data
            include_non_final = getattr(config, 'WRITE_RESULTS_INCLUDE_NON_FINAL', True)
            summary_data = convert_tracker_to_summary_data(state, include_non_final=include_non_final)
            
            logger.info(f"Writing results for {len(summary_data)} issues")
            
            # Create evaluation summary from metrics
            ground_truth = get_human_verified_results(config)
            
            # Reuse metrics if already calculated
            if state.metrics and "error" not in state.metrics:
                logger.info("Using pre-calculated metrics")
                evaluation_summary = EvaluationSummary(summary_data, config, ground_truth)
            else:
                logger.info("Calculating metrics for results")
                evaluation_summary = EvaluationSummary(summary_data, config, ground_truth)
            
            # Write to Excel
            write_to_excel_file(summary_data, evaluation_summary, config)
            logger.info(f"Results written to {config.OUTPUT_FILE_PATH}")
            
            # Print conclusion
            try:
                print_conclusion(evaluation_summary, [])  # Empty failed_item_ids
            except Exception as e:
                logger.warning(f"Failed to print conclusion: {e}")
            
            # Write metrics JSON if requested
            output_file = os.getenv('WORKFLOW_JSON_OUTPUT', None)
            if output_file and state.metrics:
                try:
                    import json
                    with open(output_file, 'w') as f:
                        json.dump(state.metrics, f, indent=2)
                    logger.info(f"Metrics written to {output_file}")
                except Exception as e:
                    logger.error(f"Failed to write metrics JSON: {e}")
            
        except Exception as e:
            logger.error(f"Failed to write results: {e}", exc_info=True)
            # Don't fail the workflow for output writing issues
        
        logger.info("Write results complete")
        return state
    
    return write_results

