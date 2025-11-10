import json
import logging
from datetime import datetime

from prettytable import PrettyTable

from common.constants import FALLBACK_JUSTIFICATION_MESSAGE
from Utils.metrics_utils import get_metrics

logger = logging.getLogger(__name__)


def cell_formatting(workbook, color):
    return workbook.add_format(
        {
            "bold": 1,
            "border": 1,
            "align": "center",
            "valign": "vcenter",
            "fg_color": color,
        }
    )


def print_conclusion(evaluation_summary, failed_item_ids=[]):
    if failed_item_ids:
        logger.error(
            f"\nNOTE: The following failed items \
                were excluded for accurate evaluation: {failed_item_ids}"
        )

    # Table for confusion matrix data
    cm_table = PrettyTable()
    cm_table.field_names = ["Metric", "Value"]
    cm_table.add_row(
        ["TP (Both human and AI labeled as not real issue)", f"{evaluation_summary.tp}"]
    )
    cm_table.add_row(["FP (AI falsely labeled as not real issue)", f"{evaluation_summary.fp}"])
    cm_table.add_row(["TN (Both human and AI labeled as real issue)", f"{evaluation_summary.tn}"])
    cm_table.add_row(["FN (AI falsely labeled as real issue)", f"{evaluation_summary.fn}"])

    logger.info(f"\n--- Confusion Matrix Data ---\n{cm_table}")

    accuracy, recall, precision, f1_score = get_metrics(
        evaluation_summary.tp, evaluation_summary.tn, evaluation_summary.fp, evaluation_summary.fn
    )

    # Table for model performance metrics
    perf_table = PrettyTable()
    perf_table.field_names = ["Performance Metric", "Value"]
    perf_table.add_row(["Accuracy", accuracy])
    perf_table.add_row(["Recall", recall])
    perf_table.add_row(["Precision", precision])
    perf_table.add_row(["F1 Score", f1_score])

    logger.info(f"\n--- Model Performance Metrics ---\n{perf_table}")


def filter_items_for_evaluation(summary_data):
    """
    This function iterates through `summary_data`, identifying items where the
    LLM response justification is a predefined fallback message. These are
    considered "failed" items. In practice, this filtering is mainly applied
    to self-hosted models, as they can have a higher incidence of such fallback
    responses (failures). Excluding these items from evaluation datasets helps
    ensure more accurate metrics.

    Args:
        summary_data: A list of tuples, where each tuple is (issue_objuct, summary_info).
              'issue' contains issue details (id, issue_type, trace).
              'summary_info' contains 'llm_response' attribute,
                which in turn has a 'justifications' attribute

    Returns:
        tuple: A tuple containing two lists:
            - items_for_evaluation (list): A list of entries from `summary_data`
              that did not use the fallback justification message.
            - failed_item_ids (list): A list of IDs from entries that used the
              fallback justification message.
    """
    items_for_evaluation = []
    failed_item_ids = []
    for issue_result in summary_data:
        if issue_result[1].llm_response.justifications != FALLBACK_JUSTIFICATION_MESSAGE:
            items_for_evaluation.append(issue_result)
        else:
            failed_item_ids.append(issue_result[0].id)
    return items_for_evaluation, failed_item_ids


def write_workflow_metrics_json(metrics, output_path, config):
    """Write workflow metrics to JSON file for orchestrator/database storage.

    Args:
        metrics: Dictionary containing calculated metrics from calculate_metrics node
        output_path: File path where JSON should be written
        config: Workflow configuration object with project details
    """
    confusion_matrix = metrics.get('confusion_matrix')

    result = {
        "aggregated_metrics": {
            "accuracy": metrics.get('accuracy', 0.0),
            "precision": metrics.get('precision', 0.0),
            "recall": metrics.get('recall', 0.0),
            "f1_score": metrics.get('f1_score', 0.0)
        },
        "cm": {
            "tp": int(confusion_matrix.get('true_positives', 0)) if confusion_matrix else 0,
            "fp": int(confusion_matrix.get('false_positives', 0)) if confusion_matrix else 0,
            "tn": int(confusion_matrix.get('true_negatives', 0)) if confusion_matrix else 0,
            "fn": int(confusion_matrix.get('false_negatives', 0)) if confusion_matrix else 0
        }
    }

    with open(output_path, 'w') as f:
        json.dump(result, f, separators=(',', ':'))

    logger.info(f"Workflow metrics written to: {output_path}")
