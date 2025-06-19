import math
import logging

from decimal import Decimal

from common.config import Config
from common.constants import YES_OPTIONS

logger = logging.getLogger()

def count_predicted_values(data):
    # Positives = real isse
    # Negatives = NOT real issue
    positives = set()
    negatives = set()
    for (issue_id, llm_response, metric_ar) in data:
        if llm_response.is_true_positive():
            positives.add(issue_id)
        else:
            negatives.add(issue_id)
    return positives, negatives

def count_actual_values(data, ground_truth):
    # Positives = real isse
    # Negatives = NOT real issue
    positives = set()
    negatives = set()
    
    for (issue_id, _, _) in data:
        if not issue_id in ground_truth:
            logger.warning(f"WARNING: Issue ID {issue_id} does not exist in the human verified excel sheet")
        elif ground_truth[issue_id].lower() in YES_OPTIONS:
            negatives.add(issue_id)
        else:
            positives.add(issue_id)
    return positives, negatives

def calculate_confusion_matrix_metrics(actual_true_positives, actual_false_positives, predicted_true_positives, predicted_false_positives):
    """
    Note: Since our goal is to detect false alarms, positives refer to cases where issues are identified as NOT real issues.

    Definitions:
    - Positives: Issues that are NOT real issues (e.g., false alarms).
    - Negatives: Issues that are real issues.
    """
    tp = len(actual_false_positives & predicted_false_positives)    # Both human and AI labeled as not real issue
    tn = len(actual_true_positives & predicted_true_positives)      # Both human and AI labeled as real issue
    fp = len(actual_true_positives - predicted_true_positives)      # AI falsely labeled as not real issue
    fn = len(predicted_true_positives - actual_true_positives)      # AI falsely labeled as real issue

    return tp, tn, fp, fn

def get_metrics(tp, tn, fp, fn):
    EPSILON = 1e-11 
    accuracy = (tp + tn) / (tp + tn + fp + fn + EPSILON)
    recall = tp / (tp + fn + EPSILON)
    precision = tp / (tp + fp + EPSILON)
    f1_score = 2 * precision * recall / (precision + recall + EPSILON)
    return accuracy, recall, precision, f1_score

def get_numeric_value(value):
    return 0 if math.isnan(value) or math.isinf(value) else value

def get_percentage_value(n):
    n = get_numeric_value(n)
    n = n if isinstance(n, Decimal) else Decimal(str(n))
    return round(n, 2) * 100

def get_predicted_summary(data, config:Config):
    summary = []

    for _, (issue, summary_info) in enumerate(data):
        ar = 0
        if summary_info and 'answer_relevancy' in summary_info.metrics:
            ar = get_percentage_value(summary_info.metrics['answer_relevancy'])
        llm_response = summary_info.critique_response if config.USE_CRITIQUE_AS_FINAL_RESULTS else summary_info.llm_response
        summary.append((issue.id, llm_response, ar))
    return summary
