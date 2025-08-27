import logging
import math
from collections import Counter
from decimal import Decimal

from dto.LLMResponse import FinalStatus
from dto.SASTWorkflowModels import PerIssueData
from sklearn.metrics import confusion_matrix, accuracy_score, precision_score, recall_score, f1_score

from common.config import Config
from common.constants import KNOWN_ISSUES_SHORT_JUSTIFICATION, NEEDS_SECOND_ANALYSIS, YES_OPTIONS, \
    TOTAL_ISSUES, NON_FINAL_ISSUES, FINAL_ISSUES, KNOWN_FALSE_POSITIVES, \
    TRUE_POSITIVES, FALSE_POSITIVES, NO_ANALYSIS_RESPONSE

logger = logging.getLogger(__name__)


def count_predicted_values(data):
    # Positives = real isse
    # Negatives = NOT real issue
    positives = set()
    negatives = set()
    for issue_id, llm_response, metric_ar in data:
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

    for issue_id, _, _ in data:
        if issue_id not in ground_truth:
            logger.warning(
                f"WARNING: Issue ID {issue_id} does not exist in the human verified excel sheet"
            )
        elif ground_truth[issue_id].lower() in YES_OPTIONS:
            negatives.add(issue_id)
        else:
            positives.add(issue_id)
    return positives, negatives


def calculate_confusion_matrix_metrics(actual_tp, actual_fp, predicted_tp, predicted_fp):
    all_ids = actual_tp | actual_fp | predicted_tp | predicted_fp
    y_true = [1 if id in actual_fp else 0 for id in all_ids]
    y_pred = [1 if id in predicted_fp else 0 for id in all_ids]
    
    cm = confusion_matrix(y_true, y_pred, labels=[0, 1])
    tn, fp, fn, tp = cm.ravel()
    return tp, tn, fp, fn


def get_metrics(tp, tn, fp, fn):
    y_true = [0] * tn + [0] * fp + [1] * fn + [1] * tp
    y_pred = [0] * tn + [1] * fp + [0] * fn + [1] * tp
    
    if len(y_true) == 0:
        return 0.0, 0.0, 0.0, 0.0
    
    return (accuracy_score(y_true, y_pred), 
            recall_score(y_true, y_pred, zero_division=0),
            precision_score(y_true, y_pred, zero_division=0), 
            f1_score(y_true, y_pred, zero_division=0))


def get_numeric_value(value):
    return 0 if math.isnan(value) or math.isinf(value) else value


def get_percentage_value(n):
    n = get_numeric_value(n)
    n = n if isinstance(n, Decimal) else Decimal(str(n))
    return round(n, 2) * 100


def get_predicted_summary(data, config: Config):
    summary = []

    for _, (issue, summary_info) in enumerate(data):
        ar = 0
        if summary_info and "answer_relevancy" in summary_info.metrics:
            ar = get_percentage_value(summary_info.metrics["answer_relevancy"])
        llm_response = (
            summary_info.critique_response
            if config.USE_CRITIQUE_AS_FINAL_RESULTS
            else summary_info.llm_response
        )
        summary.append((issue.id, llm_response, ar))
    return summary


def categorize_issues_by_status(issues: dict[str, PerIssueData]) -> Counter:
    """
    Categorize all issues by their various statuses using Counter for efficient batch processing.
    
    Args:
        issues: Dictionary of issue IDs to PerIssueData objects
        
    Returns:
        Counter with the following keys:
        - 'total_issues': Total number of issues
        - 'non_final_issues': Issues with is_final=FALSE
        - 'final_issues': Issues with is_final=TRUE  
        - 'known_false_positives': Issues containing known issue justification
        - 'true_positives': Issues marked as true positives
        - 'false_positives': Issues marked as false positives
        - 'needs_second_analysis': Issues that need second analysis
        - 'no_analysis_response': Issues without analysis response
    """
    counter = Counter()
    
    counter[TOTAL_ISSUES] = len(issues)
    
    if counter[TOTAL_ISSUES] == 0:
        counter[NO_ANALYSIS_RESPONSE], counter[NON_FINAL_ISSUES], counter[FINAL_ISSUES], \
        counter[KNOWN_FALSE_POSITIVES], counter[TRUE_POSITIVES], counter[FALSE_POSITIVES], \
        counter[NEEDS_SECOND_ANALYSIS] = 0, 0, 0, 0, 0, 0, 0
        return counter  
    
    for issue_data in issues.values():
        if not issue_data.analysis_response:
            counter[NO_ANALYSIS_RESPONSE] += 1
            continue
            
        # Count by final status
        if issue_data.analysis_response.is_final == FinalStatus.FALSE.value:
            counter[NON_FINAL_ISSUES] += 1
        else:
            counter[FINAL_ISSUES] += 1
            
        # Count known false positives
        if KNOWN_ISSUES_SHORT_JUSTIFICATION in issue_data.analysis_response.short_justifications:
            counter[KNOWN_FALSE_POSITIVES] += 1
            
        # Count true/false positives
        if issue_data.analysis_response.is_true_positive():
            counter[TRUE_POSITIVES] += 1
        else:
            counter[FALSE_POSITIVES] += 1
        
        # Count needs second analysis
        if issue_data.analysis_response.is_second_analysis_needed():
            counter[NEEDS_SECOND_ANALYSIS] += 1
            
    return counter

