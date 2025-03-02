from decimal import Decimal
import os
import math
import pandas as pd

def cell_formatting(workbook, color):
    return workbook.add_format({
        "bold": 1,
        "border": 1,
        "align": "center",
        "valign": "vcenter",
        "fg_color": color,
    })

def get_numeric_value(value):
    return 0 if math.isnan(value) or math.isinf(value) else value

def get_percentage_value(n):
    n = get_numeric_value(n)
    n = n if isinstance(n, Decimal) else Decimal(str(n))
    return round(n, 2) * 100

def count_predicted_values(data):
    positives = []
    negatives = []
    for (issue_id, llm_text, metric_ar) in data:
        if "not a false positive" in str(llm_text).lower():
            positives.append(issue_id)
        else:
            negatives.append(issue_id)
    return positives, negatives

def count_actual_values(data, ground_truth):
    positives = []
    negatives = []
    
    for (issue_id, _, _) in data:
        if not issue_id in ground_truth:
            print(f"WARNING: Issue ID {issue_id} does not exist in the human verified excel sheet")
        elif ground_truth[issue_id] == 'y':
            negatives.append(issue_id)
        else:
            positives.append(issue_id)
    return positives, negatives

def get_human_verified_results():
    filename = os.getenv("HUMAN_VERIFIED_FILE_PATH")
    print(f" Reading ground truth from {filename} ".center(80, '*'))
    df = pd.read_excel(filename)
    ground_truth = dict(zip(df['Issue ID'], df['False Positive?']))
    return ground_truth

def calculate_confusion_matrix_metrics(actual_positives, actual_negatives, predicted_positives, predicted_negatives):
    tp, tn, fp, fn = 0, 0, 0, 0

    for issue_id in actual_positives:
        if issue_id in predicted_positives:
            tp += 1
        else:
            fn += 1

    for issue_id in actual_negatives:
        if issue_id in predicted_negatives:
            tn += 1
        else:
            fp += 1
    
    return tp, tn, fp, fn




