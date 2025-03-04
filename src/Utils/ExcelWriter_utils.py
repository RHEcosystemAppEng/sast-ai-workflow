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
    # print("ground truth = ", ground_truth)
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

def print_confusion_matrix_and_model_performace(data):
    ground_truth = get_human_verified_results() 
    actual_positives, actual_negatives = count_actual_values(data, ground_truth)
    predicted_positives, predicted_negatives = count_predicted_values(data)
    tp, tn, fp, fn = calculate_confusion_matrix_metrics(actual_positives, actual_negatives, predicted_positives, predicted_negatives)

    GREEN = "\033[92m"
    RED = "\033[91m"
    RESET = "\033[0m"
    
    print("\n--- Confusion Matrix Data ---")
    print(f"TP (True Positives): {GREEN}{tp}{RESET}")
    print(f"FP (False Positives): {RED}{fp}{RESET}")
    print(f"TN (True Negatives): {GREEN}{tn}{RESET}")
    print(f"FN (False Negatives): {RED}{fn}{RESET}")

    accuracy, recall, precision, f1_score = get_metrics(tp, tn, fp, fn)
    print("\n--- Model Performance Metrics ---")
    print(f"Accuracy: {accuracy}")
    print(f"Recall: {recall}")
    print(f"Precision: {precision}")
    print(f"F1 Score: {f1_score}")


def get_metrics(tp, tn, fp, fn):
    EPSILON = 1e-11 
    accuracy = (tp + tn) / (tp + tn + fp + fn + EPSILON)
    recall = tp / (tp + fn + EPSILON)
    precision = tp / (tp + fp + EPSILON)
    f1_score = 2 * precision * recall / (precision + recall + EPSILON)
    return accuracy, recall, precision, f1_score
