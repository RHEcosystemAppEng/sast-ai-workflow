from prettytable import PrettyTable
from Utils.metrics_utils import get_metrics

def cell_formatting(workbook, color):
    return workbook.add_format({
        "bold": 1,
        "border": 1,
        "align": "center",
        "valign": "vcenter",
        "fg_color": color,
    })

def print_conclusion(evaluation_summary):
    GREEN = "\033[92m"
    RED = "\033[91m"
    RESET = "\033[0m"

    # Table for confusion matrix data
    cm_table = PrettyTable()
    cm_table.field_names = ["Metric", "Value"]
    cm_table.add_row(["TP (Both human and AI labeled as not real issue)", f"{GREEN}{evaluation_summary.tp}{RESET}"])
    cm_table.add_row(["FP (AI falsely labeled as not real issue)", f"{RED}{evaluation_summary.fp}{RESET}"])
    cm_table.add_row(["TN (Both human and AI labeled as real issue)", f"{GREEN}{evaluation_summary.tn}{RESET}"])
    cm_table.add_row(["FN (AI falsely labeled as real issue)", f"{RED}{evaluation_summary.fn}{RESET}"])

    print("\n--- Confusion Matrix Data ---")
    print(cm_table)

    accuracy, recall, precision, f1_score = get_metrics(
        evaluation_summary.tp, 
        evaluation_summary.tn, 
        evaluation_summary.fp, 
        evaluation_summary.fn
        )

    # Table for model performance metrics
    perf_table = PrettyTable()
    perf_table.field_names = ["Performance Metric", "Value"]
    perf_table.add_row(["Accuracy", accuracy])
    perf_table.add_row(["Recall", recall])
    perf_table.add_row(["Precision", precision])
    perf_table.add_row(["F1 Score", f1_score])

    print("\n--- Model Performance Metrics ---")
    print(perf_table)