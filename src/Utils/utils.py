import os
import math
import yaml
from decimal import Decimal

from prettytable import PrettyTable


def print_config(config):
    print("".center(80, '-'))
    print("LLM_URL=", config["LLM_URL"])
    print("LLM_API_KEY= ********")
    print("LLM_MODEL_NAME=", config["LLM_MODEL_NAME"])
    print("OUTPUT_FILE_PATH=", config["OUTPUT_FILE_PATH"])
    print("GIT_REPO_PATH=", config["GIT_REPO_PATH"])
    print("EMBEDDINGS_LLM_MODEL_NAME=", config["EMBEDDINGS_LLM_MODEL_NAME"])
    print("REPORT_FILE_PATH=", config["REPORT_FILE_PATH"])
    print("KNOWN_FALSE_POSITIVE_FILE_PATH=", config["KNOWN_FALSE_POSITIVE_FILE_PATH"])
    print("HUMAN_VERIFIED_FILE_PATH=", config["HUMAN_VERIFIED_FILE_PATH"])
    print("CALCULATE_METRICS=", config["CALCULATE_METRICS"])
    print("DOWNLOAD_GIT_REPO=", config["DOWNLOAD_GIT_REPO"])
    print("".center(80, '-'))

def load_config():
    config_path = os.path.join(os.path.dirname(__file__), "../..", "config", "default_config.yaml")
    with open(config_path, "r") as f:
        config = yaml.safe_load(f)

    # Override default configuration with any environment variables if they exist.
    for key in config.keys():
        env_value = os.getenv(key)
        if env_value is not None:
            config[key] = env_value
    return config

def validate_configurations(config):
    # Check for required configuration variables
    required_cfg_vars = [
        "LLM_URL",
        "LLM_MODEL_NAME",
        "EMBEDDINGS_LLM_MODEL_NAME",
        "REPORT_FILE_PATH",
        "KNOWN_FALSE_POSITIVE_FILE_PATH",
        "OUTPUT_FILE_PATH",
        "HUMAN_VERIFIED_FILE_PATH"
    ]
    required_cfg_files = [
        "REPORT_FILE_PATH",
        "KNOWN_FALSE_POSITIVE_FILE_PATH",
        "HUMAN_VERIFIED_FILE_PATH",
        "OUTPUT_FILE_PATH"
    ]

    for var in required_cfg_vars:
        value = config[var]
        if not value:
            raise ValueError(f"Configuration variable '{var}' is not set or is empty.")

    # Validate that input files exist and are accessible
    for var in required_cfg_files:
        value = config[var]
        if not os.path.exists(value):
            raise FileNotFoundError(f"Configuration variable '{var}' not found.")

    # Validate that environment variable LLM API key exist
    llm_api_key = os.environ.get("LLM_API_KEY")
    if not llm_api_key:
        raise ValueError(f"Environment variable 'LLM_API_KEY' is not set or is empty.")
    
    print("All required environment variables and files are valid and accessible.\n")

def cell_formatting(workbook, color):
    return workbook.add_format({
        "bold": 1,
        "border": 1,
        "align": "center",
        "valign": "vcenter",
        "fg_color": color,
    })

def count_predicted_values(data):
    positives = set()
    negatives = set()
    for (issue_id, llm_text, metric_ar) in data:
        if "not a false positive" in str(llm_text).lower():
            positives.add(issue_id)
        else:
            negatives.add(issue_id)
    return positives, negatives

def count_actual_values(data, ground_truth):
    positives = set()
    negatives = set()
    
    for (issue_id, _, _) in data:
        if not issue_id in ground_truth:
            print(f"WARNING: Issue ID {issue_id} does not exist in the human verified excel sheet")
        elif ground_truth[issue_id] == 'y':
            negatives.add(issue_id)
        else:
            positives.add(issue_id)
    return positives, negatives

def calculate_confusion_matrix_metrics(actual_true_positives, actual_false_positives, predicted_true_positives, predicted_false_positives):
    tp = len(actual_true_positives & predicted_true_positives)      # Both human and AI labeled as real issue
    tn = len(actual_false_positives & predicted_false_positives)    # Both human and AI labeled as not real issue
    fp = len(predicted_true_positives - actual_true_positives)      # AI falsely labeled as real issue
    fn = len(actual_true_positives - predicted_true_positives)      # AI falsely labeled as not real issue
   
    return tp, tn, fp, fn

def print_conclusion(evaluation_summary):
    GREEN = "\033[92m"
    RED = "\033[91m"
    RESET = "\033[0m"

    # Table for confusion matrix data
    cm_table = PrettyTable()
    cm_table.field_names = ["Metric", "Value"]
    cm_table.add_row(["TP (Both human and AI labeled as real issue)", f"{GREEN}{evaluation_summary.tp}{RESET}"])
    cm_table.add_row(["FP (AI falsely labeled as real issue)", f"{RED}{evaluation_summary.fp}{RESET}"])
    cm_table.add_row(["TN (Both human and AI labeled as not real issue)", f"{GREEN}{evaluation_summary.tn}{RESET}"])
    cm_table.add_row(["FN (AI falsely labeled as not real issue)", f"{RED}{evaluation_summary.fn}{RESET}"])

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

def get_predicted_summary(data):
    summary = []
    config = load_config()

    for _, (issue, summary_info) in enumerate(data):
        if not config.get("CALCULATE_METRICS", True):
            break
        ar = get_percentage_value(summary_info.metrics['answer_relevancy'])
        summary.append((issue.id, summary_info.llm_response, ar))
    return summary



    
