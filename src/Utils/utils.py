import glob
import os
import re
import sys
import math
from decimal import Decimal

import git
import requests
import torch
import pandas as pd
from bs4 import BeautifulSoup
from bs4.element import Comment
from langchain.text_splitter import RecursiveCharacterTextSplitter, Language


def cell_formatting(workbook, color):
    return workbook.add_format({
        "bold": 1,
        "border": 1,
        "align": "center",
        "valign": "vcenter",
        "fg_color": color,
    })

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

def calculate_confusion_matrix_metrics(actual_true_positives, actual_false_positives, predicted_true_positives, predicted_false_positives):
    tp, tn, fp, fn = 0, 0, 0, 0

    for issue_id in actual_true_positives:
        if issue_id in predicted_true_positives:
            tp += 1
        else:
            fn += 1

    for issue_id in actual_false_positives:
        if issue_id in predicted_false_positives:
            tn += 1
        else:
            fp += 1
    
    return tp, tn, fp, fn

def print_conclusion(data):
    ground_truth = get_human_verified_results() 
    actual_true_positives, actual_false_positives = count_actual_values(data, ground_truth)
    predicted_true_positives, predicted_false_positives = count_predicted_values(data)
    tp, tn, fp, fn = calculate_confusion_matrix_metrics(actual_true_positives, actual_false_positives, predicted_true_positives, predicted_false_positives)

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

def get_metrics(params):
    tp = params["tp"]
    tn = params["tn"]
    fp = params["fp"]
    fn = params["fn"]
    EPSILON = 1e-11 
    accuracy = (tp + tn) / (tp + tn + fp + fn + EPSILON)
    recall = tp / (tp + fn + EPSILON)
    precision = tp / (tp + fp + EPSILON)
    f1_score = 2 * precision * recall / (precision + recall + EPSILON)
    return accuracy, recall, precision, f1_score

def print_conclusion(params):
    GREEN = "\033[92m"
    RED = "\033[91m"
    RESET = "\033[0m"
    
    print("\n--- Confusion Matrix Data ---")
    print(f"TP (Both human and AI labeled as real issue): {GREEN}{params["tp"]}{RESET}")
    print(f"FP (AI falsely labeled as real issue): {RED}{params["fp"]}{RESET}")
    print(f"TN (Both human and AI labeled as not real issue): {GREEN}{params["tn"]}{RESET}")
    print(f"FN (AI falsely labeled as not real issue): {RED}{params["fn"]}{RESET}")

    accuracy, recall, precision, f1_score = get_metrics(params)
    print("\n--- Model Performance Metrics ---")
    print(f"Accuracy: {accuracy}")
    print(f"Recall: {recall}")
    print(f"Precision: {precision}")
    print(f"F1 Score: {f1_score}")

def get_numeric_value(value):
    return 0 if math.isnan(value) or math.isinf(value) else value

def get_percentage_value(n):
    n = get_numeric_value(n)
    n = n if isinstance(n, Decimal) else Decimal(str(n))
    return round(n, 2) * 100

def get_predicted_summary(data):
    summary = []

    for _, (issue, summary_info) in enumerate(data):
        ar = get_percentage_value(summary_info.metrics['answer_relevancy'])
        summary.append((issue.id, summary_info.llm_response, ar))
    return summary

def get_device():
    if sys.platform == "darwin":
        return "mps" if torch.backends.mps.is_available() else "cpu"

    return "cuda" if torch.cuda.is_available() else "cpu"

def download_repo(repo_url):
    try:
        # Identify if the URL has a branch or tag with "/tree/"
        if "/tree/" in repo_url:
            # Split URL to separate repository URL and branch/tag
            repo_url, branch_or_tag = re.split(r'/tree/', repo_url, maxsplit=1)
        else:
            branch_or_tag = None

        # Extract the project name (the last part before "/tree/")
        repo_name = repo_url.rstrip('/').split('/')[-1]
        if repo_name.endswith('.git'):
            repo_name = repo_name[:-4]

        # Set the destination path to the current directory
        destination_path = os.path.join(os.getcwd(), repo_name)

        # Clone the repo
        print(f"Cloning {repo_url} into {destination_path}...")
        repo = git.Repo.clone_from(repo_url, destination_path)

        # Checkout the specified branch or tag if provided
        if branch_or_tag:
            print(f"Checking out {branch_or_tag}...")
            repo.git.checkout(branch_or_tag)

        print("Repository cloned successfully!")

    except Exception as e:
        print(f"An error occurred: {e}")

def create_embeddings_for_all_project_files():
    res_list = []
    # reading project src folder
    src_dir_path = os.path.join(os.getcwd(), "systemd-rhel9/src/")
    count = 0
    for src_filename in glob.iglob(src_dir_path + '/**/**', recursive=True):

        if (src_filename.endswith(".c") or src_filename.endswith(".h")) and os.path.isfile(src_filename):
            count = count + 1
            for k in read_source_code_file(src_filename):
                res_list.append(k.page_content)  # adding source code file as text to embeddings
    print(f"Total files: {count}")
    return res_list

def read_html_file(path):
    text_splitter = RecursiveCharacterTextSplitter(separators=["\n\n", "\n", ".", ";", ",", " ", ""],
                                                   chunk_size=500, chunk_overlap=0)
    if path.strip().startswith("https://"):
        res = requests.get(path)
        doc_text = text_splitter.split_text(text_from_html(res.content))
        return doc_text
    else:
        with open(path, "r", encoding='utf-8') as f:
            doc_text = text_splitter.split_text(text_from_html(f.read()))
            return doc_text

def read_cve_html_file(path):
    text_splitter = RecursiveCharacterTextSplitter(separators=["\n\n", "\n", ".", ";", ",", " ", ""],
                                                   chunk_size=500, chunk_overlap=0)
    res = requests.get(path)
    soup = BeautifulSoup(res.content, 'html.parser')
    tags_to_collect = ["Description", "Alternate_Terms", "Common_Consequences", "Potential_Mitigations",
                       "Modes_Of_Introduction", "Likelihood_Of_Exploit", "Demonstrative_Examples",
                       "Observed_Examples", "Weakness_Ordinalities", "Detection_Methods", "Affected_Resources",
                       "Memberships", "Vulnerability_Mapping_Notes", "Taxonomy_Mappings"]
    visible_text_list = []
    for t in tags_to_collect:
        texts = soup.find("div", {"id": t})
        if texts is None:
            continue
        texts = texts.findAll(string=True)
        visible_text = list(filter(remove_html_tags, texts))
        for v in visible_text:
            visible_text_list.append(str(v.strip()))

    doc_text = text_splitter.split_text(" ".join(visible_text_list))
    return doc_text

def read_source_code_file(path):
    with open(path, "r", encoding='utf-8') as f:
        text_splitter = RecursiveCharacterTextSplitter.from_language(language=Language.C,
                                                                     chunk_size=100, chunk_overlap=0)
        plain_text = f.read()
        doc_list = text_splitter.create_documents([plain_text])
        return doc_list

def remove_html_tags(element):
    if element.parent.name in ['style', 'script', 'head', 'title', 'meta', '[document]']:
        return False
    if isinstance(element, Comment):
        return False
    return True


def text_from_html(body):
    soup = BeautifulSoup(body, 'html.parser')
    texts = soup.findAll(string=True)
    visible_texts = filter(remove_html_tags, texts)
    return u" ".join(t.strip() for t in visible_texts)


def extract_file_path(input_str):
    # Using regex to extract only the file path
    match = re.search(r"(src/[^:]+):(\d+):(\d+):", input_str)

    if match:
        file_path = match.group(1)
        return file_path
    else:
        print("No match found.")

def read_known_errors_file(path):
    with open(path, "r", encoding='utf-8') as f:
        text_splitter = RecursiveCharacterTextSplitter(separators=["\n\n", "\n"],
                                                       chunk_size=500, chunk_overlap=0)
        plain_text = f.read()
        doc_list = text_splitter.create_documents([plain_text])
        return doc_list
    
