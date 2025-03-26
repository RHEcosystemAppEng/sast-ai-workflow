import json
import os
import glob
import pandas as pd

from langchain.text_splitter import RecursiveCharacterTextSplitter, Language


def read_source_code_file(path):
    with open(path, "r", encoding='utf-8') as f:
        text_splitter = RecursiveCharacterTextSplitter.from_language(language=Language.C,
                                                                     chunk_size=100, chunk_overlap=0)
        plain_text = f.read()
        doc_list = text_splitter.create_documents([plain_text])
        return doc_list

def read_known_errors_file(path):
    with open(path, "r", encoding='utf-8') as f:
        plain_text = f.read()
        doc_list = [item.strip() for item in plain_text.split("\n\n\n") if item.strip()!='']
        return doc_list
    
def get_human_verified_results(filename):
    try:
        df = pd.read_excel(filename, header=get_header_row(filename))
    except Exception as e:
        raise ValueError(f"Failed to read Excel file at {filename}: {e}")
    
    df.columns = df.columns.str.strip().str.lower()
    expected_issue_id = "issue id"
    expected_false_positive = "false positive?"
    
    if expected_issue_id not in df.columns:
        raise KeyError(
            f"Expected column '{expected_issue_id}' not found in the file. Found columns: {list(df.columns)}"
        )
    if expected_false_positive not in df.columns:
        raise KeyError(
            f"Expected column '{expected_false_positive}' not found in the file. Found columns: {list(df.columns)}"
        )
    
    ground_truth = dict(zip(df[expected_issue_id], df[expected_false_positive]))
    print(f"Successfully loaded ground truth from {filename}")
    return ground_truth

def read_all_source_code_files():
    res_list = []
    # reading project src folder
    src_dir_path = os.path.join(os.getcwd(), "systemd-rhel10/src/")
    count = 0
    for src_filename in glob.iglob(src_dir_path + '/**/**', recursive=True):

        if (src_filename.endswith(".c") or src_filename.endswith(".h")) and os.path.isfile(src_filename):
            count = count + 1
            for k in read_source_code_file(src_filename):
                res_list.append(k.page_content)  # adding source code file as text to embeddings
    print(f"Total files: {count}")
    return res_list

def read_answer_template_file(path):
    with open(path, "r", encoding='utf-8') as f:
        return f.read()
    
def get_header_row(filename):
    # Locate the header row containing 'Issue ID'
    preview = pd.read_excel(filename, header=None, nrows=5)
    header_row = next((i for i, row in preview.iterrows() if any(str(cell).strip().lower() == 'issue id' for cell in row.values)), None)
    return header_row

def load_json_with_placeholders(template_path, placeholders):
    """
    Args:
        template_path (str): Path to the JSON template file.
        placeholders (dict): Dictionary where keys are placeholder names (e.g., "{ID}")
                             and values are the values to replace them with.
                             NOTE: Assume values are str

    Returns:
        dict: JSON object with placeholders replaced
    """
    with open(template_path, "r", encoding="utf-8") as f:
        json_data = json.load(f)
    json_str = json.dumps(json_data)

    for placeholder, value in placeholders.items():
        json_str = json_str.replace(placeholder, value)

    return json_str
