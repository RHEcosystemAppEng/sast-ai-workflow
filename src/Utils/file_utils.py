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

def read_known_errors_file(path):
    with open(path, "r", encoding='utf-8') as f:
        text_splitter = RecursiveCharacterTextSplitter(separators=["\n\n", "\n"],
                                                       chunk_size=500, chunk_overlap=0)
        plain_text = f.read()
        doc_list = text_splitter.create_documents([plain_text])
        return doc_list
    
def get_human_verified_results():
    filename = os.getenv("HUMAN_VERIFIED_FILE_PATH")  
    if not filename or not os.path.exists(filename):
        print(f"WARNING: Human verified results file not found at '{filename}'. Proceeding without human verified data.")
        return {}
    
    try:
        df = pd.read_excel(filename)
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