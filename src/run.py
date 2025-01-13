import os
import sys
import time
from dotenv import load_dotenv

from langchain_community.embeddings import HuggingFaceEmbeddings
from langchain_community.vectorstores import FAISS
from tornado.gen import sleep
from tqdm import tqdm

from ExcelWriter import write_to_excel_file
from MainProcess import MainProcess
from ReportReader import read_sast_report_html
from Utils import read_cve_html_file, create_embeddings_for_all_project_files, extract_file_path, download_repo, \
    read_known_errors_file
load_dotenv()  # take environment variables from .env.

NVIDIA_URL = os.environ.get("NVIDIA_URL")
NVIDIA_API_KEY = os.environ.get("NVIDIA_API_KEY")
NVIDIA_LLM_MODEL_NAME = os.environ.get("NVIDIA_LLM_MODEL_NAME")
NVIDIA_EMBEDDINGS_LLM_MODEL_NAME = os.environ.get("NVIDIA_EMBEDDINGS_LLM_MODEL_NAME")

git_repo_path = "https://github.com/redhat-plumbers/systemd-rhel9/tree/v252-46.2"
report_file_path = "/Users/jnirosha/Projects/morpheus/sast/systemd-252-46.el9_5.2.html"
html_file_path = "/Users/jnirosha/Projects/morpheus/sast/Confluence.html"
known_false_positive_file_path = "/Users/jnirosha/Projects/morpheus/sast/ignore.err"

print(" Process started! ".center(80, '-'))
main_process = MainProcess(base_url=NVIDIA_URL, llm_model_name=NVIDIA_LLM_MODEL_NAME,
                           embedding_llm_model_name=NVIDIA_EMBEDDINGS_LLM_MODEL_NAME, api_key=NVIDIA_API_KEY)
issue_list = read_sast_report_html(report_file_path)
issue_count = len(issue_list)
summary_data = []

# downloading git repository for given project
# download_repo(git_repo_path)

with tqdm(total=issue_count, file=sys.stdout, desc="Full report scanning progres: ") as pbar:
    print("\n")

    if os.path.exists("./../faiss_index/index.faiss"):
        embeddings = HuggingFaceEmbeddings(
            model_name="/Users/jnirosha/Projects/morpheus/all-mpnet-base-v2",
            model_kwargs={'device': 'mps'},
            encode_kwargs={'normalize_embeddings': False}
        )
        src_db = FAISS.load_local(
            "./../faiss_index", embeddings, allow_dangerous_deserialization=True
        )
        print("Load from file index")
    else:
        code_text = create_embeddings_for_all_project_files()
        src_embed_list = code_text if len(code_text) > 0 else []
        start = time.time()
        print("Creating embeddings...")
        src_db = main_process.populate_db(src_embed_list)
        src_db.save_local("./../faiss_index")

        end = time.time()
        print(f"Src project files have embedded completely. It took : {end - start} seconds")

    # Reading known false-positives
    text_false_positives = []
    for doc in read_known_errors_file(known_false_positive_file_path):
        text_false_positives.append(doc.page_content)
    false_positive_db = main_process.populate_db(text_false_positives)
    src_db.merge_from(false_positive_db)

    # Main loop
    selected_issue_list = [
        "def11",#1
        "def12",#2
        "def13",#3
        "def14",#4
        "def15",#5
        "def20",#6
        "def23",#7
        "def35",#8
        "def50",#9
        "def62"#10
    ]
    for issue in issue_list:
        if issue.id not in selected_issue_list:
            continue
        text_to_embed_list = []
        # reading the public cve info and add into `text_to_embed_list`
        for cve_text in read_cve_html_file(issue.issue_cve_link):
            text_to_embed_list.append(cve_text)  # adding CVE data as text to embeddings

        cve_db = main_process.populate_db(text_to_embed_list)
        src_db.merge_from(cve_db)


        question = "Investigate if the following problem need to fix or can be considered false positive. " + issue.trace
        response = main_process.query(src_db, question)
        summary_data.append((issue, response))


        pbar.update(1)
        sleep(1)



write_to_excel_file(summary_data)