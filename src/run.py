import os
import sys
import time

from dotenv import load_dotenv
from langchain_community.vectorstores import FAISS
from tornado.gen import sleep
from tqdm import tqdm

from ExcelWriter import write_to_excel_file
from MainProcess import MainProcess
from ReportReader import read_sast_report_html
from MetricHandler import metric_request_from_prompt, MetricHandler
from model.SummaryInfo import SummaryInfo
from model.EvaluationSummary import EvaluationSummary
from Utils.utils import (
    read_cve_html_file, 
    create_embeddings_for_all_project_files, 
    read_known_errors_file,
    print_conclusion,
)

load_dotenv()  # take environment variables from .env.

def print_config():
    print("".center(80, '-'))
    print("LLM_URL=",os.environ.get("LLM_URL"))
    print("LLM_API_KEY= ********")
    print("LLM_MODEL_NAME=",os.environ.get("LLM_MODEL_NAME"))
    print("GIT_REPO_PATH=",os.environ.get("GIT_REPO_PATH"))
    print("EMBEDDINGS_LLM_MODEL_NAME=",os.environ.get("EMBEDDINGS_LLM_MODEL_NAME"))
    print("REPORT_FILE_PATH=",os.environ.get("REPORT_FILE_PATH"))
    print("KNOWN_FALSE_POSITIVE_FILE_PATH=",os.environ.get("KNOWN_FALSE_POSITIVE_FILE_PATH"))
    print("".center(80, '-'))

LLM_URL = os.environ.get("LLM_URL")
LLM_API_KEY = os.environ.get("LLM_API_KEY")
LLM_MODEL_NAME = os.environ.get("LLM_MODEL_NAME")
EMBEDDINGS_LLM_MODEL_NAME = os.environ.get("EMBEDDINGS_LLM_MODEL_NAME")
REPORT_FILE_PATH = os.environ.get("REPORT_FILE_PATH")
KNOWN_FALSE_POSITIVE_FILE_PATH = os.environ.get("KNOWN_FALSE_POSITIVE_FILE_PATH")
GIT_REPO_PATH = os.environ.get("GIT_REPO_TAG_URL")

os.environ["TOKENIZERS_PARALLELISM"] = "false"

print(" Process started! ".center(80, '-'))
print_config()
main_process = MainProcess(base_url=LLM_URL, llm_model_name=LLM_MODEL_NAME,
                           embedding_llm_model_name=EMBEDDINGS_LLM_MODEL_NAME, api_key=LLM_API_KEY)
metric_handler = MetricHandler(main_process.get_main_llm(), main_process.get_embedding_llm())
issue_list = read_sast_report_html(REPORT_FILE_PATH)
summary_data = []

# downloading git repository for given project
# download_repo(GIT_REPO_PATH)

with tqdm(total=len(issue_list), file=sys.stdout, desc="Full report scanning progres: ") as pbar:
    print("\n")

    if os.path.exists("./../faiss_index/index.faiss"):
        print("Loading source code embeddings from file index")
        src_db = FAISS.load_local("./../faiss_index", main_process.get_embedding_llm(),
                                  allow_dangerous_deserialization=True)
    else:
        code_text = create_embeddings_for_all_project_files()
        src_embed_list = code_text if len(code_text) > 0 else []
        start = time.time()
        print("Creating embeddings for source code...")
        src_db = main_process.create_vdb(src_embed_list)
        src_db.save_local("./../faiss_index")

        end = time.time()
        print(f"Src project files have embedded completely. It took : {end - start} seconds")

    # Reading known false-positives
    text_false_positives = []
    for doc in read_known_errors_file(KNOWN_FALSE_POSITIVE_FILE_PATH):
        text_false_positives.append(doc.page_content)

    false_positive_db = main_process.create_vdb(text_false_positives)
    src_db.merge_from(false_positive_db)

    # Main loop
    # selected_issue_list = [
    #     "def2",
    #     "def133",
    #     "def3",
    #     "def134",
    #     "def135",
    #     "def136",
    #     "def5",
    #     "def137",
    #     "def138",
    #     "def10",
    #     "def11",
    #     "def12",
    #     "def15",
    #     "def16",
    #     "def17",
    #     "def18",
    #     "def19",
    #     "def24",
    #     "def25",
    #     "def26",
    #     "def27",
    #     "def33",
    #     "def139",
    #     "def37",
    #     "def35",
    #     "def36",
    #     "def45",
    #     "def44",
    #     "def46",
    #     "def140",
    #     "def56",
    #     "def59",
    #     "def60",
    #     "def61",
    #     "def63",
    #     "def65",
    #     "def141",
    #     "def67",
    #     "def66",
    #     "def69",
    #     "def71",
    #     "def72",
    #     "def73",
    #     "def74",
    #     "def86",
    #     "def90",
    #     "def91",
    #     "def89",
    #     "def131"
    # ]
    selected_issue_list = [
        "def11",  # 1
        # "def12",  # 2
        # "def13",  # 3
        # "def14",  # 4
        # "def15",  # 5
        # "def20",  # 6
        # "def23",  # 7
        # "def35",  # 8
        # "def50",  # 9
        # "def62"  # 10
    ]
    for issue in issue_list:
        if issue.id not in selected_issue_list:
            continue
        text_to_embed_list = []
        # reading the public cve info and add into `text_to_embed_list`
        for cve_text in read_cve_html_file(issue.issue_cve_link):
            text_to_embed_list.append(cve_text)  # adding CVE data as text to embeddings

        cve_db = main_process.create_vdb(text_to_embed_list)
        src_db.merge_from(cve_db)

        question = "Investigate if the following problem need to fix or can be considered false positive. " + issue.trace
        prompt, response = main_process.query(src_db, question)

        # let's calculate numbers for quality of the response we received here!
        metric_request = metric_request_from_prompt(prompt, response)
        score = metric_handler.evaluate_datasets(metric_request)
        print(f"METRIC RESULTS!!! -> {score}")
        summary_data.append((issue, SummaryInfo(response, score)))

        pbar.update(1)
        sleep(1)

evaluation_summary = EvaluationSummary(summary_data)

try: 
    write_to_excel_file(summary_data, evaluation_summary)
except Exception as e:
    print("Error occurred while generating excel file:", e)
finally:
    print_conclusion(evaluation_summary)
