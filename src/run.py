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
from Utils.utils import read_cve_html_file, create_embeddings_for_all_project_files, read_known_errors_file
from MetricHandler import metric_request_from_prompt, MetricHandler
from model.SummaryInfo import SummaryInfo

load_dotenv()  # take environment variables from .env.

LLM_URL = os.environ.get("LLM_URL")
LLM_API_KEY = os.environ.get("LLM_API_KEY")
LLM_MODEL_NAME = os.environ.get("LLM_MODEL_NAME")
EMBEDDINGS_LLM_MODEL_NAME = os.environ.get("EMBEDDINGS_LLM_MODEL_NAME")
REPORT_FILE_PATH = os.environ.get("REPORT_FILE_PATH")
KNOWN_FALSE_POSITIVE_FILE_PATH = os.environ.get("KNOWN_FALSE_POSITIVE_FILE_PATH")

git_repo_path = "https://github.com/redhat-plumbers/systemd-rhel9/tree/v252-46.2"

print(" Process started! ".center(80, '-'))
main_process = MainProcess(base_url=LLM_URL, llm_model_name=LLM_MODEL_NAME,
                           embedding_llm_model_name=EMBEDDINGS_LLM_MODEL_NAME, api_key=LLM_API_KEY)
metric_handler = MetricHandler(main_process.get_main_llm(), main_process.get_embedding_llm())
issue_list = read_sast_report_html(REPORT_FILE_PATH)
summary_data = []

os.environ["TOKENIZERS_PARALLELISM"] = "false"
# downloading git repository for given project
# download_repo(git_repo_path)

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
    # selected_issue_list = [
    #     "def2",  # 1
    #     "def133",  # 2
    #     "def3",  # 3
    #     "def134",  # 4
    #     "def135",  # 5
    #     "def136",  # 6
    #     "def5",  # 7
    #     "def137",  # 8
    #     "def138",  # 9
    #     "def10"  # 10
    # ]
    selected_issue_list = [
        "def2",  # 1
        "def133",  # 2
        "def5",  # 3
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

write_to_excel_file(summary_data)
