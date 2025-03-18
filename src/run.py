import os
import sys

from dotenv import load_dotenv
from tornado.gen import sleep
from tqdm import tqdm

from ExcelWriter import write_to_excel_file
from LLMService import LLMService
from MetricHandler import metric_request_from_prompt, MetricHandler
from ReportReader import read_sast_report_html
from Utils.embedding_utils import generate_code_embeddings
from Utils.utils import (
    read_cve_html_file,
    print_conclusion,
    get_human_verified_results,
    validate_configurations,
    load_config,
    download_repo
)
from model.EvaluationSummary import EvaluationSummary
from model.SummaryInfo import SummaryInfo
from src.stage.filter_known_issues import capture_known_issues

load_dotenv()           # Take environment variables from .env
config = load_config()  # Take configuration variables from default_config.yaml

LLM_URL = config["LLM_URL"]
LLM_MODEL_NAME = config["LLM_MODEL_NAME"]
GIT_REPO_PATH = config["GIT_REPO_PATH"]
EMBEDDINGS_LLM_MODEL_NAME = config["EMBEDDINGS_LLM_MODEL_NAME"]
OUTPUT_FILE_PATH = config["OUTPUT_FILE_PATH"]
REPORT_FILE_PATH = config["REPORT_FILE_PATH"]
KNOWN_FALSE_POSITIVE_FILE_PATH = config["KNOWN_FALSE_POSITIVE_FILE_PATH"]
HUMAN_VERIFIED_FILE_PATH = config["HUMAN_VERIFIED_FILE_PATH"]
USE_KNOWN_FALSE_POSITIVE_FILE = config["USE_KNOWN_FALSE_POSITIVE_FILE"]
CALCULATE_METRICS = config["CALCULATE_METRICS"]
DOWNLOAD_GIT_REPO = config["DOWNLOAD_GIT_REPO"]

LLM_API_KEY = os.environ.get("LLM_API_KEY")

def print_config():
    print("".center(80, '-'))
    print("LLM_URL=", LLM_URL)
    print("LLM_API_KEY= ********")
    print("LLM_MODEL_NAME=", LLM_MODEL_NAME)
    print("OUTPUT_FILE_PATH=", OUTPUT_FILE_PATH)
    print("GIT_REPO_PATH=", GIT_REPO_PATH)
    print("EMBEDDINGS_LLM_MODEL_NAME=", EMBEDDINGS_LLM_MODEL_NAME)
    print("REPORT_FILE_PATH=", REPORT_FILE_PATH)
    print("KNOWN_FALSE_POSITIVE_FILE_PATH=", KNOWN_FALSE_POSITIVE_FILE_PATH)
    print("HUMAN_VERIFIED_FILE_PATH=", HUMAN_VERIFIED_FILE_PATH)
    print("CALCULATE_METRICS=", CALCULATE_METRICS)
    print("DOWNLOAD_GIT_REPO=", DOWNLOAD_GIT_REPO)
    print("".center(80, '-'))

os.environ["TOKENIZERS_PARALLELISM"] = "false"

print(" Process started! ".center(80, '-'))
print_config()
validate_configurations(config) # Check for required environment variables

llm_service = LLMService(base_url=LLM_URL, llm_model_name=LLM_MODEL_NAME,
                         embedding_llm_model_name=EMBEDDINGS_LLM_MODEL_NAME, api_key=LLM_API_KEY)
metric_handler = MetricHandler(llm_service.main_llm, llm_service.embedding_llm)
issue_list = read_sast_report_html(REPORT_FILE_PATH)
summary_data = []

if DOWNLOAD_GIT_REPO:
    # downloading git repository for given project
    download_repo(GIT_REPO_PATH)
else:
    print("Skipping github repo download as per configuration.")

with tqdm(total=len(issue_list), file=sys.stdout, desc="Full report scanning progres: ") as pbar:
    print("\n")
    vector_db = generate_code_embeddings(llm_service)
    selected_issue_set = set([f"def{i}" for i in range(1, 3)]) # WE SHOULD REMOVE THIS WHEN WE RUN ENTIRE REPORT!
    already_seen_issue_ids = capture_known_issues(llm_service, set(e for e in issue_list if e.id in selected_issue_set))

    for issue in issue_list:
        if issue.id not in selected_issue_set: # WE SHOULD REMOVE THIS WHEN WE RUN ENTIRE REPORT!
            continue
        text_to_embed_list = [cve_text for cve_text in read_cve_html_file(issue.issue_cve_link)]
        cve_db = llm_service.create_vdb(text_to_embed_list)
        vector_db.merge_from(cve_db)

        question = "Investigate if the following problem need to fix or can be considered false positive. " + issue.trace
        prompt, response = llm_service.final_judge(vector_db, question)

        # let's calculate numbers for quality of the response we received here!
        if CALCULATE_METRICS:
            metric_request = metric_request_from_prompt(prompt, response)
            score = metric_handler.evaluate_datasets(metric_request)
            print(f"METRIC RESULTS!!! -> {score}")
        else:
            print("Skipping metrics calculation as per configuration.")
            score = None

        summary_data.append((issue, SummaryInfo(response, score)))

        pbar.update(1)
        sleep(1)

ground_truth = get_human_verified_results(HUMAN_VERIFIED_FILE_PATH)
evaluation_summary = EvaluationSummary(summary_data, ground_truth)

try:
    write_to_excel_file(summary_data, evaluation_summary, OUTPUT_FILE_PATH)
except Exception as e:
    print("Error occurred while generating excel file:", e)
finally:
    print_conclusion(evaluation_summary)
