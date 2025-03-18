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
    validate_environment,
    print_config
)
from model.EvaluationSummary import EvaluationSummary
from model.SummaryInfo import SummaryInfo
from src.stage.filter_known_issues import capture_known_issues

load_dotenv()  # take environment variables from .env.

LLM_URL = os.environ.get("LLM_URL")
LLM_API_KEY = os.environ.get("LLM_API_KEY")
LLM_MODEL_NAME = os.environ.get("LLM_MODEL_NAME")
EMBEDDINGS_LLM_MODEL_NAME = os.environ.get("EMBEDDINGS_LLM_MODEL_NAME")
REPORT_FILE_PATH = os.environ.get("REPORT_FILE_PATH")
GIT_REPO_PATH = os.environ.get("GIT_REPO_PATH")
USE_FALSE_POSITIVE_DATA = os.environ.get("USE_FALSE_POSITIVE_DATA")

os.environ["TOKENIZERS_PARALLELISM"] = "false"

print(" Process started! ".center(80, '-'))
print_config()
validate_environment() # Check for required environment variables

llm_service = LLMService(base_url=LLM_URL, llm_model_name=LLM_MODEL_NAME,
                         embedding_llm_model_name=EMBEDDINGS_LLM_MODEL_NAME, api_key=LLM_API_KEY)
metric_handler = MetricHandler(llm_service.main_llm, llm_service.embedding_llm)
issue_list = read_sast_report_html(REPORT_FILE_PATH)
summary_data = []

# downloading git repository for given project
# download_repo(GIT_REPO_PATH)

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

        metric_request = metric_request_from_prompt(prompt, response)
        score = metric_handler.evaluate_datasets(metric_request)
        summary_data.append((issue, SummaryInfo(response, score)))

        pbar.update(1)
        sleep(1)

ground_truth = get_human_verified_results()
evaluation_summary = EvaluationSummary(summary_data, ground_truth)

try:
    write_to_excel_file(summary_data, evaluation_summary)
except Exception as e:
    print("Error occurred while generating excel file:", e)
finally:
    print_conclusion(evaluation_summary)
