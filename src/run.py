import os
import sys

from dotenv import load_dotenv
from tornado.gen import sleep
from tqdm import tqdm

from ExcelWriter import write_to_excel_file
from LLMService import LLMService
from MetricHandler import (
    metric_request_from_prompt,
    MetricHandler,
    parse_context_from_prompt
)
from ReportReader import read_sast_report_html
from Utils.embedding_utils import generate_code_embeddings
from Utils.repo_utils import download_repo
from Utils.output_utils import print_conclusion
from Utils.html_utils import read_cve_html_file 
from Utils.file_utils import get_human_verified_results
from Utils.config_utils import (
    load_config, 
    validate_configurations, 
    print_config
)


from model.EvaluationSummary import EvaluationSummary
from model.SummaryInfo import SummaryInfo
from stage.filter_known_issues import capture_known_issues

load_dotenv()           # Take environment variables from .env
config = load_config()  # Take configuration variables from default_config.yaml

LLM_URL = config["LLM_URL"]
LLM_MODEL_NAME = config["LLM_MODEL_NAME"]
CRITIQUE_LLM_URL = config["CRITIQUE_LLM_URL"]
CRITIQUE_LLM_MODEL_NAME = config["CRITIQUE_LLM_MODEL_NAME"]
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
os.environ["TOKENIZERS_PARALLELISM"] = "false"

print(" Process started! ".center(80, '-'))
print_config(config)
validate_configurations(config) # Check for required environment variables

llm_service = LLMService(base_url=LLM_URL, llm_model_name=LLM_MODEL_NAME,
                         embedding_llm_model_name=EMBEDDINGS_LLM_MODEL_NAME, api_key=LLM_API_KEY,
                         critique_llm_model_name=CRITIQUE_LLM_MODEL_NAME, critique_base_url=CRITIQUE_LLM_URL)
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
    selected_issue_set = {  # WE SHOULD REMOVE THIS WHEN WE RUN ENTIRE REPORT!
            "def1",
            "def2",
            "def3",
            "def4",
            "def5",
            "def6",
            "def7",
            "def8",
            "def9",
            "def10",
            "def11",
            "def12",
            "def13",
            "def14",
            "def15",
            "def16",
            "def17",
            "def18",
            "def19",
            "def20",
            "def21",
            "def22",
            "def23",
            "def24",
            "def25",
            "def26",
            "def27",
            "def28",
            "def29",
            "def30",
            "def31",  # This one is known false positive
            "def32",
            "def33",
            "def34",
            "def35",
            "def36",
            "def37",
            "def38",
            "def39",
            "def40",
            "def41",
            "def42",
            "def43",
            "def44",
            "def45",
            "def46",
            "def47",
            "def48",  # This one is known false positive
            "def49",  # This one is known false positive
            "def50",  # This one is known false positive
    }
    already_seen_issue_ids = capture_known_issues(llm_service, set(e for e in issue_list if e.id in selected_issue_set), KNOWN_FALSE_POSITIVE_FILE_PATH)

    for issue in issue_list:
        if issue.id not in selected_issue_set: # WE SHOULD REMOVE THIS WHEN WE RUN ENTIRE REPORT!
            continue
        text_to_embed_list = [cve_text for cve_text in read_cve_html_file(issue.issue_cve_link)]
        cve_db = llm_service.create_vdb(text_to_embed_list)
        vector_db.merge_from(cve_db)

        question = "Investigate if the following problem need to fix or can be considered false positive. " + issue.trace
        prompt, response, critique_response = llm_service.final_judge(vector_db, question)

        # let's calculate numbers for quality of the response we received here!
        score = {}
        if CALCULATE_METRICS:
            metric_request = metric_request_from_prompt(prompt, response)
            score = metric_handler.evaluate_datasets(metric_request)
        else:
            print("Skipping metrics calculation as per configuration.")

        summary_data.append((issue, SummaryInfo(response, score, critique_response, parse_context_from_prompt(prompt))))

        pbar.update(1)
        sleep(1)

ground_truth = get_human_verified_results(HUMAN_VERIFIED_FILE_PATH)
evaluation_summary = EvaluationSummary(summary_data, ground_truth)

try:
    write_to_excel_file(summary_data, evaluation_summary)
except Exception as e:
    print("Error occurred while generating excel file:", e)
finally:
    print_conclusion(evaluation_summary)
