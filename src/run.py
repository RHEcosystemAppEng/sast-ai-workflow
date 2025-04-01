import sys
import os

from tornado.gen import sleep
from tqdm import tqdm

from ExcelWriter import write_to_excel_file
from LLMService import LLMService
from MetricHandler import metric_request_from_prompt, MetricHandler
from ReportReader import read_sast_report_html, get_report_project_info
from MetricHandler import (
    metric_request_from_prompt,
    MetricHandler,
    parse_context_from_prompt
)
from ReportReader import read_sast_report_html
from Utils.embedding_utils import generate_code_embeddings
from Utils.repo_utils import download_repo
from Utils.output_utils import print_conclusion
from Utils.html_utils import read_cve_html_file, format_cwe_context 
from Utils.file_utils import get_human_verified_results
from handlers.repo_handler_factory import repo_handler_factory
from model.EvaluationSummary import EvaluationSummary
from model.SummaryInfo import SummaryInfo
from stage.filter_known_issues import capture_known_issues
from common.config import Config
from common.constants import TOKENIZERS_PARALLELISM


def main():
    config = Config()
    os.environ[TOKENIZERS_PARALLELISM] = "false" # Turn off parallel processing for tokenization to avoid warnings

    llm_service = LLMService(base_url=config.LLM_URL, 
                            llm_model_name=config.LLM_MODEL_NAME,
                            embedding_llm_model_name=config.EMBEDDINGS_LLM_MODEL_NAME, 
                            api_key=config.LLM_API_KEY,
                            critique_llm_model_name=config.CRITIQUE_LLM_MODEL_NAME, 
                            critique_base_url=config.CRITIQUE_LLM_URL)
    metric_handler = MetricHandler(llm_service.main_llm, llm_service.embedding_llm)
    project_name, project_version = get_report_project_info(config.INPUT_REPORT_FILE_PATH)
    issue_list = read_sast_report_html(config.INPUT_REPORT_FILE_PATH)
    repo_handler = repo_handler_factory(project_name, project_version, config)

    summary_data = []

    with tqdm(total=len(issue_list), file=sys.stdout, desc="Full report scanning progress: ") as pbar:
        print("\n")
        selected_issue_set = set([f"def{i}" for i in range(1, 2)]) # WE SHOULD REMOVE THIS WHEN WE RUN ENTIRE REPORT!
        already_seen_issue_ids = capture_known_issues(llm_service, 
                                                      set(e for e in issue_list if e.id in selected_issue_set),
                                                      config)

        for issue in issue_list:
            if issue.id not in selected_issue_set: # WE SHOULD REMOVE THIS WHEN WE RUN ENTIRE REPORT!
                continue

            # get source code context by error trace
            issue_source_code = repo_handler.get_source_code_from_error_trace(issue.trace)
            source_code_context =  "".join([f'\ncode of {path} file:\n{code}' for path, code in issue_source_code.items()])

            cwe_context = ""
            if issue.issue_cve_link:
                cwe_texts = read_cve_html_file(issue.issue_cve_link, config)
                cwe_context = "".join(cwe_texts)

            combined_context = (
                f"=== Source Code Context ===\n{source_code_context}\n\n"
                f"=== CWE Context ===\n{cwe_context}"
            )

            question = "Investigate if the following problem need to fix or can be considered false positive. " + issue.trace
            prompt, response, critique_response = llm_service.final_judge(question, combined_context)

            # let's calculate numbers for quality of the response we received here!
            score = {}
            if config.CALCULATE_METRICS:
                metric_request = metric_request_from_prompt(prompt, response)
                score = metric_handler.evaluate_datasets(metric_request)
            else:
                print("Skipping metrics calculation as per configuration.")

            summary_data.append((issue, SummaryInfo(response, score, critique_response, parse_context_from_prompt(prompt))))

            pbar.update(1)
            sleep(1)

    ground_truth = get_human_verified_results(config.HUMAN_VERIFIED_FILE_PATH)
    evaluation_summary = EvaluationSummary(summary_data, ground_truth)

    try:
        write_to_excel_file(summary_data, evaluation_summary)
    except Exception as e:
        print("Error occurred while generating excel file:", e)
    finally:
        print_conclusion(evaluation_summary)


if __name__ == '__main__':
    main()