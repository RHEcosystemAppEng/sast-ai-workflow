import sys
import os

from tornado.gen import sleep
from tqdm import tqdm

from ExcelWriter import write_to_excel_file
from LLMService import LLMService
from MetricHandler import metric_request_from_prompt, MetricHandler
from ReportReader import read_sast_report
from Utils.output_utils import print_conclusion
from Utils.html_utils import read_cve_html_file, format_cwe_context 
from Utils.file_utils import get_human_verified_results
from handlers.repo_handler_factory import repo_handler_factory
from dto.EvaluationSummary import EvaluationSummary
from dto.SummaryInfo import SummaryInfo
from stage.filter_known_issues import capture_known_issues
from common.config import Config
from common.constants import TOKENIZERS_PARALLELISM
from dto.ResponseStructures import FinalJudgeResponse


def main():
    config = Config()
    os.environ[TOKENIZERS_PARALLELISM] = "false" # Turn off parallel processing for tokenization to avoid warnings

    llm_service = LLMService(config)
    metric_handler = MetricHandler(llm_service.main_llm, llm_service.embedding_llm)
    issue_list = read_sast_report(config)
    repo_handler = repo_handler_factory(config)

    summary_data = []

    with tqdm(total=len(issue_list), file=sys.stdout, desc="Full report scanning progress: ") as pbar:
        print("\n")
        selected_issue_set = {  # WE SHOULD REMOVE THIS WHEN WE RUN ENTIRE REPORT!
            "def1",
            "def2",
            "def3",
            "def4",
            "def5",
            "def6",
            "def7", # FP - Very similar to known issue
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
        already_seen_issue_ids, similar_known_issues_dict = capture_known_issues(llm_service, 
                                                      set(e for e in issue_list if e.id in selected_issue_set),
                                                      config)

        for issue in issue_list:
            if issue.id not in selected_issue_set: # WE SHOULD REMOVE THIS WHEN WE RUN ENTIRE REPORT!
                continue
            
            # Set default values
            score, critique_response = {}, ""
            if issue.id in already_seen_issue_ids.keys():
                print(f"{issue.id} already marked as a false positive since it's a known issue")
                context = already_seen_issue_ids[issue.id].equal_error_trace
                response = FinalJudgeResponse(
                        investigation_result="FALSE POSITIVE",
                        recommendations=["No fix required."],
                        justifications=[f"The error is similar to one found in the provided context: {context}"]
                        )
            else:
                # get source code context by error trace
                issue_source_code = repo_handler.get_source_code_from_error_trace(issue.trace)
                source_code_context =  "".join([f'\ncode of {path} file:\n{code}' for path, code in issue_source_code.items()])

                # cwe_context = ""
                # if issue.issue_cve_link:
                    # cwe_texts = read_cve_html_file(issue.issue_cve_link, config)
                    # cwe_context = "".join(cwe_texts)

                context = (
                    f"*** Source Code Context ***\n{source_code_context}\n\n"
                    f"*** Examples ***\n{similar_known_issues_dict.get(issue.id, '')}"
                    
                )

                question = "Investigate if the following problem need to fix or can be considered false positive. " + issue.trace
                prompt, response, critique_response = llm_service.final_judge(question, context, issue)

                # let's calculate numbers for quality of the response we received here!
                score = {}
                if config.CALCULATE_METRICS:
                    metric_request = metric_request_from_prompt(prompt, response)
                    score = metric_handler.evaluate_datasets(metric_request)

            summary_data.append((issue, SummaryInfo(response, score, critique_response, context)))

            pbar.update(1)
            sleep(1)

    ground_truth = get_human_verified_results(config.HUMAN_VERIFIED_FILE_PATH)
    evaluation_summary = EvaluationSummary(summary_data, config, ground_truth)

    try:
        write_to_excel_file(summary_data, evaluation_summary, config)
    except Exception as e:
        print("Error occurred while generating excel file:", e)
    finally:
        print_conclusion(evaluation_summary)


if __name__ == '__main__':
    main()