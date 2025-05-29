import os
import sys

from tornado.gen import sleep
from tqdm import tqdm

from ExcelWriter import write_to_excel_file
from LLMService import LLMService
from MetricHandler import metric_request_from_prompt, MetricHandler
from ReportReader import read_sast_report
from Utils.file_utils import get_human_verified_results
from Utils.output_utils import filter_items_for_evaluation, print_conclusion
from common.config import Config
from common.constants import TOKENIZERS_PARALLELISM
from dto.EvaluationSummary import EvaluationSummary
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from dto.SummaryInfo import SummaryInfo
from handlers.repo_handler_factory import repo_handler_factory
from stage.filter_known_issues import capture_known_issues


def main():
    config = Config()
    os.environ[TOKENIZERS_PARALLELISM] = "false" # Turn off parallel processing for tokenization to avoid warnings

    llm_service = LLMService(config)
    metric_handler = MetricHandler(llm_service.main_llm, llm_service.embedding_llm)
    repo_handler = repo_handler_factory(config)
    issue_set = read_sast_report(config)

    summary_data = []

    with tqdm(total=len(issue_set), file=sys.stdout, desc="Full report scanning progress: ") as pbar:
        print("\n")
        already_seen_issues_dict, similar_known_issues_dict = capture_known_issues(llm_service, issue_set, config)

        for issue in issue_set:

            score, critique_response = {}, ""


            if issue.id in already_seen_issues_dict.keys():
                print(f"{issue.id} already marked as a false positive since it's a known issue")
                context = already_seen_issues_dict[issue.id].equal_error_trace
                llm_response = AnalysisResponse(
                        investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
                        is_final='TRUE',
                        recommendations=["No fix required."],
                        justifications=[f"The error is similar to one found in the provided context: {context}"],
                        short_justifications="The error is similar to one found in the provided known issues (Details in the full Justification)"
                        )
            else:
                # build source code context by error trace
                issue_source_code = repo_handler.get_source_code_blocks_from_error_trace(issue.trace)
                source_code_context =  "".join([f'\ncode of {path} file:\n{code}' for path, code in issue_source_code.items()])

                # cwe_context = ""
                # if issue.issue_cve_link:
                    # cwe_texts = read_cve_html_file(issue.issue_cve_link, config)
                    # cwe_context = "".join(cwe_texts)

                context = (
                    f"*** Source Code Context ***\n{source_code_context}\n\n"
                    f"*** Examples ***\n{similar_known_issues_dict.get(issue.id, '')}"
                )

                llm_response, critique_response = llm_service.investigate_issue(context, issue)

                retries = 0
                while llm_response.is_second_analysis_needed() and retries < 2:
                    missing_source_code = repo_handler.extract_missing_functions_or_macros(llm_response.instructions)
                    source_code_context += f'\n{missing_source_code}'
                    context = (
                        f"*** Source Code Context ***\n{source_code_context}\n\n" 
                        f"*** Examples ***\n{similar_known_issues_dict.get(issue.id, '')}"

                    )
                    llm_response, critique_response = llm_service.investigate_issue(context, issue)

                    retries += 1

                # let's calculate numbers for quality of the response we received here!
                score = {}
                if config.CALCULATE_METRICS:
                    metric_request = metric_request_from_prompt(llm_response)
                    score = metric_handler.evaluate_datasets(metric_request)

            summary_data.append((issue, SummaryInfo(llm_response, score, critique_response, context)))

            pbar.update(1)
            sleep(1)

    # Applies mainly to self-hosted models, where failed items are excluded for accurate evaluation
    items_for_evaluation, failed_item_ids = filter_items_for_evaluation(summary_data)
    ground_truth = get_human_verified_results(config)
    evaluation_summary = EvaluationSummary(items_for_evaluation, config, ground_truth)

    try:
        write_to_excel_file(summary_data, evaluation_summary, config)
    except Exception as e:
        print("Error occurred while generating excel file:", e)
    finally:
        print_conclusion(evaluation_summary, failed_item_ids)


if __name__ == '__main__':
    main()