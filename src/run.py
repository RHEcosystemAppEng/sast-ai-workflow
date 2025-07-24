import logging
import os
import sys

from tornado.gen import sleep
from tqdm import tqdm

from common.config import Config
from common.constants import FALLBACK_JUSTIFICATION_MESSAGE, TOKENIZERS_PARALLELISM
from dto.EvaluationSummary import EvaluationSummary
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from dto.SummaryInfo import SummaryInfo
from ExcelWriter import write_to_excel_file
from handlers.repo_handler_factory import repo_handler_factory
from LLMService import LLMService
from MetricHandler import MetricHandler, metric_request_from_prompt
from ReportReader import read_sast_report
from stage.filter_known_issues import capture_known_issues
from Utils.file_utils import get_human_verified_results
from Utils.log_utils import setup_logging
from Utils.output_utils import filter_items_for_evaluation, print_conclusion

# Setup logging
setup_logging()
logger = logging.getLogger(__name__)


def main():
    config = Config()
    os.environ[TOKENIZERS_PARALLELISM] = (
        "false"  # Turn off parallel processing for tokenization to avoid warnings
    )

    llm_service = LLMService(config)
    metric_handler = MetricHandler(llm_service.main_llm, llm_service.embedding_llm)
    repo_handler = repo_handler_factory(config)
    issue_list = read_sast_report(config)

    summary_data = []

    with tqdm(
        total=len(issue_list), file=sys.stdout, desc="Full report scanning progress: "
    ) as pbar:
        # selected_issue_list = {  # WE SHOULD REMOVE THIS WHEN WE RUN ENTIRE REPORT!
        # "def1",
        # "def2",
        # "def3",
        # "def4",
        # "def5",
        # "def6",
        # "def7", # FP - Very similar to known issue
        # "def8",
        # "def9",
        # "def10",
        # "def11",
        # "def12",
        # "def13",
        # "def14",
        # "def15",
        # "def16",
        # "def17",
        # "def18",
        # "def19",
        # "def20",
        # "def21",
        # "def22",
        # "def23",
        # "def24",
        # "def25",
        # "def26",
        # "def27",
        # "def28",
        # "def29",
        # "def30",
        # "def31",  # This one is known false positive
        # "def32",
        # "def33",
        # "def34",
        # "def35",
        # "def36",
        # "def37",
        # "def38",
        # "def39",
        # "def40",
        # "def41",
        # "def42",
        # "def43",
        # "def44",
        # "def45",
        # "def46",
        # "def47",
        # "def48",  # This one is known false positive
        # "def49",  # This one is known false positive
        # "def50",  # This one is known false positive
        # }
        already_seen_issues_dict, similar_known_issues_dict = {}, {}
        if config.USE_KNOWN_FALSE_POSITIVE_FILE:
            already_seen_issues_dict, similar_known_issues_dict = capture_known_issues(
                llm_service,
                # [e for e in issue_list if e.id in selected_issue_list],
                # # WE SHOULD DISABLE THIS WHEN WE RUN ENTIRE REPORT!
                issue_list,  # WE SHOULD ENABLE THIS WHEN WE RUN ENTIRE REPORT!
                config,
            )

        for issue in issue_list:
            # if issue.id not in selected_issue_list:
            # # WE SHOULD DISABLE THIS WHEN WE RUN ENTIRE REPORT!
            #     continue

            # Set default values
            score, critique_response, context = {}, "", ""
            try:
                if issue.id in already_seen_issues_dict.keys():
                    logger.info(
                        f"{issue.id} already marked as a false positive since it's a known issue"
                    )
                    context = already_seen_issues_dict[issue.id].equal_error_trace
                    llm_response = AnalysisResponse(
                        investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
                        is_final="TRUE",
                        recommendations=["No fix required."],
                        justifications=[
                            f"The error is similar to one found in the provided context: {context}"
                        ],
                        short_justifications="The error is similar to one found in the provided \
                            known issues (Details in the full Justification)",
                    )
                else:
                    # get source code context by error trace
                    issue_source_code = repo_handler.get_source_code_blocks_from_error_trace(
                        issue.trace
                    )
                    source_code_context = "".join(
                        [
                            f"\ncode of {path} file:\n{code}"
                            for path, code in issue_source_code.items()
                        ]
                    )

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
                    while llm_response.is_second_analysis_needed() and retries < config.MAX_ANALYSIS_ITERATIONS:
                        logger.info(
                            f"{llm_response.is_final=}\n{llm_response.recommendations=}\n\
                                {llm_response.instructions=}"
                        )
                        missing_source_code = repo_handler.extract_missing_functions_or_macros(
                            llm_response.instructions
                        )
                        source_code_context += f"\n{missing_source_code}"
                        context = (
                            f"*** Source Code Context ***\n{source_code_context}\n\n"
                            f"*** Examples ***\n{similar_known_issues_dict.get(issue.id, '')}"
                        )
                        llm_response, critique_response = llm_service.investigate_issue(
                            context, issue
                        )

                        retries += 1
                    repo_handler.reset_found_symbols()
                    # let's calculate numbers for quality of the response we received here!
                    if config.CALCULATE_METRICS:
                        metric_request = metric_request_from_prompt(llm_response)
                        score = metric_handler.evaluate_datasets(metric_request)

            except Exception as e:
                logger.error(
                    f"An error occurred while processing issue ID {issue.id}.\nError is: {e}"
                )
                if not llm_response:
                    # This issue will be excluded from evaluation.
                    llm_response = AnalysisResponse(
                        investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                        is_final="TRUE",
                        justifications=FALLBACK_JUSTIFICATION_MESSAGE,
                        evaluation=[],
                        recommendations=[],
                        instructions=[],
                        prompt="",
                        short_justifications=FALLBACK_JUSTIFICATION_MESSAGE[0],
                    )
                    logger.info("Default values have been set")
                else:
                    # This issue will be included in the evaluation.
                    incomplete_message = "Note: Analysis incomplete due to a processing error."
                    llm_response.justifications.append(incomplete_message)
                    llm_response.short_justifications += " " + incomplete_message

            summary_data.append(
                (issue, SummaryInfo(llm_response, score, critique_response, context))
            )

            pbar.update(1)
            sleep(1)

    # Applies mainly to self-hosted models, where failed items are excluded for accurate evaluation
    items_for_evaluation, failed_item_ids = filter_items_for_evaluation(summary_data)
    ground_truth = get_human_verified_results(config)
    evaluation_summary = EvaluationSummary(items_for_evaluation, config, ground_truth)

    try:
        write_to_excel_file(summary_data, evaluation_summary, config)
    except Exception as e:
        logger.error("Error occurred while generating excel file:", e)
    finally:
        print_conclusion(evaluation_summary, failed_item_ids)


if __name__ == "__main__":
    main()
