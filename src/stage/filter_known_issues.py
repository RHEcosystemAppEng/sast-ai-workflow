from typing import List

from LLMService import LLMService
from Utils.file_utils import read_known_errors_file
from common.config import Config


def capture_known_issues(main_process: LLMService, issue_list: List, config: Config):
    """
    Identify and capture known false-positive issues.
    Returns:
        dict: A dictionary where keys are issue IDs and values are the FilterResponse objects
              for issues identified as known false positives.
        dict: A dictionary where keys are issue IDs and values are the contexts with the
              most N (N=SIMILARITY_ERROR_THRESHOLD) similar known issues from the same type.
    """
    # Reading known false-positives
    text_false_positives = read_known_errors_file(config.KNOWN_FALSE_POSITIVE_FILE_PATH)

    false_positive_db = main_process.create_vdb_for_known_issues(text_false_positives)

    already_seen_dict = {}
    context_dict = {}
    for issue in issue_list:
        filter_response, context = main_process.filter_known_error(false_positive_db, issue)
        context_dict[issue.id] = convert_similar_issues_to_context_string(context)
        print(f"Response of filter_known_error: {filter_response}")

        result_value = filter_response.result.strip().lower()
        print(f"{issue.id} Is known false positive? {result_value}")

        if "yes" in result_value:
            already_seen_dict[issue.id] = filter_response 
            print(f"LLM found {issue.id} error trace inside known false positives list")

    print(f"Known false positives: {len(already_seen_dict)} / {len(issue_list)} ")
    return already_seen_dict, context_dict

def convert_similar_issues_to_context_string(similar_known_issues_list: list) -> str:
    """Convert a list of known false positive CVE examples into a formatted string. """
    formatted_context = ""
    for i in range(len(similar_known_issues_list)):
        example_number = i + 1
        formatted_context += (
            f"\n** Example-{example_number} **\n" 
            f"(Example-{example_number}) Known False Positive:\n"
            f"{similar_known_issues_list[i]['false_positive_error_trace']}\n"
            f"(Example-{example_number}) Reason Marked as False Positive:\n"
            f"{similar_known_issues_list[i]['reason_marked_false_positive']}"
            )
                            
    return formatted_context