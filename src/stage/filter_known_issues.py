import json
import re

from LLMService import LLMService
from Utils.file_utils import read_known_errors_file
from LLMService import LLMService
from common.config import Config


def capture_known_issues(main_process: LLMService, issue_list: list, config: Config):
    """
    Identify and capture known false-positive issues.
    Returns:
        dict: A dictionary where keys are issue IDs and values are the filter responses
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

        question = "Do you see this exact error trace? " + issue.trace
        response, context = main_process.filter_known_error(false_positive_db, question, issue)
        context_dict[issue.id] = context
        print(f"Response of filter_known_error: {response}")

        is_valid_json = True
        try:
            filter_response = json.loads(response)
            result_value = filter_response['result'].strip().lower()
            print(f"{issue.id} Is known false positive? {result_value}")
            is_known_false_positive = "yes" in result_value
        except (json.JSONDecodeError, KeyError):
            # WA: Fallback to regex check for "Result": "YES" in case the response is not valid JSON
            # This is a temporary change to handle inconsistent response formats.
            print(f"Response is not valid JSON. Checking for fallback condition.")
            is_valid_json = False
            is_known_false_positive = re.search(r'"?result"?\s*:\s*"?yes"?', response, re.IGNORECASE) is not None

        if is_known_false_positive:
            already_seen_dict[issue.id] = filter_response if is_valid_json else {"result": "YES", 
                                                                                 "equal_error_trace": response}
            print(f"LLM found {issue.id} error trace inside known false positives list")

    print(f"Known false positives: {len(already_seen_dict)} / {len(issue_list)} ")
    return already_seen_dict, context_dict