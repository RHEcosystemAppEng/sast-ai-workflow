import json

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
    """
    # Reading known false-positives
    text_false_positives = read_known_errors_file(config.KNOWN_FALSE_POSITIVE_FILE_PATH)

    false_positive_db = main_process.create_vdb_for_knonw_issues(text_false_positives)

    already_seen_dict = {}
    for issue in issue_list:

        question = "Do you see this exact error trace? " + issue.trace
        p, response = main_process.filter_known_error(false_positive_db, question, issue)
        print(f"Response of filter_known_error: {response}")

        filter_response = json.loads(response)
        print(f"{issue.id} Is known false positive? {filter_response['Result']}")
        if "yes" in filter_response['Result'].strip().lower():
            already_seen_dict[issue.id] = filter_response
            print(f"LLM found {issue.id} error trace inside known false positives list")
            # print(issue.trace)
            # print("Proceeding to the next issue...")
            # continue

    print(f"Known false positives: {len(already_seen_dict)} / {len(issue_list)} ")
    return already_seen_dict