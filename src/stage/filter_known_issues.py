import logging
from typing import List

from common.config import Config
from LLMService import LLMService
from Utils.file_utils import read_known_errors_file

logger = logging.getLogger(__name__)


def capture_known_issues(main_process: LLMService, issue_list: List, config: Config):
    """
    Identify and capture known false-positive issues.
    Returns:
        dict: A dictionary where keys are issue IDs and values are the FilterResponse objects
              for issues identified as known false positives.
        dict: A dictionary where keys are issue IDs and values are the contexts with the
              most N (N=SIMILARITY_ERROR_THRESHOLD) similar known issues from the same type.
    """
    try:
        text_false_positives = read_known_errors_file(config.KNOWN_FALSE_POSITIVE_FILE_PATH)
    except Exception as e:
        logger.error(f"Failed to read known false positives file: {e}")
        return {}, {}
    
    if not text_false_positives:
        logger.warning("No known false positives found")
        return {}, {}

    try:
        false_positive_db = main_process.create_vdb_for_known_issues(text_false_positives)
    except Exception as e:
        logger.error(f"Failed to create vector database: {e}")
        return {}, {}

    if not issue_list:
        logger.warning("No issues to process")

    already_seen_dict = {}
    context_dict = {}
    for issue in issue_list:
        try:
            filter_response, context = main_process.filter_known_error(false_positive_db, issue)
            context_dict[issue.id] = convert_similar_issues_to_context_string(context)
            logger.debug(f"Response of filter_known_error: {filter_response}")

            result_value = filter_response.result.strip().lower()
            logger.debug(f"{issue.id} Is known false positive? {result_value}")

            if "yes" in result_value:
                already_seen_dict[issue.id] = filter_response
                logger.info(f"LLM found {issue.id} error trace inside known false positives list")
        except Exception as e:
            logger.error(f"Failed to process issue {issue.id}: {e}")
            continue

    logger.info(f"Known false positives: {len(already_seen_dict)} / {len(issue_list)} ")
    return already_seen_dict, context_dict


def convert_similar_issues_to_context_string(similar_known_issues_list: list) -> str:
    """Convert a list of known false positive CVE examples into a formatted string."""
    if not similar_known_issues_list:
        logger.debug("No similar known issues to format")
        return ""
    
    context_parts = []
    for i, issue in enumerate(similar_known_issues_list, 1):
        if 'false_positive_error_trace' not in issue or 'reason_marked_false_positive' not in issue:
            logger.warning(f"Missing required keys in similar issue {i}")
            continue
            
        context_parts.append(
            f"\n** Example-{i} **\n"
            f"(Example-{i}) Known False Positive:\n"
            f"{issue['false_positive_error_trace']}\n"
            f"(Example-{i}) Reason Marked as False Positive:\n"
            f"{issue['reason_marked_false_positive']}"
        )
    
    logger.debug(f"Formatted {len(context_parts)} similar issues")
    return "".join(context_parts)
