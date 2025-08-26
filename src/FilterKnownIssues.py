import logging
from typing import Any, List

from common.config import Config
from common.constants import KNOWN_NON_ISSUES_TEMPLATES
from dto.Issue import Issue
from dto.ResponseStructures import KnownNonIssue
from LLMService import LLMService
from Utils.file_utils import read_known_errors_file
from Utils.validation_utils import safe_validate, validate_issue_list

logger = logging.getLogger(__name__)


class KnownIssueRetriever:
    vector_store: Any
    similarity_error_threshold: int

    def __init__(self, vector_store, similarity_error_threshold: int):
        self.vector_store = vector_store
        self.similarity_error_threshold = similarity_error_threshold

    def get_relevant_known_issues(self, finding_trace: str, issue_type: str) -> list[KnownNonIssue]:
        """ "
        Get the relevant known issues from the vector store.
        Args:
            finding_trace: The trace of the finding.
            issue_type: The type of the issue.
        Returns:
            list[KnownIssue]: A list of KnownIssue objects containing error traces and metadata.
        """
        retriever = self.vector_store.as_retriever(
            search_kwargs={
                "k": self.similarity_error_threshold,
                "filter": {"issue_type": issue_type},  # try to filter by cwe too
            }
        )
        docs = retriever.invoke(finding_trace)
        known_issues = []
        for doc in docs:
            known_issues.append(
                KnownNonIssue(
                    error_trace=doc.page_content,
                    reason_of_false_positive=doc.metadata.get("reason_of_false_positive", ""),
                    issue_type=doc.metadata.get("issue_type", ""),
                    issue_cwe=doc.metadata.get("issue_cwe"),
                    similarity_score=None,
                )
            )
        return known_issues


def capture_known_issues(main_process: LLMService, issue_list: List[Issue], config: Config):
    """
    Identify and capture known non-issue cases.
    Returns:
        dict: A dictionary where keys are issue IDs and values are the FilterResponse objects
              for issues identified as known non-issues.
        dict: A dictionary where keys are issue IDs and values are the contexts with the
              most N (N=SIMILARITY_ERROR_THRESHOLD) similar known issues from the same type.
    """
    # Input validation
    if not safe_validate(validate_issue_list, issue_list):
        logger.error("Invalid issue list provided")
        return {}, {}

    known_issue_retriever = create_known_issue_retriever(main_process, config)

    already_seen_dict = {}
    examples_context_dict = {}
    for issue in issue_list:
        similar_known_issues_list = known_issue_retriever.get_relevant_known_issues(
            issue.trace, issue.issue_type
        )

        is_finding_known_non_issue, equal_error_trace = is_known_non_issue(
            issue, similar_known_issues_list, main_process
        )
        if is_finding_known_non_issue:
            already_seen_dict[issue.id] = equal_error_trace
            logger.info(f"LLM found {issue.id} error trace inside known non-issues list")

        # store the context of the similar known issues in the examples_context_dict
        examples_context_dict[issue.id] = convert_similar_issues_to_examples_context_string(
            similar_known_issues_list
        )

    logger.info(f"Known non-issues: {len(already_seen_dict)} / {len(issue_list)} ")
    return already_seen_dict, examples_context_dict


def create_known_issue_retriever(main_process: LLMService, config: Config) -> KnownIssueRetriever:
    """
    Creates a retriever for the known non-issue findings database.
    The retriever is used to retrieve the most similar known issues from the database for a given issue.
    Args:
        main_process: The main process.
        config: The config.
    Returns:
        KnownIssueRetriever: A known issue retriever.
    """
    text_false_positives = read_known_errors_file(config.KNOWN_NON_ISSUES_FILE_PATH)
    known_issue_db = main_process.vector_service.create_known_issues_vector_store(
        text_false_positives, main_process.embedding_llm
    )
    known_issue_retriever = KnownIssueRetriever(known_issue_db, config.SIMILARITY_ERROR_THRESHOLD)
    return known_issue_retriever


def is_known_non_issue(
    issue, similar_known_issues_list, main_process: LLMService
) -> tuple[bool, List[str]]:
    def convert_similar_known_issues_to_filter_known_error_context(resp) -> str:
        context_list = ""
        for index, known_issue in enumerate(resp, start=1):
            context_list += KNOWN_NON_ISSUES_TEMPLATES["FILTER_CONTEXT_TEMPLATE"].format(
                index=index,
                error_trace=known_issue.error_trace,
                reason=known_issue.reason_of_false_positive,
            )
        return context_list

    similar_findings_context = convert_similar_known_issues_to_filter_known_error_context(
        similar_known_issues_list
    )
    filter_response = main_process.filter_known_error(issue, similar_findings_context)
    logger.debug(f"Response of filter_known_error: {filter_response}")
    result_value = filter_response.result.strip().lower()
    logger.debug(f"{issue.id} Is known non-issue? {result_value}")
    return "yes" in result_value, filter_response.equal_error_trace


def convert_similar_issues_to_examples_context_string(
    similar_known_issues_list: list[KnownNonIssue],
) -> str:
    """Convert a list of known non-issue CVE examples into a formatted string."""
    formatted_context = ""
    for example_number, known_issue in enumerate(similar_known_issues_list, start=1):
        formatted_context += KNOWN_NON_ISSUES_TEMPLATES["EXAMPLE_MULTILINE_TEMPLATE"].format(
            number=example_number,
            error_trace=known_issue.error_trace,
            reason=known_issue.reason_of_false_positive,
            issue_type=known_issue.issue_type,
            issue_cwe=known_issue.issue_cwe,
        )

    return formatted_context
