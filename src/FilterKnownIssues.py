import logging
from typing import Any, List, Optional

from common.config import Config
from common.constants import KNOWN_FALSE_POSITIVE_TEMPLATES
from dto.Issue import Issue
from dto.ResponseStructures import KnownFalsePositive
from LLMService import LLMService
from Utils.embedding_utils import truncate_text_to_token_limit
from Utils.file_utils import read_known_errors_file
from Utils.validation_utils import safe_validate, validate_issue_list

logger = logging.getLogger(__name__)


class KnownIssueRetriever:
    vector_store: Any
    similarity_error_threshold: int
    embedding_model_name: Optional[str]
    max_input_tokens: Optional[int]

    def __init__(
        self,
        vector_store,
        similarity_error_threshold: int,
        embedding_model_name: Optional[str] = None,
        max_input_tokens: Optional[int] = None,
    ):
        self.vector_store = vector_store
        self.similarity_error_threshold = similarity_error_threshold
        self.embedding_model_name = embedding_model_name
        self.max_input_tokens = max_input_tokens

    def get_relevant_known_issues(
        self, finding_trace: str, issue_type: str
    ) -> list[KnownFalsePositive]:
        """ "
        Get the relevant known issues from the vector store.
        Args:
            finding_trace: The trace of the finding.
            issue_type: The type of the issue.
        Returns:
            list[KnownIssue]: A list of KnownIssue objects containing error traces and metadata.
        """
        logger.info("Getting relevant known issues")
        if self.embedding_model_name and self.max_input_tokens:
            finding_trace = truncate_text_to_token_limit(
                finding_trace, self.embedding_model_name, self.max_input_tokens
            )
        docs_with_scores = self.vector_store.similarity_search_with_score(
            finding_trace, k=self.similarity_error_threshold, filter={"issue_type": issue_type}
        )
        known_issues = []
        for doc, score in docs_with_scores:
            known_issues.append(
                KnownFalsePositive(
                    error_trace=doc.page_content,
                    reason_of_false_positive=doc.metadata.get("reason_of_false_positive", ""),
                    issue_type=doc.metadata.get("issue_type", ""),
                    issue_cwe=doc.metadata.get("issue_cwe"),
                    similarity_score=score,
                )
            )
        return known_issues


def capture_known_issues(main_process: LLMService, issue_list: List[Issue], config: Config):
    """
    Identify and capture known false-positive issues.
    Returns:
        dict: A dictionary where keys are issue IDs and values are the FilterResponse objects
              for issues identified as known false positives.
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

        is_finding_known_false_positive, equal_error_trace, filter_confidence = (
            is_known_false_positive(issue, similar_known_issues_list, main_process)
        )
        if is_finding_known_false_positive:
            already_seen_dict[issue.id] = equal_error_trace
            logger.info(f"LLM found {issue.id} error trace inside known false positives list")

        # store the context of the similar known issues in the examples_context_dict
        examples_context_dict[issue.id] = convert_similar_issues_to_examples_context_string(
            similar_known_issues_list
        )

    logger.info(f"Known false positives: {len(already_seen_dict)} / {len(issue_list)} ")
    return already_seen_dict, examples_context_dict


def create_known_issue_retriever(main_process: LLMService, config: Config) -> KnownIssueRetriever:
    """
    Creates a retriever for the known false positive findings database.
    The retriever is used to retrieve the most similar known issues from the database
    for a given issue.
    Args:
        main_process: The main process.
        config: The config.
    Returns:
        KnownIssueRetriever: A known issue retriever.
    """
    text_false_positives = read_known_errors_file(config.KNOWN_FALSE_POSITIVE_FILE_PATH)
    known_issue_db = main_process.vector_service.create_known_issues_vector_store(
        text_false_positives, main_process.embedding_llm, config.EMBEDDINGS_MAX_INPUT_TOKENS
    )
    known_issue_retriever = KnownIssueRetriever(
        known_issue_db,
        config.SIMILARITY_ERROR_THRESHOLD,
        embedding_model_name=main_process.embedding_llm.model,
        max_input_tokens=config.EMBEDDINGS_MAX_INPUT_TOKENS,
    )
    return known_issue_retriever


def is_known_false_positive(
    issue, similar_known_issues_list, main_process: LLMService
) -> tuple[bool, List[str], float]:
    def convert_similar_known_issues_to_filter_known_error_context(resp) -> str:
        context_list = []
        for index, known_issue in enumerate(resp, start=1):
            context_list.append(KNOWN_FALSE_POSITIVE_TEMPLATES["FILTER_CONTEXT_TEMPLATE"].format(
                index=index,
                error_trace=known_issue.error_trace,
                reason=known_issue.reason_of_false_positive,
            ))
        return "".join(context_list)

    similar_findings_context = convert_similar_known_issues_to_filter_known_error_context(
        similar_known_issues_list
    )
    filter_response = main_process.filter_known_error(issue, similar_findings_context)
    logger.debug(f"Response of filter_known_error: {filter_response}")
    prediction = filter_response.result.strip().lower()

    # Guard against hallucination: the model sometimes copies user_error_trace lines back
    # into equal_error_trace instead of citing actual context_false_positives lines.
    # If none of the returned lines appear in the context, override the result to NO.
    if "yes" in prediction and filter_response.equal_error_trace:
        if not any(line in similar_findings_context for line in filter_response.equal_error_trace):
            logger.warning(
                f"{issue.id} Model returned YES but equal_error_trace lines do not appear "
                f"in context_false_positives — overriding result to NO (likely hallucination)."
            )
            prediction = "no"

    logger.info(
        f"{issue.id} Is known false positive? {prediction} "
        f"with confidence: {filter_response.filter_confidence}"
    )
    return (
        "yes" in prediction,
        filter_response.equal_error_trace,
        filter_response.filter_confidence,
    )


def convert_similar_issues_to_examples_context_string(
    similar_known_issues_list: list[KnownFalsePositive],
) -> str:
    """Convert a list of known false positive CVE examples into a formatted string."""
    formatted_context = ""
    for example_number, known_issue in enumerate(similar_known_issues_list, start=1):
        formatted_context += KNOWN_FALSE_POSITIVE_TEMPLATES["EXAMPLE_MULTILINE_TEMPLATE"].format(
            number=example_number,
            error_trace=known_issue.error_trace,
            reason=known_issue.reason_of_false_positive,
            issue_type=known_issue.issue_type,
            issue_cwe=known_issue.issue_cwe,
        )

    return formatted_context
