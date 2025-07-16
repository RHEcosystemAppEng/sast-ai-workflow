CONFIG_H_PATH = "CONFIG_H_PATH"
COMPILE_COMMANDS_JSON_PATH = "COMPILE_COMMANDS_JSON_PATH"
LIBCLANG_PATH = "LIBCLANG_PATH"
PROJECT_NAME = "PROJECT_NAME"
PROJECT_VERSION = "PROJECT_VERSION"
LLM_URL = "LLM_URL"
LLM_MODEL_NAME = "LLM_MODEL_NAME"
LLM_API_KEY = "LLM_API_KEY"
LOG_FILE = "LOG_FILE"
DOWNLOAD_REPO = "DOWNLOAD_REPO"
DEBUG_MODULES = "DEBUG_MODULES"
REPO_REMOTE_URL = "REPO_REMOTE_URL"
REPO_LOCAL_PATH = "REPO_LOCAL_PATH"
EMBEDDINGS_LLM_URL = "EMBEDDINGS_LLM_URL"
EMBEDDINGS_LLM_API_KEY = "EMBEDDINGS_LLM_API_KEY"
EMBEDDINGS_LLM_MODEL_NAME = "EMBEDDINGS_LLM_MODEL_NAME"
OUTPUT_FILE_PATH = "OUTPUT_FILE_PATH"
AGGREGATE_RESULTS_G_SHEET = "AGGREGATE_RESULTS_G_SHEET"
INPUT_REPORT_FILE_PATH = "INPUT_REPORT_FILE_PATH"
KNOWN_FALSE_POSITIVE_FILE_PATH = "KNOWN_FALSE_POSITIVE_FILE_PATH"
HUMAN_VERIFIED_FILE_PATH = "HUMAN_VERIFIED_FILE_PATH"
USE_KNOWN_FALSE_POSITIVE_FILE = "USE_KNOWN_FALSE_POSITIVE_FILE"
CALCULATE_METRICS = "CALCULATE_METRICS"
TOKENIZERS_PARALLELISM = "TOKENIZERS_PARALLELISM"
RUN_WITH_CRITIQUE = "RUN_WITH_CRITIQUE"
CRITIQUE_LLM_URL = "CRITIQUE_LLM_URL"
CRITIQUE_LLM_MODEL_NAME = "CRITIQUE_LLM_MODEL_NAME"
CRITIQUE_LLM_API_KEY = "CRITIQUE_LLM_API_KEY"
SERVICE_ACCOUNT_JSON_PATH = "SERVICE_ACCOUNT_JSON_PATH"
SIMILARITY_ERROR_THRESHOLD = "SIMILARITY_ERROR_THRESHOLD"
RED_ERROR_FOR_LLM_REQUEST = (
    "WARNING: An error occurred "
    "{max_retry_limit} times in {function_name} process. "
    "Please check this Issue-id {issue_id}."
    "\nError: {error}"
)
FALLBACK_JUSTIFICATION_MESSAGE = [
    "Failed during analyze process. Defaulting to: NOT A FALSE POSITIVE."
]
YES_OPTIONS = ["y", "yes"]
NO_OPTIONS = ["n", "no"]
ALL_VALID_OPTIONS = YES_OPTIONS + NO_OPTIONS
KNOWN_FALSE_POSITIVE_ISSUE_SEPARATOR = "\n\n"

# Template and formatting constants
KNOWN_FALSE_POSITIVE_TEMPLATES = {
    "EXAMPLE_MULTILINE_TEMPLATE": (
        "** Example-{number} **\n"
        "(Example-{number}) Known False Positive:\n"
        "Error {issue_type} ({issue_cwe}):\n"
        "{error_trace}\n"
        "(Example-{number}) Reason Marked as False Positive:\n"
        "{reason}\n\n"
    ),
    "FILTER_CONTEXT_TEMPLATE": (
        "Known False Positive {index}:\n"
        "false_positive_error_trace:\n"
        "{error_trace}\n"
        "reason_marked_false_positive:\n"
        "{reason}\n\n"
    )
}

# LLM System Prompts
FILTER_SYSTEM_PROMPT = """You're an expert at identifying similar error stack traces.
Given:

1.  **Known False Positive Issues** (`context_false_positives`): A list where each issue contains:
    * `false_positive_error_trace`: The error trace of the false positive.
    * `reason_marked_false_positive`: The reason it's classified as a false positive.
2.  **New User Error Trace** (`user_error_trace`): The error trace from a new user.

Your task is to determine if the `user_error_trace` **exactly matches** any of the `false_positive_error_trace` entries.

**Comparison Rules:**

* **Ignore:** Line numbers and package version details.
* **Must Match Exactly:** Method names and their call order.

**Constraint:**

* Your response must strictly follow the provided **answer response template** and include no additional text.

---

**Answer Response Template:**

```json
{answer_template}


context_false_positives: 
{context}


user_error_trace: 
{user_error_trace}"""

# Pattern matching constants
REGEX_PATTERNS = {
    "CWE_PATTERN": r"CWE-\d+",
    "CODE_BLOCK_LINE_PATTERN": r"#\s*\d+\|"
}

# Validation constants
VALIDATION_LIMITS = {
    "MIN_SIMILARITY_THRESHOLD": 1,
    "MAX_SIMILARITY_THRESHOLD": 10
}
