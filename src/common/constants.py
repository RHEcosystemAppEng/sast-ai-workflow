CONFIG_H_PATH = "CONFIG_H_PATH"
COMPILE_COMMANDS_JSON_PATH = "COMPILE_COMMANDS_JSON_PATH"
LIBCLANG_PATH = "LIBCLANG_PATH"
PROJECT_NAME = "PROJECT_NAME"
PROJECT_VERSION = "PROJECT_VERSION"
LLM_URL = "LLM_URL"
LLM_MODEL_NAME = "LLM_MODEL_NAME"
LLM_API_KEY = "LLM_API_KEY"
LLM_API_TYPE = "LLM_API_TYPE"
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
MAX_ANALYSIS_ITERATIONS = "MAX_ANALYSIS_ITERATIONS"
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
    ),
}

# Pattern matching constants
REGEX_PATTERNS = {"CWE_PATTERN": r"CWE-\d+", "CODE_BLOCK_LINE_PATTERN": r"#\s*\d+\|"}

# Validation constants
VALIDATION_LIMITS = {"MIN_SIMILARITY_THRESHOLD": 1, "MAX_SIMILARITY_THRESHOLD": 10}
KNOWN_ISSUES_SHORT_JUSTIFICATION = "The error is similar to one found in the provided known issues (Details in the full Justification)"
NO_MATCHING_TRACE_FOUND = "No matching trace found"

# Metrics calculation constants
METRICS_ERROR_NO_ISSUES = "No completed issues found"
METRICS_ERROR_CALCULATION_FAILED = "Metrics calculation failed"
METRICS_ERROR_IMPORT = "Import error"
METRICS_ERROR_UNEXPECTED = "Unexpected error"

# Write results constants
WRITE_RESULTS_CONFIG_MISSING = "No config found in tracker - skipping results writing"
WRITE_RESULTS_DISABLED = "WRITE_RESULTS is disabled in config - skipping results writing"
WRITE_RESULTS_NO_VALID_METRICS = "No valid metrics available - creating EvaluationSummary from scratch"
WRITE_RESULTS_SUCCESS = "Successfully wrote results to configured destinations"
WRITE_RESULTS_FAILURE = "Failed to write results: {}"
WRITE_RESULTS_SUMMARY_SUCCESS = "Successfully created EvaluationSummary from existing metrics"
WRITE_RESULTS_SUMMARY_FALLBACK = "Failed to create EvaluationSummary from metrics: {}. Falling back to fresh calculation."
WRITE_RESULTS_SUMMARY_FALLBACK_FAILED = "Fallback EvaluationSummary creation also failed: {}"

# Metrics field names
METRICS_FIELD_ERROR = "error"
METRICS_FIELD_CONFUSION_MATRIX = "confusion_matrix"
METRICS_FIELD_TRUE_POSITIVES = "true_positives"
METRICS_FIELD_TRUE_NEGATIVES = "true_negatives"
METRICS_FIELD_FALSE_POSITIVES = "false_positives"
METRICS_FIELD_FALSE_NEGATIVES = "false_negatives"
METRICS_FIELD_ACTUAL_TRUE_POSITIVES = "actual_true_positives"
METRICS_FIELD_ACTUAL_FALSE_POSITIVES = "actual_false_positives"
METRICS_FIELD_PREDICTED_TRUE_POSITIVES = "predicted_true_positives"
METRICS_FIELD_PREDICTED_FALSE_POSITIVES = "predicted_false_positives"
METRICS_FIELD_COUNT_SUFFIX = "_count"
