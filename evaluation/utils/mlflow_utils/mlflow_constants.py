#!/usr/bin/env python3
"""
MLflow Constants for SAST AI Workflow Evaluation Dashboard

This module contains shared constants used across all MLflow converter classes.

APPENG-3747: Evaluation Dashboard Creation
"""

# ============================================================================
# File Name Constants
# ============================================================================
EVALUATION_METRICS_FILE = "evaluation_metrics.json"
INFERENCE_OPTIMIZATION_FILE = "inference_optimization.json"
WORKFLOW_OUTPUT_FILE = "workflow_output.json"
PROFILER_TRACES_FILE = "all_requests_profiler_traces.json"
FILTER_VALIDATION_FILE = "filter_validation_report.json"
JUSTIFICATION_QUALITY_FILE = "justification_quality_eval_output.json"
SUMMARIZATION_QUALITY_FILE = "summarization_quality_eval_output.json"

# ============================================================================
# Dictionary Key Constants
# ============================================================================
# Metrics keys
METRICS_KEY = "metrics"
EVALUATION_KEY = "evaluation"
INFERENCE_KEY = "inference"
PROFILER_KEY = "profiler"

# Common metric names
PRECISION_KEY = "precision"
RECALL_KEY = "recall"
F1_SCORE_KEY = "f1_score"
ACCURACY_KEY = "accuracy"
TOTAL_ITEMS_KEY = "total_items"
TRUE_POSITIVES_KEY = "true_positives"
FALSE_POSITIVES_KEY = "false_positives"
TRUE_NEGATIVES_KEY = "true_negatives"
FALSE_NEGATIVES_KEY = "false_negatives"

# Token usage keys
TOTAL_TOKENS_KEY = "total_tokens"
PROMPT_TOKENS_KEY = "prompt_tokens"
COMPLETION_TOKENS_KEY = "completion_tokens"

# Timing keys
TOTAL_TIME_SECONDS_KEY = "total_time_seconds"
AVG_TIME_PER_REQUEST_KEY = "average_time_per_request"

# Quality evaluation keys
EVAL_OUTPUT_ITEMS_KEY = "eval_output_items"
REASONING_KEY = "reasoning"
GENERATED_ANSWER_KEY = "generated_answer"

# Summarization quality metric names
OVERALL_SCORE_KEY = "overall_score"
SEMANTIC_SIMILARITY_KEY = "semantic_similarity"
FACTUAL_ACCURACY_KEY = "factual_accuracy"
CONCISENESS_KEY = "conciseness"
PROFESSIONAL_TONE_KEY = "professional_tone"

# Judge LLM quality metric names
CLARITY_KEY = "clarity"
COMPLETENESS_KEY = "completeness"
TECHNICAL_ACCURACY_KEY = "technical_accuracy"
LOGICAL_FLOW_KEY = "logical_flow"

# Common metric names for logging
SIMILAR_ISSUES_COUNT_KEY = "similar_issues_count"
LLM_CALL_COUNT_KEY = "llm_call_count"

# Parameter name keys
GENERATED_SUMMARY_PARAM = "generated_summary"
EXPECTED_OUTPUT_PARAM = "expected_output"
INPUT_QUESTION_PARAM = "input_question"
FILTER_RESULT_PARAM = "filter_result"
INVESTIGATION_RESULT_PARAM = "investigation_result"
IS_FINAL_PARAM = "is_final"
SHORT_JUSTIFICATIONS_PARAM = "short_justifications"
PACKAGE_NAME_PARAM = "package_name"
VERSION_PARAM = "version"
ISSUE_COUNT_PARAM = "issue_count"
ISSUES_PARAM = "issues"
TOTAL_PACKAGES_PARAM = "total_packages"
TOTAL_ISSUES_PARAM = "total_issues"
PACKAGES_PARAM = "packages"

# Default values
DEFAULT_METRIC_VALUE = 0.0
DEFAULT_COUNT_VALUE = 0
SINGLE_PACKAGE_COUNT = 1
SINGLE_ISSUE_COUNT = 1
SEMANTIC_SIMILARITY_SCALE_FACTOR = 10.0
MAX_PARAM_LENGTH = 500

# ============================================================================
# MLflow Tag Constants
# ============================================================================
TAG_NODE_TYPE = "node_type"
TAG_LEVEL = "level"
TAG_EVALUATION_RUN = "evaluation_run"
TAG_PARENT_RUN_ID = "parent_run_id"
TAG_PACKAGE = "package"
TAG_VERSION = "version"
TAG_PACKAGE_VERSION = "package_version"
TAG_ISSUE_ID = "issue_id"
TAG_ORIGINAL_ISSUE_ID = "original_issue_id"
TAG_ISSUE_COUNT = "issue_count"
TAG_TOTAL_PACKAGES = "total_packages"
TAG_TOTAL_ISSUES = "total_issues"
TAG_RUN_TIMESTAMP = "run_timestamp"
TAG_RUN_DATE = "run_date"
TAG_RUN_TIME = "run_time"

# Level values
LEVEL_EVALUATION_RUN = "evaluation_run"
LEVEL_PACKAGE = "package"
LEVEL_ISSUE = "issue"

# ============================================================================
# Other Constants
# ============================================================================
RUN_ID_PREFIX = "run--"
RUN_DIR_PREFIX = "run_"