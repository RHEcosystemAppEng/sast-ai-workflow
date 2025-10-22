"""
Evaluation Constants

This module contains constants used across the evaluation framework.
"""

# =============================================================================
# Iteration Limits
# =============================================================================

# Maximum number of analysis iterations for judge LLM evaluation
MAX_ANALYSIS_ITERATIONS = 3

# Maximum number of filter iterations
MAX_FILTER_ITERATIONS = 1


# =============================================================================
# File and Directory Paths
# =============================================================================

# Report directories
REPORTS_BASE_DIR = "evaluation/reports"
REPORTS_FILTER_DIR = "evaluation/reports/filter"
REPORTS_JUDGE_LLM_DIR = "evaluation/reports/judge_llm_analysis"
REPORTS_SUMMARIZATION_DIR = "evaluation/reports/summarize_justifications"

# Dataset directories
DATASET_BASE_DIR = "evaluation/dataset"
DATASET_FILTER_DIR = "evaluation/dataset/filter_eval"
DATASET_JUDGE_LLM_DIR = "evaluation/dataset/judge_llm_eval"
DATASET_SUMMARIZATION_DIR = "evaluation/dataset/summarize_eval"

# Configuration directory
CONFIGS_DIR = "evaluation/configs"

# Output directory
OUTPUT_DIR = "evaluation/output"

# MLflow directory
MLRUNS_DIR = "evaluation/mlruns"


# =============================================================================
# Output File Names
# =============================================================================

# Workflow output
WORKFLOW_OUTPUT_FILENAME = "workflow_output.json"

# Validation reports
FILTER_VALIDATION_REPORT_FILENAME = "filter_validation_report.json"

# Evaluation metrics
EVALUATION_METRICS_FILENAME = "evaluation_metrics.json"

# Quality evaluation outputs
JUSTIFICATION_QUALITY_OUTPUT_FILENAME = "justification_quality_eval_output.json"
SUMMARIZATION_QUALITY_OUTPUT_FILENAME = "summarization_quality_eval_output.json"

# Config files
FILTER_CONFIG_FILENAME = "filter_eval.yml"
JUDGE_LLM_CONFIG_FILENAME = "judge_llm_analysis_eval.yml"
SUMMARIZATION_CONFIG_FILENAME = "summarize_justifications_eval.yml"

# Dataset files
FILTER_DATASET_FILENAME = "filter_eval_dataset_individual_issues.json"
JUDGE_LLM_DATASET_FILENAME = "judge_llm_eval_dataset.json"
SUMMARIZATION_DATASET_FILENAME = "summarize_eval_dataset.json"

# Evaluation output files
STANDARDIZED_DATA_FILENAME = "standardized_data_all.csv"
PROFILER_TRACES_FILENAME = "all_requests_profiler_traces.json"

# Utility paths
UTILS_DIR = "evaluation/utils"
FILTER_VALIDATION_SCRIPT = "filter_validation.py"

# Prompt templates
PROMPTS_DIR = "src/templates/prompts"
JUSTIFICATION_SUMMARY_SYSTEM_PROMPT_FILENAME = "justification_summary_system_prompt.yaml"
JUSTIFICATION_SUMMARY_HUMAN_PROMPT_FILENAME = "justification_summary_human_prompt.yaml"

# Evaluation runners
RUNNERS_DIR = "evaluation/runners"

# Known non-issues data
KNOWN_NON_ISSUES_DIR = "evaluation/known_non_issues_data"
KNOWN_NON_ISSUES_FILENAME = "known_non_issues.txt"
KNOWN_NON_ISSUES_VECTOR_STORE_DIR = "evaluation/known_non_issues_data/faiss_vector_store"


# =============================================================================
# Classification Labels
# =============================================================================

# Filter classification results
CLASSIFICATION_TRUE_POSITIVE = "TRUE_POSITIVE"
CLASSIFICATION_FALSE_POSITIVE = "FALSE_POSITIVE"


# =============================================================================
# Judge LLM Quality Metric Dimensions
# =============================================================================

# Metric dimension names
JUDGE_METRIC_CLARITY = "CLARITY"
JUDGE_METRIC_COMPLETENESS = "COMPLETENESS"
JUDGE_METRIC_TECHNICAL_ACCURACY = "TECHNICAL_ACCURACY"
JUDGE_METRIC_LOGICAL_FLOW = "LOGICAL_FLOW"

# Metric weights (must sum to 1.0)
JUDGE_WEIGHT_CLARITY = 0.35
JUDGE_WEIGHT_COMPLETENESS = 0.30
JUDGE_WEIGHT_TECHNICAL_ACCURACY = 0.25
JUDGE_WEIGHT_LOGICAL_FLOW = 0.10


# =============================================================================
# Summarization Quality Metric Dimensions
# =============================================================================

# Metric dimension names
SUMMARY_METRIC_SEMANTIC_SIMILARITY = "SEMANTIC_SIMILARITY"
SUMMARY_METRIC_FACTUAL_ACCURACY = "FACTUAL_ACCURACY"
SUMMARY_METRIC_CONCISENESS = "CONCISENESS"
SUMMARY_METRIC_PROFESSIONAL_TONE = "PROFESSIONAL_TONE"

# Metric weights (must sum to 1.0)
SUMMARY_WEIGHT_SEMANTIC_SIMILARITY = 0.35
SUMMARY_WEIGHT_FACTUAL_ACCURACY = 0.30
SUMMARY_WEIGHT_CONCISENESS = 0.20
SUMMARY_WEIGHT_PROFESSIONAL_TONE = 0.15