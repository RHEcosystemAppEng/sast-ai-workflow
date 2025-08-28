"""
Configuration constants for Node Evaluation
"""

MAX_EXAMPLES_TO_TEST = 5  # Testing with 5 examples

FAITHFULNESS_THRESHOLD = 0.8
RELEVANCY_THRESHOLD = 0.75
COMPRESSION_MIN_THRESHOLD = 0.60
COMPRESSION_MAX_THRESHOLD = 0.85
SECURITY_KEYWORDS_THRESHOLD = 0.70

SECURITY_KEYWORDS = [
    'buffer', 'overrun', 'overflow', 'underflow', 'bounds', 'vulnerability', 'exploit', 'access',
    'pointer', 'memory', 'allocation', 'free', 'null', 'dereference', 'use-after-free',
    'double-free', 'leak', 'uninitialized'
]

DEFAULT_LLM_MODEL = 'nvidia/llama-3.1-nemotron-70b-instruct'
DEFAULT_LLM_URL = 'https://integrate.api.nvidia.com/v1'
DEFAULT_TEMPERATURE = 0.1

GOLDEN_DATASET_PATH = "/Users/gziv/Dev/sast-ai-workflow/evaluation/datasets/golden_dataset.json"
EVALUATION_REPORT_PATH = "/Users/gziv/Dev/sast-ai-workflow/evaluation/reports/evaluation_report.json"