# Filter Evaluation

## Overview
Evaluates SAST filter performance using NAT framework to test false positive detection via FAISS similarity matching. Uses individual issue-level evaluation for granular accuracy metrics.

## Usage
```bash
export LLM_API_KEY=your_nvidia_api_key
export EMBEDDINGS_LLM_API_KEY=your_embedding_api_key
python evaluation/runners/run_filter_evaluation.py
```

## Flow Steps
1. **Environment Check** - Validates API keys and config files
2. **NAT Evaluation** - Runs filter evaluation with automatic metrics collection
3. **Validation Analysis** - Compares results against ground truth dataset
4. **Results Archive** - Saves timestamped results for historical tracking

## Directory Structure
- `configs/filter_eval.yml` - NAT evaluation configuration
- `dataset/filter_eval/filter_eval_dataset_individual_issues.json` - Individual issue test cases
- `converter_tools/filter_converters.py` - Input/output converters for NAT integration
- `utils/filter_validation.py` - Ground truth validation and metrics calculation
- `reports/filter/` - Evaluation results and validation reports

## Dataset Format: Individual Issue Evaluation

The evaluation uses individual issue format (similar to judge/summarize) rather than package-level grouping. Each test case represents a single SAST issue:

### Test Case Structure
```json
{
  "id": "audit-4_0-buffer_overflow_known_fp",
  "question": "{\"id\": \"audit-4_0-buffer_overflow_known_fp\", \"issue_id\": \"buffer_overflow_known_fp\", \"issue_type\": \"OVERRUN\", \"severity\": \"high\", \"trace\": \"...\", \"file_path\": \"audit-4.0/auparse/auparse.c\", \"line_number\": 514, \"cwe_id\": \"CWE-120\"}",
  "expected_output_obj": {
    "filter_result": "FALSE_POSITIVE",
    "confidence": null,
    "similar_known_issues": ["audit-4.0/auparse/auparse.c", "audit-4.0/lib/netlink.c"],
    "justification": "Awaiting filter analysis"
  }
}
```

### Naming Convention for Package/Version Detection

The evaluation framework uses a specific naming convention in the `id` field to extract package name, version, and issue identifier:

**Format**: `{package}-{version}-{issue_id}`
- **Package**: Software package name (e.g., `audit`, `glibc`, `openssl`)
- **Version**: Version with underscores instead of dots (e.g., `4_0` for v4.0, `2_8` for v2.8)
- **Issue ID**: Descriptive identifier for the specific issue

**Examples**:
- `audit-4_0-buffer_overflow_known_fp` → Package: `audit`, Version: `4.0`, Issue: `buffer_overflow_known_fp`
- `glibc-2_8-memory_leak_borderline` → Package: `glibc`, Version: `2.8`, Issue: `memory_leak_borderline`
- `openssl-1_1_1-null_pointer_dereference` → Package: `openssl`, Version: `1.1.1`, Issue: `null_pointer_dereference`

This naming convention allows the evaluation system to:
1. Group results by package for package-level analysis
2. Track version-specific vulnerability patterns
3. Maintain individual issue-level granularity
4. Generate hierarchical evaluation reports

## Converters and Integration
The `filter_converters.py` handles NAT framework integration by converting between evaluation data formats and SASTWorkflowTracker objects. The converter creates individual Issue objects and extracts real FAISS vector store matches from the filter analysis.

## Output Files
- `workflow_output.json` - Raw NAT evaluation results with individual issue predictions
- `filter_validation_report.json` - Validation metrics and accuracy analysis
- `standardized_data_all.csv` - Performance metrics in CSV format
- `all_requests_profiler_traces.json` - Performance profiling data

## Evaluation Metrics

### Classification Accuracy
Measures how well the filter distinguishes between TRUE_POSITIVE and FALSE_POSITIVE issues:
- **True Positives**: Issues correctly identified as legitimate vulnerabilities
- **True Negatives**: Issues correctly identified as false positives
- **Precision/Recall/F1**: Standard binary classification metrics

### FAISS Matching Accuracy
Evaluates the filter's ability to find similar known issues in the vector store:
- **Only calculated when expected matches exist** in the ground truth
- **Returns `None`** for test cases with no expected similar issues
- **Measures exact file path matching** between expected and actual FAISS results
- **Example**: If expected `["audit-4.0/auparse/auparse.c"]` and actual `["audit-4.0/auparse/auparse.c", "audit-4.0/lib/netlink.c"]`, accuracy = 50%

This approach provides meaningful metrics only when there's something to evaluate, avoiding misleading perfect scores for cases where no similarity matching is expected.