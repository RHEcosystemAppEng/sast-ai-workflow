# Summarize Justifications Evaluation

## Overview
Evaluates summarize justifications performance using NAT framework to test text summarization quality and justification synthesis.

## Usage
```bash
export LLM_API_KEY=your_nvidia_api_key
python evaluation/runners/run_summarize_evaluation.py
```

## Flow Steps
1. **Environment Check** - Validates API keys and config files
2. **NAT Evaluation** - Runs summarize evaluation with automatic metrics collection
3. **Quality Assessment** - Uses judge LLM to evaluate summarization quality
4. **Results Archive** - Archives timestamped results for historical tracking

## Directory Structure
- `configs/summarize_justifications_eval.yml` - NAT evaluation configuration
- `dataset/summarize_eval/` - Test cases with justification data
- `tools/summarize_converters.py` - Input/output converters for NAT integration
- `reports/summarize_justifications/` - Evaluation results and quality metrics

## Input Data Structure
Dataset contains justification summarization test cases with analyzed vulnerability data. Each test case includes:
- `input_data.issues[]` - Array of SAST issues with detailed analysis results, justifications, and confidence scores from previous workflow steps
- `expected_output_obj.issues[]` - Expected summarized justifications that consolidate multiple vulnerability analyses into clear, concise summaries

### Naming Convention for Package/Version Detection

The evaluation framework uses a specific naming convention in the test case `id` field to extract package name, version, and issue identifier:

**Format**: `{package}-{version}-{issue_id}`
- **Package**: Software package name (e.g., `audit`, `glibc`, `openssl`)
- **Version**: Version with underscores instead of dots (e.g., `4_0` for v4.0, `2_8` for v2.8)
- **Issue ID**: Descriptive identifier for the specific issue

**Examples**:
- `audit-4_0-buffer_overflow_summary` → Package: `audit`, Version: `4.0`, Issue: `buffer_overflow_summary`
- `glibc-2_8-memory_leak_summary` → Package: `glibc`, Version: `2.8`, Issue: `memory_leak_summary`

This naming convention enables hierarchical analysis and package-specific performance tracking.

## Converters and Integration
The `summarize_converters.py` manages NAT framework integration by transforming evaluation data into SASTWorkflowTracker format for justification summarization. The workflow bypasses earlier workflow steps since input data already contains analyzed issues, focusing specifically on the summarization task. No graph flow modifications were needed as this evaluation tests the final summarization step in isolation.

## Output Files
- `workflow_output.json` - Raw NAT evaluation results
- `standardized_data_all.csv` - Performance metrics and scoring data
- `all_requests_profiler_traces.json` - LLM performance profiling data
- `evaluation_metrics.json` - Calculated evaluation metrics