# Filter Evaluation

## Overview
Evaluates SAST filter performance using NAT framework to test false positive detection via FAISS similarity matching.

## Usage
```bash
export LLM_API_KEY=your_nvidia_api_key
export EMBEDDING_API_KEY=your_embedding_api_key
python evaluation/runners/run_filter_evaluation.py
```

## Flow Steps
1. **Environment Check** - Validates API keys and config files
2. **NAT Evaluation** - Runs filter evaluation with automatic metrics collection
3. **Validation Analysis** - Compares results against ground truth dataset
4. **Results Archive** - Saves timestamped results for historical tracking

## Directory Structure
- `configs/filter_eval.yml` - NAT evaluation configuration
- `dataset/filter_eval/` - Test cases with ground truth data
- `tools/filter_converters.py` - Input/output converters for NAT integration
- `utils/filter_validation.py` - Ground truth validation and metrics calculation
- `reports/filter/` - Evaluation results and validation reports

## Input Data Structure
Dataset contains package-based test cases with SAST issues grouped by package. Each test case includes:
- `input_data.issues[]` - Array of SAST vulnerabilities with file paths, descriptions, and severity
- `expected_output_obj.package_analysis[]` - Expected filter decisions (FALSE_POSITIVE/TRUE_POSITIVE) and FAISS similarity matches per package

## Converters and Integration
The `filter_converters.py` handles NAT framework integration by converting between evaluation data formats and SASTWorkflowTracker objects. The filter tool in `src/sast_agent_workflow/tools/filter.py` was enhanced to dynamically import these converters when running in evaluation mode, enabling proper input/output transformation for NAT.

## Output Files
- `workflow_output.json` - Raw NAT evaluation results
- `filter_validation_report.json` - Validation metrics and accuracy analysis
- `standardized_data_all.csv` - Performance metrics in CSV format
- `all_requests_profiler_traces.json` - Performance profiling data