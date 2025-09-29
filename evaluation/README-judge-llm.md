# Judge LLM Evaluation

## Overview
Evaluates Judge LLM analysis performance using AIQ framework to test vulnerability classification and justification quality.

## Usage
```bash
export LLM_API_KEY=your_nvidia_api_key
python evaluation/runners/run_judge_llm_evaluation.py
```

## Flow Steps
1. **Environment Setup** - Configures evaluation environment variables
2. **AIQ Evaluation** - Runs judge LLM evaluation with automatic metrics collection
3. **Results Processing** - Processes evaluation outputs and calculates metrics
4. **Results Archive** - Archives timestamped results for historical tracking

## Directory Structure
- `configs/judge_llm_analysis_eval.yml` - AIQ evaluation configuration
- `dataset/judge_eval/` - Test cases with vulnerability analysis scenarios
- `tools/judge_llm_converters.py` - Input/output converters for AIQ integration
- `reports/judge_llm/` - Evaluation results and performance metrics
- `scripts/` - Additional evaluation and analysis scripts

## Input Data Structure
Dataset contains vulnerability analysis test cases with SAST issues and source code context. Each test case includes:
- `input_data.issues[]` - Array of SAST vulnerabilities with detailed context, file paths, and severity levels
- `expected_output_obj.issues[]` - Expected vulnerability classifications (TRUE_POSITIVE/FALSE_POSITIVE) with detailed justifications and confidence scores

### Naming Convention for Package/Version Detection

The evaluation framework uses a specific naming convention in the test case `id` field to extract package name, version, and issue identifier:

**Format**: `{package}-{version}-{issue_id}`
- **Package**: Software package name (e.g., `audit`, `glibc`, `openssl`)
- **Version**: Version with underscores instead of dots (e.g., `4_0` for v4.0, `2_8` for v2.8)
- **Issue ID**: Descriptive identifier for the specific issue

**Examples**:
- `audit-4_0-buffer_overflow_analysis` → Package: `audit`, Version: `4.0`, Issue: `buffer_overflow_analysis`
- `glibc-2_8-memory_leak_assessment` → Package: `glibc`, Version: `2.8`, Issue: `memory_leak_assessment`

This naming convention enables hierarchical analysis and package-specific performance tracking.

## Converters and Integration
The `judge_llm_converters.py` handles AIQ framework integration by converting evaluation data into SASTWorkflowTracker format for judge LLM analysis. The judge LLM tool in `src/sast_agent_workflow/tools/judge_llm_analysis.py` was enhanced to support evaluation mode by detecting when converters are available and utilizing them for proper input/output transformation within the AIQ evaluation pipeline.

## Output Files
- `workflow_output.json` - Raw AIQ evaluation results
- `standardized_data_all.csv` - Performance metrics and scoring data
- `all_requests_profiler_traces.json` - LLM performance profiling data
- `evaluation_metrics.json` - Calculated evaluation metrics