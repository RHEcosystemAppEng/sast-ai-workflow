# Summarize Justifications Evaluation

## Overview
Evaluates summarize justifications performance using NAT framework to test text summarization quality and justification synthesis.

## Usage
```bash
export LLM_API_KEY=your_nvidia_api_key
python evaluation/runners/run_summarize_evaluation.py
```

## Dataset Configuration

### Local Dataset (Default)
The default configuration uses a local dataset file. The dataset path is specified in `evaluation/configs/summarize_justifications_eval.yml`:

```yaml
eval:
  general:
    dataset:
      _type: json
      file_path: /Users/gziv/Dev/sast-ai-workflow/evaluation/dataset/summarize_eval/summarize_eval_dataset.json
      structure:
        question_key: "question"
        answer_key: "answer_description"
```

**Update the `file_path`** to match your local system's absolute path, or use a path relative to the project root.

### Remote Dataset (S3)
To use a dataset stored in Amazon S3:

```yaml
eval:
  general:
    dataset:
      _type: json
      file_path: s3://your-bucket-name/evaluation/datasets/summarize_eval_dataset.json
      structure:
        question_key: "question"
        answer_key: "answer_description"
```

**Requirements for S3:**
- Set AWS credentials via environment variables:
  ```bash
  export AWS_ACCESS_KEY_ID=your_access_key
  export AWS_SECRET_ACCESS_KEY=your_secret_key
  export AWS_DEFAULT_REGION=us-east-1  # Optional
  ```
- OR configure `~/.aws/credentials` file
- OR use IAM roles if running on AWS infrastructure
- Ensure the S3 bucket has appropriate read permissions

### Remote Dataset (HTTP/HTTPS)
To use a dataset from a web server:

```yaml
eval:
  general:
    dataset:
      _type: json
      file_path: https://example.com/datasets/summarize_eval_dataset.json
      structure:
        question_key: "question"
        answer_key: "answer_description"
```

**Note:** The URL must be publicly accessible or properly authenticated via NAT configuration.

### Dataset Path Best Practices
1. **Use absolute paths** for local datasets to avoid path resolution issues
2. **Test accessibility** before running evaluations (especially for remote URLs)
3. **Keep datasets version-controlled** or in a reliable storage location
4. **Update the config file** when changing dataset locations
5. **Use constants** from `evaluation/constants.py` for standard dataset locations in code

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