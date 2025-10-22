# SAST-AI-Workflow Evaluation Datasets

This directory contains evaluation datasets for testing the SAST-AI-Workflow system's different components. Each dataset is structured to work with the NAT (Neural Architecture for Testing) evaluation framework.

## Directory Structure

```
evaluation/dataset/
├── filter_eval/
│   ├── filter_eval_dataset.json                      # Original filter evaluation dataset
│   └── filter_eval_dataset_individual_issues.json    # Individual issues filter dataset (default)
├── judge_llm_eval/
│   ├── judge_llm_eval_dataset_6.json                 # 6 test cases (default, quick testing)
│   ├── judge_llm_eval_dataset_12.json                # 12 test cases (medium)
│   ├── judge_llm_eval_dataset_24.json                # 24 test cases (comprehensive)
│   ├── judge_llm_eval_dataset_36.json                # 36 test cases (extended)
│   └── judge_llm_eval_dataset_full.json              # Full dataset (all test cases)
└── summarize_eval/
    └── summarize_eval_dataset.json                   # Summarization evaluation dataset
```

## Dataset Types

### 1. Filter Evaluation Datasets

**Purpose**: Evaluate the filter node's ability to classify SAST findings as TRUE_POSITIVE or FALSE_POSITIVE.

**Location**: `filter_eval/`

**Default Dataset**: `filter_eval_dataset_individual_issues.json`

**JSON Structure**:
```json
[
  {
    "id": "unique_test_case_id",
    "question": "{...SAST issue JSON with id, issue_type, severity, trace, file_path, line_number, cwe_id...}",
    "expected_output_obj": {
      "filter_result": "TRUE_POSITIVE" | "FALSE_POSITIVE",
      "confidence": null | float,
      "similar_known_issues": ["file_path_1", "file_path_2"],
      "justification": "Awaiting filter analysis"
    },
    "generated_answer": "",
    "intermediate_steps": [],
    "expected_intermediate_steps": []
  }
]
```

**Key Fields**:
- `id`: Unique identifier for the test case
- `question`: Stringified JSON containing SAST issue details
- `expected_output_obj.filter_result`: Expected classification (TRUE_POSITIVE or FALSE_POSITIVE)
- `expected_output_obj.similar_known_issues`: List of similar known issues found
- `generated_answer`: Populated during evaluation with the system's output
- `intermediate_steps`: Populated during evaluation with execution traces

**Usage**: Tests the filter's ability to correctly classify security issues using RAG-based similarity matching against known false positives.

---

### 2. Judge LLM Evaluation Datasets

**Purpose**: Evaluate the Judge LLM's ability to analyze security vulnerabilities and provide detailed justifications.

**Location**: `judge_llm_eval/`

**Default Dataset**: `judge_llm_eval_dataset_6.json` (6 test cases for quick testing)

**Note**: Multiple dataset sizes are available (6, 12, 24, 36, and full) for different testing needs. The 6-case dataset is the default for rapid iteration.

**JSON Structure**:
```json
[
  {
    "id": "unique_test_case_id",
    "question": "{...SAST issue details with source code context and examples...}",
    "expected_investigation_result": "TRUE POSITIVE" | "FALSE POSITIVE",
    "generated_answer": "",
    "intermediate_steps": [],
    "expected_intermediate_steps": []
  }
]
```

**Key Fields**:
- `id`: Unique identifier for the test case
- `question`: Stringified JSON with detailed SAST issue information including:
  - `issue_name`: Type of security issue (e.g., INTEGER_OVERFLOW, USE_AFTER_FREE)
  - `error_description`: Detailed trace of the vulnerability
  - `source_code_context`: Relevant source code showing the issue
  - Examples of known false positives or true positives for reference
- `expected_investigation_result`: Expected analysis result (TRUE POSITIVE or FALSE POSITIVE)
- `generated_answer`: Populated during evaluation
- `intermediate_steps`: Populated during evaluation

**Usage**: Tests the Judge LLM's ability to perform deep security analysis, considering code context, vulnerability patterns, and providing clear justifications.

---

### 3. Summarization Evaluation Datasets

**Purpose**: Evaluate the summarization function's ability to generate concise, accurate summaries of security justifications.

**Location**: `summarize_eval/`

**Default Dataset**: `summarize_eval_dataset.json`

**JSON Structure**:
```json
[
  {
    "id": "unique_test_case_id",
    "question": "security_justification_text_to_summarize",
    "full_justification": "detailed_justification",
    "investigation_result": "TRUE POSITIVE" | "FALSE POSITIVE",
    "issue_type": "SECURITY_ISSUE",
    "severity": "HIGH" | "MEDIUM" | "LOW",
    "source_file": "source_file.xlsx",
    "is_valid": true | false,
    "expected_output_obj": "expected_summary_text"
  }
]
```

**Key Fields**:
- `id`: Unique identifier for the test case
- `question`: Input text to summarize (may be the same as id)
- `full_justification`: Complete justification text with detailed analysis
- `investigation_result`: Analysis conclusion
- `expected_output_obj`: Expected summary output
- `generated_answer`: Populated during evaluation

**Usage**: Tests the summarization capability to condense detailed security analysis into concise, actionable summaries while preserving key technical details.

---

## Configuration Reference

Each evaluation type has a corresponding configuration file that specifies which dataset to use:

### Filter Evaluation Config
**File**: `evaluation/configs/filter_eval.yml`
**Dataset Path**:
```yaml
eval:
  general:
    dataset:
      _type: json
      file_path: evaluation/dataset/filter_eval/filter_eval_dataset_individual_issues.json
      structure:
        question_key: "question"
        answer_key: "expected_output_obj"
```

### Judge LLM Evaluation Config
**File**: `evaluation/configs/judge_llm_analysis_eval.yml`
**Dataset Path**:
```yaml
eval:
  general:
    dataset:
      _type: json
      file_path: evaluation/dataset/judge_llm_eval/judge_llm_eval_dataset_6.json
      structure:
        question_key: "question"
        answer_key: "expected_investigation_result"
```

### Summarization Evaluation Config
**File**: `evaluation/configs/summarize_justifications_eval.yml`
**Dataset Path**:
```yaml
eval:
  general:
    dataset:
      _type: json
      file_path: evaluation/dataset/summarize_eval/summarize_eval_dataset.json
      structure:
        question_key: "question"
        answer_key: "expected_output_obj"
```

---

## Using Different Dataset Locations

The NAT framework supports loading datasets from three locations: local filesystem, S3, and HTTP/HTTPS. You control this by changing the `file_path` value in your config YAML file.

### Local Datasets (Default - Recommended)

**Use relative paths from the project root** for datasets stored locally in the repository:

```yaml
# In evaluation/configs/filter_eval.yml
eval:
  general:
    dataset:
      _type: json
      file_path: evaluation/dataset/filter_eval/filter_eval_dataset_individual_issues.json
      structure:
        question_key: "question"
        answer_key: "expected_output_obj"
```

**When to use**:
- ✅ Default setup - datasets already in the repo
- ✅ Local development and testing
- ✅ CI/CD pipelines with checked-out code
- ✅ Best for version-controlled datasets

**Path format**:
- **Relative path** (recommended): `evaluation/dataset/filter_eval/dataset.json`
- **Absolute path** (if needed): `/full/path/to/sast-ai-workflow/evaluation/dataset/filter_eval/dataset.json`

**Best Practices**:
- Use relative paths (from project root) for portability across environments
- Verify the dataset file exists: `ls -l evaluation/dataset/filter_eval/filter_eval_dataset_individual_issues.json`
- No additional configuration needed - works out of the box

---

### Remote Datasets (S3)

**Use S3 URLs** for datasets stored in Amazon S3 buckets:

```yaml
# In evaluation/configs/filter_eval.yml
eval:
  general:
    dataset:
      _type: json
      file_path: s3://sast-ai-datasets/evaluation/filter_eval_dataset_individual_issues.json
      structure:
        question_key: "question"
        answer_key: "expected_output_obj"
```

**When to use**:
- ✅ Large datasets not suitable for git
- ✅ Shared datasets across teams
- ✅ Production deployments on AWS
- ✅ Datasets that change frequently

**Path format**:
- `s3://bucket-name/path/to/dataset.json`
- Example: `s3://sast-ai-datasets/evaluation/judge_llm_eval_dataset_6.json`

**Setup Requirements**:

1. **Configure AWS credentials** (choose one method):

   **Option A: Environment variables** (recommended for CI/CD):
   ```bash
   export AWS_ACCESS_KEY_ID=AKIA...
   export AWS_SECRET_ACCESS_KEY=your_secret_key
   export AWS_DEFAULT_REGION=us-east-1  # Optional
   ```

   **Option B: AWS credentials file** (recommended for local development):
   ```bash
   # Create/edit ~/.aws/credentials
   [default]
   aws_access_key_id = AKIA...
   aws_secret_access_key = your_secret_key

   # Create/edit ~/.aws/config
   [default]
   region = us-east-1
   ```

   **Option C: IAM roles** (recommended for AWS EC2/ECS):
   - Attach IAM role to your EC2 instance or ECS task
   - No credentials needed in config

2. **Verify S3 access**:
   ```bash
   # Test access to your bucket
   aws s3 ls s3://sast-ai-datasets/evaluation/

   # Download a file to verify permissions
   aws s3 cp s3://sast-ai-datasets/evaluation/filter_eval_dataset_individual_issues.json /tmp/test.json
   ```

3. **Ensure S3 bucket permissions**:
   - Bucket must allow `s3:GetObject` permission
   - Check bucket policy or IAM user/role permissions

**Common Issues**:
- ❌ `NoCredentialsError` → AWS credentials not configured
- ❌ `AccessDenied` → Check IAM permissions on bucket
- ❌ `NoSuchBucket` → Verify bucket name and region

---

### Remote Datasets (HTTP/HTTPS)

**Use HTTP/HTTPS URLs** for datasets hosted on web servers:

```yaml
# In evaluation/configs/filter_eval.yml
eval:
  general:
    dataset:
      _type: json
      file_path: https://datasets.example.com/sast-ai/filter_eval_dataset_individual_issues.json
      structure:
        question_key: "question"
        answer_key: "expected_output_obj"
```

**When to use**:
- ✅ Publicly hosted datasets
- ✅ Datasets behind CDN
- ✅ Third-party dataset sources

**Path format**:
- `https://domain.com/path/to/dataset.json`
- `http://internal-server/datasets/dataset.json`

**Requirements**:
- URL must be publicly accessible OR
- Configure authentication in NAT config (if required)
- HTTPS is recommended for security

**Verification**:
```bash
# Test URL accessibility
curl -I https://datasets.example.com/sast-ai/filter_eval_dataset_individual_issues.json

# Download to verify
curl -o /tmp/test.json https://datasets.example.com/sast-ai/filter_eval_dataset_individual_issues.json
```

---

### Switching Between Dataset Locations

To switch between local and S3, simply change the `file_path` in your config:

**Local (current default)**:
```yaml
file_path: evaluation/dataset/filter_eval/filter_eval_dataset_individual_issues.json
```

**Switch to S3**:
```yaml
file_path: s3://sast-ai-datasets/evaluation/filter_eval_dataset_individual_issues.json
```

**Switch to HTTP**:
```yaml
file_path: https://datasets.example.com/sast-ai/filter_eval_dataset_individual_issues.json
```

The NAT framework automatically detects the location type based on the path prefix (`s3://`, `https://`, or filesystem path).

---

## Dataset Constants

Standard dataset paths are defined in `evaluation/constants.py`:

```python
# Dataset directories
DATASET_BASE_DIR = "evaluation/dataset"
DATASET_FILTER_DIR = "evaluation/dataset/filter_eval"
DATASET_JUDGE_LLM_DIR = "evaluation/dataset/judge_llm_eval"
DATASET_SUMMARIZATION_DIR = "evaluation/dataset/summarize_eval"

# Dataset files
FILTER_DATASET_FILENAME = "filter_eval_dataset_individual_issues.json"
JUDGE_LLM_DATASET_FILENAME = "judge_llm_eval_dataset_6.json"
SUMMARIZATION_DATASET_FILENAME = "summarize_eval_dataset.json"
```

Use these constants in code to reference standard dataset locations:

```python
from evaluation.constants import DATASET_FILTER_DIR, FILTER_DATASET_FILENAME
import os

dataset_path = os.path.join(DATASET_FILTER_DIR, FILTER_DATASET_FILENAME)
```

---

## Common Dataset Fields

All datasets follow NAT conventions and include these standard fields:

- **`id`**: Unique identifier for each test case
- **`question`**: Input data (format varies by evaluation type)
- **`expected_output_obj`** or **`expected_investigation_result`**: Expected output for comparison
- **`generated_answer`**: Field populated during evaluation with system output
- **`intermediate_steps`**: Populated during evaluation with execution trace
- **`expected_intermediate_steps`**: Expected intermediate steps (optional)

---

## Dataset Validation

Before running evaluations, ensure:

1. **File exists**: Verify the dataset file exists at the configured path
2. **Valid JSON**: Dataset is properly formatted JSON
3. **Required fields**: All required fields are present in each test case
4. **Accessibility**: Remote datasets (S3/HTTP) are accessible
5. **Structure matches config**: `question_key` and `answer_key` match the dataset structure

---

## Creating New Datasets

When creating new evaluation datasets:

1. Follow the JSON structure for the appropriate evaluation type
2. Include all required fields (`id`, `question`, expected output field)
3. Use descriptive IDs that indicate the test case purpose
4. Provide realistic SAST issue data with proper context
5. Include diverse test cases covering edge cases and common scenarios
6. Validate JSON formatting before use
7. Add the dataset to the appropriate subdirectory
8. Update configuration files to reference the new dataset

---

## See Also

- **Filter Evaluation**: `evaluation/README-filter.md`
- **Judge LLM Evaluation**: `evaluation/README-judge-llm.md`
- **Summarization Evaluation**: `evaluation/README-summarize.md`
- **Configuration Guide**: `evaluation/configs/`
- **Constants Reference**: `evaluation/constants.py`
- **Base Runner Documentation**: `evaluation/runners/base_runner.py`