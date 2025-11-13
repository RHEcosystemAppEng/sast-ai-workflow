# SAST AI Workflow - Evaluation Framework

This directory contains the evaluation framework for testing and validating the SAST AI Workflow components.

## Table of Contents

- [Overview](#overview)
- [DVC Integration](#dvc-integration)
- [Dataset Configuration](#dataset-configuration)
  - [Local Datasets](#local-datasets-default---recommended)
  - [S3 Remote Datasets](#remote-datasets-s3)
  - [HTTP/HTTPS Remote Datasets](#remote-datasets-httphttps)
- [Dataset Structure](#dataset-structure-requirements)
- [Running Evaluations](#running-evaluations)
- [Additional Resources](#additional-resources)

## Overview

The evaluation framework provides comprehensive testing capabilities for the SAST AI Workflow:

### Evaluation Modes

1. **Individual Node Evaluations**: Test specific workflow components in isolation
   - **Filter Evaluation**: Tests the issue filtering logic
   - **Judge LLM Evaluation**: Tests the LLM-based analysis and judgment
   - **Summarize Evaluation**: Tests the summarization and justification generation

2. **Full Workflow Evaluation ("all")**: Runs the complete end-to-end SAST workflow
   - Executes all workflow nodes sequentially (filter → judge → summarize)
   - Generates comprehensive metrics for the entire pipeline
   - Produces final output with aggregated results
   - Used for integration testing and production validation

## DVC Integration

### What is DVC?

[DVC (Data Version Control)](https://dvc.org) is an open-source version control system for machine learning projects. It brings version control capabilities similar to Git, but designed specifically for large datasets, models, and other ML artifacts.

### Why We Use DVC

In the SAST AI Workflow project, DVC serves several critical purposes:

1. **Dataset Versioning**: Track changes to evaluation datasets over time without storing large files in Git
2. **Reproducibility**: Ensure evaluations can be reproduced with exact dataset versions
3. **Storage Efficiency**: Store large datasets in remote storage (S3/MinIO) while keeping Git repositories lightweight
4. **Team Collaboration**: Share datasets across team members and CI/CD pipelines consistently
5. **Dataset Lineage**: Track which dataset version was used for each evaluation run

### How DVC Works in This Project

The project uses DVC with S3-compatible storage (MinIO or AWS S3) to:

- Store ground truth datasets for evaluation
- Version control evaluation datasets alongside code changes
- Fetch specific dataset versions during Tekton pipeline runs
- Track dataset evolution as the project develops

Key DVC operations in the Tekton pipelines:

```bash
# Fetch a specific dataset version
dvc get <repo_url> <path> --rev <version> -o <output_path>

# List available datasets
dvc list <repo_url> <path> --rev <version>
```

For more information on DVC configuration in Tekton pipelines, see:
- `deploy/tekton/overlays/mlops/transform-report-patch.yaml` - Report fetching from DVC
- `deploy/tekton/overlays/mlops/fetch-false-positives-patch.yaml` - False positives fetching from DVC

## Dataset Configuration

Evaluation datasets can be configured in NAT config YAML files to use local filesystem, S3, or HTTP/HTTPS locations. The NAT framework automatically detects the location type based on the file_path prefix.

### Local Datasets (Default - Recommended)

Use relative paths from project root for datasets in the repository.

**Configuration:**

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

**When to use:**
- Default setup with datasets in the repo
- Local development and testing
- CI/CD pipelines with checked-out code
- Version-controlled datasets

**Path formats:**
- Relative (recommended): `evaluation/dataset/filter_eval/dataset.json`
- Absolute (if needed): `/full/path/to/sast-ai-workflow/evaluation/dataset/dataset.json`

**Verification:**

```bash
ls -l evaluation/dataset/filter_eval/dataset.json
```

No additional configuration needed - works out of the box.

### Remote Datasets (S3)

Use S3 URLs for datasets stored in Amazon S3 buckets or S3-compatible storage like MinIO.

**Configuration:**

```yaml
eval:
  general:
    dataset:
      _type: json
      file_path: s3://sast-ai-datasets/evaluation/filter_eval_dataset_individual_issues.json
      structure:
        question_key: "question"
        answer_key: "expected_output_obj"
```

**When to use:**
- Large datasets not suitable for git
- Shared datasets across teams
- Production deployments on AWS
- Datasets that change frequently
- DVC-managed datasets

**Setup requirements:**

1. Configure AWS credentials (choose one):

   Environment variables:
   ```bash
   export AWS_ACCESS_KEY_ID=AKIA...
   export AWS_SECRET_ACCESS_KEY=your_secret_key
   export AWS_DEFAULT_REGION=us-east-1  # Optional
   ```

   AWS credentials file (`~/.aws/credentials`):
   ```ini
   [default]
   aws_access_key_id = AKIA...
   aws_secret_access_key = your_secret_key
   ```

   IAM role (for EC2/ECS - no config needed)

2. Verify S3 access:
   ```bash
   aws s3 ls s3://sast-ai-datasets/evaluation/
   aws s3 cp s3://sast-ai-datasets/evaluation/dataset.json /tmp/test.json
   ```

3. Ensure bucket permissions:
   - Bucket must allow s3:GetObject permission
   - Check bucket policy or IAM user/role permissions

**Common issues:**
- **NoCredentialsError**: AWS credentials not configured
- **AccessDenied**: Check IAM permissions on bucket
- **NoSuchBucket**: Verify bucket name and region

### Remote Datasets (HTTP/HTTPS)

Use HTTP/HTTPS URLs for datasets hosted on web servers.

**Configuration:**

```yaml
eval:
  general:
    dataset:
      _type: json
      file_path: https://datasets.example.com/sast-ai/filter_eval_dataset.json
      structure:
        question_key: "question"
        answer_key: "expected_output_obj"
```

**When to use:**
- Publicly hosted datasets
- Datasets behind CDN
- Third-party dataset sources

**Requirements:**
- URL must be publicly accessible OR
- Configure authentication in NAT config (if required)
- HTTPS is recommended for security

**Verification:**

```bash
curl -I https://datasets.example.com/sast-ai/filter_eval_dataset.json
```

### Switching Between Locations

Simply change the file_path in your config YAML:

```yaml
# Local
file_path: evaluation/dataset/filter_eval/dataset.json

# S3
file_path: s3://sast-ai-datasets/evaluation/dataset.json

# HTTP
file_path: https://datasets.example.com/sast-ai/dataset.json
```

The NAT framework automatically detects and handles each location type.

## Dataset Structure Requirements

All evaluation datasets must follow the NAT format:
- **question_key**: Field containing input data
- **answer_key**: Field containing expected output

**Example dataset entry:**

```json
{
  "id": "test_case_1",
  "question": "{...input data...}",
  "expected_output_obj": {...expected output...}
}
```

## File Path Best Practices

1. Use relative paths in configs for portability across environments
2. Use constants from `evaluation.constants` for standard dataset locations in Python code
3. Ensure remote URLs/S3 buckets are accessible before running evaluations
4. Test dataset accessibility:
   - Local: `ls -l evaluation/dataset/filter_eval/dataset.json`
   - S3: `aws s3 ls s3://bucket/path/dataset.json`
   - HTTP: `curl -I https://url/dataset.json`
5. For S3, verify bucket permissions and credentials are properly configured

## Running Evaluations

Each evaluation component has its own runner script:

```bash
# Filter evaluation
export LLM_API_KEY=your_nvidia_api_key
python evaluation/runners/run_filter_evaluation.py

# Judge LLM evaluation
export LLM_API_KEY=your_nvidia_api_key
python evaluation/runners/run_judge_llm_evaluation.py

# Summarize evaluation
export LLM_API_KEY=your_nvidia_api_key
python evaluation/runners/run_summarize_evaluation.py
```

### Debug Mode

For PyCharm debugging with breakpoints:

```bash
python evaluation/runners/run_filter_evaluation.py --debug
```

### Custom Config Files

To use a custom config file:

```bash
python evaluation/runners/run_filter_evaluation.py path/to/custom_config.yml
```

## Additional Resources

See individual evaluation README files for detailed format specifications:

- [Filter Evaluation](README-filter.md)
- [Judge LLM Evaluation](README-judge-llm.md)
- [Summarize Evaluation](README-summarize.md)
- [Dataset Details](dataset/README.md)

## DVC Resources

- [DVC Documentation](https://dvc.org/doc)
- [DVC with S3](https://dvc.org/doc/user-guide/data-management/remote-storage/amazon-s3)
- [DVC Best Practices](https://dvc.org/doc/user-guide/best-practices)