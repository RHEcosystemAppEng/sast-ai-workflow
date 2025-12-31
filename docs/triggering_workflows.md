# Triggering SAST AI Workflows

Quick reference guide for triggering SAST AI workflows using the orchestrator API.

## When to Use Which Endpoint

| Endpoint | Use Case | Input Source | Batch Processing |
|----------|----------|--------------|------------------|
| `/api/v1/jobs/simple` | Dev/Prod single package analysis | Google Sheets | No |
| `/api/v1/mlops-batch` | MLOps evaluation with DVC | S3/MinIO via DVC | Yes (all packages in DVC version) |

---

## `/api/v1/jobs/simple` - Dev/Prod

### Prerequisites

1. **Orchestrator deployed** and accessible
2. **Pipeline deployed** in target namespace
3. **Secrets configured**:
   - `sast-ai-default-llm-creds` (LLM API credentials)
   - `sast-ai-google-service-account` (Google service account JSON for Sheets input + GCS uploads)

   Create from service account JSON file:
   ```bash
   kubectl create secret generic sast-ai-google-service-account \
     --from-file=service_account.json=/path/to/your/service_account.json \
     -n <namespace>
   ```

4. **Pipeline defaults set** (if using GCS uploads):
   - `GCS_BUCKET_NAME` - GCS bucket for SARIF reports

   Set pipeline default:
   ```bash
   kubectl get pipeline sast-ai-workflow-pipeline -n <namespace> -o json | \
     jq '.spec.params |= map(if .name == "GCS_BUCKET_NAME" then .default = "your-gcs-bucket" else . end)' | \
     kubectl apply -f -
   ```

5. **Google Sheet** prepared with SAST report data

### Request Format

```bash
curl -X POST 'https://<orchestrator-url>/api/v1/jobs/simple' \
  -H 'Content-Type: application/json' \
  -d '{
    "projectName": "systemd-project",
    "projectVersion": "257-9.el10",
    "packageName": "systemd",
    "packageNvr": "systemd-257-9.el10",
    "packageSourceCodeUrl": "https://download.devel.redhat.com/brewroot/vol/rhel-10/packages/systemd/257/9.el10/src/systemd-257-9.el10.src.rpm",
    "knownFalsePositivesUrl": "https://gitlab.cee.redhat.com/osh/known-false-positives/-/raw/master/systemd/ignore.err",
    "inputSourceUrl": "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID",
    "useKnownFalsePositiveFile": true,
    "workflowSettings": {
      "llmModelName": "nvidia/llama-3.1-nemotron-70b-instruct",
      "embeddingsLlmModelName": "sentence-transformers/all-mpnet-base-v2",
      "secretName": "sast-ai-default-llm-creds"
    }
  }'
```

### Required Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `projectName` | string | Human-readable project name |
| `projectVersion` | string | Project version |
| `packageName` | string | Package name |
| `packageNvr` | string | Package NVR (Name-Version-Release) |
| `packageSourceCodeUrl` | string | URL to source RPM or Git repository |
| `knownFalsePositivesUrl` | string | URL to known false positives file |
| `inputSourceUrl` | string | Google Sheets URL with SAST report data |
| `useKnownFalsePositiveFile` | boolean | Whether to filter using known false positives |

### Optional Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `workflowSettings.llmModelName` | string | LLM model name (uses default if not provided) |
| `workflowSettings.embeddingsLlmModelName` | string | Embeddings model name |
| `workflowSettings.secretName` | string | Kubernetes secret name for LLM credentials |

---

## `/api/v1/mlops-batch` - MLOps Evaluation

### Prerequisites

1. **Orchestrator deployed** in MLOps namespace
2. **Pipeline deployed** in MLOps namespace
3. **Secrets configured**:
   - `sast-ai-default-llm-creds` (LLM API credentials)
   - `sast-ai-s3-input-creds` (S3/MinIO credentials for DVC data)
   - `sast-ai-s3-output-credentials` (S3 credentials for results upload)

   Create S3 secrets:
   ```bash
   # S3 input credentials (for DVC data)
   kubectl create secret generic sast-ai-s3-input-creds \
     --from-literal=AWS_ACCESS_KEY_ID=your-access-key \
     --from-literal=AWS_SECRET_ACCESS_KEY=your-secret-key \
     --from-literal=S3_ENDPOINT_URL=https://minio.example.com \
     -n <namespace>

   # S3 output credentials (for results upload)
   kubectl create secret generic sast-ai-s3-output-credentials \
     --from-literal=AWS_ACCESS_KEY_ID=your-access-key \
     --from-literal=AWS_SECRET_ACCESS_KEY=your-secret-key \
     -n <namespace>
   ```

4. **DVC service running** and accessible
5. **Pipeline defaults set**:
   - `S3_OUTPUT_BUCKET_NAME` - S3 bucket for results upload

   Set pipeline default:
   ```bash
   kubectl get pipeline sast-ai-workflow-pipeline -n <namespace> -o json | \
     jq '.spec.params |= map(if .name == "S3_OUTPUT_BUCKET_NAME" then .default = "your-s3-bucket" else . end)' | \
     kubectl apply -f -
   ```

6. **DVC data prepared** in MinIO/S3:
   - Testing data (NVR CSV files with package metadata)
   - Prompts files
   - Known non-issues (false positives) files
7. **MinIO file structure** for each package:
   ```
   s3://bucket/{dvc-version}/{package-nvr}/
   ├── input.csv                    # SAST report in CSV format
   ├── known_non_issues.txt         # False positives file
   └── metadata.json                # Package metadata (source URL, etc.)
   ```

### Request Format

```bash
curl -X POST 'https://<orchestrator-url>/api/v1/mlops-batch' \
  -H 'Content-Type: application/json' \
  -d '{
    "submittedBy": "mlops-evaluation-pipeline",
    "testingDataNvrsVersion": "v2.0.2",
    "promptsVersion": "v1.0.0",
    "knownNonIssuesVersion": "v1.0.0",
    "sastAiImage": "quay.io/ecosystem-appeng/sast-ai-workflow:latest",
    "jobSettings": {
      "useKnownFalsePositiveFile": true
    }
  }'
```

### Required Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `submittedBy` | string | Identifier of requester (for tracking) |
| `testingDataNvrsVersion` | string | DVC data version tag (e.g., `v2.0.2`) |
| `promptsVersion` | string | DVC prompts version tag |
| `knownNonIssuesVersion` | string | DVC known non-issues version tag |
| `sastAiImage` | string | Full container image path for workflow |

### Optional Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `jobSettings.useKnownFalsePositiveFile` | boolean | Whether to use known false positives (default: true) |


**Note:** Creates one PipelineRun per package in the DVC version. For example, `v2.0.2` with 23 packages creates 23 PipelineRuns.

---

## Monitoring

### List PipelineRuns
```bash
kubectl get pipelinerun -n <namespace>
```

### View Logs
```bash
tkn pipelinerun logs <name> -f -n <namespace>
```

### Check Status
```bash
kubectl get pipelinerun <name> -n <namespace> -o jsonpath='{.status.conditions[0].reason}'
```

---

## Common Issues

### "CouldntGetSecret" or "CouldntGetPipelineResult"
**Cause:** Required secret missing or pipeline not deployed
**Fix:** Verify all required secrets exist and pipeline is deployed in the namespace

### "Source code URL is invalid"
**Cause:** Package URL returns 404 or is unreachable
**Fix:** Verify URL is accessible from cluster

### "Skipping S3/GCS upload"
**Cause:** Bucket name not configured as pipeline default
**Fix:** Set pipeline default parameters:

For S3 uploads:
```bash
kubectl get pipeline sast-ai-workflow-pipeline -n <namespace> -o json | \
  jq '.spec.params |= map(if .name == "S3_OUTPUT_BUCKET_NAME" then .default = "your-bucket" else . end)' | \
  kubectl apply -f -
```

For GCS uploads (dev/prod only):
```bash
kubectl get pipeline sast-ai-workflow-pipeline -n <namespace> -o json | \
  jq '.spec.params |= map(if .name == "GCS_BUCKET_NAME" then .default = "your-gcs-bucket" else . end)' | \
  kubectl apply -f -
```