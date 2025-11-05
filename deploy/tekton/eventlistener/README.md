# Tekton EventListener for MLOps Benchmarking

This directory contains a Tekton EventListener implementation that triggers the sast-ai-orchestrator MLOps batch API via webhook. This enables automated MLOps performance testing and benchmarking with DVC data versioning.

## 🎯 Purpose

Enable MLOps benchmark testing for batch SAST analysis jobs:
- ✅ Webhook-based triggering (curl/HTTP POST)
- ✅ Integration with sast-ai-orchestrator MLOps API (`/api/v1/mlops-batch`)
- ✅ DVC data versioning support
- ✅ Container image version testing
- ✅ Separation from production workflows
- ✅ Fork-friendly configuration

## 📁 Directory Contents

```
eventlistener/
├── README.md                        # This file
├── kustomization.yaml               # Kustomize configuration
├── benchmark-config.yaml.template   # ConfigMap template
├── benchmark-config.yaml            # Generated ConfigMap (git-ignored)
├── call-orchestrator-api.yaml       # Task that calls orchestrator MLOps API
├── poll-batch-status.yaml           # Task that monitors batch completion
├── benchmark-pipeline.yaml          # MLOps benchmark pipeline
├── eventlistener.yaml               # EventListener + Service
├── triggerbinding.yaml              # Extracts webhook parameters (including MLOps params)
├── triggertemplate.yaml             # Generates PipelineRuns
└── test-eventlistener.sh            # Helper script for testing
```

**Note:** `benchmark-config.yaml` is automatically generated from `benchmark-config.yaml.template` when you run `make eventlistener` and is git-ignored.

## 📋 Prerequisites

- OpenShift/Kubernetes cluster with Tekton Pipelines installed
- `oc` or `kubectl` CLI tool
- `curl` for sending test requests
- (Optional) `tkn` CLI for easier pipeline management
- (Optional) `jq` for JSON parsing

Check Tekton installation:
```bash
oc get pods -n openshift-pipelines
# or
kubectl get pods -n tekton-pipelines
```

## 🚀 Quick Start

### Step 1: Deploy MLOps Pipeline

First, ensure you have the MLOps pipeline deployed:

```bash
cd deploy
make tasks ENV=mlop NAMESPACE=your-namespace
```

### Step 2: Deploy EventListener

Deploy the EventListener (uses defaults for both namespace and URL):

```bash
cd deploy
make eventlistener
```

**Default Configuration:**
- Namespace: Auto-detected from current `oc` context
- Orchestrator URL: `http://sast-ai-orchestrator.<namespace>.svc.cluster.local:80`
- Uses existing orchestrator service (matches Helm deployment)
- Uses automatic K8s service discovery
- No manual configuration needed

**Override Options:**
```bash
# Override namespace only
make eventlistener NAMESPACE=custom-namespace

# Override orchestrator URL only
make eventlistener ORCHESTRATOR_API_URL=http://custom-service.sast-ai.svc.cluster.local:8080

# Override both
make eventlistener \
  ORCHESTRATOR_API_URL=http://custom-service.custom-ns.svc.cluster.local:8080 \
  NAMESPACE=custom-ns
```

**Parameters:**
- `NAMESPACE` - Target namespace (optional, auto-detected from current context)
- `ORCHESTRATOR_API_URL` - Orchestrator service URL (optional, uses K8s service DNS default)

### Step 3: Verify Orchestrator Service

The workflow uses the orchestrator's existing Helm service.

**Quick Verification:**
```bash
# Verify orchestrator service exists
oc get svc sast-ai-orchestrator -n your-namespace

**Expected Service Configuration:**
- **Name**: `sast-ai-orchestrator` (from orchestrator's Helm chart)
- **Port**: 80 (maps to targetPort 8080)
- **Type**: ClusterIP
- **Endpoints**: Should show pod IP:8080

**What happens:**
- ✅ Validates required parameters
- ✅ Generates `benchmark-config.yaml` with orchestrator URL and API endpoint
- ✅ Deploys all EventListener resources via Kustomize
- ✅ Shows verification and testing commands

**Note:** The Google Sheet URL is provided via the webhook payload when triggering the EventListener, not during deployment.

**Note:** The EventListener always calls `/api/v1/mlops-batch` endpoint (hardcoded for MLOps benchmarking).

Verify deployment:
```bash
oc get eventlistener,task,pipeline,cm -l app.kubernetes.io/component=benchmark-mlop -n your-namespace
```

### Step 4: Test the EventListener

**Option A: Manual testing**

1. Port-forward to the EventListener service (for testing from outside the cluster):
```bash
oc port-forward svc/el-benchmark-mlop-listener 8080:8080 -n your-namespace
```

**Note:** Port-forwarding is **only needed for external testing** (e.g., from your local machine). The EventListener service is already accessible within the cluster at:
```
http://el-benchmark-mlop-listener.<namespace>.svc.cluster.local:8080
```

2. In another terminal, send a test request from your local machine:
```bash
curl -X POST http://localhost:8080 \
  -H 'Content-Type: application/json' \
  -d '{
    "submitted_by": "manual-test",
    "image_version": "v2.1.0",
    "dvc_nvr_version": "v1.0.0",
    "dvc_known_false_positives_version": "v1.0.0",
    "dvc_prompts_version": "v1.0.0"
  }'
```

**Optional:** Test with custom container image:
```bash
curl -X POST http://localhost:8080 \
  -H 'Content-Type: application/json' \
  -d '{
    "submitted_by": "version-test",
    "image_version": "v2.1.0",
    "dvc_nvr_version": "v1.0.0",
    "dvc_known_false_positives_version": "v1.0.0",
    "dvc_prompts_version": "v1.0.0"
  }'
```

3. Watch the PipelineRun:
```bash
# With tkn CLI
tkn pipelinerun logs -L -f

# With kubectl/oc
oc get pipelinerun -l app.kubernetes.io/component=benchmark-mlop
oc logs -l tekton.dev/pipelineTask=call-orchestrator-api -f
```

## 📊 Expected Results

### Successful Test

When everything works correctly, you should see:

1. **EventListener Response** (HTTP 201):
```json
{
  "eventListener": "benchmark-mlop-listener",
  "namespace": "your-namespace",
  "eventID": "abc123..."
}
```

2. **PipelineRun Created**:
```bash
$ oc get pipelinerun -l app.kubernetes.io/component=benchmark-mlop
NAME                              SUCCEEDED   REASON      STARTTIME   COMPLETIONTIME
benchmark-mlop-pipeline-abc123    True        Succeeded   5m          2m
```

3. **Task Logs Show API Call**:
```
=========================================
Calling Orchestrator MLOps Batch API
=========================================
Configuration:
  Orchestrator URL: http://sast-ai-orchestrator...
  API Endpoint: /api/v1/mlops-batch (MLOps benchmarking)
  Batch Sheet URL: https://docs.google.com/...
  DVC Repo: https://gitlab.com/...
  S3 Bucket: mlops-test-data
  ...
✓ API call successful!
Batch ID: batch-12345

Polling batch status...
✓ Batch completed successfully!
```

### Troubleshooting

#### EventListener Pod Not Running

```bash
# Check pod status
oc get pods -l eventlistener=benchmark-mlop-listener

# Check pod logs
oc logs -l eventlistener=benchmark-mlop-listener
```

**Common issues:**
- Service account `pipeline` doesn't exist (create with Tekton operator)
- RBAC permissions missing

#### API Call Fails

Check task logs for detailed error:
```bash
oc logs -l tekton.dev/pipelineTask=call-orchestrator-api --tail=100
```

**Common issues:**
- Orchestrator URL incorrect in ConfigMap
- Orchestrator service not running: `oc get pods -l app=sast-ai-orchestrator`
- Network policy blocking connections
- DVC version parameters not provided in webhook payload

#### Verify ConfigMap

```bash
# View current configuration
oc get configmap benchmark-config -o yaml -n your-namespace

# Update if needed - regenerate (uses current namespace by default)
cd deploy
make eventlistener

# Or override namespace
make eventlistener NAMESPACE=your-namespace

# Or with custom orchestrator URL
make eventlistener \
  ORCHESTRATOR_API_URL=http://custom-orchestrator-service.your-namespace.svc.cluster.local:8080
```

## 🔧 Configuration Reference

### Webhook Payload Format

Send JSON payload with these fields:

```json
{
  "submitted_by": "trigger-source",
  "dvc_nvr_version": "v1.2.3",
  "dvc_known_false_positives_version": "v1.2.3",
  "dvc_prompts_version": "v1.2.3",
  "image_version": "v2.0.0"
}
```

**Required Fields:**
- `dvc_nvr_version` - DVC NVR resource version
- `dvc_prompts_version` - DVC prompts resource version
- `dvc_known_false_positives_version` - DVC known false positives resource version

**Optional Fields:**
- `submitted_by` - Defaults to "eventlistener-webhook"
- `image_version` - Defaults to "latest" (e.g., "v2.1.0", "sha-abc123")

### ConfigMap Keys

The `benchmark-config` ConfigMap is automatically generated by `make eventlistener`:

| Key | Description | Example |
|-----|-------------|---------|
| `orchestrator-api-url` | Base URL of orchestrator service | `http://sast-ai-orchestrator.<namespace>.svc.cluster.local:80` |
| `api-batch-endpoint` | API endpoint path for MLOps batches | `/api/v1/mlop-batch` |

**Note:** The `api-batch-endpoint` is automatically set to `/api/v1/mlops-batch` for MLOps benchmarking.

**To regenerate:** Simply run `make eventlistener` again with updated parameters.

### Pipeline Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `dvc-nvr-version` | string | **Yes** | - | DVC NVR resource version |
| `dvc-prompts-version` | string | **Yes** | - | DVC prompts resource version |
| `dvc-known-false-positives-version` | string | **Yes** | - | DVC known false positives resource version |
| `submitted-by` | string | No | `eventlistener-webhook` | Trigger source identifier |
| `dvc-data-version` | string | Yes | - | DVC data version tag |
| `prompts-version` | string | Yes | - | Prompts version tag |
| `known-non-issues-version` | string | Yes | - | Known non-issues version tag |
| `image-version` | string | No | (default from pipeline) | Workflow image version for testing (tag only, e.g., "v2.1.0") |
| `use-known-false-positive-file` | string | No | `true` | Whether to use known false positive file |

## 🎓 Understanding the Architecture

### Flow Diagram

```
┌─────────┐
│  curl   │ POST JSON payload
│ webhook │────────────────────┐
└─────────┘                    │
                               ▼
                    ┌──────────────────────┐
                    │   EventListener      │
                    │  (benchmark-mlop)    │
                    └──────────┬───────────┘
                               │ Creates
                               ▼
                    ┌──────────────────────┐
                    │   PipelineRun        │
                    │ (auto-generated name)│
                    └──────────┬───────────┘
                               │ Executes
                               ▼
                    ┌──────────────────────┐
                    │  Pipeline            │
                    │ (benchmark-mlop)     │
                    └──────────┬───────────┘
                               │ Runs Tasks
                               ▼
                    ┌──────────────────────┐
                    │  Task 1              │
                    │ (call-orchestrator)  │
                    └──────────┬───────────┘
                               │ Then
                               ▼
                    ┌──────────────────────┐
                    │  Task 2              │
                    │ (poll-batch-status)  │
                    └──────────┬───────────┘
                               │ Reads Config
                               ▼
                    ┌──────────────────────┐
                    │   ConfigMap          │
                    │ (benchmark-config)   │
                    └──────────┬───────────┘
                               │ Uses URL
                               ▼
                    ┌──────────────────────┐
                    │  Orchestrator API    │
                    │  POST /api/v1/       │
                    │  mlops-batch       │
                    └──────────────────────┘
```

### Component Responsibilities

1. **EventListener**: Accepts webhook (exposed as Kubernetes Service), validates request, triggers pipeline
   - Service name: `el-benchmark-mlop-listener`
   - Internal cluster access: `http://el-benchmark-mlop-listener.<namespace>.svc.cluster.local:8080`
   - External testing: Use `oc port-forward` (see testing section)
2. **TriggerBinding**: Extracts parameters from webhook JSON payload (including MLOps params)
3. **TriggerTemplate**: Generates PipelineRun with extracted parameters
4. **Pipeline**: Orchestrates task execution, monitors completion, handles results
5. **Task 1 (call-orchestrator-api)**: Calls orchestrator MLOps API with DVC version params
6. **Task 2 (poll-batch-status)**: Monitors batch completion until done or timeout
7. **ConfigMap**: Stores environment-specific configuration (orchestrator URL, API endpoint)

## 🔄 Production Enhancements

For production use, consider:

### Automation

1. **Create CronJob** for scheduled benchmarking
2. **Set up monitoring** (Prometheus metrics)
3. **Configure notifications** (Slack/email on completion/failure)
4. **Add retry logic** for transient failures

### Production Deployment

Deploy to dedicated namespace:

```bash
# Create and switch to namespace
oc new-project sast-ai-benchmark

# Deploy MLOps pipeline overlay (uses current namespace)
cd deploy
make tasks ENV=mlop

# Deploy EventListener (auto-detects namespace from context)
make eventlistener

# Verify orchestrator service exists (from orchestrator's Helm deployment)
oc get svc sast-ai-orchestrator -n sast-ai-benchmark
```

**Note:** The default configuration auto-detects the current namespace and uses `http://sast-ai-orchestrator.<namespace>.svc.cluster.local:80` (matches the orchestrator's existing Helm service).

This creates both:
- The `mlop-sast-ai-workflow-pipeline` that the orchestrator will trigger
- The EventListener webhook endpoint for triggering benchmarks

## 🧹 Cleanup

To remove all MLOps benchmark resources:

```bash
# From deploy directory - Recommended
cd deploy
make eventlistener-clean NAMESPACE=your-namespace

# Or manual cleanup
oc delete -k deploy/tekton/eventlistener/ -n your-namespace

# Or individually
oc delete eventlistener benchmark-mlop-listener -n your-namespace
oc delete pipeline benchmark-mlop-pipeline -n your-namespace
oc delete task call-orchestrator-api-mlop poll-batch-status-mlop -n your-namespace
oc delete configmap benchmark-config -n your-namespace
oc delete service el-benchmark-mlop-listener -n your-namespace
```

## 📚 Additional Resources

- [Tekton Triggers Documentation](https://tekton.dev/docs/triggers/)
- [EventListener Guide](https://tekton.dev/docs/triggers/eventlisteners/)

## 🤝 For Project Forks

If you're using this project as a base for your own:

1. **Switch to your namespace and deploy** (auto-detects namespace):
   ```bash
   oc project <your-namespace>
   cd deploy
   make eventlistener
   ```
   
2. **Ensure orchestrator service** is deployed:
   ```bash
   oc get svc sast-ai-orchestrator -n <your-namespace>
   
   # Should show port 80 -> targetPort 8080
   # The workflow will use: http://sast-ai-orchestrator.<namespace>.svc.cluster.local:80
   ```

3. **Customize** labels and naming if needed (edit YAML files in `tekton/eventlistener/`)
4. **Test** with your orchestrator instance using `test-eventlistener.sh`
5. **Extend** pipeline with your specific requirements

All configuration is passed as parameters - no manual file editing needed!

## ❓ Questions or Issues?

- Check troubleshooting section above
- Review EventListener logs: `oc logs -l eventlistener=benchmark-mlop-listener`
- Review task logs: `oc logs -l tekton.dev/pipelineTask=call-orchestrator-api`
- Validate ConfigMap: `oc get cm benchmark-config -o yaml`
- Test orchestrator connectivity from a pod
