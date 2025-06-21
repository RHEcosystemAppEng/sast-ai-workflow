## Deploying the SAST AI Workflow

This guide covers deployment on a local OpenShift cluster using CodeReady Containers (CRC) or an existing OpenShift cluster.

### 1. Install CRC (Local Development)

**For existing OpenShift clusters, skip to step 2.**

1. Download CRC from: https://developers.redhat.com/products/openshift-local/overview
2. Install and setup:
   ```bash
   crc setup
   crc config set disk-size 100  # Minimum 60GB recommended
   crc start
   crc status  # Verify installation
   ```

### 2. Install OpenShift Pipelines Operator

Install the OpenShift Pipelines operator from OperatorHub in the OpenShift console.

```bash
oc whoami --show-console  # Get console URL
```

### 3. Setup Environment and Secrets

#### 3.1. Create .env File

Create a `.env` file in the project root:

```env
# Required credentials
GITLAB_TOKEN=your_gitlab_token_here
LLM_API_KEY=your_llm_api_key_here
EMBEDDINGS_LLM_API_KEY=your_embeddings_api_key_here

# LLM Configuration  
LLM_URL=https://your-llm-endpoint.com/v1
EMBEDDINGS_LLM_URL=https://your-embeddings-endpoint.com/v1
LLM_MODEL_NAME=your-llm-model-name
EMBEDDINGS_LLM_MODEL_NAME=your-embeddings-model-name

# Google Service Account
GOOGLE_SERVICE_ACCOUNT_JSON_PATH=./service_account.json
```

#### 3.2. Prepare Prerequisites

1. Place your Google service account JSON file as `service_account.json` in project root
2. Login to Quay.io: `podman login quay.io` (or `docker login quay.io`)

#### 3.3. Create Secrets Automatically

```bash
make secrets
```

This creates all required Kubernetes secrets and patches the pipeline service account.

### 4. Makefile Commands

| Command | Description |
|---------|-------------|
| `all` | Complete deployment: setup + tasks + pipeline + run |
| `setup` | Create PVCs and secrets |
| `secrets` | Create secrets from .env file |
| `pvc` | Create persistent volume claims |
| `tasks` | Apply Tekton task definitions |
| `pipeline` | Apply pipeline definition |
| `run` | Execute pipeline (requires tkn CLI or shows manual command) |
| `logs` | View pipeline logs |
| `clean` | **⚠️ Deletes ALL resources in namespace including PVCs** |

### 5. Quick Start

#### 5.1. Create OpenShift Project

```bash
oc new-project sast-ai-workflow
oc project sast-ai-workflow
```

#### 5.2. Run Everything

```bash
make all
```

**Note:** If Tekton CLI (`tkn`) is not installed, the command completes infrastructure setup and shows the manual pipeline execution command.

#### 5.3. Run with Custom Parameters

```bash
make all PROJECT_NAME="systemd" \
 PROJECT_VERSION="257-9" \
 REPO_REMOTE_URL="https://download.devel.redhat.com/brewroot/vol/rhel-10/packages/systemd/257/9.el10/src/systemd-257-9.el10.src.rpm" \
 INPUT_REPORT_FILE_PATH="https://docs.google.com/spreadsheets/d/1NPGmERBsSTdHjQK2vEocQ-PvQlRGGLMds02E_RGF8vY/export?format=csv" \
 FALSE_POSITIVES_URL="https://gitlab.cee.redhat.com/osh/known-false-positives/-/raw/master/systemd/ignore.err"
```

### 6. Step-by-Step Alternative

If you prefer individual steps:

```bash
make setup          # Infrastructure only
make tasks pipeline  # Tekton resources
make run            # Execute pipeline
```

### 7. Troubleshooting

- **View logs:** `make logs`
- **Clean environment:** `make clean` (⚠️ deletes everything)
- **Check secrets:** `oc get secrets`
- **Manual pipeline execution:** Use the command displayed when `tkn` CLI is not available
