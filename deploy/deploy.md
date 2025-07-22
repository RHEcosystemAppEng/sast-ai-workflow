## Deploying the SAST AI Workflow

This guide covers deployment on a local OpenShift cluster using CodeReady Containers (CRC) or an existing OpenShift cluster.

### Directory Structure

```
deploy/
├── Makefile                    # Main deployment automation
├── deploy.md                  # This documentation  
├── scripts/                   # Deployment utility scripts
│   └── generate_prompts.py    # ConfigMap generation from templates
└── tekton/                    # Kubernetes/Tekton resources
    ├── tasks/                 # Individual pipeline tasks
    ├── scripts/               # ConfigMaps for pipeline scripts  
    ├── prompts-config-map.yaml # Generated prompt templates
    └── *.yaml                 # Other pipeline resources
```

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
| `generate-prompts` | Generate ConfigMap from prompt template files |
| `prompts` | Generate and apply prompts ConfigMap to cluster |
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
### 7. Customizing Prompts

The SAST AI Workflow uses a template-based prompt system with a single source of truth. Prompts are now managed through individual template files rather than being hardcoded.

#### 7.1. Template-Based Prompt System

All prompts are stored as individual YAML template files in `src/templates/prompts/`:

```
src/templates/prompts/
├── analysis_system_prompt.yaml
├── analysis_human_prompt.yaml
├── filter_system_prompt.yaml
├── filter_human_prompt.yaml
├── recommendations_prompt.yaml
├── justification_summary_system_prompt.yaml
├── justification_summary_human_prompt.yaml
└── evaluation_prompt.yaml
```

#### 7.2. How to Customize Prompts

**Option 1: Edit Template Files (Recommended)**

1. Edit the appropriate template file in `src/templates/prompts/`
2. Each file has this structure:
   ```yaml
   template: |
     Your prompt content here...
     {placeholders} are preserved for runtime substitution
   ```
3. Regenerate the ConfigMap: `make generate-prompts`
4. Apply to cluster: `make prompts`

**Option 2: Environment Variable Override**

Set environment variables to override specific prompts:
```bash
export ANALYSIS_SYSTEM_PROMPT="Your custom prompt here..."
export FILTER_SYSTEM_PROMPT="Another custom prompt..."
```

**Option 3: Direct ConfigMap Edit (Not Recommended)**

Edit `deploy/tekton/prompts-config-map.yaml` directly, but note that changes will be lost when `make generate-prompts` is run.

#### 7.3. Prompt Template Guidelines

- Keep `{placeholder}` variables intact (e.g., `{cve_error_trace}`, `{context}`)
- Use YAML literal block scalar (`|`) for multi-line prompts
- Test prompts locally before deploying to cluster
- Document any significant changes for team members

#### 7.4. Applying Prompt Changes

```bash
# Option 1: Regenerate and apply (recommended)
make prompts

# Option 2: Step by step
make generate-prompts  # Generate ConfigMap from templates
make prompts          # Apply to cluster

# Option 3: Apply without regenerating (if manually edited)
oc apply -f tekton/prompts-config-map.yaml
```

### 8. Testing Prompt Generation

Before deploying, you can test prompt generation locally:

```bash
# Test from project root
cd deploy
make generate-prompts

# Verify the generated ConfigMap looks correct
head -20 tekton/prompts-config-map.yaml

# Check that all 8 prompts are included
grep -c "prompt:" tekton/prompts-config-map.yaml  # Should show 8
```

This ensures all template files are valid and the ConfigMap generation works correctly.

### 9. Troubleshooting

#### General Issues
- **View logs:** `make logs`
- **Clean environment:** `make clean` (⚠️ deletes everything)
- **Check secrets:** `oc get secrets`
- **Manual pipeline execution:** Use the command displayed when `tkn` CLI is not available

