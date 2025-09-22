## Deploying the SAST AI Workflow

This guide covers deployment on a local OpenShift cluster using CodeReady Containers (CRC) or an existing OpenShift cluster.

### Directory Structure

```
deploy/
├── Makefile                    # Main deployment automation
├── README.md                   # This documentation  
├── argocd/                     # GitOps configuration
│   └── argocd-application.yaml # ArgoCD Application definition
├── scripts/                    # Deployment utility scripts
│   └── generate_prompts.py     # ConfigMap generation from templates
└── tekton/                     # Kubernetes/Tekton resources
    ├── tasks/                  # Individual pipeline tasks
    ├── scripts/                # ConfigMaps for pipeline scripts  
    ├── sast-ai-prompt-templates.yaml # Generated prompt templates
    └── *.yaml                  # Other pipeline resources
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

### 4. Resource Naming Convention

All SAST AI Workflow resources use the `sast-ai-` prefix for easy identification and management:

**Secrets:**
- `sast-ai-gitlab-token` - GitLab access token
- `sast-ai-default-llm-creds` - LLM and embeddings API credentials
- `sast-ai-google-service-account` - Google service account JSON
- `sast-ai-quay-registry-config` - Container registry pull credentials

**ConfigMaps:**
- `sast-ai-prompt-templates` - LLM prompt templates
- `sast-ai-gdrive-upload-scripts` - Google Drive upload scripts
- `sast-ai-gdrive-config` - Google Drive folder ID (optional)


**PVCs:**
- `sast-ai-workflow-pvc` - Main workspace storage
- `sast-ai-cache-pvc` - Build cache storage

This naming convention allows administrators to quickly filter and manage SAST AI resources:
```bash
# View all SAST AI resources
oc get all,secrets,configmaps,pvc -l app=sast-ai
# Or use grep filtering
oc get secrets | grep sast-ai
```

### 5. Makefile Commands

| Command | Description |
|---------|-------------|
| `deploy` | Complete deployment: setup + tasks + pipeline + prompts + argocd-deploy |
| `setup` | Create PVCs and secrets |
| `secrets` | Create secrets from .env file |
| `pvc` | Create persistent volume claims |
| `tasks` | Apply Tekton task definitions |
| `generate-prompts` | Generate ConfigMap from prompt template files |
| `prompts` | Generate and apply prompts ConfigMap to cluster |
| `pipeline` | Apply pipeline definition |
| `run` | Execute pipeline using oc apply with PipelineRun |
| `clean` | **⚠️ Deletes ALL resources in namespace including PVCs** |
| **ArgoCD GitOps** | |
| `argocd-deploy` | Deploy ArgoCD Application for automated GitOps |
| `argocd-clean` | Remove ArgoCD Application |

### 6. Quick Start

#### 6.1. Create OpenShift Project

```bash
oc new-project sast-ai-workflow
oc project sast-ai-workflow
```

#### 6.2. Run Everything

```bash
make deploy
```

**Note:** This sets up the complete infrastructure including ArgoCD GitOps but does not execute the pipeline. To run the pipeline, use `make run` separately.

#### 6.3. Run with Custom Parameters

```bash
make deploy PROJECT_NAME="systemd" \
 PROJECT_VERSION="257-9" \
 REPO_REMOTE_URL="https://download.devel.redhat.com/brewroot/vol/rhel-10/packages/systemd/257/9.el10/src/systemd-257-9.el10.src.rpm" \
 INPUT_REPORT_FILE_PATH="https://docs.google.com/spreadsheets/d/1NPGmERBsSTdHjQK2vEocQ-PvQlRGGLMds02E_RGF8vY/export?format=csv" \
 FALSE_POSITIVES_URL="https://gitlab.cee.redhat.com/osh/known-false-positives/-/raw/master/systemd/ignore.err"
```

### 7. Step-by-Step Alternative

If you prefer individual steps:

```bash
make setup                # Infrastructure only
make tasks pipeline       # Tekton resources
make prompts             # Prompt templates
make argocd-deploy       # GitOps setup
make run                 # Execute pipeline (optional)
```

### 8. GitOps with ArgoCD

For VPN-protected clusters, use GitOps to automatically sync Tekton resources from GitHub.

#### 8.1. Prerequisites
- ArgoCD installed in your cluster (e.g., in `sast-ai` namespace)
- Repository access from within the cluster

#### 8.2. Deploy GitOps
```bash
# Deploy ArgoCD Application
make argocd-deploy
```

#### 8.3. How It Works
- **Auto-sync**: Changes to `main` branch deploy automatically (~3 min)
- **Self-healing**: Manual changes are automatically reverted
- **Pruning**: Deleted files are removed from cluster
- **Path**: Only syncs `deploy/tekton/` directory

#### 8.4. Configuration
Set in `.env` file (optional):
```env
GITHUB_REPO_URL=https://github.com/your-org/sast-ai-workflow.git
ARGOCD_NAMESPACE=sast-ai
```

#### 8.5. Prompt Changes with GitOps
When modifying prompts in `src/templates/prompts/`, you must regenerate the ConfigMap:

```bash
# 1. Edit prompt templates in src/templates/prompts/
# 2. Regenerate ConfigMap
make generate-prompts

# 3. Commit the updated ConfigMap
git add deploy/tekton/sast-ai-prompt-templates.yaml
git commit -m "Update prompts"
git push

# 4. ArgoCD will sync the new prompts automatically
```

**Note**: This is only needed when changing prompts, not for regular commits.

### 9. Customizing Prompts

The SAST AI Workflow uses a template-based prompt system with a single source of truth. Prompts are now managed through individual template files rather than being hardcoded.

#### 9.1. Template-Based Prompt System

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

#### 9.2. How to Customize Prompts

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

Edit `deploy/tekton/sast-ai-prompt-templates.yaml` directly, but note that changes will be lost when `make generate-prompts` is run.

#### 9.3. Prompt Template Guidelines

- Keep `{placeholder}` variables intact (e.g., `{cve_error_trace}`, `{context}`)
- Use YAML literal block scalar (`|`) for multi-line prompts
- Test prompts locally before deploying to cluster
- Document any significant changes for team members

#### 9.4. Applying Prompt Changes

```bash
# Option 1: Regenerate and apply (recommended)
make prompts

# Option 2: Step by step
make generate-prompts  # Generate ConfigMap from templates
make prompts          # Apply to cluster

# Option 3: Apply without regenerating (if manually edited)
oc apply -f tekton/sast-ai-prompt-templates.yaml
```

### 10. Testing Prompt Generation

Before deploying, you can test prompt generation locally:

```bash
# Test from project root
cd deploy
make generate-prompts

# Verify the generated ConfigMap looks correct
head -20 tekton/sast-ai-prompt-templates.yaml

# Check that all 8 prompts are included
grep -c "prompt:" tekton/sast-ai-prompt-templates.yaml  # Should show 8
```

This ensures all template files are valid and the ConfigMap generation works correctly.

### 11. Troubleshooting

#### General Issues
- **View logs:** `oc logs -l tekton.dev/pipelineRun=sast-ai-workflow-pipelinerun -f`
- **Clean environment:** `make clean` (⚠️ deletes everything)
- **Check secrets:** `oc get secrets`
- **Manual pipeline execution:** Use `make run` or execute via OpenShift console

