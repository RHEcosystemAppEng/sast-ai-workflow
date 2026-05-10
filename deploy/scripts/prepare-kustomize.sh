#!/bin/bash
# =============================================================================
# prepare-kustomize.sh
# =============================================================================
# Prepares Kustomize overlays with secrets and service account files from .env
# This script runs BEFORE 'oc apply -k' to generate environment-specific files
# =============================================================================

set -e

# =============================================================================
# Configuration
# =============================================================================
ENV="${1:-dev}"  # dev, prod, or mlops
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEPLOY_DIR="$(dirname "$SCRIPT_DIR")"
OVERLAY_DIR="${DEPLOY_DIR}/tekton/overlays/${ENV}"
ENV_FILE="${DEPLOY_DIR}/../.env"

# =============================================================================
# Validation: Environment
# =============================================================================
if [[ ! "$ENV" =~ ^(dev|prod|mlops)$ ]]; then
    echo "❌ ERROR: Invalid environment. Must be: dev, prod, or mlops" >&2
    echo "Usage: $0 <dev|prod|mlops>" >&2
    exit 1
fi

if [[ ! -d "$OVERLAY_DIR" ]]; then
    echo "❌ ERROR: Overlay directory not found: $OVERLAY_DIR" >&2
    exit 1
fi

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🔧 Preparing Kustomize overlay: ${ENV}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# =============================================================================
# Step 1: Load and validate .env file
# =============================================================================
echo "📋 Step 1: Loading .env file..."

if [[ ! -f "$ENV_FILE" ]]; then
    echo "❌ ERROR: .env file not found at: $ENV_FILE" >&2
    echo "💡 Create .env file in project root with required variables" >&2
    exit 1
fi

# Load .env safely (without executing shell commands)
# Parse KEY=VALUE pairs without shell execution to prevent code injection
while IFS='=' read -r key value || [[ -n "$key" ]]; do
    # Skip empty lines and comments
    [[ -z "$key" || "$key" =~ ^[[:space:]]*# ]] && continue

    # Remove leading/trailing whitespace from key
    key=$(echo "$key" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

    # Remove leading/trailing whitespace and quotes from value
    value=$(echo "$value" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//' | sed "s/^['\"]//;s/['\"]$//")

    # Export the variable
    export "$key"="$value"
done < "$ENV_FILE"

echo "   ✓ .env file loaded securely"

# =============================================================================
# Step 2: Validate required variables
# =============================================================================
echo "🔍 Step 2: Validating required variables..."

REQUIRED_VARS=(
    "GITLAB_TOKEN"
    "LLM_URL"
    "LLM_API_KEY"
    "LLM_API_TYPE"
    "LLM_MODEL_NAME"
    "EMBEDDINGS_LLM_URL"
    "EMBEDDINGS_LLM_API_KEY"
    "EMBEDDINGS_LLM_MODEL_NAME"
    "GOOGLE_SERVICE_ACCOUNT_JSON_PATH"
)

MISSING_VARS=()
for var in "${REQUIRED_VARS[@]}"; do
    if [[ -z "${!var}" ]]; then
        MISSING_VARS+=("$var")
    fi
done

if [[ ${#MISSING_VARS[@]} -gt 0 ]]; then
    echo "❌ ERROR: Missing required variables in .env:" >&2
    for var in "${MISSING_VARS[@]}"; do
        echo "   - $var" >&2
    done
    exit 1
fi

echo "   ✓ All required variables present"

# MLOps-specific validation
if [[ "$ENV" = "mlops" ]]; then
    echo "🔍 Step 2b: Validating MLOps-specific variables..."

    MLOPS_REQUIRED_VARS=(
        "DVC_REPO_URL"
        "DVC_DATA_VERSION"
        "S3_ENDPOINT_URL"
        "S3_INPUT_BUCKET_NAME"
        "S3_OUTPUT_BUCKET_NAME"
    )

    MLOPS_MISSING_VARS=()
    for var in "${MLOPS_REQUIRED_VARS[@]}"; do
        if [[ -z "${!var}" ]]; then
            MLOPS_MISSING_VARS+=("$var")
        fi
    done

    if [[ ${#MLOPS_MISSING_VARS[@]} -gt 0 ]]; then
        echo "❌ ERROR: Missing MLOps-specific variables in .env:" >&2
        for var in "${MLOPS_MISSING_VARS[@]}"; do
            echo "   - $var" >&2
        done
        echo "" >&2
        echo "💡 MLOps environment requires S3/MinIO and DVC configuration" >&2
        echo "   Add these variables to your .env file:" >&2
        echo "   - DVC_REPO_URL=https://github.com/your-org/sast-ai-dvc" >&2
        echo "   - DVC_DATA_VERSION=v2.0" >&2
        echo "   - S3_ENDPOINT_URL=http://minio.your-cluster.com" >&2
        echo "   - S3_INPUT_BUCKET_NAME=mlops-input" >&2
        echo "   - S3_OUTPUT_BUCKET_NAME=mlops-output" >&2
        exit 1
    fi

    echo "   ✓ MLOps-specific variables present"
fi

# =============================================================================
# Step 3: Create separate secret env files for secretGenerator
# =============================================================================
echo "🔐 Step 3: Generating secret env files..."

# GitLab token secret
cat > "${OVERLAY_DIR}/secrets-gitlab.env" <<EOF
# GitLab Token
gitlab_token=${GITLAB_TOKEN}
EOF
echo "   ✓ secrets-gitlab.env created"

# LLM credentials secret
cat > "${OVERLAY_DIR}/secrets-llm.env" <<EOF
# LLM Credentials
llm_url=${LLM_URL}
llm_api_key=${LLM_API_KEY}
llm_api_type=${LLM_API_TYPE}
llm_model_name=${LLM_MODEL_NAME}
embeddings_llm_url=${EMBEDDINGS_LLM_URL}
embeddings_llm_api_key=${EMBEDDINGS_LLM_API_KEY}
embedding_llm_model_name=${EMBEDDINGS_LLM_MODEL_NAME}
EOF
echo "   ✓ secrets-llm.env created"

# S3/MinIO credentials secret (optional)
if [[ -n "${AWS_ACCESS_KEY_ID}" ]] && [[ -n "${AWS_SECRET_ACCESS_KEY}" ]]; then
    cat > "${OVERLAY_DIR}/secrets-s3.env" <<EOF
# S3/MinIO Credentials
access_key_id=${AWS_ACCESS_KEY_ID}
secret_access_key=${AWS_SECRET_ACCESS_KEY}
EOF

    if [[ -n "${S3_ENDPOINT_URL}" ]]; then
        echo "endpoint_url=${S3_ENDPOINT_URL}" >> "${OVERLAY_DIR}/secrets-s3.env"
    fi

    echo "   ✓ secrets-s3.env created (S3/MinIO credentials included)"
else
    # Create empty S3 secret file so Kustomize doesn't fail
    cat > "${OVERLAY_DIR}/secrets-s3.env" <<EOF
# S3/MinIO Credentials (not configured)
EOF

    if [[ "$ENV" = "mlops" ]]; then
        echo "   ⚠️  Warning: S3/MinIO credentials (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY) not configured"
        echo "   💡 MLOps typically needs these for S3/MinIO access"
    else
        echo "   ⚠️  S3/MinIO credentials not configured (optional for ${ENV})"
    fi
fi

# =============================================================================
# Step 4: Copy service account files
# =============================================================================
echo "📁 Step 4: Copying service account files..."

SA_DIR="${OVERLAY_DIR}/service-accounts"
mkdir -p "$SA_DIR"

# Google Service Account (REQUIRED)
# Normalize path (handles both absolute and relative paths)
GOOGLE_SA_PATH="$(realpath -m "${GOOGLE_SERVICE_ACCOUNT_JSON_PATH}" 2>/dev/null || echo "${DEPLOY_DIR}/${GOOGLE_SERVICE_ACCOUNT_JSON_PATH}")"

if [[ ! -f "$GOOGLE_SA_PATH" ]]; then
    echo "❌ ERROR: Google service account not found at: $GOOGLE_SA_PATH" >&2
    echo "💡 Set GOOGLE_SERVICE_ACCOUNT_JSON_PATH in .env to correct path" >&2
    exit 1
fi

cp "$GOOGLE_SA_PATH" "${SA_DIR}/service_account.json"
echo "   ✓ Google service account: ${SA_DIR}/service_account.json"

# GCS Service Account (OPTIONAL)
if [[ -n "${GCS_SERVICE_ACCOUNT_JSON_PATH}" ]]; then
    # Normalize path (handles both absolute and relative paths)
    GCS_SA_PATH="$(realpath -m "${GCS_SERVICE_ACCOUNT_JSON_PATH}" 2>/dev/null || echo "${DEPLOY_DIR}/${GCS_SERVICE_ACCOUNT_JSON_PATH}")"

    if [[ -f "$GCS_SA_PATH" ]]; then
        cp "$GCS_SA_PATH" "${SA_DIR}/gcs_service_account.json"
        echo "   ✓ GCS service account: ${SA_DIR}/gcs_service_account.json"
    else
        echo "   ⚠️  GCS service account not found (optional): $GCS_SA_PATH"
    fi
else
    echo "   ⚠️  GCS service account not configured (optional)"
fi

# =============================================================================
# Step 5: Find and copy Docker/Podman auth config
# =============================================================================
echo "🐳 Step 5: Locating container registry authentication..."

DOCKER_AUTH_FILE=""

# Check locations in order of preference
DOCKER_CONFIG_PATH="${DOCKER_CONFIG_PATH:-${HOME}/.config/containers/auth.json}"

if [[ -f "$DOCKER_CONFIG_PATH" ]]; then
    DOCKER_AUTH_FILE="$DOCKER_CONFIG_PATH"
elif [[ -f "${XDG_RUNTIME_DIR}/containers/auth.json" ]]; then
    DOCKER_AUTH_FILE="${XDG_RUNTIME_DIR}/containers/auth.json"
elif [[ -f "${HOME}/.docker/config.json" ]]; then
    DOCKER_AUTH_FILE="${HOME}/.docker/config.json"
elif [[ -f "${HOME}/.config/containers/auth.json" ]]; then
    DOCKER_AUTH_FILE="${HOME}/.config/containers/auth.json"
fi

if [[ -z "$DOCKER_AUTH_FILE" ]]; then
    echo "❌ ERROR: Container registry authentication not found" >&2
    echo "💡 Run: podman login quay.io (or docker login quay.io)" >&2
    echo "" >&2
    echo "Searched locations:" >&2
    echo "   - ${DOCKER_CONFIG_PATH}" >&2
    echo "   - ${XDG_RUNTIME_DIR}/containers/auth.json" >&2
    echo "   - ${HOME}/.docker/config.json" >&2
    echo "   - ${HOME}/.config/containers/auth.json" >&2
    exit 1
fi

cp "$DOCKER_AUTH_FILE" "${SA_DIR}/.dockerconfigjson"
echo "   ✓ Container registry auth: ${SA_DIR}/.dockerconfigjson"
echo "   ✓ Source: ${DOCKER_AUTH_FILE}"

# =============================================================================
# Step 6: Generate prompt templates
# =============================================================================
echo "💬 Step 6: Generating prompt templates..."

if [[ ! -f "${SCRIPT_DIR}/generate_prompts.py" ]]; then
    echo "❌ ERROR: generate_prompts.py not found at: ${SCRIPT_DIR}/generate_prompts.py" >&2
    exit 1
fi

cd "$DEPLOY_DIR"
python3 scripts/generate_prompts.py > /dev/null 2>&1

if [[ ! -f "${DEPLOY_DIR}/tekton/base/sast-ai-prompt-templates.yaml" ]]; then
    echo "❌ ERROR: Failed to generate prompt templates" >&2
    exit 1
fi

echo "   ✓ Prompt templates generated: tekton/base/sast-ai-prompt-templates.yaml"

# =============================================================================
# Step 7: Environment-specific configurations
# =============================================================================
echo "⚙️  Step 7: Environment-specific configurations..."

case "$ENV" in
    dev)
        echo "   ✓ Dev environment: Using :latest image tag, Google Drive storage"
        ;;
    prod)
        echo "   ✓ Prod environment: Using versioned image tag, Google Drive storage"
        ;;
    mlops)
        echo "   ✓ MLOps environment: S3/MinIO storage, DVC versioning enabled"
        echo "   ✓ DVC Repository: ${DVC_REPO_URL}"
        echo "   ✓ DVC Version: ${DVC_DATA_VERSION}"
        echo "   ✓ S3 Endpoint: ${S3_ENDPOINT_URL}"
        ;;
esac

# =============================================================================
# Summary
# =============================================================================
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "✅ Kustomize overlay prepared successfully: ${ENV}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Generated files:"
echo "   - ${OVERLAY_DIR}/secrets-gitlab.env"
echo "   - ${OVERLAY_DIR}/secrets-llm.env"
echo "   - ${OVERLAY_DIR}/secrets-s3.env"
echo "   - ${SA_DIR}/service_account.json"
echo "   - ${SA_DIR}/.dockerconfigjson"
[[ -f "${SA_DIR}/gcs_service_account.json" ]] && echo "   - ${SA_DIR}/gcs_service_account.json"
echo ""
echo "Next step: Run deployment"
echo "   make -f Makefile.new deploy-${ENV}"
echo ""
