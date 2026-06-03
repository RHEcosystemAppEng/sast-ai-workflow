#!/usr/bin/env bash
set -e

echo "=== STEP 0: DOWNLOAD FROM TRUSTED ARTIFACTS ==="

# Skip if IMAGE_DIGEST is empty (not a Konflux scan)
if [[ -z "$IMAGE_DIGEST" ]]; then
  echo "IMAGE_DIGEST not provided - skipping Trusted Artifacts download"
  exit 0
fi

echo "Image digest: $IMAGE_DIGEST"

# Check if ORAS is available
if ! command -v oras &> /dev/null; then
  echo "ERROR: ORAS CLI not found. Please ensure ORAS v1.3.0+ is installed in BASE_IMAGE." >&2
  exit 1
fi

# Extract registry host from IMAGE_DIGEST
REGISTRY_HOST=$(echo "$IMAGE_DIGEST" | cut -d'/' -f1)

# Authenticate to Konflux registry if credentials are provided
if [[ -f "$KONFLUX_REGISTRY_CREDS_PATH" ]]; then
  echo "Authenticating to Konflux registry: $REGISTRY_HOST"

  # Read token from secret file
  KONFLUX_TOKEN=$(cat "$KONFLUX_REGISTRY_CREDS_PATH")

  if [[ -z "$KONFLUX_TOKEN" ]]; then
    echo "ERROR: Konflux registry token is empty in $KONFLUX_REGISTRY_CREDS_PATH" >&2
    exit 1
  fi

  # Login to registry using token (using stdin to avoid exposing token in logs)
  echo "$KONFLUX_TOKEN" | oras login "$REGISTRY_HOST" \
    --username openshift-user \
    --password-stdin

  echo "Successfully authenticated to $REGISTRY_HOST"
else
  echo "WARNING: No Konflux registry credentials provided at $KONFLUX_REGISTRY_CREDS_PATH"
  echo "Attempting unauthenticated pull (may fail for private registries)"
fi

# Download artifacts from Trusted Artifacts
# Note: SARIF is embedded directly in the image, not as an OCI referrer
echo "Downloading artifacts from Trusted Artifacts..."
mkdir -p /shared-data/oras-downloads
oras pull "$IMAGE_DIGEST" --output /shared-data/oras-downloads/

# Find and move SARIF file to expected location
SARIF_FILE=$(find /shared-data/oras-downloads -name "*.sarif" -o -name "*.json" | head -1)
if [[ -z "$SARIF_FILE" ]]; then
  echo "ERROR: No SARIF file found in downloaded artifact" >&2
  ls -la /shared-data/oras-downloads/
  exit 1
fi

mv "$SARIF_FILE" /shared-data/input-report.sarif
echo "SARIF downloaded to: /shared-data/input-report.sarif"

# TODO: Download source artifacts from Trusted Artifacts if needed
# Source code may be embedded in the image or available via separate artifact.
# For now, REPO_REMOTE_URL will handle source download in prepare-source step.
# If source is embedded in the same image, it will be downloaded alongside SARIF
# in the oras pull step above.

echo "=== Trusted Artifacts download complete ==="
