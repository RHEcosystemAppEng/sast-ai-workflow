#!/usr/bin/env bash
set -e

echo "=== DOWNLOAD FROM TRUSTED ARTIFACTS ==="

# Skip if SARIF_URI is empty (not a Konflux scan)
if [[ -z "$SARIF_URI" ]]; then
  echo "SARIF_URI not provided - skipping Trusted Artifacts download"
  exit 0
fi

echo "Image digest: $SARIF_URI"

# Check if ORAS is available
if ! command -v oras &> /dev/null; then
  echo "ERROR: ORAS CLI not found. Please ensure ORAS v1.3.0+ is installed in BASE_IMAGE." >&2
  exit 1
fi

# Extract registry host from SARIF_URI
REGISTRY_HOST=$(echo "$SARIF_URI" | cut -d'/' -f1)

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
oras pull "$SARIF_URI" --output /shared-data/oras-downloads/

# Find and move SARIF file to expected location
SARIF_FILE=$(find /shared-data/oras-downloads -name "*.sarif" -o -name "*.json" | head -1)
if [[ -z "$SARIF_FILE" ]]; then
  echo "ERROR: No SARIF file found in downloaded artifact" >&2
  ls -la /shared-data/oras-downloads/
  exit 1
fi

mv "$SARIF_FILE" /shared-data/input-report.sarif
echo "SARIF downloaded to: /shared-data/input-report.sarif"

# Note: Source code is NOT downloaded from Trusted Artifacts.
# Konflux provides git URL + revision via REPO_REMOTE_URL and GIT_REVISION parameters.
# Source checkout happens in prepare-source step using git clone.

echo "=== Trusted Artifacts download complete ==="
