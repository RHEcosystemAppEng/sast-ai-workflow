#!/usr/bin/env bash
set -e

echo "=== STEP 0: DOWNLOAD FROM TRUSTED ARTIFACTS ==="

# Skip if IMAGE_DIGEST is empty (not a Konflux scan)
if [ -z "$IMAGE_DIGEST" ]; then
  echo "IMAGE_DIGEST not provided - skipping Trusted Artifacts download"
  exit 0
fi

echo "Image digest: $IMAGE_DIGEST"

# Check if ORAS is available
if ! command -v oras &> /dev/null; then
  echo "ERROR: ORAS CLI not found. Please ensure ORAS v1.3.0+ is installed in BASE_IMAGE."
  exit 1
fi

# Discover SARIF artifact attached to the image
echo "Discovering SARIF artifact using ORAS..."
SARIF_DIGEST=$(oras discover "$IMAGE_DIGEST" \
  --artifact-type application/sarif+json \
  --format json 2>/dev/null | jq -r '.manifests[0].digest' || echo "")

if [ -z "$SARIF_DIGEST" ] || [ "$SARIF_DIGEST" = "null" ]; then
  echo "ERROR: No SARIF artifact found attached to image $IMAGE_DIGEST"
  echo "Expected artifactType: application/sarif+json"
  exit 1
fi

echo "Found SARIF artifact: $SARIF_DIGEST"

# Extract registry and repository from IMAGE_DIGEST
REGISTRY=$(echo "$IMAGE_DIGEST" | cut -d'/' -f1)
REPO_PATH=$(echo "$IMAGE_DIGEST" | cut -d'@' -f1 | cut -d'/' -f2-)

# Download SARIF using the discovered digest
echo "Downloading SARIF artifact..."
mkdir -p /shared-data/oras-downloads
oras pull "${REGISTRY}/${REPO_PATH}@${SARIF_DIGEST}" \
  --output /shared-data/oras-downloads/

# Find and move SARIF file to expected location
SARIF_FILE=$(find /shared-data/oras-downloads -name "*.sarif" -o -name "*.json" | head -1)
if [ -z "$SARIF_FILE" ]; then
  echo "ERROR: No SARIF file found in downloaded artifact"
  ls -la /shared-data/oras-downloads/
  exit 1
fi

mv "$SARIF_FILE" /shared-data/input-report.sarif
echo "SARIF downloaded to: /shared-data/input-report.sarif"

# TODO: Download source artifacts from Trusted Artifacts
# Source code can be attached to the same image as an OCI referrer (like SARIF).
# Before implementing, we need to determine:
#   1. Artifact type for source (e.g., application/vnd.cachi2.source, application/vnd.oci.image.layer.v1.tar+gzip)
#   2. Authentication requirements (if any)
#   3. Extraction format and destination path
# Implementation approach (once above is known):
#   SOURCE_DIGEST=$(oras discover "$IMAGE_DIGEST" --artifact-type <TYPE> --format json | jq -r '.manifests[0].digest')
#   oras pull "${REGISTRY}/${REPO_PATH}@${SOURCE_DIGEST}" --output /shared-data/source/
# For now, REPO_REMOTE_URL will handle source download in prepare-source step

echo "=== Trusted Artifacts download complete ==="
