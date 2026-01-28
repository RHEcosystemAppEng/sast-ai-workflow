#!/bin/bash
set -euo pipefail
echo "=== FETCH SAST FALSE POSITIVE PATTERNS FROM DVC ==="

# Inline S3 availability check (from s3-helper.sh check mode)
if [[ -f "/shared-data/s3-available.txt" ]]; then
  S3_AVAILABLE=$(cat /shared-data/s3-available.txt)
  if [[ "$S3_AVAILABLE" = "false" ]]; then
    echo "Skipped: S3 endpoint not available (see Step 4)"
    exit 0
  fi
else
  echo "Warning: S3 availability status file not found, proceeding with caution"
fi

# Constants for DVC paths
readonly DVC_PATTERNS_DIR="patterns"

# Create patterns directory
mkdir -p /shared-data/patterns

echo "DVC Repository: $DVC_REPO_URL"
echo "DVC Patterns Path: $DVC_PATTERNS_DIR"
echo "DVC Version: $DVC_DATA_VERSION"
echo "S3 Endpoint: $S3_ENDPOINT_URL"

# Check if patterns directory exists in DVC before attempting download
if dvc list "$DVC_REPO_URL" --rev "$DVC_DATA_VERSION" 2>/dev/null | grep -q "$DVC_PATTERNS_DIR"; then
  echo "Patterns directory found in DVC, downloading..."
  dvc get "$DVC_REPO_URL" "$DVC_PATTERNS_DIR" --rev "$DVC_DATA_VERSION" -o "/shared-data/patterns" \
    || (echo "Error: Failed to download patterns despite directory existing in DVC" >&2 && exit 1)
  echo "Patterns downloaded successfully via DVC"
  echo "Pattern files:"
  ls -lh /shared-data/patterns/*.json 2>/dev/null || echo "No pattern JSON files found"
  echo "Total pattern files: $(ls -1 /shared-data/patterns/*.json 2>/dev/null | wc -l)"
else
  echo "No patterns directory found in DVC - continuing without patterns"
  echo "Pattern-based analysis will be disabled"
  exit 0
fi