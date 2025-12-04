#!/usr/bin/env sh
echo "=== STEP 1: VALIDATE INPUT URLS ==="

# Validate source URL
echo "Validating source URL..."
curl -ksSfL "$REPO_REMOTE_URL" >/dev/null 2>&1 || (echo "Error: Source code URL is invalid" && exit 1)
echo "Source URL validated successfully"

# Validate false positives URL (if provided)
if [ -z "$FALSE_POSITIVES_URL" ]; then
  echo "No FALSE_POSITIVES_URL provided; skipping validation"
else
  echo "Validating false positives URL..."
  curl -ksSfL "$FALSE_POSITIVES_URL" >/dev/null 2>&1 || (echo "Error: False positives URL is invalid" && exit 1)
  echo "False positives URL validated successfully"
fi