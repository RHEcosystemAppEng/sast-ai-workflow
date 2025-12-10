#!/bin/bash
echo "USE_KNOWN_FALSE_POSITIVE_FILE: $USE_KNOWN_FALSE_POSITIVE_FILE"
if [[ "$USE_KNOWN_FALSE_POSITIVE_FILE" = "false" ]]; then
  echo "USE_KNOWN_FALSE_POSITIVE_FILE is false; skipping fetch..."
  exit 0
fi

set -euo pipefail
echo "=== STEP 5: FETCH FALSE POSITIVES ==="

# Create false positives directory
mkdir -p /shared-data/false-positives

if [[ -z "$FP_URL" ]]; then
  echo "No falsePositivesUrl provided; skipping fetch..."
  echo "Creating empty ignore.err file"
  touch /shared-data/false-positives/ignore.err
  exit 0
fi

echo "Fetching false positives from GitLab/HTTP..."

# If a token is needed for private repos, read it from the secret (if present)
if [[ -f "$GITLAB_TOKEN_PATH" ]]; then
    GITLAB_TOKEN=$(cat "$GITLAB_TOKEN_PATH")
    echo "GitLab token found. Fetching file with authentication..."
    curl --retry 3 --retry-delay 5 -k -H "PRIVATE-TOKEN: $GITLAB_TOKEN" -fL "$FP_URL" -o "/shared-data/false-positives/ignore.err" \
      || (echo "Error: Could not fetch false positives file with token." && exit 1)
else
    echo "No GitLab token file found; attempting unauthenticated fetch..."
    curl --retry 3 --retry-delay 5 -k -fL "$FP_URL" -o "/shared-data/false-positives/ignore.err" \
      || (echo "Error: Could not fetch false positives file unauthenticated." && exit 1)
fi

echo "False positives file downloaded successfully"
echo "File size: $(du -h /shared-data/false-positives/ignore.err | cut -f1)"