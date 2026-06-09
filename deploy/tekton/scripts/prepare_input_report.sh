#!/usr/bin/env bash
set -e

# Option 1: Konflux scan - Download SARIF from Trusted Artifacts
if [[ -n "$SARIF_URI" ]]; then
  /scripts/download_from_ta.sh
  exit 0
fi

# Option 2: Regular scan - Transform report to SARIF
/scripts/transform_report.sh
