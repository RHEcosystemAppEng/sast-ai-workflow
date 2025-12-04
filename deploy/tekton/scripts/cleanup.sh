#!/usr/bin/env sh
echo "=== STEP 9: CLEANUP ==="

CLEANED_ITEMS=""

# Remove source code directory
if [ -d "/shared-data/source" ]; then
    rm -rf /shared-data/source/* >/dev/null 2>&1
    CLEANED_ITEMS="$CLEANED_ITEMS source-code"
fi

# Remove false positives file
if [ -f "/shared-data/false-positives/ignore.err" ]; then
    rm -f /shared-data/false-positives/ignore.err >/dev/null 2>&1
    CLEANED_ITEMS="$CLEANED_ITEMS false-positives"
fi

# Clean up any temporary files in cache
if [ -d "/cache-data/tmp" ]; then
    rm -rf /cache-data/tmp/* >/dev/null 2>&1
    CLEANED_ITEMS="$CLEANED_ITEMS temp-files"
fi

# Report what was cleaned
if [ -n "$CLEANED_ITEMS" ]; then
    echo "Cleaned:$CLEANED_ITEMS"
else
    echo "Nothing to clean"
fi

# Preserve output file
if [ -f "/shared-data/output/sast_ai_output.xlsx" ]; then
    echo "Output file preserved"
fi

echo "Cleanup completed successfully"