"""
Data Processing Constants

Static constants for data processing operations that don't belong in YAML configuration.
"""

LOG_FORMATS = {
    'SPLIT_STAGE_1': "Stage 1: Splitting into train ({train}%) and temp ({temp}%)...",
    'SPLIT_STAGE_2': "Stage 2: Splitting temp into validation ({val}%) and test ({test}%)...",
    'SPLIT_COMPLETE': "✅ Split complete in {elapsed:.1f}s!",
    'SPLIT_SUMMARY': "📊 {split_name}: {count} packages, {issue_count} total issues ({fp_count} FP, {tp_count} TP)",

    'PACKAGE_PROCESSING': "Processing {package_name}...",
    'PACKAGE_SUCCESS': "  ✅ {package_name}: {entry_count} entries, {source_info}",
    'PACKAGE_FAILED': "  ❌ {package_name}: {error}",
    'PACKAGE_SKIPPED': "  ⏭️  Skipped {package_name}: {reason}",
    'PACKAGE_VALIDATION_FAILED': "⚠️  Validation failed for {package_name}: {reason}",

    'FILES_DISCOVERED': "📁 Found {count} Excel files",
    'FILES_EXCLUDED': "   Excluded {count} files based on exclusion list",

    'PROGRESS': "Progress: {current}/{total} ({percent:.1f}%)",
}

PROCESSING_STATUS = {
    'SUCCESS': 'success',
    'PARTIAL_SUCCESS': 'partial_success',
    'FAILED': 'failed',
    'SKIPPED': 'skipped',
    'UNKNOWN': 'unknown',
}

VALIDATION_STATUS = {
    'VALID': 'valid',
    'INVALID': 'invalid',
    'WARNING': 'warning',
}

DATASET_SPLITS = {
    'TRAIN': 'train',
    'VALIDATION': 'validation',
    'TEST': 'test',
}

REPORT_HEADERS = {
    'MAIN': "=" * 80,
    'SUBSECTION': "-" * 60,
    'PROCESSING_SUMMARY': "PROCESSING SUMMARY",
    'VALIDATION_FAILURES': "VALIDATION FAILURES",
    'PACKAGE_DETAILS': "PACKAGE DETAILS",
    'STATISTICS': "STATISTICS",
}
