#!/usr/bin/env python3
"""
Batch process all ground-truth Excel files into ready-to-use LLM input files.

Each output file contains all ground-truth entries with source code for that package.

Validation:
- Files must have at least one justification column ('Hint' or 'Comment') with data
- Files must have 'False Positive?' column with annotations
- Files not meeting criteria are automatically skipped

Usage:
    # Process all files
    python scripts/batch_prepare_patterns.py

    # Process specific files
    python scripts/batch_prepare_patterns.py --pattern "bzip2*.xlsx"

    # Resume interrupted run
    python scripts/batch_prepare_patterns.py --resume

    # Parallel processing
    python scripts/batch_prepare_patterns.py --workers 4

    # Test with limit (process only XX packages)
    python scripts/batch_prepare_patterns.py --limit 5
"""

import sys
import os
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed
import argparse
import logging
from datetime import datetime
import json
import time
from typing import List, Dict, Optional, Tuple
import glob
import pandas as pd
import shutil

sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent.parent / 'src'))

from common.config import Config
from handlers.c_repo_handler import CRepoHandler
from handlers.rpm_source_handler import RpmSourceHandler
from process_mining.src.common.process_mining_config import ProcessMiningConfig
from process_mining.src.services.process_mining_service import ProcessMiningService
from process_mining.src.services.ignore_err_service import IgnoreErrService
from process_mining.src.services.validation_service import ValidationService
from process_mining.src.dto.ProcessMiningEntry import ProcessMiningEntry
from Utils.data_access_utils import ExcelFileDiscovery, PackageNameExtractor

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)


def extract_package_name(excel_path: Path) -> str:
    """Extract package name from Excel filename."""
    return excel_path.stem


def extract_package_name_from_ignore_err(ignore_err_path: Path) -> str:
    """
    Extract package name from ignore.err file path.

    Args:
        ignore_err_path: Path to ignore.err file (e.g., known_non_issue/ModemManager/ignore.err)

    Returns:
        Package name (e.g., "ModemManager")
    """
    return ignore_err_path.parent.name


def extract_nvr_from_ignore_err_filename(ignore_err_path: Path) -> str:
    """
    Extract full NVR from NVR-enriched ignore.err filename.

    Args:
        ignore_err_path: Path to NVR-enriched ignore.err file (e.g., known_non_issue/acl-2.3.2-1.el10_ignore.err)

    Returns:
        Full NVR (e.g., "acl-2.3.2-1.el10")

    Examples:
        >>> extract_nvr_from_ignore_err_filename(Path("train/known_non_issue/acl-2.3.2-1.el10_ignore.err"))
        'acl-2.3.2-1.el10'
        >>> extract_nvr_from_ignore_err_filename(Path("train/known_non_issue/device-mapper-multipath-0.9.7-7.el10_ignore.err"))
        'device-mapper-multipath-0.9.7-7.el10'
    """
    filename = ignore_err_path.name
    if filename.endswith('_ignore.err'):
        return filename[:-11]  # len('_ignore.err') = 11
    return filename.replace('.err', '')


def load_manifest(manifest_path: Path) -> Optional[Dict[str, str]]:
    """
    Load package manifest file.

    Args:
        manifest_path: Path to manifest file (e.g., train_manifest.txt)

    Returns:
        Dict mapping package_name -> pairing_status, or None if file doesn't exist

    Example:
        {'acl': 'paired', 'bluez': 'ignore_err_only', ...}
    """
    if not manifest_path.exists():
        logger.error(f"❌ Manifest file not found: {manifest_path}")
        logger.error(f"❌ Manifest file is required for dataset-specific processing")
        return None

    manifest = {}
    try:
        with open(manifest_path, 'r') as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#'):
                    continue

                parts = line.split('[')
                if len(parts) == 2:
                    package_name = parts[0].strip()
                    pairing_status = parts[1].rstrip(']').strip()
                    manifest[package_name] = pairing_status

        logger.info(f"📋 Loaded manifest with {len(manifest)} packages from {manifest_path.name}")
        return manifest
    except Exception as e:
        logger.error(f"❌ Failed to read manifest file {manifest_path}: {e}")
        return None


def discover_package_data(
    excel_dir: Path,
    known_non_issue_dir: Optional[Path],
    process_mode: str = "both",
    manifest_filter: Optional[Dict[str, str]] = None
) -> List[Tuple[str, Optional[Path], Optional[Path]]]:
    """
    Discover packages from both Excel and NVR-enriched ignore.err sources.

    Args:
        excel_dir: Directory containing Excel files
        known_non_issue_dir: Directory containing NVR-enriched ignore.err files (optional)
        process_mode: Processing mode:
            - "both": Process all packages (paired and excel_only)
            - "paired_only": Only process when Excel + ignore.err exist together
        manifest_filter: Optional dict mapping package_name -> pairing_status.
            If provided, only packages in this dict will be included in results.

    Returns:
        List of (nvr, excel_path, ignore_err_path) tuples
        Note: Returns full NVR, not base package name
    """
    package_map = {}

    pm_config = ProcessMiningConfig()
    discovery = ExcelFileDiscovery(pm_config)
    excel_files = discovery.discover_files(excel_dir, use_full_dataset=False)

    for excel_path in excel_files:
        nvr = extract_package_name(excel_path)
        base_name = PackageNameExtractor.from_nvr(nvr)

        package_map[base_name] = {
            'nvr': nvr,
            'excel_path': excel_path,
            'ignore_err_path': None
        }

    if known_non_issue_dir and known_non_issue_dir.exists():
        ignore_err_files = list(known_non_issue_dir.glob("*_ignore.err"))

        for ignore_err_path in ignore_err_files:
            nvr = extract_nvr_from_ignore_err_filename(ignore_err_path)
            base_name = PackageNameExtractor.from_nvr(nvr)

            if manifest_filter and manifest_filter.get(base_name) != 'paired':
                logger.debug(f"Skipping {base_name} - not [paired]")
                continue

            if base_name in package_map:
                package_map[base_name]['ignore_err_path'] = ignore_err_path
            else:
                logger.warning(f"Found ignore.err without Excel: {base_name}")

    result = []
    for base_name, data in package_map.items():
        if manifest_filter is not None and base_name not in manifest_filter:
            continue

        nvr = data['nvr']
        excel_path = data['excel_path']
        ignore_err_path = data.get('ignore_err_path')

        if process_mode == "paired_only":
            if excel_path and ignore_err_path:
                result.append((nvr, excel_path, ignore_err_path))
        else:
            result.append((nvr, excel_path, ignore_err_path))

    return sorted(result, key=lambda x: x[0])


def validate_package_sources(
    excel_path: Optional[Path],
    ignore_err_path: Optional[Path],
    validate_justifications: bool = True,
    config: Optional[ProcessMiningConfig] = None
) -> Tuple[bool, str]:
    """
    Validate both Excel and ignore.err files.

    Args:
        excel_path: Optional path to Excel file
        ignore_err_path: Optional path to ignore.err file
        validate_justifications: Whether to validate ignore.err justifications
        config: ProcessMiningConfig instance (optional)

    Returns:
        Tuple of (is_valid, reason_if_invalid)

    Examples:
        (True, "") - Package sources are valid
        (False, "Excel: Missing 'Hint' column") - Invalid
    """
    errors = []

    if excel_path:
        validator = ValidationService(config.config_path if config else None)
        is_valid, reason = validator.validate_excel_file(excel_path)
        if not is_valid:
            errors.append(f"Excel: {reason}")

    if ignore_err_path and validate_justifications:
        service = IgnoreErrService()
        is_valid, reason = service.validate_ignore_err_file(ignore_err_path)
        if not is_valid:
            errors.append(f"ignore.err: {reason}")

    if not excel_path and not ignore_err_path:
        return (False, "No Excel or ignore.err file found")

    if errors:
        return (False, "; ".join(errors))

    return (True, "")


def format_entry_for_llm(entry: ProcessMiningEntry, entry_number: int) -> str:
    """
    Format a ProcessMiningEntry for LLM consumption.

    Args:
        entry: ProcessMiningEntry object
        entry_number: Sequential entry number

    Returns:
        Formatted string
    """
    classification = "FALSE POSITIVE" if entry.is_false_positive else "TRUE POSITIVE"

    source_code_formatted = ""
    if entry.source_code:
        for file_path, code in entry.source_code.items():
            source_code_formatted += f"\nSource Code ({file_path}):\n```c\n{code}\n```\n"
    else:
        source_code_formatted = "\nSource Code:\n```c\n(Source code not available)\n```\n"

    comment_formatted = ""
    if entry.comment:
        comment_formatted = f"\nAnalyst Comment: {entry.comment}\n"

    output = f"""---
Entry #{entry_number}:
Issue Type: {entry.issue_type}
CWE: {entry.issue_cwe}

Error Trace:
{entry.error_trace}
{source_code_formatted}
Ground Truth Classification: {classification}
Human Expert Justification: {entry.human_justification}{comment_formatted}
---
"""
    return output


def process_single_package(
    package_nvr: str,
    excel_path: Optional[Path],
    ignore_err_path: Optional[Path],
    repo_handler: Optional[CRepoHandler],
    output_dir: Path,
    skip_source_code: bool = False,
    use_rpm_source: bool = True,
    rpm_cache_dir: str = "/tmp/rpm_cache",
    config_path: Optional[str] = None
) -> Dict:
    """
    Process a single package from Excel and/or ignore.err sources.

    Combines entries from both sources, fetches source code for all entries,
    and generates unified pattern_data output file.

    Args:
        package_nvr: Full package NVR (e.g., "acl-2.3.2-1.el10")
        excel_path: Optional path to Excel file
        ignore_err_path: Optional path to ignore.err file
        repo_handler: CRepoHandler object (or None if unavailable, for Git mode)
        output_dir: Output directory
        skip_source_code: Skip source code fetching
        use_rpm_source: Use RPM source handler instead of Git repo handler
        rpm_cache_dir: RPM cache directory (default: /tmp/rpm_cache)
        config_path: Path to process mining config YAML (optional)

    Returns:
        Dict with processing statistics and status
    """
    output_path = output_dir / f"{package_nvr}.txt"

    result = {
        'package_name': package_nvr,
        'excel_path': str(excel_path) if excel_path else None,
        'ignore_err_path': str(ignore_err_path) if ignore_err_path else None,
        'output_path': str(output_path),
        'status': 'unknown',
        'entry_count': 0,
        'excel_entry_count': 0,
        'ignore_err_entry_count': 0,
        'fp_count': 0,
        'tp_count': 0,
        'entries_with_source': 0,
        'entries_without_source': 0,
        'errors': []
    }

    try:
        logger.info(f"Processing {package_nvr}...")
        entries = []

        if excel_path:
            try:
                service = ProcessMiningService(config_path=config_path)
                metadata_list = service.read_ground_truth_excel(str(excel_path))
                result['excel_entry_count'] = len(metadata_list)

                for metadata in metadata_list:
                    entry = ProcessMiningEntry(
                        issue_type=metadata["issue_type"],
                        issue_cwe=metadata["issue_cwe"],
                        error_trace=metadata["error_trace"],
                        is_false_positive=metadata["is_false_positive"],
                        human_justification=metadata["human_justification"],
                        comment=metadata.get("comment"),
                        source_code={},
                        raw_finding=metadata["raw_finding"],
                    )

                    if entry.is_false_positive:
                        result['fp_count'] += 1
                    else:
                        result['tp_count'] += 1

                    entries.append(entry)

                logger.debug(f"  Loaded {len(metadata_list)} entries from Excel")

            except Exception as e:
                result['errors'].append(f"Excel read error: {str(e)}")
                logger.error(f"  ⚠️  Failed to read Excel: {e}")

        if ignore_err_path:
            try:
                ignore_err_service = IgnoreErrService(config_path=config_path)
                ignore_err_metadata_list = ignore_err_service.read_ignore_err_file(ignore_err_path)
                result['ignore_err_entry_count'] = len(ignore_err_metadata_list)

                for metadata in ignore_err_metadata_list:
                    entry = ProcessMiningEntry(
                        issue_type=metadata["issue_type"],
                        issue_cwe=metadata["issue_cwe"],
                        error_trace=metadata["error_trace"],
                        is_false_positive=metadata["is_false_positive"],
                        human_justification=metadata["human_justification"],
                        comment=metadata.get("comment"),
                        source_code={},
                        raw_finding=metadata["raw_finding"],
                    )

                    if entry.is_false_positive:
                        result['fp_count'] += 1
                    else:
                        result['tp_count'] += 1

                    entries.append(entry)

                logger.debug(f"  Loaded {len(ignore_err_metadata_list)} entries from ignore.err")

            except Exception as e:
                result['errors'].append(f"ignore.err read error: {str(e)}")
                logger.error(f"  ⚠️  Failed to read ignore.err: {e}")

        if not entries:
            result['status'] = 'failed'
            result['errors'].append("No entries loaded from any source")
            logger.error(f"  ❌ No entries found for {package_nvr}")
            return result

        result['entry_count'] = len(entries)

        if not skip_source_code:
            handler = None
            try:
                if use_rpm_source:
                    handler = RpmSourceHandler(
                        package_nvr=package_nvr,
                        cache_dir=rpm_cache_dir
                    )
                    logger.debug(f"Using RPM source handler for {package_nvr}")
                elif repo_handler:
                    handler = repo_handler
                    logger.debug(f"Using Git repo handler for {package_nvr}")

                if handler:
                    for entry in entries:
                        try:
                            source_code_dict = handler.get_source_code_blocks_from_error_trace(
                                entry.error_trace
                            )
                            entry.source_code = source_code_dict
                            if source_code_dict:
                                result['entries_with_source'] += 1
                            else:
                                result['entries_without_source'] += 1
                        except Exception as e:
                            logger.warning(f"    ⚠️  Source code fetch failed for entry: {e}")
                            result['entries_without_source'] += 1
                            result['errors'].append(f"Source code fetch error: {str(e)}")
                else:
                    result['entries_without_source'] = len(entries)

            except Exception as e:
                logger.error(f"  ❌ Failed to initialize source handler: {e}")
                result['errors'].append(f"Handler initialization error: {str(e)}")
                result['entries_without_source'] = len(entries)
            finally:
                if use_rpm_source and handler and hasattr(handler, 'cleanup'):
                    try:
                        handler.cleanup()
                    except Exception as e:
                        logger.warning(f"    ⚠️  Cleanup failed: {e}")
        else:
            result['entries_without_source'] = len(entries)

        if not skip_source_code and result['entries_without_source'] == len(entries):
            result['status'] = 'failed'
            result['errors'].append("All entries failed to fetch source code - skipping file generation")
            logger.error(f"  ❌ {package_nvr}: All {len(entries)} entries failed source code fetch - no file will be generated")
            return result

        generate_package_input_file(entries, package_nvr, output_path)

        result['status'] = 'success' if not result['errors'] else 'partial_success'

        source_info = f"Excel: {result['excel_entry_count']}, ignore.err: {result['ignore_err_entry_count']}"
        logger.info(f"  ✅ {package_nvr}: {result['entry_count']} entries ({source_info}), {result['entries_with_source']} with source code")

    except Exception as e:
        result['status'] = 'failed'
        result['errors'].append(f"Unexpected error: {str(e)}")
        logger.exception(f"  ❌ Unexpected error processing {package_nvr}")

    return result


def generate_package_input_file(
    entries: List[ProcessMiningEntry],
    package_name: str,
    output_path: Path
) -> None:
    """
    Generate file with formatted ground-truth entries.

    Template instructions are in pattern_learning_prompt.md (provided to LLM separately).
    Generated files contain ONLY the data.

    Args:
        entries: List of ProcessMiningEntry objects
        package_name: Package name
        output_path: Output file path
    """
    fp_count = sum(1 for e in entries if e.is_false_positive)
    tp_count = len(entries) - fp_count

    output_lines = []

    output_lines.append("=" * 80)
    output_lines.append(f"GROUND-TRUTH ENTRIES FOR: {package_name}")
    output_lines.append("=" * 80)
    output_lines.append("")
    output_lines.append(f"Package: {package_name}")
    output_lines.append(f"Total Entries: {len(entries)}")
    output_lines.append(f"False Positives: {fp_count}")
    output_lines.append(f"True Positives: {tp_count}")
    output_lines.append("")

    for idx, entry in enumerate(entries, start=1):
        output_lines.append(format_entry_for_llm(entry, idx))
        output_lines.append("")

    output_text = "\n".join(output_lines)
    output_path.write_text(output_text)


def load_processed_files(output_dir: Path) -> set:
    """Load list of already-processed files from summary JSON."""
    summary_file = output_dir / "_processing_summary.json"
    if summary_file.exists():
        try:
            data = json.loads(summary_file.read_text())
            return set(data.get('processed_files', []))
        except:
            return set()
    return set()


def should_skip_file(package_name: str, processed: set, force: bool) -> bool:
    """Check if file should be skipped (already processed)."""
    if force:
        return False
    return package_name in processed


def generate_summary_report(results: List[Dict], skipped_invalid: List[Dict], output_dir: Path, processing_time: float) -> None:
    """
    Generate summary report JSON and error log.

    Args:
        results: List of processing results
        skipped_invalid: List of skipped files due to validation failures
        output_dir: Output directory
        processing_time: Total processing time in seconds
    """
    success = [r for r in results if r['status'] == 'success']
    partial = [r for r in results if r['status'] == 'partial_success']
    failed = [r for r in results if r['status'] == 'failed']

    total_entries = sum(r['entry_count'] for r in results)
    total_excel_entries = sum(r.get('excel_entry_count', 0) for r in results)
    total_ignore_err_entries = sum(r.get('ignore_err_entry_count', 0) for r in results)
    total_fp = sum(r['fp_count'] for r in results)
    total_tp = sum(r['tp_count'] for r in results)
    total_with_source = sum(r['entries_with_source'] for r in results)
    total_without_source = sum(r['entries_without_source'] for r in results)

    paired_packages = sum(1 for r in results if r.get('excel_path') and r.get('ignore_err_path'))
    excel_only_packages = sum(1 for r in results if r.get('excel_path') and not r.get('ignore_err_path'))
    ignore_err_only_packages = sum(1 for r in results if r.get('ignore_err_path') and not r.get('excel_path'))

    summary = {
        'processing_timestamp': datetime.now().isoformat(),
        'total_packages': len(results),
        'successful': len(success),
        'partial_success': len(partial),
        'failed': len(failed),

        'skipped_invalid': len(skipped_invalid),
        'skipped_invalid_files': [
            {
                'file': item['package_name'],
                'reason': item['reason']
            } for item in skipped_invalid
        ],

        'pairing_statistics': {
            'paired_packages': paired_packages,
            'excel_only_packages': excel_only_packages,
            'ignore_err_only_packages': ignore_err_only_packages
        },

        'statistics': {
            'total_entries': total_entries,
            'excel_entries': total_excel_entries,
            'ignore_err_entries': total_ignore_err_entries,
            'total_false_positives': total_fp,
            'total_true_positives': total_tp,
            'entries_with_source_code': total_with_source,
            'entries_without_source_code': total_without_source
        },

        'processed_files': [r['package_name'] for r in results if r['status'] in ['success', 'partial_success']],

        'partial_success_files': [
            {
                'file': r['package_name'],
                'entry_count': r['entry_count'],
                'errors': r['errors']
            } for r in partial
        ],

        'failed_files': [
            {
                'file': r['package_name'],
                'errors': r['errors']
            } for r in failed
        ],

        'processing_time_seconds': round(processing_time, 2),
        'avg_time_per_package': round(processing_time / len(results), 2) if results else 0
    }

    summary_file = output_dir / "_processing_summary.json"
    summary_file.write_text(json.dumps(summary, indent=2))
    logger.info(f"\n✅ Summary report saved to: {summary_file}")

    if partial or failed:
        error_log_lines = []
        for r in partial + failed:
            error_log_lines.append(f"\n{'=' * 80}")
            error_log_lines.append(f"Package: {r['package_name']}")
            error_log_lines.append(f"Status: {r['status']}")
            for error in r['errors']:
                error_log_lines.append(f"  - {error}")

        error_log = output_dir / "_errors.log"
        error_log.write_text("\n".join(error_log_lines))
        logger.info(f"⚠️  Error log saved to: {error_log}")

    print("\n" + "=" * 80)
    print("PROCESSING SUMMARY")
    print("=" * 80)
    print(f"Total Packages Found: {len(results) + len(skipped_invalid)}")
    print(f"  ✅ Successful: {len(success)}")
    print(f"  ⚠️  Partial Success: {len(partial)}")
    print(f"  ❌ Failed: {len(failed)}")
    print(f"  ⏭️  Skipped (Invalid): {len(skipped_invalid)}")
    print(f"\nPackage Pairing:")
    print(f"  Paired (Excel + ignore.err): {paired_packages}")
    print(f"  Excel-only: {excel_only_packages}")
    print(f"  ignore.err-only: {ignore_err_only_packages}")
    print(f"\nTotal Entries: {total_entries}")
    print(f"  From Excel: {total_excel_entries}")
    print(f"  From ignore.err: {total_ignore_err_entries}")
    print(f"  False Positives: {total_fp}")
    print(f"  True Positives: {total_tp}")
    print(f"  With Source Code: {total_with_source}")
    print(f"  Without Source Code: {total_without_source}")
    print(f"\nProcessing Time: {processing_time:.2f}s ({processing_time/60:.1f}m)")
    print(f"Avg Time/Package: {processing_time/len(results):.2f}s" if results else "N/A")
    print("=" * 80)


def batch_process(args):
    """Main batch processing function."""
    start_time = time.time()

    config = ProcessMiningConfig(args.config if hasattr(args, 'config') and args.config else None)
    prep_config = config.get_preparation_config()

    base_dir = Path(__file__).parent.parent.parent

    ground_truth_dir = base_dir / prep_config.get('ground_truth_dir', 'process_mining/data/ground-truth')

    dataset_dir = ground_truth_dir / args.dataset
    if not dataset_dir.exists():
        logger.error(f"❌ Dataset directory not found: {dataset_dir}")
        logger.info(f"Available subdirectories in {ground_truth_dir}:")
        for subdir in sorted(ground_truth_dir.iterdir()):
            if subdir.is_dir():
                logger.info(f"  - {subdir.name}")
        sys.exit(1)

    known_non_issue_dir = dataset_dir / 'known_non_issue'
    if not known_non_issue_dir.exists():
        known_non_issue_dir = None
        logger.info("ℹ️  No dataset-specific known_non_issue directory found, processing Excel files only")
    else:
        logger.info(f"📁 Using dataset-specific known_non_issue directory: {known_non_issue_dir}")

    process_mode = getattr(args, 'ignore_err_mode', None) or prep_config.get('ignore_err_mode', 'both')
    validate_justifications = getattr(args, 'validate_ignore_err', None)
    if validate_justifications is None:
        validate_justifications = prep_config.get('validate_ignore_err_justifications', True)

    logger.info(f"📋 ignore.err processing mode: {process_mode}")
    logger.info(f"📋 Validate justifications: {validate_justifications}")

    dataset_name = args.dataset
    manifest_path = ground_truth_dir / f"{dataset_name}_manifest.txt"
    manifest_filter = load_manifest(manifest_path)

    if manifest_filter is None:
        logger.error(f"❌ Failed to load manifest file: {manifest_path}")
        logger.error(f"❌ Manifest file is required for dataset-specific processing")
        sys.exit(1)

    logger.info(f"📋 Loaded manifest with {len(manifest_filter)} packages")
    logger.info(f"📋 Package discovery will be filtered by manifest")

    excel_dir = dataset_dir / 'excel'
    package_data = discover_package_data(excel_dir, known_non_issue_dir, process_mode, manifest_filter)
    if not package_data:
        logger.error(f"❌ No packages found in {dataset_dir}")
        sys.exit(1)

    logger.info(f"📁 Found {len(package_data)} packages")

    if args.output_dir:
        output_dir = Path(args.output_dir)
    else:
        default_output_base = base_dir / prep_config.get('output_dir', 'process_mining/data/pattern_data')
        if dataset_name:
            output_dir = default_output_base / f"{dataset_name}_pattern_data"
            logger.info(f"📂 Output directory: {output_dir} (based on source: {dataset_name})")
        else:
            output_dir = default_output_base
            logger.info(f"📂 Output directory: {output_dir}")

    output_dir.mkdir(parents=True, exist_ok=True)

    processed_files = load_processed_files(output_dir) if args.resume else set()
    if args.resume and processed_files:
        logger.info(f"📋 Resume mode: {len(processed_files)} files already processed")

    packages_to_process = []
    skipped_invalid = []

    for pkg_nvr, excel_path, ignore_err_path in package_data:
        if should_skip_file(pkg_nvr, processed_files, args.force):
            logger.info(f"⏭️  Skipping {pkg_nvr} (already processed)")
            continue

        is_valid, reason = validate_package_sources(excel_path, ignore_err_path, validate_justifications, config)
        if not is_valid:
            logger.warning(f"⚠️  Skipping {pkg_nvr}: {reason}")
            skipped_invalid.append({
                'package_name': pkg_nvr,
                'reason': reason
            })
            continue

        packages_to_process.append((pkg_nvr, excel_path, ignore_err_path))

    if skipped_invalid:
        logger.info(f"\n⚠️  Skipped {len(skipped_invalid)} packages due to validation failures")

    if args.limit:
        packages_to_process = packages_to_process[:args.limit]

    logger.info(f"🚀 Processing {len(packages_to_process)} packages...")

    if args.dry_run:
        print("\n🔍 DRY RUN - Would process:")
        for pkg_nvr, excel_path, ignore_err_path in packages_to_process:
            sources = []
            if excel_path:
                sources.append("Excel")
            if ignore_err_path:
                sources.append("ignore.err")
            print(f"  - {pkg_nvr} ({', '.join(sources)})")
        return

    source_mode = args.source_mode if args.source_mode else prep_config.get('default_source_mode', 'rpm')
    use_rpm_source = (source_mode == "rpm")
    rpm_cache_dir = args.rpm_cache_dir if args.rpm_cache_dir else prep_config.get('rpm_cache_dir', '/tmp/rpm_cache')

    repo_handler = None

    if not args.skip_source_code and not use_rpm_source:
        try:
            main_config = Config()
            repo_handler = CRepoHandler(main_config)
            logger.info(f"✅ Initialized repo handler: {main_config.REPO_LOCAL_PATH}")
        except Exception as e:
            logger.warning(f"⚠️  Could not initialize config/repo handler: {e}")
            logger.warning("⚠️  Git source code fetching will be skipped")
            args.skip_source_code = True
    elif use_rpm_source and not args.skip_source_code:
        logger.info(f"✅ Using RPM source mode (cache: {rpm_cache_dir})")

    results = []

    config_path = args.config if hasattr(args, 'config') and args.config else None

    if args.workers > 1:
        with ThreadPoolExecutor(max_workers=args.workers) as executor:
            futures = {
                executor.submit(
                    process_single_package,
                    pkg_nvr,
                    excel_path,
                    ignore_err_path,
                    repo_handler,
                    output_dir,
                    args.skip_source_code,
                    use_rpm_source,
                    rpm_cache_dir,
                    config_path
                ): (pkg_nvr, excel_path, ignore_err_path) for pkg_nvr, excel_path, ignore_err_path in packages_to_process
            }

            for future in as_completed(futures):
                result = future.result()
                results.append(result)
    else:
        for pkg_nvr, excel_path, ignore_err_path in packages_to_process:
            result = process_single_package(
                pkg_nvr,
                excel_path,
                ignore_err_path,
                repo_handler,
                output_dir,
                args.skip_source_code,
                use_rpm_source,
                rpm_cache_dir,
                config_path
            )
            results.append(result)

    processing_time = time.time() - start_time
    generate_summary_report(results, skipped_invalid, output_dir, processing_time)

    logger.info(f"\n✅ All done! Output files in: {output_dir}")


def main():
    parser = argparse.ArgumentParser(
        description="Batch process ground-truth Excel files for pattern learning",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Process all files
  python scripts/batch_prepare_patterns.py

  # Process specific pattern
  python scripts/batch_prepare_patterns.py --pattern "bzip2*.xlsx"

  # Resume interrupted run
  python scripts/batch_prepare_patterns.py --resume

  # Parallel processing with 4 workers
  python scripts/batch_prepare_patterns.py --workers 4

  # Test with 5 packages
  python scripts/batch_prepare_patterns.py --limit 5
        """
    )

    parser.add_argument(
        "--config",
        type=str,
        help="Path to process mining config YAML (default: config/process_mining_config.yaml)"
    )
    parser.add_argument(
        "--pattern",
        default="*.xlsx",
        help="Glob pattern for Excel files (default: *.xlsx)"
    )
    parser.add_argument(
        "--dataset",
        choices=["full_dataset", "train", "validation", "test"],
        default="train",
        help="Which dataset subdirectory to process (default: train)"
    )
    parser.add_argument(
        "--output-dir",
        help="Output directory (default: from config - pattern_data/)"
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=1,
        help="Number of parallel workers (default: 1)"
    )
    parser.add_argument(
        "--limit",
        type=int,
        help="Limit number of packages to process (for testing)"
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Resume: skip already-processed files"
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Force re-process all files (ignore cache)"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be processed without actually processing"
    )
    parser.add_argument(
        "--skip-source-code",
        action="store_true",
        help="Skip source code fetching (faster, but less useful)"
    )
    parser.add_argument(
        "--source-mode",
        choices=["rpm", "git"],
        default=None,
        help="Source code fetching mode (default: from config - rpm)"
    )
    parser.add_argument(
        "--rpm-cache-dir",
        default=None,
        help="RPM cache directory (default: from config - /tmp/rpm_cache)"
    )
    parser.add_argument(
        "--clear-rpm-cache",
        action="store_true",
        help="Clear RPM cache before processing"
    )
    parser.add_argument(
        "--ignore-err-mode",
        choices=["both", "paired_only"],
        default=None,
        help="How to process ignore.err files: both (paired+excel_only), paired_only (default: from config - both)"
    )
    parser.add_argument(
        "--validate-ignore-err",
        action="store_true",
        default=None,
        help="Validate ignore.err files for justification comments (default: from config - true)"
    )
    parser.add_argument(
        "--no-validate-ignore-err",
        dest="validate_ignore_err",
        action="store_false",
        help="Skip ignore.err validation"
    )

    args = parser.parse_args()

    if args.clear_rpm_cache:
        config = ProcessMiningConfig(args.config if args.config else None)
        prep_config = config.get_preparation_config()
        cache_dir = args.rpm_cache_dir if args.rpm_cache_dir else prep_config.get('rpm_cache_dir', '/tmp/rpm_cache')

        if os.path.exists(cache_dir):
            logger.info(f"Clearing RPM cache: {cache_dir}")
            shutil.rmtree(cache_dir)
            os.makedirs(cache_dir, exist_ok=True)

    try:
        batch_process(args)
    except KeyboardInterrupt:
        logger.info("\n\n⚠️  Interrupted by user")
        sys.exit(1)
    except Exception as e:
        logger.exception(f"\n\n❌ Fatal error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
