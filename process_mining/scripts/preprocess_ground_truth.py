#!/usr/bin/env python3
"""
Preprocess Ground-Truth Data - Identify Invalid Files

This script scans all ground-truth Excel files and identifies which ones should
be excluded from train/validation/test datasets due to missing or incomplete annotations.

It generates three output files:
1. invalid_files.txt - Simple list of invalid files
2. invalid_files_detailed.json - Detailed breakdown with reasons
3. preprocessing_report.txt - Human-readable summary report

Files are considered invalid if they lack:
- Justification data (neither 'Hint' nor 'Comment' columns have data)
- Annotation data ('False Positive?' column is missing or empty)

Usage:
    # Standard run
    python scripts/preprocess_ground_truth.py

    # Custom directory
    python scripts/preprocess_ground_truth.py --ground-truth-dir /path/to/data

    # With output prefix
    python scripts/preprocess_ground_truth.py --output-prefix "2025-12-28_"
"""

import sys
import os
from pathlib import Path
import argparse
import logging
import json
from datetime import datetime
from typing import List, Dict, Tuple
from collections import defaultdict

sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent.parent / 'src'))

from process_mining.src.common.process_mining_config import ProcessMiningConfig
from process_mining.src.services.validation_service import ValidationService
from Utils.data_access_utils import ExcelFileDiscovery

import pandas as pd
from typing import Optional
import requests
import urllib3

urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

from Utils.rpm_utils import parse_nvr, construct_brew_url, NvrParseError

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)


def check_brew_connectivity() -> Tuple[bool, Optional[str]]:
    """
    Test connectivity to Red Hat Brew before processing files.

    This pre-flight check ensures VPN is connected and Brew is accessible.
    Uses a known package URL to test connectivity.

    Returns:
        Tuple of (is_connected, error_message)
        - (True, None): Brew is accessible
        - (False, "error message"): Connection failed
    """
    test_url = "https://download.devel.redhat.com/brewroot/vol/rhel-10/packages/bash/5.2.26/4.el10/src/bash-5.2.26-4.el10.src.rpm"

    logger.info("Testing connectivity to Red Hat Brew...")

    try:
        response = requests.head(
            test_url,
            timeout=30,
            allow_redirects=True,
            verify=False
        )

        if response.status_code in [200, 302, 404, 403]:
            logger.info(f"✓ Brew connectivity verified (HTTP {response.status_code})")
            return (True, None)
        else:
            logger.warning(f"Unexpected HTTP status: {response.status_code}")
            return (True, None)

    except requests.exceptions.ConnectionError as e:
        error_msg = (
            "Cannot reach Red Hat Brew: Connection refused.\n"
            "This usually means:\n"
            "  1. VPN is not connected\n"
            "  2. Network is down\n"
            "  3. Brew server is unavailable\n\n"
            f"Technical details: {str(e)}"
        )
        return (False, error_msg)

    except requests.exceptions.Timeout:
        error_msg = (
            "Cannot reach Red Hat Brew: Connection timeout.\n"
            "This usually means VPN is not connected or network is very slow."
        )
        return (False, error_msg)

    except Exception as e:
        error_msg = f"Network error testing Brew connectivity: {str(e)}"
        return (False, error_msg)


def check_source_code_availability(excel_path: Path) -> Tuple[bool, str]:
    """
    Check if source code is available in Red Hat Brew for this package.

    Parses NVR from Excel filename, constructs Brew URL, and performs
    HTTP HEAD request to verify source RPM exists.

    Args:
        excel_path: Path to Excel file (filename encodes NVR)

    Returns:
        Tuple of (is_available, reason_if_unavailable)
    """
    try:
        nvr_info = parse_nvr(excel_path.name)
        package = nvr_info['package']
        version = nvr_info['version']
        release = nvr_info['release']
        rhel_version = nvr_info['rhel_version']

    except NvrParseError as e:
        return (False, f"Failed to parse NVR from filename '{excel_path.name}': {str(e)}")

    url = construct_brew_url(package, version, release, rhel_version)

    try:
        response = requests.head(
            url,
            timeout=60,
            allow_redirects=True,
            verify=False
        )

        if response.status_code == 200:
            logger.debug(f"✓ Source available: {excel_path.name}")
            return (True, "")
        elif response.status_code == 404:
            return (False, f"Source RPM not found in Brew (HTTP 404): {url}")
        elif response.status_code == 403:
            return (False, f"Access denied to source RPM (HTTP 403): {url}")
        else:
            return (False, f"Unexpected HTTP status {response.status_code}: {url}")

    except requests.exceptions.Timeout:
        return (False, f"Timeout checking source availability for {package}-{version}-{release}")
    except requests.exceptions.RequestException as e:
        return (False, f"Network error checking source availability: {str(e)}")
    except Exception as e:
        return (False, f"Unexpected error checking source availability: {str(e)}")


def scan_ground_truth_directory(ground_truth_dir: Path, config: Optional[ProcessMiningConfig] = None, check_source_availability: bool = True) -> Dict:
    """
    Scan all Excel files and categorize them as valid or invalid.

    Args:
        ground_truth_dir: Path to ground-truth directory
        config: ProcessMiningConfig instance (optional)
        check_source_availability: Whether to verify source RPMs exist in Brew

    Returns:
        Dict with validation results and statistics
    """
    logger.info(f"Scanning directory: {ground_truth_dir}")

    if not config:
        config = ProcessMiningConfig()
    discovery = ExcelFileDiscovery(config)
    excel_files = discovery.discover_files(ground_truth_dir, use_full_dataset=True)
    logger.info(f"Found {len(excel_files)} Excel files")

    if check_source_availability:
        logger.info(f"Validating files and checking source code availability in Brew (this may take several minutes)...")
    else:
        logger.info(f"Validating files...")

    validator = ValidationService(config.config_path)

    valid_files = []
    invalid_files = []
    category_counts = defaultdict(int)

    for excel_file in excel_files:
        package_name = excel_file.stem

        is_valid, reason = validator.validate_excel_file(excel_file)

        if is_valid and check_source_availability:
            is_available, availability_reason = check_source_code_availability(excel_file)
            if not is_available:
                is_valid = False
                reason = availability_reason

        if is_valid:
            valid_files.append({
                'filename': excel_file.name,
                'package_name': package_name,
                'path': str(excel_file)
            })
        else:
            category = validator.categorize_validation_failure(reason)
            category_counts[category] += 1

            invalid_files.append({
                'filename': excel_file.name,
                'package_name': package_name,
                'reason': reason,
                'validation_category': category,
                'path': str(excel_file)
            })

    logger.info(f"Validation complete: {len(valid_files)} valid, {len(invalid_files)} invalid")

    return {
        'total_files': len(excel_files),
        'valid_files': valid_files,
        'invalid_files': invalid_files,
        'category_counts': dict(category_counts)
    }


def generate_invalid_files_txt(invalid_files: List[Dict], output_path: Path):
    """Generate simple text file listing invalid files."""
    logger.info(f"Generating {output_path.name}...")

    with open(output_path, 'w') as f:
        f.write("# Files excluded from train/validation/test datasets due to validation failures\n")
        f.write(f"# Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write(f"# Total invalid: {len(invalid_files)}\n")
        f.write("\n")

        for item in sorted(invalid_files, key=lambda x: x['filename']):
            f.write(f"{item['filename']}\n")

    logger.info(f"Created {output_path}")


def generate_invalid_files_json(scan_results: Dict, output_path: Path):
    """Generate detailed JSON file with validation results."""
    logger.info(f"Generating {output_path.name}...")

    data = {
        'metadata': {
            'generation_timestamp': datetime.now().isoformat(),
            'total_files_scanned': scan_results['total_files'],
            'valid_files': len(scan_results['valid_files']),
            'invalid_files': len(scan_results['invalid_files'])
        },
        'invalid_files': [
            {
                'filename': item['filename'],
                'package_name': item['package_name'],
                'reason': item['reason'],
                'validation_category': item['validation_category']
            }
            for item in sorted(scan_results['invalid_files'], key=lambda x: x['filename'])
        ],
        'summary_by_category': scan_results['category_counts']
    }

    with open(output_path, 'w') as f:
        json.dump(data, f, indent=2)

    logger.info(f"Created {output_path}")


def generate_preprocessing_report(scan_results: Dict, output_path: Path, ground_truth_dir: Path):
    """Generate human-readable summary report."""
    logger.info(f"Generating {output_path.name}...")

    total_files = scan_results['total_files']
    valid_count = len(scan_results['valid_files'])
    invalid_count = len(scan_results['invalid_files'])
    category_counts = scan_results['category_counts']

    report_lines = []
    report_lines.append("=" * 80)
    report_lines.append("GROUND-TRUTH DATA PREPROCESSING REPORT")
    report_lines.append("=" * 80)
    report_lines.append("")
    report_lines.append(f"Scan Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    report_lines.append(f"Directory: {ground_truth_dir}")
    report_lines.append("")

    report_lines.append("## Summary")
    report_lines.append(f"Total Files Scanned: {total_files}")
    report_lines.append(f"Valid Files: {valid_count} ({valid_count/total_files*100:.1f}%)")
    report_lines.append(f"Invalid Files: {invalid_count} ({invalid_count/total_files*100:.1f}%)")
    report_lines.append("")

    if category_counts:
        report_lines.append("## Validation Failures by Category")
        category_labels = {
            'no_justification': 'Missing justification columns',
            'empty_justification': 'Empty justification data',
            'missing_fp_column': 'Missing FP column',
            'empty_fp_annotations': 'Empty FP annotations',
            'read_error': 'Read errors',
            'source_code_unavailable': 'Source code unavailable in Brew',
            'nvr_parse_error': 'Invalid NVR format',
            'other': 'Other errors'
        }

        for category, count in sorted(category_counts.items(), key=lambda x: x[1], reverse=True):
            label = category_labels.get(category, category)
            pct = count / invalid_count * 100 if invalid_count > 0 else 0
            report_lines.append(f"{label}: {count} ({pct:.1f}%)")
        report_lines.append("")

    report_lines.append("## Valid Files Statistics")
    report_lines.append("These files are suitable for train/validation/test splitting:")
    report_lines.append(f"- {valid_count} packages")
    report_lines.append("- Ready for stratified sampling")
    report_lines.append("- These files will be used by split_train_val_test.py")
    report_lines.append("")

    if invalid_count > 0:
        report_lines.append("## Invalid Files")
        report_lines.append("See invalid_files.txt for simple list")
        report_lines.append("See invalid_files_detailed.json for detailed breakdown")
        report_lines.append("")

    report_lines.append("## Next Steps")
    report_lines.append("1. Review invalid files to ensure no important data is excluded")
    report_lines.append("2. Run split_train_val_test.py which will automatically skip invalid files")
    report_lines.append("3. Invalid files remain in ground-truth/ but won't be included in train/validation/test splits")
    report_lines.append("")
    report_lines.append("=" * 80)

    report_text = "\n".join(report_lines)

    with open(output_path, 'w') as f:
        f.write(report_text)

    print("\n" + report_text)

    logger.info(f"Created {output_path}")


def main():
    parser = argparse.ArgumentParser(
        description="Preprocess ground-truth data to identify invalid files",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Standard run
  python scripts/preprocess_ground_truth.py

  # Custom directory
  python scripts/preprocess_ground_truth.py --ground-truth-dir /path/to/data

  # With output prefix
  python scripts/preprocess_ground_truth.py --output-prefix "2025-12-28_"
        """
    )

    parser.add_argument(
        '--ground-truth-dir',
        type=str,
        help='Path to ground-truth directory (default: from config)'
    )
    parser.add_argument(
        '--config',
        type=str,
        help='Path to process mining config YAML (default: config/process_mining_config.yaml)'
    )
    parser.add_argument(
        '--output-prefix',
        type=str,
        default='',
        help='Prefix for output files (e.g., "2025-12-28_")'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Perform validation checks without writing output files'
    )

    args = parser.parse_args()

    if args.ground_truth_dir:
        ground_truth_dir = Path(args.ground_truth_dir)
    else:
        config = ProcessMiningConfig(args.config)
        prep_config = config.get_preparation_config()
        base_dir = Path(__file__).parent.parent.parent
        ground_truth_dir = base_dir / prep_config.get('ground_truth_dir', 'data/ground-truth')

    if not ground_truth_dir.exists():
        logger.error(f"Ground-truth directory not found: {ground_truth_dir}")
        sys.exit(1)

    logger.info(f"Ground-truth directory: {ground_truth_dir}")

    is_connected, error_msg = check_brew_connectivity()
    if not is_connected:
        logger.error("=" * 80)
        logger.error("CONNECTIVITY CHECK FAILED")
        logger.error("=" * 80)
        logger.error(error_msg)
        logger.error("")
        logger.error("Please resolve connectivity issues and try again.")
        logger.error("Preprocessing aborted to avoid marking all files as invalid.")
        logger.error("=" * 80)
        sys.exit(1)

    logger.info("✓ Brew connectivity verified")

    config = ProcessMiningConfig(args.config) if args.config else None

    scan_results = scan_ground_truth_directory(ground_truth_dir, config)

    if args.dry_run:
        logger.info("\n" + "=" * 80)
        logger.info("DRY RUN MODE - No files will be written")
        logger.info("=" * 80)
        logger.info(f"\nValidation Summary:")
        logger.info(f"  Total files: {scan_results['total_files']}")
        logger.info(f"  Valid files: {len(scan_results['valid_files'])}")
        logger.info(f"  Invalid files: {len(scan_results['invalid_files'])}")

        if scan_results['category_counts']:
            logger.info(f"\nInvalid File Categories:")
            for category, count in sorted(scan_results['category_counts'].items(), key=lambda x: x[1], reverse=True):
                logger.info(f"  - {category}: {count}")

        if scan_results['invalid_files']:
            logger.info(f"\nInvalid Files:")
            for item in sorted(scan_results['invalid_files'], key=lambda x: x['filename'])[:10]:
                logger.info(f"  - {item['filename']}: {item['validation_category']}")
            if len(scan_results['invalid_files']) > 10:
                logger.info(f"  ... and {len(scan_results['invalid_files']) - 10} more")

        logger.info("\nDry run complete! Use without --dry-run to write output files.")
    else:
        prefix = args.output_prefix

        invalid_txt_path = ground_truth_dir / f"{prefix}invalid_files.txt"
        invalid_json_path = ground_truth_dir / f"{prefix}invalid_files_detailed.json"
        report_path = ground_truth_dir / f"{prefix}preprocessing_report.txt"

        generate_invalid_files_txt(scan_results['invalid_files'], invalid_txt_path)
        generate_invalid_files_json(scan_results, invalid_json_path)
        generate_preprocessing_report(scan_results, report_path, ground_truth_dir)

        logger.info("\nPreprocessing complete!")
        logger.info(f"Output files:")
        logger.info(f"  - {invalid_txt_path}")
        logger.info(f"  - {invalid_json_path}")
        logger.info(f"  - {report_path}")


if __name__ == "__main__":
    main()
