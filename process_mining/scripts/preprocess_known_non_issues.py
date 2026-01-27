#!/usr/bin/env python3
"""
Preprocess Known Non-Issues Data - Pair and Rename with NVR

This script processes the full_dataset directory to extract only [paired] ignore.err files
and renames them with NVR (Name-Version-Release) from paired Excel filenames.

It pairs ignore.err files with their corresponding Excel files by matching base package names,
then extracts the full NVR from the Excel filename and renames the ignore.err file accordingly.

Usage:
    # Standard run
    python scripts/preprocess_known_non_issues.py

    # Custom directories
    python scripts/preprocess_known_non_issues.py --full-dataset-dir /path/to/full_dataset
"""

import sys
import os
from pathlib import Path
import argparse
import logging
import shutil
from datetime import datetime
from typing import List, Dict, Tuple, Optional

sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent.parent / 'src'))

from process_mining.src.common.process_mining_config import ProcessMiningConfig
from process_mining.src.services.ignore_err_service import IgnoreErrService
from Utils.rpm_utils import parse_nvr, NvrParseError

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)


def discover_excel_files(excel_dir: Path) -> Dict[str, Path]:
    """
    Discover Excel files and build mapping from base package name to Excel path.

    Args:
        excel_dir: Directory containing Excel files

    Returns:
        Dict mapping base_package_name -> excel_path
        Example: {'acl': Path('acl-2.3.2-1.el10.xlsx'), ...}
    """
    if not excel_dir.exists():
        logger.error(f"Excel directory not found: {excel_dir}")
        return {}

    excel_files = list(excel_dir.glob("*.xlsx"))
    excel_files.extend(excel_dir.glob("*.xls"))

    excel_files = [f for f in excel_files if f.is_file() and not f.name.startswith('~')]

    excel_mapping = {}

    for excel_path in excel_files:
        try:
            nvr_info = parse_nvr(excel_path.name)
            base_name = nvr_info['package']

            if base_name in excel_mapping:
                logger.warning(f"Duplicate package found: {base_name} - using {excel_path.name}")

            excel_mapping[base_name] = excel_path

        except NvrParseError as e:
            logger.warning(f"Failed to parse NVR from {excel_path.name}: {e}")
            continue

    logger.info(f"Discovered {len(excel_mapping)} Excel files with valid NVR")
    return excel_mapping


def extract_nvr_from_excel_filename(excel_path: Path) -> str:
    """
    Extract full NVR from Excel filename.

    Args:
        excel_path: Path to Excel file

    Returns:
        Full NVR string (e.g., "acl-2.3.2-1.el10")
    """
    filename = excel_path.name
    nvr = filename.replace('.xlsx', '').replace('.xls', '')
    return nvr


def match_ignore_err_to_excel(
    ignore_err_files: List[Path],
    excel_mapping: Dict[str, Path]
) -> Tuple[List[Dict], List[Dict]]:
    """
    Match ignore.err files to Excel files by base package name.

    Args:
        ignore_err_files: List of paths to ignore.err files
        excel_mapping: Dict mapping base_package_name -> excel_path

    Returns:
        Tuple of (matched_pairs, skipped)
        - matched_pairs: List of dicts with package, nvr, ignore_err_path, excel_path
        - skipped: List of dicts with package and reason
    """
    matched_pairs = []
    skipped = []

    for ignore_err_path in ignore_err_files:
        package_name = ignore_err_path.parent.name

        if package_name in excel_mapping:
            excel_path = excel_mapping[package_name]
            nvr = extract_nvr_from_excel_filename(excel_path)

            matched_pairs.append({
                'package': package_name,
                'nvr': nvr,
                'ignore_err_path': ignore_err_path,
                'excel_path': excel_path
            })
            logger.debug(f"Matched {package_name} -> {nvr}")
        else:
            skipped.append({
                'package': package_name,
                'reason': 'No matching Excel file found'
            })
            logger.debug(f"Skipped {package_name} - no matching Excel file")

    logger.info(f"Matched {len(matched_pairs)} pairs, skipped {len(skipped)}")
    return matched_pairs, skipped


def copy_and_rename_ignore_err_files(
    matched_pairs: List[Dict],
    output_dir: Path,
    validate_justifications: bool = True
) -> Tuple[List[Dict], List[Dict]]:
    """
    Copy and rename ignore.err files with NVR.

    Args:
        matched_pairs: List of matched pair dicts
        output_dir: Output directory
        validate_justifications: Whether to validate justification comments

    Returns:
        Tuple of (processed, validation_failed)
    """
    processed = []
    validation_failed = []

    service = IgnoreErrService()

    for pair in matched_pairs:
        package = pair['package']
        nvr = pair['nvr']
        ignore_err_path = pair['ignore_err_path']

        if validate_justifications:
            is_valid, reason = service.validate_ignore_err_file(ignore_err_path)
            if not is_valid:
                validation_failed.append({
                    'package': package,
                    'nvr': nvr,
                    'reason': reason,
                    'path': str(ignore_err_path)
                })
                logger.warning(f"Validation failed for {package}: {reason}")
                continue

        output_filename = f"{nvr}_ignore.err"
        output_path = output_dir / output_filename

        try:
            shutil.copy2(ignore_err_path, output_path)
            processed.append({
                'package': package,
                'nvr': nvr,
                'input_path': str(ignore_err_path),
                'output_path': str(output_path)
            })
            logger.debug(f"Copied {package} -> {output_filename}")
        except Exception as e:
            validation_failed.append({
                'package': package,
                'nvr': nvr,
                'reason': f"Copy failed: {str(e)}",
                'path': str(ignore_err_path)
            })
            logger.error(f"Failed to copy {package}: {e}")

    logger.info(f"Processed {len(processed)} files, validation failed: {len(validation_failed)}")
    return processed, validation_failed


def generate_preprocessing_report(
    processed: List[Dict],
    skipped: List[Dict],
    validation_failed: List[Dict],
    output_dir: Path,
    datasets_processed: List[str]
):
    """
    Generate comprehensive preprocessing report.

    Args:
        processed: List of successfully processed files
        skipped: List of skipped files (no Excel match)
        validation_failed: List of files that failed validation
        output_dir: Output directory
        datasets_processed: List of dataset names processed
    """
    report_lines = []
    report_lines.append("=" * 80)
    report_lines.append("KNOWN NON-ISSUES (FALSE POSITIVES) PREPROCESSING REPORT")
    report_lines.append("NVR-BASED PAIRING")
    report_lines.append("=" * 80)
    report_lines.append("")
    report_lines.append(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    report_lines.append(f"Datasets: {', '.join(datasets_processed)}")
    report_lines.append(f"Output Directory: {output_dir}")
    report_lines.append("")

    total_input = len(processed) + len(skipped) + len(validation_failed)

    report_lines.append("## Summary")
    report_lines.append(f"Total ignore.err Files Found: {total_input}")
    report_lines.append(f"Successfully Processed: {len(processed)}")
    report_lines.append(f"Skipped (No Excel Match): {len(skipped)}")
    report_lines.append(f"Validation Failed: {len(validation_failed)}")
    report_lines.append("")

    if processed:
        report_lines.append("## Successfully Processed Files")
        report_lines.append(f"Total: {len(processed)}")
        report_lines.append("")
        report_lines.append("Sample output files:")
        for item in sorted(processed, key=lambda x: x['package'])[:10]:
            output_file = Path(item['output_path']).name
            report_lines.append(f"  - {item['package']} -> {output_file}")
        if len(processed) > 10:
            report_lines.append(f"  ... and {len(processed) - 10} more")
        report_lines.append("")

    if skipped:
        report_lines.append("## Skipped Files (No Excel Match)")
        report_lines.append(f"Total: {len(skipped)}")
        report_lines.append("")
        for item in sorted(skipped, key=lambda x: x['package']):
            report_lines.append(f"  - {item['package']}: {item['reason']}")
        report_lines.append("")

    if validation_failed:
        report_lines.append("## Validation Failed Files")
        report_lines.append(f"Total: {len(validation_failed)}")
        report_lines.append("")
        for item in sorted(validation_failed, key=lambda x: x['package']):
            report_lines.append(f"  - {item['package']} ({item['nvr']}): {item['reason']}")
        report_lines.append("")

    report_lines.append("## Next Steps")
    report_lines.append("1. Review skipped and validation failed files")
    report_lines.append("2. Run split_train_val_test.py to create train/validation/test splits")
    report_lines.append("3. Run batch_prepare_patterns.py to generate pattern data")
    report_lines.append("")
    report_lines.append("=" * 80)

    report_text = "\n".join(report_lines)

    report_path = output_dir / "preprocessing_report.txt"
    report_path.write_text(report_text)

    print("\n" + report_text)

    logger.info(f"Report saved to: {report_path}")


def main():
    parser = argparse.ArgumentParser(
        description="Preprocess known non-issues: pair ignore.err with Excel files and rename with NVR",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Standard run
  python scripts/preprocess_known_non_issues.py

  # Custom directory
  python scripts/preprocess_known_non_issues.py --full-dataset-dir /path/to/full_dataset

  # Skip validation
  python scripts/preprocess_known_non_issues.py --no-validate-justifications
        """
    )

    parser.add_argument(
        '--ground-truth-dir',
        type=str,
        help='Base ground-truth directory (default: process_mining/data/ground-truth)'
    )
    parser.add_argument(
        '--full-dataset-dir',
        type=str,
        help='Full dataset directory (default: ground-truth/full_dataset)'
    )
    parser.add_argument(
        '--output-dir',
        type=str,
        help='Output directory (default: ground-truth/processed_known_non_issues)'
    )
    parser.add_argument(
        '--validate-justifications',
        action='store_true',
        default=True,
        help='Validate ignore.err files for justification comments (default: True)'
    )
    parser.add_argument(
        '--no-validate-justifications',
        dest='validate_justifications',
        action='store_false',
        help='Skip ignore.err validation'
    )

    args = parser.parse_args()

    base_dir = Path(__file__).parent.parent
    ground_truth_dir = Path(args.ground_truth_dir or base_dir / 'data' / 'ground-truth')
    full_dataset_dir = Path(args.full_dataset_dir or ground_truth_dir / 'full_dataset')

    output_dir = Path(args.output_dir or ground_truth_dir / 'processed_known_non_issues')

    logger.info("=" * 80)
    logger.info("Starting Known Non-Issues Preprocessing - NVR-Based Pairing")
    logger.info("=" * 80)
    logger.info(f"Full dataset directory: {full_dataset_dir}")
    logger.info(f"Output directory: {output_dir}")
    logger.info(f"Validate justifications: {args.validate_justifications}")
    logger.info("")

    if not full_dataset_dir.exists():
        logger.error(f"Full dataset directory not found: {full_dataset_dir}")
        sys.exit(1)

    excel_dir = full_dataset_dir / 'excel'
    if not excel_dir.exists():
        excel_dir = full_dataset_dir
        logger.info(f"Using Excel files from: {excel_dir}")

    excel_mapping = discover_excel_files(excel_dir)

    if not excel_mapping:
        logger.error("No Excel files with valid NVR found")
        sys.exit(1)

    known_non_issue_dir = ground_truth_dir / 'known_non_issue'
    if not known_non_issue_dir.exists():
        logger.error(f"No known_non_issue directory found: {known_non_issue_dir}")
        sys.exit(1)

    ignore_err_files = []
    for package_dir in known_non_issue_dir.iterdir():
        if package_dir.is_dir() and not package_dir.name.startswith('.'):
            ignore_err_file = package_dir / 'ignore.err'
            if ignore_err_file.exists() and ignore_err_file.is_file():
                ignore_err_files.append(ignore_err_file)

    logger.info(f"Found {len(ignore_err_files)} ignore.err files")

    if not ignore_err_files:
        logger.error("No ignore.err files found")
        sys.exit(1)

    matched_pairs, skipped = match_ignore_err_to_excel(ignore_err_files, excel_mapping)

    if not matched_pairs:
        logger.error("No matched pairs found")
        sys.exit(1)

    if output_dir.exists():
        logger.info(f"Removing existing directory: {output_dir}")
        shutil.rmtree(output_dir)

    output_dir.mkdir(parents=True, exist_ok=True)
    logger.info(f"Created output directory: {output_dir}")

    processed, validation_failed = copy_and_rename_ignore_err_files(
        matched_pairs, output_dir, args.validate_justifications
    )

    config = ProcessMiningConfig()
    data_config = config.get_data_processing_config()
    dataset_name = data_config.get('directories', {}).get('full_dataset', 'full_dataset')

    generate_preprocessing_report(
        processed, skipped, validation_failed, output_dir, [dataset_name]
    )

    logger.info("")
    logger.info("=" * 80)
    logger.info("✅ Preprocessing Complete!")
    logger.info("=" * 80)
    logger.info(f"Processed: {len(processed)}")
    logger.info(f"Skipped: {len(skipped)}")
    logger.info(f"Validation failed: {len(validation_failed)}")
    logger.info(f"Output directory: {output_dir}")
    logger.info("")
    logger.info("Next steps:")
    logger.info("1. Run split_train_val_test.py to create train/validation/test splits")
    logger.info("2. Run batch_prepare_patterns.py to generate pattern data")


if __name__ == "__main__":
    main()
