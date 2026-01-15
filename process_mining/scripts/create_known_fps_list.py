#!/usr/bin/env python3
"""
Script to filter known false positive packages for RHEL-relevant packages only.
Matches package directories from known-false-positives repo against ground-truth packages.
"""

import os
import re
import sys
from pathlib import Path
from typing import Set, Dict, List, Tuple
from datetime import datetime

sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent.parent / 'src'))

from Utils.data_access_utils import PackageNameExtractor


def get_ground_truth_packages(full_dataset_dir: Path) -> Set[str]:
    """
    Scan full_dataset directory and extract unique base package names.
    """
    packages = set()

    if not full_dataset_dir.exists():
        print(f"ERROR: Directory not found: {full_dataset_dir}")
        return packages

    excel_files = list(full_dataset_dir.glob('*.xlsx'))

    for excel_file in excel_files:
        base_name = PackageNameExtractor.from_nvr_regex(excel_file.name, rhel_version='el10')
        if base_name:
            packages.add(base_name)

    return packages


def count_ignore_err_entries(ignore_err_path: Path) -> int:
    """
    Count the number of error entries in an ignore.err file.
    Entries are separated by "Error:" lines.
    """
    if not ignore_err_path.exists() or not ignore_err_path.is_file():
        return 0

    try:
        with open(ignore_err_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
            return len([line for line in content.split('\n') if line.startswith('Error:')])
    except Exception as e:
        print(f"WARNING: Could not read {ignore_err_path}: {e}")
        return 0


def scan_known_fps_repo(known_fps_dir: Path, ground_truth_packages: Set[str]) -> Dict[str, Dict]:
    """
    Scan known-false-positives repository and match against ground-truth packages.

    Returns:
        Dictionary mapping package name to metadata (path, entry count)
    """
    matched_packages = {}

    if not known_fps_dir.exists():
        print(f"ERROR: Directory not found: {known_fps_dir}")
        return matched_packages

    package_dirs = [d for d in known_fps_dir.iterdir() if d.is_dir()]

    ground_truth_lower = {pkg.lower() for pkg in ground_truth_packages}

    for pkg_dir in package_dirs:
        pkg_name = pkg_dir.name
        pkg_name_lower = pkg_name.lower()

        if pkg_name_lower not in ground_truth_lower:
            continue

        ignore_err_path = pkg_dir / 'ignore.err'
        if not ignore_err_path.exists():
            print(f"WARNING: {pkg_name} directory found but no ignore.err file")
            continue

        entry_count = count_ignore_err_entries(ignore_err_path)
        if entry_count == 0:
            print(f"WARNING: {pkg_name} has empty or unreadable ignore.err file")
            continue

        matched_packages[pkg_name] = {
            'path': str(pkg_dir),
            'ignore_err_path': str(ignore_err_path),
            'entry_count': entry_count
        }

    return matched_packages


def write_package_list(output_path: Path, matched_packages: Dict[str, Dict]):
    """
    Write simple list of matched package names to file.
    """
    sorted_packages = sorted(matched_packages.keys())

    with open(output_path, 'w') as f:
        for pkg_name in sorted_packages:
            f.write(f"{pkg_name}\n")

    print(f"✓ Package list written to: {output_path}")
    print(f"  Total packages: {len(sorted_packages)}")


def write_validation_report(output_path: Path,
                            ground_truth_packages: Set[str],
                            matched_packages: Dict[str, Dict]):
    """
    Write detailed validation report with matching statistics.
    """
    sorted_matched = sorted(matched_packages.keys())
    unmatched = sorted(ground_truth_packages - set(matched_packages.keys()))

    total_entries = sum(pkg['entry_count'] for pkg in matched_packages.values())

    with open(output_path, 'w') as f:
        f.write("=" * 80 + "\n")
        f.write("RHEL Known False Positives Matching Report\n")
        f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write("=" * 80 + "\n\n")

        f.write("SUMMARY\n")
        f.write("-" * 80 + "\n")
        f.write(f"Total ground-truth packages (full_dataset): {len(ground_truth_packages)}\n")
        f.write(f"Total known-FP packages matched: {len(matched_packages)}\n")
        f.write(f"Match rate: {len(matched_packages)/len(ground_truth_packages)*100:.1f}%\n")
        f.write(f"Total false positive entries: {total_entries}\n")
        f.write(f"Average entries per package: {total_entries/len(matched_packages):.1f}\n")
        f.write("\n")

        f.write("MATCHED PACKAGES\n")
        f.write("-" * 80 + "\n")
        for pkg_name in sorted_matched:
            pkg_info = matched_packages[pkg_name]
            f.write(f"{pkg_name}\n")
            f.write(f"  Path: {pkg_info['path']}\n")
            f.write(f"  Entries: {pkg_info['entry_count']}\n")
        f.write("\n")

        f.write("UNMATCHED GROUND-TRUTH PACKAGES\n")
        f.write("-" * 80 + "\n")
        if unmatched:
            for pkg_name in unmatched:
                f.write(f"  {pkg_name}\n")
        else:
            f.write("  (All ground-truth packages matched!)\n")
        f.write("\n")

    print(f"✓ Validation report written to: {output_path}")


def main():
    """Main execution function."""
    print("=" * 80)
    print("RHEL Known False Positives Package Filter")
    print("=" * 80)
    print()

    base_dir = Path('/home/ikrispin/work/sast-ai-project')
    ground_truth_dir = base_dir / 'sast-ai-workflow/process_mining/data/ground-truth'
    full_dataset_dir = ground_truth_dir / 'full_dataset'
    known_fps_dir = base_dir / 'known-false-positives'

    output_list_path = ground_truth_dir / 'known_non_issue_packages.txt'
    output_report_path = ground_truth_dir / 'known_fps_matching_report.txt'

    print("Step 1: Extracting ground-truth package names...")
    ground_truth_packages = get_ground_truth_packages(full_dataset_dir)
    print(f"✓ Found {len(ground_truth_packages)} unique ground-truth packages")
    print()

    print("Step 2: Scanning known-false-positives repository...")
    matched_packages = scan_known_fps_repo(known_fps_dir, ground_truth_packages)
    print(f"✓ Matched {len(matched_packages)} packages")
    print()

    print("Step 3: Writing package list...")
    write_package_list(output_list_path, matched_packages)
    print()

    print("Step 4: Writing validation report...")
    write_validation_report(output_report_path, ground_truth_packages, matched_packages)
    print()

    print("=" * 80)
    print("DONE!")
    print("=" * 80)
    print(f"\nOutput files:")
    print(f"  - Package list: {output_list_path}")
    print(f"  - Report: {output_report_path}")


if __name__ == '__main__':
    main()
