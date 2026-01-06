#!/usr/bin/env python3
"""
Script to copy known false positive package directories with their ignore.err files
to the ground-truth/known_non_issue_packages directory.
"""

import os
import shutil
from pathlib import Path


def copy_package_directories(package_list_file: Path, source_dir: Path, dest_dir: Path):
    """
    Copy package directories from source to destination based on package list.

    Args:
        package_list_file: Path to known_non_issue_packages.txt
        source_dir: Path to known-false-positives repository
        dest_dir: Path to destination directory (known_non_issue_packages)
    """
    with open(package_list_file, 'r') as f:
        packages = [line.strip() for line in f if line.strip()]

    print(f"Found {len(packages)} packages to copy")
    print(f"Source: {source_dir}")
    print(f"Destination: {dest_dir}")
    print()

    copied_count = 0
    skipped_count = 0

    for package_name in packages:
        source_package_dir = source_dir / package_name
        dest_package_dir = dest_dir / package_name

        if not source_package_dir.exists():
            print(f"WARNING: Source directory not found: {package_name}")
            skipped_count += 1
            continue

        ignore_err_file = source_package_dir / 'ignore.err'
        if not ignore_err_file.exists():
            print(f"WARNING: ignore.err not found in: {package_name}")
            skipped_count += 1
            continue

        dest_package_dir.mkdir(parents=True, exist_ok=True)

        dest_ignore_err = dest_package_dir / 'ignore.err'
        shutil.copy2(ignore_err_file, dest_ignore_err)

        copied_count += 1

        if copied_count % 20 == 0:
            print(f"Copied {copied_count}/{len(packages)} packages...")

    print()
    print("=" * 80)
    print("COPY SUMMARY")
    print("=" * 80)
    print(f"Total packages in list: {len(packages)}")
    print(f"Successfully copied: {copied_count}")
    print(f"Skipped: {skipped_count}")
    print()


def main():
    """Main execution function."""
    print("=" * 80)
    print("Copying Known False Positive Package Directories")
    print("=" * 80)
    print()

    script_dir = Path(__file__).parent
    base_dir = script_dir.parent.parent.parent
    ground_truth_dir = base_dir / 'sast-ai-workflow/process_mining/data/ground-truth'

    package_list_file = ground_truth_dir / 'known_non_issue_packages.txt'
    source_dir = base_dir / 'known-false-positives'
    dest_dir = ground_truth_dir / 'known_non_issue_packages'

    if not package_list_file.exists():
        print(f"ERROR: Package list not found: {package_list_file}")
        return

    if not source_dir.exists():
        print(f"ERROR: Source directory not found: {source_dir}")
        return

    copy_package_directories(package_list_file, source_dir, dest_dir)

    print(f"Done! Package directories copied to:")
    print(f"  {dest_dir}")
    print()


if __name__ == '__main__':
    main()
