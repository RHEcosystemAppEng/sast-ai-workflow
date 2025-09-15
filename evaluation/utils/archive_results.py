#!/usr/bin/env python3
"""
Utility to archive evaluation results with timestamps to preserve evaluation history.

This utility moves the latest evaluation results to a timestamped folder,
preventing overwriting of previous results.
"""

import os
import shutil
from datetime import datetime
from pathlib import Path


def archive_evaluation_results(reports_dir: str, evaluation_name: str) -> str:
    """
    Archive the latest evaluation results to a timestamped folder.

    Args:
        reports_dir: Base reports directory (e.g., "./evaluation/reports")
        evaluation_name: Name of the evaluation (e.g., "judge_llm_analysis", "summarize_justifications")

    Returns:
        Path to the archived results folder
    """
    reports_path = Path(reports_dir)
    eval_dir = reports_path / evaluation_name

    if not eval_dir.exists():
        print(f"No results found to archive at {eval_dir}")
        return None

    # Check if there are any files to archive (only files, not directories)
    files = [f for f in eval_dir.glob("*") if f.is_file()]
    if not files:
        print(f"No files found to archive in {eval_dir}")
        return None

    # Create timestamp
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

    # Create timestamped folder directly in the evaluation directory
    timestamped_dir = eval_dir / f"run_{timestamp}"
    timestamped_dir.mkdir(exist_ok=True)

    # Move all files to timestamped folder
    archived_files = []
    for file_path in files:
        dest_path = timestamped_dir / file_path.name
        shutil.move(str(file_path), str(dest_path))
        archived_files.append(file_path.name)

    if archived_files:
        print(f"Archived {len(archived_files)} files to: {timestamped_dir}")
        print(f"Archived files: {', '.join(archived_files)}")

    return str(timestamped_dir)


def main():
    """CLI interface for archiving results."""
    import sys

    if len(sys.argv) != 3:
        print("Usage: python archive_results.py <reports_dir> <evaluation_name>")
        print("Example: python archive_results.py ./evaluation/reports judge_llm_analysis")
        sys.exit(1)

    reports_dir = sys.argv[1]
    evaluation_name = sys.argv[2]

    archived_path = archive_evaluation_results(reports_dir, evaluation_name)
    if archived_path:
        print(f"Results archived to: {archived_path}")
    else:
        print("No results were archived")


if __name__ == "__main__":
    main()