#!/usr/bin/env python3
"""
Script to process multiple NVRs from TrainDataset CSV file.
For each NVR, runs the workflow with package-specific environment variables.

Usage:
    # Run all packages in parallel (default: 4 workers)
    python run_train_dataset_batch.py --dataset path/to/dataset.csv

    # Run with specific number of parallel workers
    python run_train_dataset_batch.py --dataset path/to/dataset.csv --workers 8

    # Run sequentially (1 worker)
    python run_train_dataset_batch.py --dataset path/to/dataset.csv --sequential
    # Or: python run_train_dataset_batch.py --dataset path/to/dataset.csv --workers 1

    # Run only specific packages (e.g. --packages at-3.2.5 opencryptoki-3.23.0)
    python run_train_dataset_batch.py --dataset path/to/dataset.csv --packages PKG1 PKG2

    # Exclude specific packages
    python run_train_dataset_batch.py --dataset path/to/dataset.csv --exclude mtools-4.0.43

    # Update config only for a specific NVR
    python run_train_dataset_batch.py --dataset path/to/dataset.csv --config-only --nvr at-3.2.5
"""

import argparse
import csv
import logging
import os
import subprocess
import sys
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Optional

import yaml

# Workspace path - update this for different projects
WORKSPACE_PATH = Path("/Users/yfishel/Projects/sast-ai/sast-ai-workflow")
ANALYZED_RPM_PATH = "/Users/yfishel/Projects/sast-ai/analyzed_rpm"
KNOWN_FP_PATH = "/Users/yfishel/Projects/sast-ai/known-false-positives"
OUTPUT_PATH = "/Users/yfishel/Projects/sast-ai/agent_tests"

# CSV column header for package NVR in train dataset
PACKAGE_NVR_COLUMN = "Package NVR(Name Version Release)"


def parse_nvr(nvr):
    """
    Parse Package NVR into name and version.
    Format: name-version-release or name-version

    The version part starts at the first numeric component after a dash.

    Example:
        at-3.2.5 -> name="at", version="3.2.5"
        tpm2-tss-4.0.1-7.el10 -> name="tpm2-tss", version="4.0.1-7.el10"
        mtools-4.0.43 -> name="mtools", version="4.0.43"
    """
    parts = nvr.split("-")
    if len(parts) < 2:
        raise ValueError(f"Invalid NVR format: {nvr}")

    # Find the first part that starts with a digit (this is where version begins)
    version_start_idx = None
    for i, part in enumerate(parts):
        if part and part[0].isdigit():
            version_start_idx = i
            break

    if version_start_idx is None:
        raise ValueError(f"Could not find version in NVR: {nvr}")

    # Everything before the version is the name
    name = "-".join(parts[:version_start_idx])
    # Everything from the version onwards is the version
    version = "-".join(parts[version_start_idx:])

    return name, version


def update_config(config_path, nvr, report_link):
    """Update the config YAML file with new values for the given NVR."""

    name, version = parse_nvr(nvr)

    # Read the current config
    with open(config_path, "r") as f:
        config = yaml.safe_load(f)

    # Update the fields
    config["PROJECT_NAME"] = name
    config["PROJECT_VERSION"] = version
    config["INPUT_REPORT_FILE_PATH"] = report_link
    config["KNOWN_FALSE_POSITIVE_FILE_PATH"] = f"{KNOWN_FP_PATH}/{nvr}/ignore.err"
    config["OUTPUT_FILE_PATH"] = f"{OUTPUT_PATH}/{config['TEST_RUN_ID']}/{nvr}-sast_ai_output.xlsx"
    config["REPO_LOCAL_PATH"] = f"{ANALYZED_RPM_PATH}/{nvr}"

    # Write the updated config
    with open(config_path, "w") as f:
        yaml.dump(config, f, default_flow_style=False, sort_keys=False)

    print(f"✓ Updated config for {nvr}")
    print(f"  PROJECT_NAME: {name}")
    print(f"  PROJECT_VERSION: {version}")
    print(f"  INPUT_REPORT_FILE_PATH: {report_link}")
    print(f"  REPO_LOCAL_PATH: {config['REPO_LOCAL_PATH']}")


def check_repo_exists(repo_path):
    """Check if the repository path exists."""
    return Path(repo_path).exists()


def find_repo_path(nvr: str, base_path: str = ANALYZED_RPM_PATH) -> Optional[str]:
    """
    Find the repository path for a given NVR.

    Tries multiple path patterns since some repos don't include the full release in folder name:
    - {base_path}/{nvr}  (e.g., analyzed_rpm/tpm2-tss-4.0.1-7.el10)
    - {base_path}/{name}-{version} without release (e.g., analyzed_rpm/tpm2-tss-4.0.1)
    - {base_path}/{name}-{major_version} (e.g., analyzed_rpm/libtracecmd-1.5.1)
    """
    base = Path(base_path)

    # Try exact NVR match first
    exact_path = base / nvr
    if exact_path.exists():
        return str(exact_path)

    # Parse NVR to try alternative paths
    try:
        name, version = parse_nvr(nvr)
    except ValueError:
        return None

    # Try without release suffix (e.g., -7.el10, -2.el10)
    # Version might be "4.0.1-7.el10", try just "4.0.1"
    version_parts = version.split("-")
    if len(version_parts) > 1:
        # Check if last part looks like a release (contains 'el' or is just a number)
        last_part = version_parts[-1]
        if "el" in last_part.lower() or last_part.isdigit():
            # Try without the release part
            base_version = "-".join(version_parts[:-1])
            alt_path = base / f"{name}-{base_version}"
            if alt_path.exists():
                return str(alt_path)

    # Try with just name-version (no release at all)
    simple_path = base / f"{name}-{version}"
    if simple_path.exists():
        return str(simple_path)

    return None


@dataclass
class PackageResult:
    """Result of processing a single package."""

    nvr: str
    status: str  # 'success', 'failed', 'skipped', 'error'
    message: str
    log_file: Optional[Path] = None
    returncode: Optional[int] = None


# Thread-safe counter for progress tracking
class ProgressTracker:
    def __init__(self, total: int):
        self.total = total
        self.completed = 0
        self.lock = threading.Lock()

    def increment(self) -> int:
        with self.lock:
            self.completed += 1
            return self.completed


def setup_package_logger(logs_dir, nvr):
    """Setup a logger for a specific package."""
    log_file = logs_dir / f"{nvr}.log"

    # Create a logger for this package
    logger = logging.getLogger(f"package.{nvr}")
    logger.setLevel(logging.INFO)

    # Remove any existing handlers
    logger.handlers = []

    # File handler
    file_handler = logging.FileHandler(log_file, mode="w")
    file_handler.setLevel(logging.INFO)
    formatter = logging.Formatter("%(asctime)s - %(levelname)s - %(message)s")
    file_handler.setFormatter(formatter)
    logger.addHandler(file_handler)

    # Console handler
    console_handler = logging.StreamHandler()
    console_handler.setLevel(logging.INFO)
    console_handler.setFormatter(formatter)
    logger.addHandler(console_handler)

    return logger, log_file


def run_workflow(env_vars, log_file, nvr: str):
    """Run the workflow with the specified environment variables."""

    # Merge with current environment
    env = os.environ.copy()
    env.update(env_vars)

    # Run the workflow and capture output to log file
    with open(log_file, "a") as log:
        log.write("\n" + "=" * 80 + "\n")
        log.write(f"WORKFLOW EXECUTION - {nvr}\n")
        log.write("=" * 80 + "\n\n")

        result = subprocess.run(
            ["python", "src/sast_agent_workflow/research_agent/scripts/run_workflow.py"],
            env=env,
            cwd=str(WORKSPACE_PATH),
            stdout=log,
            stderr=log,
            text=True,
        )

    return result.returncode


def process_package(
    nvr: str, report_link: str, base_env_vars: dict, logs_dir: Path, progress: ProgressTracker
) -> PackageResult:
    """
    Process a single package. This function is designed to run in parallel.

    Instead of updating a shared config file, it passes all package-specific
    settings as environment variables which the Config class will pick up.
    """
    # Setup logger for this package
    logger, log_file = setup_package_logger(logs_dir, nvr)
    logger.info(f"Starting processing for {nvr}")
    logger.info(f"Report Link: {report_link}")

    try:
        # Parse NVR to get name and version
        name, version = parse_nvr(nvr)

        # Find repo path (tries multiple patterns)
        repo_path = find_repo_path(nvr)
        if not repo_path:
            msg = f"Repository path not found for {nvr} (tried multiple patterns)"
            logger.warning(f"Skipping {nvr} - {msg}")
            return PackageResult(nvr=nvr, status="skipped", message=msg, log_file=log_file)

        # Create package-specific environment variables
        # These override the default config file values
        package_env = base_env_vars.copy()
        package_env.update(
            {
                "PROJECT_NAME": name,
                "PROJECT_VERSION": version,
                "INPUT_REPORT_FILE_PATH": report_link,
                "KNOWN_FALSE_POSITIVE_FILE_PATH": f"{KNOWN_FP_PATH}/{nvr}/ignore.err",
                "OUTPUT_FILE_PATH": f"{OUTPUT_PATH}/{nvr}-sast_ai_output.xlsx",
                "REPO_LOCAL_PATH": repo_path,
            }
        )

        logger.info(f"Config for {nvr}:")
        logger.info(f"  PROJECT_NAME: {name}")
        logger.info(f"  PROJECT_VERSION: {version}")
        logger.info(f"  REPO_LOCAL_PATH: {repo_path}")

        # Run workflow with package-specific environment
        returncode = run_workflow(package_env, log_file, nvr)

        # Update progress
        completed = progress.increment()

        if returncode == 0:
            msg = f"Successfully processed ({completed}/{progress.total})"
            logger.info(f"✓ {nvr}: {msg}")
            return PackageResult(
                nvr=nvr, status="success", message=msg, log_file=log_file, returncode=returncode
            )
        else:
            msg = f"Workflow failed with exit code {returncode}"
            logger.error(f"✗ {nvr}: {msg}")
            return PackageResult(
                nvr=nvr, status="failed", message=msg, log_file=log_file, returncode=returncode
            )

    except Exception as e:
        msg = f"Error: {str(e)}"
        logger.error(f"✗ {nvr}: {msg}", exc_info=True)
        return PackageResult(nvr=nvr, status="error", message=msg, log_file=log_file)


def update_config_for_nvr(workspace_path, nvr_name, csv_path):
    """Update config for a specific NVR (config-only mode)."""
    config_path = workspace_path / "config" / "default_config.yaml"

    # Read the CSV file to find the NVR
    with open(csv_path, "r") as f:
        reader = csv.DictReader(f)
        rows = list(reader)

    # Find the matching NVR
    matching_row = None
    for row in rows:
        if row[PACKAGE_NVR_COLUMN] == nvr_name:
            matching_row = row
            break

    if not matching_row:
        print(f"✗ Error: NVR '{nvr_name}' not found in {csv_path}")
        print("\nAvailable NVRs:")
        for row in rows:
            print(f"  - {row[PACKAGE_NVR_COLUMN]}")
        return 1

    # Update config
    report_link = matching_row["Report Link"]
    print(f"Found NVR: {nvr_name}")
    print(f"Report Link: {report_link}\n")

    try:
        update_config(config_path, nvr_name, report_link)
        print(f"\n✓ Successfully updated config for {nvr_name}")
        print(f"\nConfig file: {config_path}")
        return 0
    except Exception as e:
        print(f"\n✗ Error updating config: {e}")
        import traceback

        traceback.print_exc()
        return 1


def main():
    parser = _create_parser()
    args = parser.parse_args()
    _validate_args(parser, args)

    csv_path = _resolve_csv_path(args)
    if csv_path is None:
        print("Error: Dataset file not found: %s" % Path(args.dataset).resolve())
        return 1

    if args.config_only:
        return update_config_for_nvr(WORKSPACE_PATH, args.nvr, csv_path)

    logs_base_dir = WORKSPACE_PATH / "logs"
    logs_base_dir.mkdir(exist_ok=True)
    logs_dir, shared_test_run_id = _setup_logs_and_run_id(args, logs_base_dir)
    run_log_file = logs_dir / "batch_run.log"
    base_env_vars = _build_base_env(shared_test_run_id)

    with open(csv_path, "r") as f:
        reader = csv.DictReader(f)
        all_rows = list(reader)

    rows, err = _filter_rows(all_rows, args)
    if err is not None:
        return err

    num_workers = min(args.workers, len(rows))
    print("=" * 80)
    _print_batch_header(
        args, rows, all_rows, num_workers, logs_dir, run_log_file, shared_test_run_id
    )
    _init_run_log(run_log_file, args, rows, all_rows, num_workers, shared_test_run_id)

    # Progress tracker for thread-safe progress updates
    progress = ProgressTracker(len(rows))

    # Collect results
    results: list[PackageResult] = []

    # Process packages in parallel
    print(f"Starting parallel processing with {num_workers} workers...\n")

    with ThreadPoolExecutor(max_workers=num_workers) as executor:
        # Submit all tasks
        future_to_nvr = {
            executor.submit(
                process_package,
                row[PACKAGE_NVR_COLUMN],
                row["Report Link"],
                base_env_vars,
                logs_dir,
                progress,
            ): row[PACKAGE_NVR_COLUMN]
            for row in rows
        }

        # Collect results as they complete
        for future in as_completed(future_to_nvr):
            nvr = future_to_nvr[future]
            try:
                result = future.result()
                results.append(result)

                # Print progress
                status_icon = {"success": "✓", "failed": "✗", "skipped": "⊘", "error": "⚠"}.get(
                    result.status, "?"
                )

                print(
                    f"[{progress.completed}/{progress.total}] {status_icon} {nvr}: {result.message}"
                )

                # Log to run log
                status_tag = result.status.upper()
                with open(run_log_file, "a") as run_log:
                    run_log.write(f"[{status_tag}] {nvr} - {result.message}\n")

            except Exception as e:
                print(f"[{progress.completed}/{progress.total}] ⚠ {nvr}: Unexpected error: {e}")
                results.append(
                    PackageResult(nvr=nvr, status="error", message=f"Unexpected error: {e}")
                )
                with open(run_log_file, "a") as run_log:
                    run_log.write(f"[ERROR] {nvr} - Unexpected error: {e}\n")

    # Calculate statistics
    successful = sum(1 for r in results if r.status == "success")
    failed = sum(1 for r in results if r.status == "failed")
    skipped = sum(1 for r in results if r.status == "skipped")
    errors = sum(1 for r in results if r.status == "error")

    skipped_packages = [r.nvr for r in results if r.status == "skipped"]
    failed_packages = [r.nvr for r in results if r.status in ("failed", "error")]

    # Summary
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print(f"Total packages: {len(rows)}")
    print(f"Successful: {successful}")
    print(f"Failed: {failed}")
    print(f"Errors: {errors}")
    print(f"Skipped: {skipped}")

    if skipped_packages:
        print("\nSkipped packages (repo not found):")
        for pkg in skipped_packages:
            print(f"  - {pkg}")

    if failed_packages:
        print("\nFailed/Error packages:")
        for pkg in failed_packages:
            print(f"  - {pkg}")

    print("=" * 80)

    # Write final summary to run log
    with open(run_log_file, "a") as run_log:
        run_log.write("\n" + "=" * 80 + "\n")
        run_log.write("FINAL SUMMARY\n")
        run_log.write("=" * 80 + "\n")
        run_log.write(f"Total packages: {len(rows)}\n")
        run_log.write(f"Successful: {successful}\n")
        run_log.write(f"Failed: {failed}\n")
        run_log.write(f"Errors: {errors}\n")
        run_log.write(f"Skipped: {skipped}\n")

        if skipped_packages:
            run_log.write("\nSkipped packages:\n")
            for pkg in skipped_packages:
                run_log.write(f"  - {pkg}\n")

        if failed_packages:
            run_log.write("\nFailed/Error packages:\n")
            for pkg in failed_packages:
                run_log.write(f"  - {pkg}\n")

        run_log.write(f"\nBatch Run Completed: {datetime.now()}\n")

    return 0 if (failed + errors) == 0 else 1


def _create_parser():
    """Create and return the argument parser for batch script."""
    parser = argparse.ArgumentParser(
        description="Process NVRs from TrainDataset CSV file in parallel",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Run all packages in parallel (default: 4 workers)
  python run_train_dataset_batch.py

  # Run with 8 parallel workers
  python run_train_dataset_batch.py --workers 8

  # Run sequentially (1 worker)
  python run_train_dataset_batch.py --sequential

  # Run only specific packages
  python run_train_dataset_batch.py --packages at-3.2.5 opencryptoki-3.23.0

  # Exclude specific packages
  python run_train_dataset_batch.py --exclude mtools-4.0.43

  # Update config only for a specific NVR
  python run_train_dataset_batch.py --dataset path/to/dataset.csv --config-only --nvr at-3.2.5
  python run_train_dataset_batch.py --dataset path/to/dataset.csv --config-only \\
      --nvr tpm2-tss-4.0.1-7.el10
        """,
    )
    parser.add_argument(
        "--dataset", type=str, required=True, help="Path to the training dataset CSV file"
    )
    parser.add_argument(
        "--config-only",
        action="store_true",
        help="Only update the config file, do not run the workflow",
    )
    parser.add_argument(
        "--nvr", type=str, help="Specific NVR to process (required with --config-only)"
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=4,
        help="Number of parallel workers (default: 4, use 1 for sequential)",
    )
    parser.add_argument(
        "--sequential",
        action="store_true",
        help="Run sequentially with 1 worker (shorthand for --workers 1)",
    )
    parser.add_argument(
        "--packages",
        nargs="+",
        type=str,
        help="Specific packages to process (space-separated list of NVRs)",
    )
    parser.add_argument(
        "--exclude", nargs="+", type=str, help="Packages to exclude (space-separated list of NVRs)"
    )
    parser.add_argument(
        "--test-run-id",
        type=str,
        help="Use existing TEST_RUN_ID (for continuing a previous batch run)",
    )
    return parser


def _validate_args(parser, args):
    """Validate parsed args; calls parser.error() on failure."""
    if args.config_only and not args.nvr:
        parser.error("--config-only requires --nvr to be specified")
    if args.nvr and not args.config_only:
        parser.error("--nvr requires --config-only flag")
    if args.workers < 1:
        parser.error("--workers must be at least 1")
    if args.sequential:
        args.workers = 1
    if args.packages and args.exclude:
        parser.error("--packages and --exclude cannot be used together")


def _resolve_csv_path(args) -> Optional[Path]:
    """Resolve dataset path; returns None if file not found (caller should print and return 1)."""
    csv_path = Path(args.dataset)
    if not csv_path.is_absolute():
        csv_path = WORKSPACE_PATH / args.dataset
    return csv_path if csv_path.exists() else None


def _setup_logs_and_run_id(args, logs_base_dir: Path):
    """Return (logs_dir, shared_test_run_id)."""
    if args.test_run_id:
        shared_test_run_id = args.test_run_id
        run_timestamp = (
            shared_test_run_id[6:]
            if shared_test_run_id.startswith("batch_")
            else shared_test_run_id
        )
        logs_dir = logs_base_dir / ("run_%s" % run_timestamp)
        logs_dir.mkdir(exist_ok=True)
        print("Continuing with existing TEST_RUN_ID: %s" % shared_test_run_id)
        return (logs_dir, shared_test_run_id)
    run_timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    logs_dir = logs_base_dir / ("run_%s" % run_timestamp)
    logs_dir.mkdir(exist_ok=True)
    shared_test_run_id = "batch_%s" % run_timestamp
    return (logs_dir, shared_test_run_id)


def _build_base_env(shared_test_run_id: str) -> dict:
    """Build base environment variables for the workflow."""
    return {
        "TEST_RUN_ID": shared_test_run_id,
        "LLM_URL": os.environ.get("LLM_URL", "https://integrate.api.nvidia.com/v1"),
        "LLM_API_TYPE": os.environ.get("LLM_API_TYPE", "nim"),
        "LLM_MODEL_NAME": os.environ.get("LLM_MODEL_NAME", "qwen/qwen3-next-80b-a3b-thinking"),
        "LLM_API_KEY": os.environ.get("LLM_API_KEY", ""),
        "DISABLE_SSL_VERIFY": "true",
        "LANGFUSE_SECRET_KEY": "sk-lf-bbe9289f-bd69-40ed-8982-14cd3f1d0e1e",
        "LANGFUSE_PUBLIC_KEY": "pk-lf-1beba22d-8a15-4e5c-a9db-296c8301d564",
        "LANGFUSE_HOST": "http://localhost:3000",
        "LANGFUSE_TRACE_PER_ISSUE": "true",
    }


def _filter_rows(all_rows: list, args):
    """Filter rows by --packages or --exclude. Returns (rows, None) or (None, error_code 1)."""
    if args.packages:
        package_set = set(args.packages)
        rows = [row for row in all_rows if row[PACKAGE_NVR_COLUMN] in package_set]
        found_packages = {row[PACKAGE_NVR_COLUMN] for row in rows}
        missing = package_set - found_packages
        if missing:
            print("Warning: The following packages were not found in the CSV:")
            for pkg in missing:
                print("  - %s" % pkg)
            print()
        if not rows:
            print("Error: No matching packages found in CSV file.")
            print("\nAvailable packages:")
            for row in all_rows:
                print("  - %s" % row[PACKAGE_NVR_COLUMN])
            return (None, 1)
        return (rows, None)
    if args.exclude:
        exclude_set = set(args.exclude)
        rows = [row for row in all_rows if row[PACKAGE_NVR_COLUMN] not in exclude_set]
        excluded = [
            row[PACKAGE_NVR_COLUMN] for row in all_rows if row[PACKAGE_NVR_COLUMN] in exclude_set
        ]
        if excluded:
            print("Excluding packages: %s\n" % ", ".join(excluded))
        return (rows, None)
    return (all_rows, None)


def _print_batch_header(
    args, rows, all_rows, num_workers, logs_dir, run_log_file, shared_test_run_id
):
    """Print batch run header to stdout."""
    print("SAST Batch Processing - Parallel Mode")
    print("=" * 80)
    print("Workspace: %s" % WORKSPACE_PATH)
    print("TEST_RUN_ID: %s" % shared_test_run_id)
    if args.packages:
        print("Running selected packages: %s of %s" % (len(rows), len(all_rows)))
    elif args.exclude:
        print(
            "Running packages: %s of %s (excluding %s)"
            % (len(rows), len(all_rows), len(all_rows) - len(rows))
        )
    else:
        print("Total packages: %s" % len(rows))
    print("Parallel workers: %s" % num_workers)
    print("Logs directory: %s" % logs_dir)
    print("Run log: %s" % run_log_file)
    print("=" * 80 + "\n")


def _init_run_log(run_log_file, args, rows, all_rows, num_workers, shared_test_run_id):
    """Initialize or append to batch run log file."""
    log_mode = "a" if args.test_run_id else "w"
    with open(run_log_file, log_mode) as run_log:
        if args.test_run_id:
            run_log.write("\n" + "=" * 80 + "\n")
            run_log.write("Batch Run Continued: %s\n" % datetime.now())
        else:
            run_log.write("Batch Run Started: %s\n" % datetime.now())
        run_log.write("Workspace: %s\n" % WORKSPACE_PATH)
        run_log.write("TEST_RUN_ID: %s\n" % shared_test_run_id)
        if args.packages:
            run_log.write("Selected packages: %s of %s\n" % (len(rows), len(all_rows)))
            run_log.write("Packages: %s\n" % ", ".join(args.packages))
        elif args.exclude:
            run_log.write("Running packages: %s of %s\n" % (len(rows), len(all_rows)))
            run_log.write("Excluded: %s\n" % ", ".join(args.exclude))
        else:
            run_log.write("Total packages: %s\n" % len(rows))
        run_log.write("Parallel workers: %s\n" % num_workers)
        run_log.write("=" * 80 + "\n\n")


if __name__ == "__main__":
    sys.exit(main())
