#!/usr/bin/env python3
"""
Run evaluation for filter node using NAT.
Usage:
    export LLM_API_KEY=your_nvidia_api_key
    python evaluation/runners/run_filter_evaluation.py

Or run directly:
    LLM_API_KEY=your_key python evaluation/runners/run_filter_evaluation.py
"""

import os
import sys
from pathlib import Path
from evaluation.utils import archive_evaluation_results
import subprocess
from nat.cli.main import run_cli
import sys

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))


def check_environment(config_file=None):
    """Check if required environment variables are available."""
    llm_api_key = os.getenv('LLM_API_KEY')
    embedding_api_key = os.getenv('EMBEDDING_API_KEY') or os.getenv('EMBEDDINGS_LLM_API_KEY')

    if not llm_api_key:
        print("Error: LLM_API_KEY environment variable not set")
        print("Please set it with: export LLM_API_KEY=your_nvidia_api_key")
        return False

    if not embedding_api_key:
        print("Error: EMBEDDING_API_KEY or EMBEDDINGS_LLM_API_KEY environment variable not set")
        print("Please set it with: export EMBEDDING_API_KEY=your_embedding_api_key")
        print("Or: export EMBEDDINGS_LLM_API_KEY=your_embedding_api_key")
        return False

    # Use provided config file or default
    if config_file:
        config_path = Path(config_file)
    else:
        config_path = project_root / "evaluation" / "configs" / "filter_eval.yml"

    if not config_path.exists():
        print(f"Error: Config file not found: {config_path}")
        return False

    print("Environment checks passed")
    return True

def setup_evaluation_environment():
    """Set up required environment variables for SAST Config."""
    print("Setting up evaluation environment variables...")

    # Set minimal required environment variables for Config
    os.environ.setdefault('PROJECT_NAME', 'filter-eval')
    os.environ.setdefault('PROJECT_VERSION', '1.0.0')
    os.environ.setdefault('INPUT_REPORT_FILE_PATH', '/dev/null')
    os.environ.setdefault('OUTPUT_FILE_PATH', '/dev/null')
    os.environ.setdefault('REPO_LOCAL_PATH', str(project_root))

def run_nat_evaluation(config_file=None, debug_mode=False):
    """Run NAT evaluation with automatic metrics collection."""
    print("\nRunning NAT Evaluation for filter...")
    print("This will automatically collect:")
    print("- Token counts (input/output/total)")
    print("- Processing time metrics")
    print("- Memory usage tracking")
    print("- Error counting")
    print("")

    if config_file:
        config_path = Path(config_file)
    else:
        config_path = project_root / "evaluation" / "configs" / "filter_eval.yml"

    if debug_mode:
        print("🐛 DEBUG MODE: Running NAT evaluation directly in Python")
        print("You can set breakpoints in PyCharm and they will be hit!")
        print("Key places to set breakpoints:")
        print("  - src/services/vector_store_service.py (FAISS creation)")
        print("  - evaluation/tools/filter_converters.py (input/output conversion)")
        print("  - src/sast_agent_workflow/tools/filter.py (filter logic)")
        print("")
        original_argv = sys.argv.copy()
        sys.argv = ['nat', 'eval', '--config_file', str(config_path)]
        print(f"Executing NAT directly: {' '.join(sys.argv)}")
        run_cli()
        sys.argv = original_argv
        print("NAT evaluation completed successfully!")
    else:
        run_nat_evaluation_subprocess(config_path)

def run_nat_evaluation_subprocess(config_path):
    """Run NAT evaluation using subprocess (original method)."""
    try:
        print(f"Executing: nat eval --config_file {config_path}")
        result = subprocess.run([
            "nat", "eval", "--config_file", str(config_path)
        ], check=True, capture_output=True, text=True)

        print("NAT evaluation completed successfully!")
        if result.stdout:
            print("Output:", result.stdout)

    except subprocess.CalledProcessError as e:
        print(f"Error running NAT evaluation: {e}")
        if e.stdout:
            print("Stdout:", e.stdout)
        if e.stderr:
            print("Stderr:", e.stderr)
        raise
    except FileNotFoundError:
        print("Error: 'nat' command not found. Please ensure NAT is installed and in PATH.")
        print("Try: source .venv-test/bin/activate")
        raise

def run_validation_analysis(reports_dir):
    """Run filter validation analysis against ground truth."""
    print("\n" + "=" * 60)
    print("Running Filter Validation Analysis")
    print("=" * 60)

    validation_script = project_root / "evaluation" / "utils" / "filter_validation.py"
    dataset_file = project_root / "evaluation" / "dataset" / "filter_eval" / "filter_eval_dataset.json"

    try:
        print(f"Executing validation: python {validation_script} {reports_dir} {dataset_file}")
        result = subprocess.run([
            "python", str(validation_script), str(reports_dir), str(dataset_file)
        ], check=True, capture_output=True, text=True)

        print("Validation analysis completed successfully!")
        if result.stdout:
            print("Validation Output:")
            print(result.stdout)

        return True

    except subprocess.CalledProcessError as e:
        print(f"Error running validation analysis: {e}")
        if e.stdout:
            print("Stdout:", e.stdout)
        if e.stderr:
            print("Stderr:", e.stderr)
        return False
    except FileNotFoundError:
        print("Error: Python not found or validation script missing")
        return False

def main():
    """Main evaluation runner."""
    print("=" * 60)
    print("SAST-AI-Workflow: Filter Evaluation")
    print("=" * 60)

    # Check for debug flag and config file
    config_file = None
    debug_mode = False

    # Parse command line arguments
    args = sys.argv[1:]
    for arg in args:
        if arg == "--debug" or arg == "-d":
            debug_mode = True
            print("🐛 Debug mode enabled")
        elif not arg.startswith("-"):
            config_file = arg

    if not check_environment(config_file):
        sys.exit(1)

    setup_evaluation_environment()
    run_nat_evaluation(config_file, debug_mode)

    print("\nEvaluation completed!")
    print("Results saved to:")
    print("  - evaluation/reports/filter/workflow_output.json")
    print("  - evaluation/reports/filter/standardized_data_all.csv")
    print("  - evaluation/reports/filter/all_requests_profiler_traces.json")

    # Run validation analysis before archiving
    reports_dir = project_root / "evaluation" / "reports" / "filter"
    validation_success = run_validation_analysis(reports_dir)

    if validation_success:
        print("  - evaluation/reports/filter/filter_validation_report.json")
    else:
        print("Warning: Validation analysis failed, but continuing with archival")

    print("\nTo run this evaluation, use:")
    print("  export LLM_API_KEY=your_nvidia_api_key")
    print("  export EMBEDDING_API_KEY=your_embedding_api_key")
    print("  python evaluation/runners/run_filter_evaluation.py")
    print("\nFor PyCharm debugging with breakpoints:")
    print("  python evaluation/runners/run_filter_evaluation.py --debug")

    # Archive the results after evaluation completes (including validation report)
    reports_dir = project_root / "evaluation" / "reports"
    archived_path = archive_evaluation_results(str(reports_dir), "filter")
    if archived_path:
        print(f"\nResults archived to: {archived_path}")
        if validation_success:
            print("  (includes validation report)")
    else:
        print("\nNote: Results were not archived (no files found)")

if __name__ == "__main__":
    main()