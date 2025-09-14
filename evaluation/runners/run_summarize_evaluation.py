#!/usr/bin/env python3
"""
Run evaluation for summarize_justifications node using NAT.

This script demonstrates how to run NAT evaluation for the summarize_justifications
function with automatic token counting and profiling.

Usage:
    export LLM_API_KEY=your_nvidia_api_key
    python evaluation/runners/run_summarize_evaluation.py

Or run directly:
    LLM_API_KEY=your_key python evaluation/runners/run_summarize_evaluation.py
"""

import os
import sys
from pathlib import Path

# Add project root to Python path
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

def check_environment():
    """Check if required environment variables are available."""
    api_key = os.getenv('LLM_API_KEY')
    if not api_key:
        print("Error: LLM_API_KEY environment variable not set")
        print("Please set it with: export LLM_API_KEY=your_nvidia_api_key")
        return False

    # Check if config file exists
    config_path = project_root / "evaluation" / "configs" / "summarize_justifications_eval.yml"
    if not config_path.exists():
        print(f"Error: Config file not found: {config_path}")
        return False

    # Check if dataset exists
    dataset_path = project_root / "evaluation" / "dataset" / "summarize_eval_dataset.json"
    if not dataset_path.exists():
        print(f"Error: Dataset file not found: {dataset_path}")
        return False

    print("Environment checks passed")
    return True

def setup_evaluation_environment():
    """Set up required environment variables for SAST Config."""
    print("Setting up evaluation environment variables...")

    # Set minimal required environment variables for Config
    os.environ.setdefault('PROJECT_NAME', 'test-eval')
    os.environ.setdefault('PROJECT_VERSION', '1.0.0')
    os.environ.setdefault('INPUT_REPORT_FILE_PATH', '/dev/null')
    os.environ.setdefault('OUTPUT_FILE_PATH', '/dev/null')
    os.environ.setdefault('REPO_LOCAL_PATH', str(project_root))

def run_nat_evaluation():
    """Run NAT evaluation with automatic metrics collection."""
    print("\\nRunning NAT Evaluation for summarize_justifications...")
    print("This will automatically collect:")
    print("- Token counts (input/output/total)")
    print("- Processing time metrics")
    print("- Memory usage tracking")
    print("- Error counting")
    print("")

    config_path = project_root / "evaluation" / "configs" / "summarize_justifications_eval.yml"

    # Run actual NAT evaluation command
    import subprocess
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

def main():
    """Main evaluation runner."""
    print("=" * 60)
    print("SAST-AI-Workflow: Summarize Justifications Evaluation")
    print("=" * 60)

    if not check_environment():
        sys.exit(1)

    setup_evaluation_environment()
    run_nat_evaluation()

    print("\\nEvaluation completed!")
    print("Results saved to:")
    print("  - evaluation/reports/summarize_justifications/workflow_output.json")
    print("  - evaluation/reports/summarize_justifications/standardized_data_all.csv")
    print("  - evaluation/reports/summarize_justifications/all_requests_profiler_traces.json")

if __name__ == "__main__":
    main()