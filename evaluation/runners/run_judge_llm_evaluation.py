#!/usr/bin/env python3
"""
Run evaluation for judge_llm_analysis node using NAT.

This script demonstrates how to run NAT evaluation for the judge_llm_analysis
function with automatic token counting and profiling.

Usage:
    export LLM_API_KEY=your_nvidia_api_key
    python evaluation/runners/run_judge_llm_evaluation.py

Or run directly:
    LLM_API_KEY=your_key python evaluation/runners/run_judge_llm_evaluation.py
"""

import os
import sys
from pathlib import Path

# Add project root to Python path
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

# Import archiving utility and metrics calculation
from evaluation.utils import archive_evaluation_results
from evaluation.utils.calculate_eval_metrics import calculate_metrics_from_workflow
import json

def check_environment(config_file=None):
    """Check if required environment variables are available."""
    api_key = os.getenv('LLM_API_KEY')
    if not api_key:
        print("Error: LLM_API_KEY environment variable not set")
        print("Please set it with: export LLM_API_KEY=your_nvidia_api_key")
        return False

    # Use provided config file or default
    if config_file:
        config_path = Path(config_file)
    else:
        config_path = project_root / "evaluation" / "configs" / "judge_llm_analysis_eval.yml"

    if not config_path.exists():
        print(f"Error: Config file not found: {config_path}")
        return False

    print("Environment checks passed")
    return True

def setup_evaluation_environment():
    """Set up required environment variables for SAST Config."""
    print("Setting up evaluation environment variables...")

    # Set minimal required environment variables for Config
    os.environ.setdefault('PROJECT_NAME', 'judge-llm-eval')
    os.environ.setdefault('PROJECT_VERSION', '1.0.0')
    os.environ.setdefault('INPUT_REPORT_FILE_PATH', '/dev/null')
    os.environ.setdefault('OUTPUT_FILE_PATH', '/dev/null')
    os.environ.setdefault('REPO_LOCAL_PATH', str(project_root))

def run_nat_evaluation(config_file=None):
    """Run NAT evaluation with automatic metrics collection."""
    print("\\nRunning NAT Evaluation for judge_llm_analysis...")
    print("This will automatically collect:")
    print("- Token counts (input/output/total)")
    print("- Processing time metrics")
    print("- Memory usage tracking")
    print("- Error counting")
    print("")

    # Use provided config file or default
    if config_file:
        config_path = Path(config_file)
    else:
        config_path = project_root / "evaluation" / "configs" / "judge_llm_analysis_eval.yml"

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
    print("SAST-AI-Workflow: Judge LLM Analysis Evaluation")
    print("=" * 60)

    # Check if config file provided as argument
    config_file = None
    if len(sys.argv) > 1:
        config_file = sys.argv[1]

    if not check_environment(config_file):
        sys.exit(1)

    setup_evaluation_environment()
    run_nat_evaluation(config_file)

    print("\\nEvaluation completed!")
    print("Results saved to:")
    print("  - evaluation/reports/judge_llm_analysis/workflow_output.json")
    print("  - evaluation/reports/judge_llm_analysis/standardized_data_all.csv")
    print("  - evaluation/reports/judge_llm_analysis/all_requests_profiler_traces.json")

    # Calculate evaluation metrics before archiving
    workflow_output_path = project_root / "evaluation" / "reports" / "judge_llm_analysis" / "workflow_output.json"
    if workflow_output_path.exists():
        print("\\nCalculating evaluation metrics...")
        try:
            metrics_results = calculate_metrics_from_workflow(str(workflow_output_path))

            if "error" in metrics_results:
                print(f"Warning: Could not calculate metrics - {metrics_results['error']}")
            else:
                # Save metrics to JSON file
                metrics_file = project_root / "evaluation" / "reports" / "judge_llm_analysis" / "evaluation_metrics.json"
                with open(metrics_file, 'w') as f:
                    json.dump(metrics_results, f, indent=2)

                # Print summary
                metrics = metrics_results["metrics"]
                metadata = metrics_results["metadata"]

                print(f"  Processed {metadata['processed_items']}/{metadata['total_items']} items")
                print(f"  Accuracy:  {metrics['accuracy']:.4f}")
                print(f"  Precision: {metrics['precision']:.4f}")
                print(f"  Recall:    {metrics['recall']:.4f}")
                print(f"  F1 Score:  {metrics['f1_score']:.4f}")
                print(f"  Metrics saved to: evaluation_metrics.json")
        except Exception as e:
            print(f"Warning: Error calculating metrics - {e}")
    else:
        print("\\nWarning: workflow_output.json not found, skipping metrics calculation")

    # Archive the results after evaluation completes
    reports_dir = project_root / "evaluation" / "reports"
    archived_path = archive_evaluation_results(str(reports_dir), "judge_llm_analysis")
    if archived_path:
        print(f"\\nResults archived to: {archived_path}")
    else:
        print("\\nNote: Results were not archived (no files found)")

if __name__ == "__main__":
    main()