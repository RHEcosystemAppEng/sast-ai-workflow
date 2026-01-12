#!/usr/bin/env python3
"""
MLflow Runner for SAST AI Workflow Evaluation Dashboard

This script orchestrates the conversion of all SAST AI evaluation reports
into MLflow format using modular node-specific converters.

APPENG-3747: Evaluation Dashboard Creation
"""

import argparse
import subprocess
import webbrowser
from pathlib import Path
from time import sleep
from typing import List

import mlflow

from .filter_mlflow_converter import FilterNodeConverter
from .judge_llm_mlflow_converter import JudgeLLMNodeConverter
from .summarize_mlflow_converter import SummarizeNodeConverter


class MLflowEvaluationRunner:
    """Main runner that orchestrates all node-specific MLflow converters."""

    def __init__(self, reports_dir: str, mlruns_dir: str):
        self.reports_dir = Path(reports_dir)
        self.mlruns_dir = Path(mlruns_dir)

        # Initialize all node converters
        self.converters = [
            FilterNodeConverter(reports_dir, mlruns_dir),
            JudgeLLMNodeConverter(reports_dir, mlruns_dir),
            SummarizeNodeConverter(reports_dir, mlruns_dir)
        ]

    def setup_mlflow_tracking(self):
        """Set up MLflow tracking URI and create mlruns directory."""
        self.mlruns_dir.mkdir(exist_ok=True)
        mlflow.set_tracking_uri(f"file://{self.mlruns_dir.absolute()}")
        print(f"MLflow tracking URI set to: {mlflow.get_tracking_uri()}")

    def convert_all_reports(self) -> int:
        """Convert all evaluation reports to MLflow format using modular converters."""
        if not self.reports_dir.exists():
            raise FileNotFoundError(f"Reports directory not found: {self.reports_dir}")

        self.setup_mlflow_tracking()

        total_runs = 0
        for converter in self.converters:
            print(f"\n🔄 Processing {converter.node_type} reports...")
            runs_processed = converter.convert_reports()
            total_runs += runs_processed
            print(f"✅ Completed {converter.node_type}: {runs_processed} runs processed")

        return total_runs

    def launch_dashboard(self, host: str = "localhost", port: int = 5000):
        """Launch MLflow dashboard."""
        print(f"\n🚀 Launching MLflow dashboard on http://{host}:{port}")  # NOSONAR - localhost only, local dev tool

        try:
            # Start MLflow UI
            process = subprocess.Popen([
                "mlflow", "ui",
                "--backend-store-uri", str(mlflow.get_tracking_uri()),
                "--host", host,
                "--port", str(port)
            ])

            # Wait a moment for the server to start
            sleep(2)

            # Open browser
            webbrowser.open(f"http://{host}:{port}")  # NOSONAR - localhost only, local dev tool

            print("✅ MLflow dashboard launched!")
            print("Press Ctrl+C to stop the dashboard")

            # Wait for the process
            process.wait()

        except KeyboardInterrupt:
            print("\n🛑 Stopping MLflow dashboard")
            process.terminate()
        except Exception as e:
            print(f"Error launching dashboard: {e}")

    def get_summary_report(self) -> dict:
        """Generate a summary report of all available data."""
        summary = {
            "experiments": [],
            "total_runs": 0,
            "total_packages": 0,
            "total_issues": 0
        }

        try:
            # Get all experiments
            experiments = mlflow.search_experiments()

            for experiment in experiments:
                if experiment.name == "Default":
                    continue

                # Get runs for this experiment
                runs = mlflow.search_runs(experiment_ids=[experiment.experiment_id])

                experiment_info = {
                    "name": experiment.name,
                    "experiment_id": experiment.experiment_id,
                    "runs_count": len(runs),
                    "runs": []
                }

                # Get run-level summaries
                for _, run in runs.iterrows():
                    if run.get("tags.level") == "evaluation_run":  # Only top-level runs
                        run_info = {
                            "run_name": run["tags.evaluation_run"],
                            "run_id": run["run_id"],
                            "packages": run.get("tags.total_packages", 0),
                            "issues": run.get("tags.total_issues", 0),
                            "timestamp": run.get("tags.run_timestamp", "")
                        }
                        experiment_info["runs"].append(run_info)
                        summary["total_packages"] += int(run_info["packages"]) if run_info["packages"] else 0
                        summary["total_issues"] += int(run_info["issues"]) if run_info["issues"] else 0

                summary["experiments"].append(experiment_info)
                summary["total_runs"] += experiment_info["runs_count"]

        except Exception as e:
            print(f"Warning: Could not generate summary report: {e}")

        return summary

    def print_summary_report(self):
        """Print a formatted summary report."""
        summary = self.get_summary_report()

        print("\n" + "="*60)
        print("📊 SAST AI EVALUATION DASHBOARD SUMMARY")
        print("="*60)
        print(f"Total Experiments: {len(summary['experiments'])}")
        print(f"Total Evaluation Runs: {summary['total_runs']}")
        print(f"Total Packages Processed: {summary['total_packages']}")
        print(f"Total Issues Analyzed: {summary['total_issues']}")
        print("\n📋 EXPERIMENTS BREAKDOWN:")
        print("-"*60)

        for exp in summary["experiments"]:
            print(f"\n🧪 {exp['name']}")
            print(f"   Runs: {len(exp['runs'])}")

            for run in exp["runs"]:
                print(f"   └── {run['run_name']}")
                print(f"       📦 Packages: {run['packages']}, 🐛 Issues: {run['issues']}")
                if run['timestamp']:
                    print(f"       🕒 {run['timestamp']}")

        print("\n" + "="*60)
        print(f"🌐 Dashboard URL: {mlflow.get_tracking_uri()}")
        print("="*60)


def main():
    """Main function with command line interface."""
    parser = argparse.ArgumentParser(
        description="Convert SAST AI evaluation reports to MLflow format using modular converters",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Convert reports to MLflow
  python mlflow_runner.py

  # Convert and launch dashboard
  python mlflow_runner.py --launch

  # Custom directories
  python mlflow_runner.py --reports-dir /path/to/reports --mlruns-dir /path/to/mlruns

  # Launch dashboard on different port
  python mlflow_runner.py --launch --port 5001

  # Show summary report only
  python mlflow_runner.py --summary-only
        """
    )

    parser.add_argument(
        "--reports-dir",
        default="../reports",
        help="Path to evaluation reports directory (default: ../reports)"
    )

    parser.add_argument(
        "--mlruns-dir",
        default="../mlruns",
        help="Path to mlruns directory (default: ../mlruns)"
    )

    parser.add_argument(
        "--launch",
        action="store_true",
        help="Launch MLflow dashboard after conversion"
    )

    parser.add_argument(
        "--summary-only",
        action="store_true",
        help="Show summary report without converting (requires existing mlruns)"
    )

    parser.add_argument(
        "--host",
        default="localhost",
        help="Host for MLflow dashboard (default: localhost)"
    )

    parser.add_argument(
        "--port",
        type=int,
        default=5000,
        help="Port for MLflow dashboard (default: 5000)"
    )

    args = parser.parse_args()

    # Convert relative paths to absolute from current working directory
    reports_dir = Path(args.reports_dir).resolve()
    mlruns_dir = Path(args.mlruns_dir).resolve()

    print("🚀 SAST AI Evaluation MLflow Runner")
    print("="*50)
    print(f"Reports directory: {reports_dir}")
    print(f"MLruns directory:  {mlruns_dir}")

    # Create runner
    runner = MLflowEvaluationRunner(str(reports_dir), str(mlruns_dir))

    if args.summary_only:
        # Set up tracking for summary report
        runner.setup_mlflow_tracking()
        runner.print_summary_report()
        return

    # Convert reports
    print(f"\n🔄 Starting conversion process...")
    total_runs = runner.convert_all_reports()

    print(f"\n✅ Successfully converted {total_runs} evaluation runs to MLflow format")
    print(f"MLflow tracking URI: {mlflow.get_tracking_uri()}")

    # Show summary report
    runner.print_summary_report()

    # Launch dashboard if requested
    if args.launch:
        runner.launch_dashboard(args.host, args.port)
    else:
        print(f"\n🌐 To view the dashboard, run:")
        print(f"   mlflow ui --backend-store-uri {mlflow.get_tracking_uri()}")
        print(f"   Or visit: http://localhost:5000")  # NOSONAR - localhost only, local dev tool


if __name__ == "__main__":
    main()