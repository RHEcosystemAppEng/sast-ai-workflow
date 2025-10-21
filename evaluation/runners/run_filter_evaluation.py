#!/usr/bin/env python3
"""
Run evaluation for filter node using NAT.

Usage:
    export LLM_API_KEY=your_nvidia_api_key
    export EMBEDDINGS_LLM_API_KEY=your_embedding_api_key
    python evaluation/runners/run_filter_evaluation.py

Or run directly:
    LLM_API_KEY=your_key python evaluation/runners/run_filter_evaluation.py
"""

import os
import subprocess
import sys
from pathlib import Path
from typing import List, Dict

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from evaluation.runners.base_runner import BaseEvaluationRunner


class FilterEvaluationRunner(BaseEvaluationRunner):
    """Filter evaluation runner."""

    def __init__(self):
        super().__init__("filter", "filter_eval.yml")

    def get_required_env_vars(self) -> List[str]:
        """Get required environment variables for filter evaluation."""
        return ['LLM_API_KEY', 'EMBEDDINGS_LLM_API_KEY']

    def get_default_env_vars(self) -> Dict[str, str]:
        """Get default environment variables for filter evaluation."""
        return {
            'PROJECT_NAME': 'filter-eval',
            'PROJECT_VERSION': '1.0.0',
            'INPUT_REPORT_FILE_PATH': '/dev/null',
            'OUTPUT_FILE_PATH': '/dev/null',
            'REPO_LOCAL_PATH': str(self.project_root),
            'EMBEDDINGS_LLM_API_KEY': os.getenv('EMBEDDINGS_LLM_API_KEY', '')
        }

    def get_reports_dir(self) -> Path:
        """Get the reports directory for filter evaluation."""
        return self.project_root / "evaluation" / "reports" / "filter"

    def get_debug_hints(self) -> List[str]:
        """Get debug hints for filter evaluation."""
        return [
            "src/services/vector_store_service.py (FAISS creation)",
            "evaluation/tools/filter_converters.py (input/output conversion)",
            "src/sast_agent_workflow/tools/filter.py (filter logic)"
        ]

    def check_additional_env_vars(self) -> bool:
        """Check for additional environment variables specific to filter evaluation."""
        import os

        # Check for embedding API key with multiple possible names
        embedding_api_key = os.getenv('EMBEDDINGS_LLM_API_KEY')

        if not embedding_api_key:
            print("Error: EMBEDDINGS_LLM_API_KEY environment variable not set")
            print("Please set it with: export EMBEDDINGS_LLM_API_KEY=your_embedding_api_key")
            return False

        return True

    def run_post_evaluation_tasks(self):
        """Run filter validation analysis against ground truth."""
        print("\n" + "=" * 60)
        print("Running Filter Validation Analysis")
        print("=" * 60)

        validation_script = self.project_root / "evaluation" / "utils" / "filter_validation.py"
        dataset_file = self.project_root / "evaluation" / "dataset" / "filter_eval" / "filter_eval_dataset_individual_issues.json"
        reports_dir = self.get_reports_dir()

        try:
            print(f"Executing validation: python {validation_script} {reports_dir} {dataset_file}")
            result = subprocess.run([
                "python", str(validation_script), str(reports_dir), str(dataset_file)
            ], check=True, capture_output=True, text=True)

            print("Validation analysis completed successfully!")
            if result.stdout:
                print("Validation Output:")
                print(result.stdout)

            print("  - evaluation/reports/filter/filter_validation_report.json")
            return True

        except subprocess.CalledProcessError as e:
            print(f"Error running validation analysis: {e}")
            if e.stdout:
                print("Stdout:", e.stdout)
            if e.stderr:
                print("Stderr:", e.stderr)
            print("Warning: Validation analysis failed, but continuing with archival")
            return False
        except FileNotFoundError:
            print("Error: Python not found or validation script missing")
            print("Warning: Validation analysis failed, but continuing with archival")
            return False

def main():
    """Main evaluation runner."""
    runner = FilterEvaluationRunner()
    runner.run()

if __name__ == "__main__":
    main()