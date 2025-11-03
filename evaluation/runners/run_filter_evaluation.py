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
import yaml
from pathlib import Path
from typing import List, Dict

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from evaluation.constants import (
    REPORTS_FILTER_DIR, DATASET_FILTER_DIR, FILTER_DATASET_FILENAME,
    FILTER_VALIDATION_REPORT_FILENAME, UTILS_DIR, FILTER_VALIDATION_SCRIPT,
    FILTER_CONFIG_FILENAME
)
from evaluation.runners.base_runner import BaseEvaluationRunner
from evaluation.utils.generate_evaluation_json import FilterJsonGenerator


class FilterEvaluationRunner(BaseEvaluationRunner):
    """Filter evaluation runner."""

    def __init__(self):
        super().__init__("filter", FILTER_CONFIG_FILENAME)

    def get_required_env_vars(self) -> List[str]:
        """Get required environment variables for filter evaluation."""
        return ['LLM_API_KEY', 'EMBEDDINGS_LLM_API_KEY']

    def get_default_env_vars(self) -> Dict[str, str]:
        """Get default environment variables for filter evaluation."""
        # Default to audit false positives file for local testing
        # (matches the default filter evaluation dataset which uses audit package)
        default_fp_path = os.path.expanduser('~/Dev/known-false-positives/audit/ignore.err')

        return {
            'PROJECT_NAME': 'filter-eval',
            'PROJECT_VERSION': '1.0.0',
            'INPUT_REPORT_FILE_PATH': '/dev/null',
            'OUTPUT_FILE_PATH': '/dev/null',
            'REPO_LOCAL_PATH': str(self.project_root),
            'EMBEDDINGS_LLM_API_KEY': os.getenv('EMBEDDINGS_LLM_API_KEY', ''),
            'KNOWN_FALSE_POSITIVE_FILE_PATH': os.getenv('KNOWN_FALSE_POSITIVE_FILE_PATH', default_fp_path)
        }

    def get_reports_dir(self) -> Path:
        """Get the reports directory for filter evaluation."""
        return self.project_root / REPORTS_FILTER_DIR

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

    def additional_environment_checks(self) -> bool:
        """Additional checks for filter evaluation."""
        eval_dataset_path = os.environ.get('EVALUATION_DATASET_PATH')

        if eval_dataset_path:
            print(f"Using dynamic dataset path from EVALUATION_DATASET_PATH: {eval_dataset_path}")

            if not Path(eval_dataset_path).exists():
                print(f"Error: Dynamic dataset file not found: {eval_dataset_path}")
                return False

            config_path = self.project_root / 'evaluation' / 'configs' / FILTER_CONFIG_FILENAME
            print(f"Updating config file: {config_path}")

            try:
                with open(config_path, 'r') as f:
                    config = yaml.safe_load(f)

                # DYNAMICALLY OVERRIDE THE DATASET PATH
                config['eval']['general']['dataset']['file_path'] = eval_dataset_path

                with open(config_path, 'w') as f:
                    yaml.dump(config, f, default_flow_style=False, sort_keys=False)

                print(f"Config updated to use dynamic dataset: {eval_dataset_path}")
                return True

            except Exception as e:
                print(f"Error updating config: {e}")
                return False
        else:
            # Use default path from config file
            dataset_path = self.project_root / DATASET_FILTER_DIR / FILTER_DATASET_FILENAME
            if not dataset_path.exists():
                print(f"Error: Dataset file not found: {dataset_path}")
                return False
            return True

    def run_post_evaluation_tasks(self):
        """Run filter validation analysis and generate JSON output."""
        print("\n" + "=" * 60)
        print("Running Filter Validation Analysis")
        print("=" * 60)

        validation_script = self.project_root / UTILS_DIR / FILTER_VALIDATION_SCRIPT
        dataset_file = self.project_root / DATASET_FILTER_DIR / FILTER_DATASET_FILENAME
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

            print(f"  - {REPORTS_FILTER_DIR}/{FILTER_VALIDATION_REPORT_FILENAME}")

        except subprocess.CalledProcessError as e:
            print(f"Error running validation analysis: {e}")
            if e.stdout:
                print("Stdout:", e.stdout)
            if e.stderr:
                print("Stderr:", e.stderr)
            print("Warning: Validation analysis failed, but continuing")
        except FileNotFoundError:
            print("Error: Python not found or validation script missing")
            print("Warning: Validation analysis failed, but continuing")

        print("\n" + "=" * 60)
        print("Generating JSON Output for Orchestrator")
        print("=" * 60)

        generator = FilterJsonGenerator(reports_dir, FILTER_DATASET_FILENAME)
        generator.generate_json()

        return True

def main():
    """Main evaluation runner."""
    runner = FilterEvaluationRunner()
    runner.run()

if __name__ == "__main__":
    main()