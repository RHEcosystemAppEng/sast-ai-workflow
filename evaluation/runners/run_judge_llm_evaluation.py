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

import json
import os
import sys
import yaml
from pathlib import Path
from typing import List, Dict

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from evaluation.constants import (
    REPORTS_JUDGE_LLM_DIR, WORKFLOW_OUTPUT_FILENAME, EVALUATION_METRICS_FILENAME,
    JUDGE_LLM_CONFIG_FILENAME, DATASET_JUDGE_LLM_DIR, JUDGE_LLM_DATASET_FILENAME
)
from evaluation.runners.base_runner import BaseEvaluationRunner
from evaluation.utils.calculate_eval_metrics import calculate_metrics_from_workflow
from evaluation.utils.generate_evaluation_json import JudgeLLMJsonGenerator

class JudgeLLMEvaluationRunner(BaseEvaluationRunner):
    """Judge LLM analysis evaluation runner."""

    def __init__(self):
        super().__init__("judge_llm_analysis", JUDGE_LLM_CONFIG_FILENAME)

    def get_required_env_vars(self) -> List[str]:
        """Get required environment variables for judge LLM evaluation."""
        return ['LLM_API_KEY']

    def get_default_env_vars(self) -> Dict[str, str]:
        """Get default environment variables for judge LLM evaluation."""
        return {
            'PROJECT_NAME': 'judge-llm-eval',
            'PROJECT_VERSION': '1.0.0',
            'INPUT_REPORT_FILE_PATH': '/dev/null',
            'OUTPUT_FILE_PATH': '/dev/null',
            'KNOWN_FALSE_POSITIVE_FILE_PATH': '/dev/null',
            'REPO_LOCAL_PATH': str(self.project_root)
        }

    def get_reports_dir(self) -> Path:
        """Get the reports directory for judge LLM evaluation."""
        return self.project_root / REPORTS_JUDGE_LLM_DIR

    def get_debug_hints(self) -> List[str]:
        """Get debug hints for judge LLM evaluation."""
        return [
            "evaluation/tools/judge_llm_converters.py (input/output conversion)",
            "src/sast_agent_workflow/tools/judge_llm_analysis.py (analysis logic)",
            "src/sast_agent_workflow/tools/iac.py (context analysis)"
        ]

    def additional_environment_checks(self) -> bool:
        """Additional checks for judge LLM evaluation."""
        eval_dataset_path = os.environ.get('EVALUATION_DATASET_PATH')

        if eval_dataset_path:
            print(f"Using dynamic dataset path from EVALUATION_DATASET_PATH: {eval_dataset_path}")

            if not Path(eval_dataset_path).exists():
                print(f"Error: Dynamic dataset file not found: {eval_dataset_path}")
                return False

            config_path = self.project_root / 'evaluation' / 'configs' / JUDGE_LLM_CONFIG_FILENAME
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
            dataset_path = self.project_root / DATASET_JUDGE_LLM_DIR / JUDGE_LLM_DATASET_FILENAME
            if not dataset_path.exists():
                print(f"Error: Dataset file not found: {dataset_path}")
                return False
            return True

    def run_post_evaluation_tasks(self):
        """Calculate evaluation metrics and generate JSON output for judge LLM evaluation."""
        workflow_output_path = self.get_reports_dir() / WORKFLOW_OUTPUT_FILENAME
        if workflow_output_path.exists():
            print("\\nCalculating evaluation metrics...")
            try:
                metrics_results = calculate_metrics_from_workflow(str(workflow_output_path))

                if "error" in metrics_results:
                    print(f"Warning: Could not calculate metrics - {metrics_results['error']}")
                else:
                    metrics_file = self.get_reports_dir() / EVALUATION_METRICS_FILENAME
                    with open(metrics_file, 'w') as f:
                        json.dump(metrics_results, f, indent=2)

                    metrics = metrics_results["metrics"]
                    metadata = metrics_results["metadata"]

                    print(f"  Processed {metadata['processed_items']}/{metadata['total_items']} items")
                    print(f"  Accuracy:  {metrics['accuracy']:.4f}")
                    print(f"  Precision: {metrics['precision']:.4f}")
                    print(f"  Recall:    {metrics['recall']:.4f}")
                    print(f"  F1 Score:  {metrics['f1_score']:.4f}")
                    print(f"  Metrics saved to: {EVALUATION_METRICS_FILENAME}")
            except Exception as e:
                print(f"Warning: Error calculating metrics - {e}")

            print("\\nGenerating JSON output for orchestrator...")
            reports_dir = self.get_reports_dir()
            generator = JudgeLLMJsonGenerator(reports_dir, JUDGE_LLM_DATASET_FILENAME)
            generator.generate_json()
        else:
            print(f"\\nWarning: {WORKFLOW_OUTPUT_FILENAME} not found, skipping metrics calculation")

def main():
    """Main evaluation runner."""
    runner = JudgeLLMEvaluationRunner()
    runner.run()

if __name__ == "__main__":
    main()