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
import sys
from pathlib import Path
from typing import List, Dict

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from evaluation.constants import REPORTS_JUDGE_LLM_DIR, WORKFLOW_OUTPUT_FILENAME, EVALUATION_METRICS_FILENAME, JUDGE_LLM_CONFIG_FILENAME
from evaluation.runners.base_runner import BaseEvaluationRunner
from evaluation.utils.calculate_eval_metrics import calculate_metrics_from_workflow

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

    def run_post_evaluation_tasks(self):
        """Calculate evaluation metrics for judge LLM evaluation."""
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
        else:
            print(f"\\nWarning: {WORKFLOW_OUTPUT_FILENAME} not found, skipping metrics calculation")

def main():
    """Main evaluation runner."""
    runner = JudgeLLMEvaluationRunner()
    runner.run()

if __name__ == "__main__":
    main()