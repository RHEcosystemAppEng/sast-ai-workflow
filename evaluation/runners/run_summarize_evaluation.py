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

import sys
from pathlib import Path
from typing import List, Dict

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from evaluation.constants import REPORTS_SUMMARIZATION_DIR, DATASET_SUMMARIZATION_DIR, SUMMARIZATION_DATASET_FILENAME, SUMMARIZATION_CONFIG_FILENAME
from evaluation.runners.base_runner import BaseEvaluationRunner

class SummarizeEvaluationRunner(BaseEvaluationRunner):
    """Summarize justifications evaluation runner."""

    def __init__(self):
        super().__init__("summarize_justifications", SUMMARIZATION_CONFIG_FILENAME)

    def get_required_env_vars(self) -> List[str]:
        """Get required environment variables for summarize evaluation."""
        return ['LLM_API_KEY']

    def get_default_env_vars(self) -> Dict[str, str]:
        """Get default environment variables for summarize evaluation."""
        return {
            'PROJECT_NAME': 'test-eval',
            'PROJECT_VERSION': '1.0.0',
            'INPUT_REPORT_FILE_PATH': '/dev/null',
            'OUTPUT_FILE_PATH': '/dev/null',
            'REPO_LOCAL_PATH': str(self.project_root)
        }

    def get_reports_dir(self) -> Path:
        """Get the reports directory for summarize evaluation."""
        return self.project_root / REPORTS_SUMMARIZATION_DIR

    def additional_environment_checks(self) -> bool:
        """Additional checks for summarize evaluation."""
        dataset_path = self.project_root / DATASET_SUMMARIZATION_DIR / SUMMARIZATION_DATASET_FILENAME
        if not dataset_path.exists():
            print(f"Error: Dataset file not found: {dataset_path}")
            return False
        return True

    def get_debug_hints(self) -> List[str]:
        """Get debug hints for summarize evaluation."""
        return [
            "evaluation/tools/summarize_converters.py (input/output conversion)",
            "src/sast_agent_workflow/tools/summarize_justifications.py (summarization logic)",
            "src/templates/prompts/justification_summary_*.yaml (prompt templates)"
        ]

    def run_post_evaluation_tasks(self):
        """Run post-evaluation tasks for summarize evaluation."""
        print("\\nNote: Classification metrics not calculated for summarization task")
        print("Evaluation quality is measured through the summarization_quality_eval judge LLM")

def main():
    """Main evaluation runner."""
    runner = SummarizeEvaluationRunner()
    runner.run()

if __name__ == "__main__":
    main()