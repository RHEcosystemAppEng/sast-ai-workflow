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

import logging
import os
import sys
import yaml
from pathlib import Path
from typing import List, Dict

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from evaluation.constants import (
    REPORTS_SUMMARIZATION_DIR,
    DATASET_SUMMARIZATION_DIR,
    SUMMARIZATION_DATASET_FILENAME,
    SUMMARIZATION_CONFIG_FILENAME
)
from evaluation.runners.base_runner import BaseEvaluationRunner
from evaluation.utils.generate_evaluation_json import SummarizeJsonGenerator

logger = logging.getLogger(__name__)

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
        eval_dataset_path = os.environ.get('EVALUATION_DATASET_PATH')

        if eval_dataset_path:
            logger.info(f"Using dynamic dataset path from EVALUATION_DATASET_PATH: {eval_dataset_path}")

            if not Path(eval_dataset_path).exists():
                logger.error(f"Dynamic dataset file not found: {eval_dataset_path}")
                return False

            config_path = self.project_root / 'evaluation' / 'configs' / SUMMARIZATION_CONFIG_FILENAME
            logger.info(f"Updating config file: {config_path}")

            try:
                with open(config_path, 'r') as f:
                    config = yaml.safe_load(f)

                config['eval']['general']['dataset']['file_path'] = eval_dataset_path

                with open(config_path, 'w') as f:
                    yaml.dump(config, f, default_flow_style=False, sort_keys=False)

                logger.info(f"Config updated to use dynamic dataset: {eval_dataset_path}")
                return True

            except Exception as e:
                logger.error(f"Error updating config: {e}")
                return False
        else:
            dataset_path = self.project_root / DATASET_SUMMARIZATION_DIR / SUMMARIZATION_DATASET_FILENAME
            if not dataset_path.exists():
                logger.error(f"Dataset file not found: {dataset_path}")
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
        reports_dir = self.get_reports_dir()
        generator = SummarizeJsonGenerator(reports_dir, SUMMARIZATION_DATASET_FILENAME)

        # Check if running in Tekton evaluation mode with direct file output
        output_file = os.getenv('EVALUATION_JSON_OUTPUT', None)
        if output_file:
            generator.generate_json_to_file(output_file)
            logger.info(f"Evaluation results written to: {output_file}")
        else:
            generator.generate_json()

        logger.info("Note: Classification metrics not calculated for summarization task")
        logger.info("Evaluation quality is measured through the summarization_quality_eval judge LLM")

def main():
    """Main evaluation runner."""
    runner = SummarizeEvaluationRunner()
    runner.run()

if __name__ == "__main__":
    main()