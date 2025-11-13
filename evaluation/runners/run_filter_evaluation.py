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

import logging
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

logger = logging.getLogger(__name__)


class FilterEvaluationRunner(BaseEvaluationRunner):
    """Filter evaluation runner."""

    def __init__(self):
        super().__init__("filter", FILTER_CONFIG_FILENAME)

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
            'EMBEDDINGS_LLM_API_KEY': os.getenv('EMBEDDINGS_LLM_API_KEY', ''),
            'KNOWN_FALSE_POSITIVE_FILE_PATH': os.getenv('KNOWN_FALSE_POSITIVE_FILE_PATH', '')
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
            logger.error("EMBEDDINGS_LLM_API_KEY environment variable not set")
            logger.error("Please set it with: export EMBEDDINGS_LLM_API_KEY=your_embedding_api_key")
            return False

        return True

    def additional_environment_checks(self) -> bool:
        """Additional checks for filter evaluation."""
        eval_dataset_path = os.environ.get('EVALUATION_DATASET_PATH')

        if eval_dataset_path:
            logger.info(f"Using dynamic dataset path from EVALUATION_DATASET_PATH: {eval_dataset_path}")

            if not Path(eval_dataset_path).exists():
                logger.error(f"Dynamic dataset file not found: {eval_dataset_path}")
                return False

            config_path = self.project_root / 'evaluation' / 'configs' / FILTER_CONFIG_FILENAME
            logger.info(f"Updating config file: {config_path}")

            try:
                with open(config_path, 'r') as f:
                    config = yaml.safe_load(f)

                # DYNAMICALLY OVERRIDE THE DATASET PATH
                config['eval']['general']['dataset']['file_path'] = eval_dataset_path

                with open(config_path, 'w') as f:
                    yaml.dump(config, f, default_flow_style=False, sort_keys=False)

                logger.info(f"Config updated to use dynamic dataset: {eval_dataset_path}")
                return True

            except Exception as e:
                logger.error(f"Error updating config: {e}")
                return False
        else:
            # Use default path from config file
            dataset_path = self.project_root / DATASET_FILTER_DIR / FILTER_DATASET_FILENAME
            if not dataset_path.exists():
                logger.error(f"Dataset file not found: {dataset_path}")
                return False
            return True

    def run_post_evaluation_tasks(self):
        """Run filter validation analysis and generate JSON output."""
        logger.info("=" * 60)
        logger.info("Running Filter Validation Analysis")
        logger.info("=" * 60)

        validation_script = self.project_root / UTILS_DIR / FILTER_VALIDATION_SCRIPT

        # Use dynamic dataset path if available, otherwise use default
        eval_dataset_path = os.environ.get('EVALUATION_DATASET_PATH')
        if eval_dataset_path:
            dataset_file = Path(eval_dataset_path)
            logger.info(f"Using dynamic dataset for validation: {dataset_file}")
        else:
            dataset_file = self.project_root / DATASET_FILTER_DIR / FILTER_DATASET_FILENAME
            logger.info(f"Using default dataset for validation: {dataset_file}")

        reports_dir = self.get_reports_dir()

        try:
            logger.info(f"Executing validation: python {validation_script} {reports_dir} {dataset_file}")
            result = subprocess.run([
                "python", str(validation_script), str(reports_dir), str(dataset_file)
            ], check=True, capture_output=True, text=True)

            logger.info("Validation analysis completed successfully!")
            if result.stdout:
                logger.info("Validation Output:")
                logger.info(result.stdout)

            logger.info(f"  - {REPORTS_FILTER_DIR}/{FILTER_VALIDATION_REPORT_FILENAME}")

        except subprocess.CalledProcessError as e:
            logger.error(f"Error running validation analysis: {e}")
            if e.stdout:
                logger.error(f"Stdout: {e.stdout}")
            if e.stderr:
                logger.error(f"Stderr: {e.stderr}")
            logger.warning("Validation analysis failed, but continuing")
        except FileNotFoundError:
            logger.error("Python not found or validation script missing")
            logger.warning("Validation analysis failed, but continuing")

        logger.info("=" * 60)
        logger.info("Generating JSON Output for Orchestrator")
        logger.info("=" * 60)

        generator = FilterJsonGenerator(reports_dir, FILTER_DATASET_FILENAME)

        # Check if running in Tekton evaluation mode with direct file output
        output_file = os.getenv('EVALUATION_JSON_OUTPUT', None)
        if output_file:
            generator.generate_json_to_file(output_file, summary_only=True, tekton_compact=True)
            logger.info(f"Evaluation results written to: {output_file}")
        else:
            generator.generate_json()

        return True


def main():
    """Main evaluation runner."""
    runner = FilterEvaluationRunner()
    runner.run()

if __name__ == "__main__":
    main()