#!/usr/bin/env python3
"""
Base class for NAT evaluation runners.

This class provides common functionality for all evaluation runners,
including environment checking, NAT execution, and result archiving.

Dataset Configuration
---------------------
Evaluation datasets can be configured in NAT config YAML files to use local filesystem,
S3, or HTTP/HTTPS locations. The NAT framework automatically detects the location type
based on the file_path prefix.

LOCAL DATASET (Default - Recommended):
    Use relative paths from project root for datasets in the repository:

    Configuration:
        eval:
          general:
            dataset:
              _type: json
              file_path: evaluation/dataset/filter_eval/filter_eval_dataset_individual_issues.json
              structure:
                question_key: "question"
                answer_key: "expected_output_obj"

    When to use:
        - Default setup with datasets in the repo
        - Local development and testing
        - CI/CD pipelines with checked-out code
        - Version-controlled datasets

    Path formats:
        - Relative (recommended): evaluation/dataset/filter_eval/dataset.json
        - Absolute (if needed): /full/path/to/sast-ai-workflow/evaluation/dataset/dataset.json

    No additional configuration needed - works out of the box.

REMOTE DATASET (S3):
    Use S3 URLs for datasets stored in Amazon S3 buckets:

    Configuration:
        eval:
          general:
            dataset:
              _type: json
              file_path: s3://sast-ai-datasets/evaluation/filter_eval_dataset_individual_issues.json
              structure:
                question_key: "question"
                answer_key: "expected_output_obj"

    When to use:
        - Large datasets not suitable for git
        - Shared datasets across teams
        - Production deployments on AWS
        - Datasets that change frequently

    Setup requirements:
        1. Configure AWS credentials (choose one):
           - Environment variables:
             export AWS_ACCESS_KEY_ID=AKIA...
             export AWS_SECRET_ACCESS_KEY=your_secret_key
             export AWS_DEFAULT_REGION=us-east-1  # Optional

           - AWS credentials file (~/.aws/credentials):
             [default]
             aws_access_key_id = AKIA...
             aws_secret_access_key = your_secret_key

           - IAM role (for EC2/ECS - no config needed)

        2. Verify S3 access:
           aws s3 ls s3://sast-ai-datasets/evaluation/
           aws s3 cp s3://sast-ai-datasets/evaluation/dataset.json /tmp/test.json

        3. Ensure bucket permissions:
           - Bucket must allow s3:GetObject permission
           - Check bucket policy or IAM user/role permissions

    Common issues:
        - NoCredentialsError: AWS credentials not configured
        - AccessDenied: Check IAM permissions on bucket
        - NoSuchBucket: Verify bucket name and region

REMOTE DATASET (HTTP/HTTPS):
    Use HTTP/HTTPS URLs for datasets hosted on web servers:

    Configuration:
        eval:
          general:
            dataset:
              _type: json
              file_path: https://datasets.example.com/sast-ai/filter_eval_dataset.json
              structure:
                question_key: "question"
                answer_key: "expected_output_obj"

    When to use:
        - Publicly hosted datasets
        - Datasets behind CDN
        - Third-party dataset sources

    Requirements:
        - URL must be publicly accessible OR
        - Configure authentication in NAT config (if required)
        - HTTPS is recommended for security

    Verification:
        curl -I https://datasets.example.com/sast-ai/filter_eval_dataset.json

SWITCHING BETWEEN LOCATIONS:
    Simply change the file_path in your config YAML:

    Local:  file_path: evaluation/dataset/filter_eval/dataset.json
    S3:     file_path: s3://sast-ai-datasets/evaluation/dataset.json
    HTTP:   file_path: https://datasets.example.com/sast-ai/dataset.json

    The NAT framework automatically detects and handles each location type.

Dataset Structure Requirements:
    All evaluation datasets must follow the NAT format:
        - question_key: Field containing input data
        - answer_key: Field containing expected output

    Example dataset entry:
        {
          "id": "test_case_1",
          "question": "{...input data...}",
          "expected_output_obj": {...expected output...}
        }

File Path Best Practices:
    1. Use relative paths in configs for portability across environments
    2. Use constants from evaluation.constants for standard dataset locations in Python code
    3. Ensure remote URLs/S3 buckets are accessible before running evaluations
    4. Test dataset accessibility:
       - Local: ls -l evaluation/dataset/filter_eval/dataset.json
       - S3: aws s3 ls s3://bucket/path/dataset.json
       - HTTP: curl -I https://url/dataset.json
    5. For S3, verify bucket permissions and credentials are properly configured

See individual evaluation README files for dataset format specifications:
    - evaluation/README-filter.md
    - evaluation/README-judge-llm.md
    - evaluation/README-summarize.md
    - evaluation/dataset/README.md
"""

import logging
import os
import subprocess
import sys
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Optional, List, Dict, Any

logger = logging.getLogger(__name__)

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from evaluation.constants import (
    REPORTS_BASE_DIR,
    CONFIGS_DIR,
    WORKFLOW_OUTPUT_FILENAME,
    STANDARDIZED_DATA_FILENAME,
    PROFILER_TRACES_FILENAME,
    RUNNERS_DIR
)
from evaluation.utils import archive_evaluation_results
from nat.cli.main import run_cli


class BaseEvaluationRunner(ABC):
    """Base class for NAT evaluation runners."""

    def __init__(self, evaluation_name: str, config_filename: str):
        """
        Initialize the base runner.

        Args:
            evaluation_name: Name of the evaluation (e.g., "filter", "judge_llm_analysis", "summarize_justifications")
            config_filename: Name of the config file (e.g., "filter_eval.yml")
        """
        self.evaluation_name = evaluation_name
        self.config_filename = config_filename
        self.project_root = project_root
        self.config_file = None
        self.debug_mode = False

    @abstractmethod
    def get_required_env_vars(self) -> List[str]:
        """
        Get list of required environment variables.

        Returns:
            List of required environment variable names
        """
        pass

    @abstractmethod
    def get_default_env_vars(self) -> Dict[str, str]:
        """
        Get default environment variables for this evaluation.

        Returns:
            Dictionary of environment variable names and default values
        """
        pass

    @abstractmethod
    def get_reports_dir(self) -> Path:
        """
        Get the reports directory for this evaluation.

        Returns:
            Path to the reports directory
        """
        pass

    def get_display_name(self) -> str:
        """
        Get the display name for this evaluation.

        Returns:
            Human-readable display name
        """
        return self.evaluation_name.replace('_', ' ').title()

    def check_environment(self, config_file: Optional[str] = None) -> bool:
        """Check if required environment variables and files are available."""
        # Check required environment variables
        required_vars = self.get_required_env_vars()
        for var_name in required_vars:
            if not os.getenv(var_name):
                logger.error(f"Error: {var_name} environment variable not set")
                if var_name == 'LLM_API_KEY':
                    logger.error("Please set it with: export LLM_API_KEY=your_nvidia_api_key")
                elif var_name == 'EMBEDDINGS_LLM_API_KEY':
                    logger.error("Please set it with: export EMBEDDINGS_LLM_API_KEY=your_embedding_api_key")
                return False

        # Check config file
        if config_file:
            config_path = Path(config_file)
        else:
            config_path = self.project_root / CONFIGS_DIR / self.config_filename

        if not config_path.exists():
            logger.error(f"Error: Config file not found: {config_path}")
            return False

        # Additional checks can be overridden by subclasses
        if not self.additional_environment_checks():
            return False

        logger.info("Environment checks passed")
        return True

    def additional_environment_checks(self) -> bool:
        """
        Additional environment checks specific to the evaluation type.

        Returns:
            True if all checks pass, False otherwise
        """
        return True

    def setup_evaluation_environment(self):
        """Set up required environment variables for SAST Config."""
        logger.info("Setting up evaluation environment variables...")

        default_vars = self.get_default_env_vars()
        for var_name, default_value in default_vars.items():
            os.environ.setdefault(var_name, default_value)

    def get_config_path(self) -> Path:
        """Get the config file path."""
        if self.config_file:
            return Path(self.config_file)
        else:
            return self.project_root / CONFIGS_DIR / self.config_filename

    def run_nat_evaluation(self, config_file: Optional[str] = None, debug_mode: bool = False):
        """Run NAT evaluation with automatic metrics collection."""
        logger.info(f"\nRunning NAT Evaluation for {self.evaluation_name}...")
        logger.info("This will automatically collect:")
        logger.info("- Token counts (input/output/total)")
        logger.info("- Processing time metrics")
        logger.info("- Memory usage tracking")
        logger.info("- Error counting")
        logger.info("")

        config_path = self.get_config_path()

        if debug_mode:
            self._run_nat_debug_mode(config_path)
        else:
            self._run_nat_subprocess_mode(config_path)

    def _run_nat_debug_mode(self, config_path: Path):
        """Run NAT evaluation in debug mode (direct Python call)."""
        logger.info("🐛 DEBUG MODE: Running NAT evaluation directly in Python")
        logger.info("You can set breakpoints in PyCharm and they will be hit!")

        # Add debug hints specific to the evaluation type
        debug_hints = self.get_debug_hints()
        if debug_hints:
            logger.info("Key places to set breakpoints:")
            for hint in debug_hints:
                logger.info(f"  - {hint}")
            logger.info("")

        original_argv = sys.argv.copy()
        sys.argv = ['nat', 'eval', '--config_file', str(config_path)]
        try:
            logger.info(f"Executing NAT directly: {' '.join(sys.argv)}")
            run_cli()
        finally:
            sys.argv = original_argv
        logger.info("NAT evaluation completed successfully!")

    def get_debug_hints(self) -> List[str]:
        """
        Get debug hints for this evaluation type.

        Returns:
            List of debug hint strings
        """
        return []

    def _run_nat_subprocess_mode(self, config_path: Path):
        """Run NAT evaluation using subprocess (original method)."""
        try:
            logger.info(f"Executing: nat eval --config_file {config_path}")
            result = subprocess.run([
                "nat", "eval", "--config_file", str(config_path)
            ], check=True, capture_output=True, text=True)

            logger.info("NAT evaluation completed successfully!")
            if result.stdout:
                logger.info(f"Output: {result.stdout}")

        except subprocess.CalledProcessError as e:
            logger.error(f"Error running NAT evaluation: {e}")
            if e.stdout:
                logger.error(f"Stdout: {e.stdout}")
            if e.stderr:
                logger.error(f"Stderr: {e.stderr}")
            raise
        except FileNotFoundError:
            logger.info("Error: 'nat' command not found. Please ensure NAT is installed and in PATH.")
            logger.info("Try: source .venv-test/bin/activate")
            raise

    def parse_command_line_args(self, args: List[str]):
        """Parse command line arguments."""
        for arg in args:
            if arg == "--debug" or arg == "-d":
                self.debug_mode = True
                logger.info("🐛 Debug mode enabled")
            elif not arg.startswith("-"):
                self.config_file = arg

    def print_evaluation_header(self):
        """Print the evaluation header."""
        logger.info("=" * 60)
        logger.info(f"SAST-AI-Workflow: {self.get_display_name()} Evaluation")
        logger.info("=" * 60)

    def print_results_info(self):
        """Print information about where results are saved."""
        logger.info("\nEvaluation completed!")
        logger.info("Results saved to:")
        reports_dir = self.get_reports_dir()
        logger.info(f"  - {reports_dir}/{WORKFLOW_OUTPUT_FILENAME}")
        logger.info(f"  - {reports_dir}/{STANDARDIZED_DATA_FILENAME}")
        logger.info(f"  - {reports_dir}/{PROFILER_TRACES_FILENAME}")

    def archive_results(self) -> Optional[str]:
        """Archive evaluation results."""
        reports_dir = self.project_root / REPORTS_BASE_DIR
        archived_path = archive_evaluation_results(str(reports_dir), self.evaluation_name)
        if archived_path:
            logger.info(f"\nResults archived to: {archived_path}")
            return archived_path
        else:
            logger.info("\nNote: Results were not archived (no files found)")
            return None

    def run_post_evaluation_tasks(self):
        """
        Run any post-evaluation tasks specific to the evaluation type.
        Override this method in subclasses for custom post-processing.
        """
        pass

    def print_usage_info(self):
        """Print usage information."""
        required_vars = self.get_required_env_vars()
        logger.info("\\nTo run this evaluation, use:")
        for var in required_vars:
            if var == 'LLM_API_KEY':
                logger.info("  export LLM_API_KEY=your_nvidia_api_key")
            elif var == 'EMBEDDINGS_LLM_API_KEY':
                logger.info("  export EMBEDDINGS_LLM_API_KEY=your_embedding_api_key")
        logger.info(f"  python {RUNNERS_DIR}/run_{self.evaluation_name}_evaluation.py")
        logger.info("\\nFor PyCharm debugging with breakpoints:")
        logger.info(f"  python {RUNNERS_DIR}/run_{self.evaluation_name}_evaluation.py --debug")

    def run(self):
        """Main runner method."""
        self.print_evaluation_header()

        # Parse command line arguments
        self.parse_command_line_args(sys.argv[1:])

        # Check environment
        if not self.check_environment(self.config_file):
            sys.exit(1)

        # Setup environment
        self.setup_evaluation_environment()

        # Run evaluation
        self.run_nat_evaluation(self.config_file, self.debug_mode)

        # Print results
        self.print_results_info()

        # Run post-evaluation tasks
        self.run_post_evaluation_tasks()

        # Print usage info
        self.print_usage_info()

        # Archive results
        self.archive_results()


__all__ = ['BaseEvaluationRunner']