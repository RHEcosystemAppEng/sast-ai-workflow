#!/usr/bin/env python3
"""
Base class for NAT evaluation runners.

This class provides common functionality for all evaluation runners,
including environment checking, NAT execution, and result archiving.
"""

import os
import subprocess
import sys
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Optional, List, Dict, Any

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
                print(f"Error: {var_name} environment variable not set")
                if var_name == 'LLM_API_KEY':
                    print("Please set it with: export LLM_API_KEY=your_nvidia_api_key")
                elif var_name == 'EMBEDDINGS_LLM_API_KEY':
                    print("Please set it with: export EMBEDDINGS_LLM_API_KEY=your_embedding_api_key")
                return False

        # Check config file
        if config_file:
            config_path = Path(config_file)
        else:
            config_path = self.project_root / CONFIGS_DIR / self.config_filename

        if not config_path.exists():
            print(f"Error: Config file not found: {config_path}")
            return False

        # Additional checks can be overridden by subclasses
        if not self.additional_environment_checks():
            return False

        print("Environment checks passed")
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
        print("Setting up evaluation environment variables...")

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
        print(f"\\nRunning NAT Evaluation for {self.evaluation_name}...")
        print("This will automatically collect:")
        print("- Token counts (input/output/total)")
        print("- Processing time metrics")
        print("- Memory usage tracking")
        print("- Error counting")
        print("")

        config_path = self.get_config_path()

        if debug_mode:
            self._run_nat_debug_mode(config_path)
        else:
            self._run_nat_subprocess_mode(config_path)

    def _run_nat_debug_mode(self, config_path: Path):
        """Run NAT evaluation in debug mode (direct Python call)."""
        print("🐛 DEBUG MODE: Running NAT evaluation directly in Python")
        print("You can set breakpoints in PyCharm and they will be hit!")

        # Add debug hints specific to the evaluation type
        debug_hints = self.get_debug_hints()
        if debug_hints:
            print("Key places to set breakpoints:")
            for hint in debug_hints:
                print(f"  - {hint}")
            print("")

        original_argv = sys.argv.copy()
        sys.argv = ['nat', 'eval', '--config_file', str(config_path)]
        try:
            print(f"Executing NAT directly: {' '.join(sys.argv)}")
            run_cli()
        finally:
            sys.argv = original_argv
        print("NAT evaluation completed successfully!")

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

    def parse_command_line_args(self, args: List[str]):
        """Parse command line arguments."""
        for arg in args:
            if arg == "--debug" or arg == "-d":
                self.debug_mode = True
                print("🐛 Debug mode enabled")
            elif not arg.startswith("-"):
                self.config_file = arg

    def print_evaluation_header(self):
        """Print the evaluation header."""
        print("=" * 60)
        print(f"SAST-AI-Workflow: {self.get_display_name()} Evaluation")
        print("=" * 60)

    def print_results_info(self):
        """Print information about where results are saved."""
        print("\\nEvaluation completed!")
        print("Results saved to:")
        reports_dir = self.get_reports_dir()
        print(f"  - {reports_dir}/{WORKFLOW_OUTPUT_FILENAME}")
        print(f"  - {reports_dir}/{STANDARDIZED_DATA_FILENAME}")
        print(f"  - {reports_dir}/{PROFILER_TRACES_FILENAME}")

    def archive_results(self) -> Optional[str]:
        """Archive evaluation results."""
        reports_dir = self.project_root / REPORTS_BASE_DIR
        archived_path = archive_evaluation_results(str(reports_dir), self.evaluation_name)
        if archived_path:
            print(f"\\nResults archived to: {archived_path}")
            return archived_path
        else:
            print("\\nNote: Results were not archived (no files found)")
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
        print("\\nTo run this evaluation, use:")
        for var in required_vars:
            if var == 'LLM_API_KEY':
                print("  export LLM_API_KEY=your_nvidia_api_key")
            elif var == 'EMBEDDINGS_LLM_API_KEY':
                print("  export EMBEDDINGS_LLM_API_KEY=your_embedding_api_key")
        print(f"  python {RUNNERS_DIR}/run_{self.evaluation_name}_evaluation.py")
        print("\\nFor PyCharm debugging with breakpoints:")
        print(f"  python {RUNNERS_DIR}/run_{self.evaluation_name}_evaluation.py --debug")

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