#!/usr/bin/env python3
"""
Base class for NAT evaluation converters.

This class provides common functionality for all evaluation converters,
including configuration setup, environment handling, and basic conversion flow.
"""

import json
import logging
import os
import sys
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Dict, Any, Optional

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from dto.SASTWorkflowModels import SASTWorkflowTracker

try:
    from common.config import Config
except ImportError:
    try:
        from Config import Config
    except ImportError:
        Config = None

logger = logging.getLogger(__name__)


class BaseEvaluationConverter(ABC):
    """Base class for NAT evaluation converters."""

    def __init__(self, evaluation_name: str, dataset_filename: str):
        """
        Initialize the base converter.

        Args:
            evaluation_name: Name of the evaluation (e.g., "filter", "judge_llm", "summarize")
            dataset_filename: Name of the dataset file (e.g., "filter_eval_dataset.json")
        """
        self.evaluation_name = evaluation_name
        self.dataset_filename = dataset_filename
        self.project_root = project_root

    @abstractmethod
    def parse_input_data(self, input_str: str) -> Dict[str, Any]:
        """
        Parse input string into structured data.

        Args:
            input_str: The evaluation input string

        Returns:
            Dictionary containing parsed input data
        """
        pass

    @abstractmethod
    def create_issue_objects(self, parsed_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Create Issue objects from parsed data.

        Args:
            parsed_data: Dictionary containing parsed input data

        Returns:
            Dictionary mapping issue_id to PerIssueData objects
        """
        pass

    @abstractmethod
    def extract_output_data(self, tracker: SASTWorkflowTracker) -> Dict[str, Any]:
        """
        Extract output data from processed tracker.

        Args:
            tracker: The processed SASTWorkflowTracker

        Returns:
            Dictionary containing evaluation-specific output data
        """
        pass

    @abstractmethod
    def get_minimal_config(self):
        """
        Get evaluation-specific minimal config.

        Returns:
            Minimal configuration object for this evaluation type
        """
        pass

    def setup_environment(self, **kwargs):
        """Setup common environment variables."""
        defaults = {
            'INPUT_REPORT_FILE_PATH': '/dev/null',
            'OUTPUT_FILE_PATH': '/dev/null',
            'KNOWN_FALSE_POSITIVE_FILE_PATH': '/dev/null',
            'PROJECT_NAME': f'{self.evaluation_name}-eval',
            'PROJECT_VERSION': '1.0.0',
            'REPO_LOCAL_PATH': str(self.project_root)
        }
        defaults.update(kwargs)

        for key, value in defaults.items():
            os.environ.setdefault(key, value)

    def create_config(self):
        """Create Config object with fallback to minimal config."""
        self.setup_environment()

        try:
            if Config is not None:
                config = Config()
                logger.info(f"Successfully created real Config object for {self.evaluation_name} evaluation")
                return config
            else:
                raise ImportError("Config class not available")
        except Exception as e:
            logger.info(f"Main Config failed ({e}), using minimal config for {self.evaluation_name} evaluation")
            return self.get_minimal_config()

    def load_dataset_entry(self, input_str: str) -> Optional[Dict[str, Any]]:
        """Load dataset entry from evaluation dataset."""
        try:
            from evaluation.tools.convertor_utils import load_evaluation_dataset, find_dataset_entry

            dataset = load_evaluation_dataset(self.dataset_filename)
            return find_dataset_entry(dataset, input_str)
        except Exception as e:
            logger.error(f"Failed to load dataset entry: {e}")
            return None

    def create_empty_tracker(self) -> Optional[SASTWorkflowTracker]:
        """Create empty tracker for error cases."""
        try:
            config = self.create_config()
            return SASTWorkflowTracker(issues={}, config=config)
        except Exception as e:
            logger.error(f"Failed to create empty tracker: {e}")
            return None

    def convert_str_to_sast_tracker(self, input_str: str) -> SASTWorkflowTracker:
        """
        Main conversion method from string to SASTWorkflowTracker.

        This method orchestrates the conversion process using the abstract methods
        that each converter implements.

        Args:
            input_str: The evaluation input string

        Returns:
            SASTWorkflowTracker configured for this evaluation
        """
        logger.info(f"Converting evaluation input to SASTWorkflowTracker: {input_str}")

        try:
            # Parse input data (implementation varies by converter)
            parsed_data = self.parse_input_data(input_str)

            # Create tracker with config
            config = self.create_config()
            tracker = SASTWorkflowTracker(config=config, issues={})

            # Create and add issue objects (implementation varies by converter)
            issues = self.create_issue_objects(parsed_data)
            tracker.issues = issues

            logger.info(f"Created SASTWorkflowTracker for {self.evaluation_name} evaluation with {len(tracker.issues)} issues")
            return tracker

        except Exception as e:
            logger.error(f"Failed to convert input to SASTWorkflowTracker: {e}")
            return self.create_empty_tracker() or SASTWorkflowTracker(issues={}, config=self.get_minimal_config())

    def convert_sast_tracker_to_str(self, tracker: SASTWorkflowTracker) -> str:
        """
        Main output conversion method from SASTWorkflowTracker to string.

        This method orchestrates the output conversion process using the abstract
        extract_output_data method that each converter implements.

        Args:
            tracker: The processed SASTWorkflowTracker

        Returns:
            JSON string containing evaluation results
        """
        try:
            # Extract output data (implementation varies by converter)
            output_data = self.extract_output_data(tracker)

            # Convert to JSON string
            output = json.dumps(output_data, indent=2)
            logger.info(f"Converted SASTWorkflowTracker to {self.evaluation_name} results")
            return output

        except Exception as e:
            logger.error(f"Failed to convert SASTWorkflowTracker to string: {e}")
            return json.dumps({"error": f"Conversion failed: {str(e)}"}, indent=2)


__all__ = ['BaseEvaluationConverter']