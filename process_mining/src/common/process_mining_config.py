"""
Process Mining Configuration Loader - Preprocessing Stage

This module provides configuration management for preprocessing operations,
including data preparation, validation, and train/val/test splitting.
"""

import logging
import os
from typing import Optional

import yaml

logger = logging.getLogger(__name__)


class ProcessMiningConfig:
    """Configuration loader for preprocessing operations."""

    def __init__(self, config_path: Optional[str] = None):
        """
        Load preprocessing configuration from YAML file.

        Args:
            config_path: Path to configuration file. If None, uses default path.
        """
        self.config_path = config_path or self._get_default_config_path()
        self.load_config()

    def _get_default_config_path(self) -> str:
        """Get default config path."""
        return os.path.join(
            os.path.dirname(__file__), "..", "..", "config", "process_mining_config.yaml"
        )

    def load_config(self):
        """Load configuration from YAML file with fallback to hardcoded defaults."""
        try:
            with open(self.config_path, "r") as f:
                config = yaml.safe_load(f)

            self.PATTERN_PREPARATION = config.get('PATTERN_PREPARATION', {})
            self.TRAIN_VAL_TEST_SPLIT = config.get('TRAIN_VAL_TEST_SPLIT', {})
            self.DATA_PROCESSING = config.get('DATA_PROCESSING', {})
            self.VALIDATION_RULES = config.get('VALIDATION_RULES', {})

            logger.debug(f"Loaded preprocessing config from: {self.config_path}")

        except FileNotFoundError:
            logger.warning(
                f"Config file not found: {self.config_path}. Using hardcoded defaults."
            )
            self._load_hardcoded_defaults()
        except Exception as e:
            logger.warning(
                f"Error loading config file: {e}. Using hardcoded defaults."
            )
            self._load_hardcoded_defaults()

    def _load_hardcoded_defaults(self):
        """Load hardcoded defaults when config file is missing or malformed."""
        logger.info("Loading hardcoded default configuration values")

        self.PATTERN_PREPARATION = {
            'ground_truth_dir': 'process_mining/data/ground-truth',
            'output_dir': 'process_mining/data/pattern_data',
            'rpm_cache_dir': '/tmp/rpm_cache',
            'default_source_mode': 'rpm',
            'false_positive_markers': ['y', 'yes', 'true']
        }

        self.TRAIN_VAL_TEST_SPLIT = {
            'train_ratio': 0.60,
            'validation_ratio': 0.20,
            'test_ratio': 0.20,
            'split_ratio_tolerance': 0.03,
            'fp_tp_ratio_tolerance': 0.03,
            'random_seed': 43,
            'size_categories': {
                'small_max': 5,
                'medium_max': 20
            },
            'fp_ratio_buckets': {
                'low_max': 0.25,
                'high_min': 0.75
            }
        }

        self.DATA_PROCESSING = {
            'column_names': {
                'hint': 'hint',
                'comment': 'comment',
                'false_positive': 'false positive?',
                'finding': 'finding',
                'issue_id': 'issue id',
                'ai_prediction': 'ai prediction'
            },
            'directories': {
                'full_dataset': 'full_dataset',
                'processed_known_non_issues': 'processed_known_non_issues',
                'excel_subdir': 'excel',
                'known_non_issue_subdir': 'known_non_issue'
            },
            'file_patterns': {
                'excel_extension': '.xlsx',
                'temp_file_prefix': '~',
                'ignore_err_suffix': '_ignore.err'
            }
        }

        self.VALIDATION_RULES = {
            'excel': {
                'required_columns': ['finding', 'false positive?'],
                'justification_columns': ['hint', 'comment'],
                'min_justification_rows': 1
            },
            'ignore_err': {
                'require_justification_comments': True,
                'justification_marker': '#'
            },
            'failure_categories': {
                'no_justification': 'no_justification',
                'empty_justification': 'empty_justification',
                'missing_fp_column': 'missing_fp_column',
                'empty_fp_annotations': 'empty_fp_annotations',
                'read_error': 'read_error',
                'source_unavailable': 'source_code_unavailable',
                'nvr_parse_error': 'nvr_parse_error',
                'other': 'other'
            }
        }

    def get_preparation_config(self) -> dict:
        """
        Get pattern preparation configuration.

        Returns:
            Dictionary containing preparation configuration
        """
        return self.PATTERN_PREPARATION

    def get_split_config(self) -> dict:
        """
        Get train/validation/test split configuration.

        Returns:
            Dictionary containing split configuration
        """
        return self.TRAIN_VAL_TEST_SPLIT

    def get_data_processing_config(self) -> dict:
        """
        Get data processing configuration.

        Returns:
            Dictionary containing data processing configuration including
            column names, directory names, and file patterns
        """
        return self.DATA_PROCESSING

    def get_validation_rules(self) -> dict:
        """
        Get validation rules configuration.

        Returns:
            Dictionary containing validation rules for Excel and ignore.err files
        """
        return self.VALIDATION_RULES

    def get_stratification_config(self) -> dict:
        """
        Get stratification configuration for dataset splitting.

        Returns:
            Dictionary containing size categories and FP ratio buckets
        """
        split_config = self.get_split_config()
        return {
            'size_categories': split_config.get('size_categories', {}),
            'fp_ratio_buckets': split_config.get('fp_ratio_buckets', {})
        }
