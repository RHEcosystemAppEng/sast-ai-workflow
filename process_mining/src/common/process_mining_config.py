"""
Process Mining Configuration Loader

This module provides configuration management for process mining operations,
including pattern aggregation and data preparation.
"""

import logging
import os
from typing import Optional

import yaml

logger = logging.getLogger(__name__)


class ProcessMiningConfig:
    """Configuration loader for process mining operations."""

    def __init__(self, config_path: Optional[str] = None):
        """
        Load process mining configuration from YAML file.

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

            self.PATTERN_AGGREGATION = config.get('PATTERN_AGGREGATION', {})
            self.PATTERN_PREPARATION = config.get('PATTERN_PREPARATION', {})
            self.TRAIN_VAL_TEST_SPLIT = config.get('TRAIN_VAL_TEST_SPLIT', {})
            self.DATA_PROCESSING = config.get('DATA_PROCESSING', {})
            self.VALIDATION_RULES = config.get('VALIDATION_RULES', {})
            self.PATTERN_LEARNING_PROMPTS = config.get('PATTERN_LEARNING_PROMPTS', {})
            self.BATCH_PATTERN_LEARNING = config.get('BATCH_PATTERN_LEARNING', {})

            self.validate_confidence_weights()
            logger.debug(f"Loaded process mining config from: {self.config_path}")

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

        self.PATTERN_AGGREGATION = {
            'confidence_weights': {
                'coverage': 0.4,
                'frequency': 0.3,
                'consistency': 0.2,
                'evidence_quality': 0.1
            },
            'consistency_calculation': {
                'rca_weight': 0.4,
                'function_weight': 0.4,
                'issue_type_weight': 0.2
            },
            'pattern_confidence_fallback': 0.5,
            'min_package_threshold': 2,
            'min_confidence_threshold': 0.7,
            'evidence_quality': {
                'high_threshold': 5,
                'medium_threshold': 1
            },
            'generalizability': {
                'high_threshold': 5,
                'medium_threshold': 2
            },
            'pattern_matching': {
                'min_shared_concepts': 2,
                'strong_match_threshold': 3
            },
            'key_concepts': [
                'ownership', 'transfer', 'reference', 'count', 'glib', 'gerror',
                'propagate', 'leak', 'free', 'cleanup', 'destructor', 'task',
                'callback', 'error', 'path', 'bounds', 'check', 'validation',
                'early', 'return', 'initialization', 'mutex', 'lock', 'thread'
            ],
            'api_families': [
                {'pattern': 'g_object', 'family_name': 'g_object_ref/unref'},
                {'pattern': 'g_task', 'family_name': 'g_task'},
                {'pattern': 'g_error', 'family_name': 'g_error'},
                {'pattern': 'pthread', 'family_name': 'pthreads'}
            ],
            'function_prefix_parts': 2,
            'default_patterns_dir': 'process_mining/data/patterns/',
            'default_output_file': 'process_mining/data/patterns/rhel_c_patterns_final.json'
        }

        self.PATTERN_PREPARATION = {
            'ground_truth_dir': 'process_mining/data/ground-truth',
            'output_dir': 'process_mining/data/pattern_data',
            'rpm_cache_dir': '/tmp/rpm_cache',
            'default_source_mode': 'rpm',
            'false_positive_markers': ['y', 'yes', 'true']
        }

        self.TRAIN_VAL_TEST_SPLIT = {
            'train_ratio': 0.70,
            'validation_ratio': 0.10,
            'test_ratio': 0.20,
            'split_ratio_tolerance': 0.02,
            'fp_tp_ratio_tolerance': 0.03,
            'random_seed': 42,
            'size_categories': {
                'small_max': 5,
                'medium_max': 20
            },
            'fp_ratio_buckets': {
                'low_max': 0.25,
                'high_min': 0.75
            }
        }

        self.PATTERN_LEARNING_PROMPTS = {
            'confidence_ranges': {
                'high': [0.8, 1.0],
                'medium': [0.5, 0.8],
                'low': [0.0, 0.5]
            },
            'aggregation': {
                'sast_assumption_similarity_threshold': 0.80,
                'actual_behavior_similarity_threshold': 0.80,
                'code_pattern_overlap_threshold': 0.50,
                'min_package_threshold': 2,
                'min_confidence_threshold': 0.7,
                'min_investigation_steps': 1
            }
        }

        self.BATCH_PATTERN_LEARNING = {
            'vertex_ai': {
                'project_id': 'itpc-gcp-eco-eng-claude',
                'region': 'us-east5',
                'model': 'claude-sonnet-4-5@20250929',
                'max_tokens': 16000,
                'temperature': 0.0
            },
            'retry': {
                'max_retries': 3,
                'retry_delay': 10
            },
            'cost_estimation': {
                'input_cost_per_million': 3.3,  # Sonnet 4.5 regional (us-east5): $3.00 * 1.10
                'output_cost_per_million': 16.5  # Sonnet 4.5 regional (us-east5): $15.00 * 1.10
            },
            'processing': {
                'default_workers': 5,
                'model_suffix': 'claude'
            },
            'paths': {
                'prompt_file': 'process_mining/prompts/pattern_learning_prompt.md',
                'pattern_data_dir_template': 'process_mining/data/pattern_data/{dataset}_pattern_data',
                'output_dir_template': 'process_mining/data/patterns/{dataset}_patterns'
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

    def get_aggregation_config(self) -> dict:
        """
        Get pattern aggregation configuration.

        Returns:
            Dictionary containing aggregation configuration
        """
        return self.PATTERN_AGGREGATION

    def get_preparation_config(self) -> dict:
        """
        Get pattern preparation configuration.

        Returns:
            Dictionary containing preparation configuration
        """
        return self.PATTERN_PREPARATION

    def get_prompt_config(self) -> dict:
        """
        Get prompt template configuration.

        Returns:
            Dictionary containing prompt configuration
        """
        return self.PATTERN_LEARNING_PROMPTS

    def get_split_config(self) -> dict:
        """
        Get train/validation/test split configuration.

        Returns:
            Dictionary containing split configuration
        """
        return self.TRAIN_VAL_TEST_SPLIT

    def get_batch_learning_config(self) -> dict:
        """
        Get batch pattern learning configuration.

        Returns:
            Dictionary containing batch learning configuration
        """
        return self.BATCH_PATTERN_LEARNING

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

    def validate_confidence_weights(self):
        """Ensure confidence weights sum to 1.0. Warns if invalid."""
        try:
            weights = self.PATTERN_AGGREGATION.get('confidence_weights', {})
            if not weights:
                return

            total = sum(weights.values())
            if not (0.99 <= total <= 1.01):  # Allow small floating-point error
                logger.warning(
                    f"Confidence weights should sum to 1.0, got {total}. "
                    f"This may affect confidence calculations."
                )
        except Exception as e:
            logger.warning(f"Could not validate confidence weights: {e}")
