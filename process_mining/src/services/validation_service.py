"""
Unified Validation Service

Centralizes all validation logic for process mining data sources:
- Excel file validation
- ignore.err file validation
- Package source validation
- Validation failure categorization
"""

from pathlib import Path
from typing import Tuple, Optional
import logging

from process_mining.src.common.process_mining_config import ProcessMiningConfig
from Utils.data_access_utils import ExcelDataValidator

logger = logging.getLogger(__name__)


class ValidationService:
    """
    Centralized validation service for all process mining data sources.

    Provides unified validation for:
    - Excel ground-truth files
    - ignore.err files
    - Combined package sources

    All validation uses case-insensitive column matching and
    configuration-driven rules.
    """

    def __init__(self, config_path: Optional[str] = None):
        """
        Initialize validation service.

        Args:
            config_path: Optional path to process mining config YAML.
                        If None, uses default config location.
        """
        self.config = ProcessMiningConfig(config_path)
        self.excel_validator = ExcelDataValidator(self.config)

    def validate_excel_file(self, excel_path: Path) -> Tuple[bool, str]:
        """
        Validate Excel file has required columns and data.

        Checks:
        - At least ONE justification column ('Hint' or 'Comment') has data
        - 'False Positive?' column exists and has annotations
        - Uses case-insensitive column matching

        Args:
            excel_path: Path to Excel file

        Returns:
            Tuple of (is_valid, reason_if_invalid)
                (True, "") - File is valid
                (False, "reason") - File is invalid with specific reason

        Examples:
            >>> service = ValidationService()
            >>> is_valid, reason = service.validate_excel_file(Path("systemd.xlsx"))
            >>> if not is_valid:
            ...     print(f"Validation failed: {reason}")
        """
        return self.excel_validator.validate_file(excel_path)

    def categorize_validation_failure(self, reason: str) -> str:
        """
        Categorize validation failure reason for reporting.

        Maps free-form validation failure reasons to standardized categories
        for consistent reporting and analysis.

        This replaces:
        - preprocess_ground_truth.py:categorize_validation_failure()

        Args:
            reason: Validation failure reason string

        Returns:
            Category string from VALIDATION_RULES.failure_categories:
            - 'no_justification': Missing both Hint and Comment columns
            - 'empty_justification': Columns exist but are empty
            - 'missing_fp_column': Missing False Positive column
            - 'empty_fp_annotations': FP column exists but has no data
            - 'read_error': Error reading the file
            - 'source_code_unavailable': Source code not found
            - 'nvr_parse_error': Failed to parse package NVR
            - 'other': Unrecognized failure reason

        Examples:
            >>> service = ValidationService()
            >>> category = service.categorize_validation_failure(
            ...     "Missing both 'hint' and 'comment' columns"
            ... )
            >>> print(category)
            'no_justification'
        """
        return self.excel_validator.categorize_failure(reason)

    def validate_ignore_err_file(self, ignore_err_path: Path) -> Tuple[bool, str]:
        """
        Validate ignore.err file has required justification comments.

        Uses the existing IgnoreErrService for validation.

        Args:
            ignore_err_path: Path to ignore.err file

        Returns:
            Tuple of (is_valid, reason_if_invalid)
        """
        from process_mining.src.services.ignore_err_service import IgnoreErrService

        ignore_err_service = IgnoreErrService(self.config.config_path)
        return ignore_err_service.validate_ignore_err_file(ignore_err_path)

    def validate_package_sources(
        self,
        package_name: str,
        excel_path: Optional[Path],
        ignore_err_path: Optional[Path],
        validate_ignore_err_justifications: bool = True
    ) -> Tuple[bool, str]:
        """
        Validate both Excel and ignore.err files for a package.

        Performs comprehensive validation of all available data sources
        for a package.

        Args:
            package_name: Package name (for logging)
            excel_path: Optional path to Excel file
            ignore_err_path: Optional path to ignore.err file
            validate_ignore_err_justifications: Whether to validate
                ignore.err justifications (default: True)

        Returns:
            Tuple of (is_valid, reason_if_invalid)
                Combines validation results from all sources

        Examples:
            >>> service = ValidationService()
            >>> is_valid, reason = service.validate_package_sources(
            ...     "systemd",
            ...     Path("systemd.xlsx"),
            ...     Path("systemd/ignore.err")
            ... )
        """
        errors = []

        if excel_path and excel_path.exists():
            is_valid, reason = self.validate_excel_file(excel_path)
            if not is_valid:
                errors.append(f"Excel: {reason}")
        elif excel_path:
            errors.append(f"Excel file not found: {excel_path}")

        if ignore_err_path and ignore_err_path.exists() and validate_ignore_err_justifications:
            is_valid, reason = self.validate_ignore_err_file(ignore_err_path)
            if not is_valid:
                errors.append(f"ignore.err: {reason}")
        elif ignore_err_path and not ignore_err_path.exists():
            errors.append(f"ignore.err file not found: {ignore_err_path}")

        if not excel_path and not ignore_err_path:
            return (False, f"No Excel or ignore.err file found for package '{package_name}'")

        if errors:
            return (False, "; ".join(errors))

        return (True, "")
