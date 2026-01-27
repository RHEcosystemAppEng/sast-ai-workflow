"""
Data Access Utilities - Base classes for file discovery, validation, and reading.

Provides format-agnostic abstractions for working with ground-truth data in the
process mining workflow. Consolidates duplicate functions from multiple scripts.

Key Features:
- Case-insensitive column matching throughout
- Configurable via ProcessMiningConfig
- Extensible for multiple file formats (Excel, CSV, etc.)
- Single source of truth for validation logic
"""

from abc import ABC, abstractmethod
from pathlib import Path
from typing import List, Dict, Tuple, Optional, Set
import pandas as pd
import logging
import re

logger = logging.getLogger(__name__)


class BaseFileDiscovery(ABC):
    """
    Abstract base class for discovering files in ground-truth directories.

    Supports:
    - Format-agnostic discovery (Excel, CSV, etc.)
    - Exclusion list filtering
    - Full dataset vs split dataset handling
    - Temp file filtering
    """

    def __init__(self, config):
        """
        Initialize file discovery.

        Args:
            config: ProcessMiningConfig instance
        """
        self.config = config
        self.data_config = config.get_data_processing_config()

    @abstractmethod
    def get_file_extensions(self) -> List[str]:
        """Return list of supported file extensions (e.g., ['.xlsx', '.csv'])."""
        pass

    def discover_files(
        self,
        base_dir: Path,
        use_full_dataset: bool = True,
        exclusion_list: Optional[Set[str]] = None
    ) -> List[Path]:
        """
        Discover all supported files in directory.

        Args:
            base_dir: Base directory to search
            use_full_dataset: If True, look in full_dataset/ subdirectory first
            exclusion_list: Set of filenames (without extension) to exclude

        Returns:
            Sorted list of file paths
        """
        if use_full_dataset:
            full_dataset_name = self.data_config.get('directories', {}).get('full_dataset', 'full_dataset')
            full_dataset_dir = base_dir / full_dataset_name
            search_dir = full_dataset_dir if full_dataset_dir.exists() else base_dir
        else:
            search_dir = base_dir

        files = []
        for ext in self.get_file_extensions():
            files.extend(search_dir.glob(f"*{ext}"))

        temp_prefix = self.data_config.get('file_patterns', {}).get('temp_file_prefix', '~')
        files = [f for f in files if f.is_file() and not f.name.startswith(temp_prefix)]

        if exclusion_list:
            original_count = len(files)
            files = [f for f in files if f.stem not in exclusion_list]
            excluded_count = original_count - len(files)
            if excluded_count > 0:
                logger.info(f"Excluded {excluded_count} files based on exclusion list")

        return sorted(files)


class ExcelFileDiscovery(BaseFileDiscovery):
    """Discover Excel files (.xlsx, .xls)."""

    def get_file_extensions(self) -> List[str]:
        return ['.xlsx', '.xls']


class CsvFileDiscovery(BaseFileDiscovery):
    """Discover CSV files (.csv)."""

    def get_file_extensions(self) -> List[str]:
        return ['.csv']


class BaseDataValidator(ABC):
    """
    Abstract base class for validating ground-truth data files.

    Implements case-insensitive column matching as a core feature.
    """

    def __init__(self, config):
        self.config = config
        self.data_config = config.get_data_processing_config()
        self.validation_rules = config.get_validation_rules()

    def normalize_columns(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Normalize DataFrame column names to lowercase.

        Args:
            df: Input DataFrame

        Returns:
            DataFrame with normalized column names
        """
        df.columns = df.columns.astype(str).str.strip().str.lower()
        return df

    def get_column_name(self, column_key: str) -> str:
        """
        Get normalized column name from config.

        Args:
            column_key: Key in column_names config (e.g., 'hint', 'comment')

        Returns:
            Normalized column name (lowercase)
        """
        column_names = self.data_config.get('column_names', {})
        return column_names.get(column_key, column_key.lower())

    @abstractmethod
    def validate_file(self, file_path: Path) -> Tuple[bool, str]:
        """
        Validate a single file.

        Args:
            file_path: Path to file

        Returns:
            Tuple of (is_valid, reason_if_invalid)
        """
        pass

    def categorize_failure(self, reason: str) -> str:
        """
        Categorize validation failure reason.

        Maps failure reasons to standardized categories for reporting.

        Args:
            reason: Validation failure reason string

        Returns:
            Category string from VALIDATION_RULES.failure_categories
        """
        categories = self.validation_rules.get('failure_categories', {})
        reason_lower = reason.lower()

        if "justification" in reason_lower or ("hint" in reason_lower and "comment" in reason_lower):
            if "missing" in reason_lower and ("both" in reason_lower or "and" in reason_lower):
                return categories.get('no_justification', 'no_justification')
            elif "empty" in reason_lower:
                return categories.get('empty_justification', 'empty_justification')

        if "false positive" in reason_lower:
            if "missing" in reason_lower:
                return categories.get('missing_fp_column', 'missing_fp_column')
            elif "empty" in reason_lower or "no annotations" in reason_lower:
                return categories.get('empty_fp_annotations', 'empty_fp_annotations')

        if "error reading" in reason_lower:
            return categories.get('read_error', 'read_error')

        if "source" in reason_lower and ("unavailable" in reason_lower or "not found" in reason_lower):
            return categories.get('source_unavailable', 'source_code_unavailable')

        if "nvr" in reason_lower or ("parse" in reason_lower and "failed" in reason_lower):
            return categories.get('nvr_parse_error', 'nvr_parse_error')

        return categories.get('other', 'other')


class ExcelDataValidator(BaseDataValidator):
    """
    Validate Excel files for ground-truth data.

    Checks:
    - At least ONE justification column ('Hint' or 'Comment') has data
    - 'False Positive?' column exists and has annotations
    - Uses case-insensitive column matching throughout
    """

    def validate_file(self, file_path: Path) -> Tuple[bool, str]:
        """
        Validate Excel file has required columns and data.

        Args:
            file_path: Path to Excel file

        Returns:
            Tuple of (is_valid, reason_if_invalid)
                (True, "") - File is valid
                (False, "reason") - File is invalid with specific reason
        """
        try:
            from Utils.file_utils import get_header_row
            header_row = get_header_row(str(file_path))
            df = pd.read_excel(file_path, header=header_row, engine='openpyxl')

            df = self.normalize_columns(df)

            hint_col = self.get_column_name('hint')
            comment_col = self.get_column_name('comment')
            fp_col = self.get_column_name('false_positive')

            has_hint = hint_col in df.columns
            has_comment = comment_col in df.columns

            hint_count = df[hint_col].notna().sum() if has_hint else 0
            comment_count = df[comment_col].notna().sum() if has_comment else 0

            if hint_count == 0 and comment_count == 0:
                match (has_hint, has_comment):
                    case (False, False):
                        return (False, f"Missing both '{hint_col}' and '{comment_col}' columns")
                    case (True, True):
                        return (False, f"Both '{hint_col}' and '{comment_col}' columns are empty (no justification data)")
                    case (True, False):
                        return (False, f"'{hint_col}' column is empty and '{comment_col}' column missing")
                    case (False, True):
                        return (False, f"'{comment_col}' column is empty and '{hint_col}' column missing")

            if fp_col not in df.columns:
                return (False, f"Missing '{fp_col}' column")

            fp_annotations = df[fp_col].notna().sum()
            if fp_annotations == 0:
                return (False, f"No annotations in '{fp_col}' column (all rows are empty)")

            return (True, "")

        except Exception as e:
            return (False, f"Error reading file: {str(e)}")


class PackageNameExtractor:
    """
    Utility class for extracting package names from various formats.
    """

    @staticmethod
    def from_excel_filename(filename: str) -> str:
        """
        Extract package name from Excel filename using simple split approach.

        This is the simplest method, suitable when exact NVR parsing is not required.

        Args:
            filename: Excel filename (e.g., 'systemd-257-4.el10.xlsx')

        Returns:
            Package name (e.g., 'systemd')

        Examples:
            >>> PackageNameExtractor.from_excel_filename('systemd-257-4.el10.xlsx')
            'systemd'
            >>> PackageNameExtractor.from_excel_filename('glibc-2.34-1.el9.xlsx')
            'glibc'
        """
        stem = filename.replace('.xlsx', '').replace('.xls', '').replace('.csv', '')

        parts = stem.split('-')
        return parts[0] if parts else stem

    @staticmethod
    def from_nvr(nvr: str) -> str:
        """
        Extract package name from NVR using rpm_utils for accurate parsing.

        This method uses the rpm_utils.parse_nvr() function which properly
        handles complex package names. Falls back to simple split on failure.

        Args:
            nvr: Full NVR string (e.g., 'systemd-257-4.el10' or 'systemd-257-4.el10.xlsx')

        Returns:
            Package name (e.g., 'systemd')

        Examples:
            >>> PackageNameExtractor.from_nvr('systemd-257-4.el10')
            'systemd'
            >>> PackageNameExtractor.from_nvr('python3.11-3.11.5-1.el9.xlsx')
            'python3.11'
        """
        from Utils.rpm_utils import parse_nvr, NvrParseError

        try:
            if not any(nvr.endswith(ext) for ext in ['.xlsx', '.xls', '.csv']):
                nvr_with_ext = nvr + '.xlsx'
            else:
                nvr_with_ext = nvr

            nvr_info = parse_nvr(nvr_with_ext)
            return nvr_info['package']
        except (NvrParseError, Exception) as e:
            logger.warning(f"Failed to parse NVR '{nvr}': {e}, falling back to simple split")
            return PackageNameExtractor.from_excel_filename(nvr)

    @staticmethod
    def from_nvr_regex(filename: str, rhel_version: str = 'el10') -> str:
        """
        Extract package name using regex pattern.

        Args:
            filename: Filename with NVR pattern (e.g., 'systemd-257-4.el10.xlsx')
            rhel_version: RHEL version pattern (default: 'el10')

        Returns:
            Package name (e.g., 'systemd')

        Examples:
            >>> PackageNameExtractor.from_nvr_regex('systemd-257-4.el10.xlsx')
            'systemd'
            >>> PackageNameExtractor.from_nvr_regex('kernel-5.14.0-1.el9.rpm', 'el9')
            'kernel'
        """
        name_without_ext = filename
        for ext in ['.xlsx', '.xls', '.csv', '.rpm']:
            name_without_ext = name_without_ext.replace(ext, '')

        version_pattern = rf'-[\d]+[^-]*-[\d]+\.{rhel_version}.*$'

        base_name = re.sub(version_pattern, '', name_without_ext)

        return base_name
