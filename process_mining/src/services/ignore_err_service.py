"""
IgnoreErrService - Parse and validate ignore.err files (known false positives).

This service centralizes all ignore.err file parsing logic, making it reusable
across multiple scripts (batch_prepare_patterns, split_train_val_test, etc.).

Each ignore.err file contains known false positive entries with:
- Error type and CWE information
- Complete error trace
- Human justification (required, marked with '#' comments)
"""

import logging
from pathlib import Path
from typing import List, Dict, Tuple, Optional

import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from Utils.file_utils import read_known_errors_file
from process_mining.src.common.process_mining_config import ProcessMiningConfig

logger = logging.getLogger(__name__)


class IgnoreErrService:
    """Service for parsing and validating ignore.err files."""

    def __init__(self, config_path: Optional[str] = None):
        """
        Initialize IgnoreErrService.

        Args:
            config_path: Optional path to process mining config YAML
        """
        self.config = ProcessMiningConfig(config_path)

    def read_ignore_err_file(self, ignore_err_path: Path) -> List[Dict]:
        """
        Parse ignore.err file completely to extract all fields for pattern learning.

        Uses existing read_known_errors_file() utility and parses each entry to extract:
        - Error type and CWE from first line
        - Complete error trace (all lines before # comments)
        - Human justification (lines starting with #)

        All issues in ignore.err are 100% false positives.

        Args:
            ignore_err_path: Path to ignore.err file

        Returns:
            List of dicts matching Excel format with fields:
            - issue_type, issue_cwe, error_trace, is_false_positive (always True),
              human_justification, comment (None), ai_prediction (None), raw_finding

        Examples:
            >>> service = IgnoreErrService()
            >>> entries = service.read_ignore_err_file(Path("package/ignore.err"))
            >>> len(entries)
            5
            >>> entries[0]['is_false_positive']
            True
        """
        try:
            error_entries = read_known_errors_file(str(ignore_err_path))
            parsed_entries = []

            for entry in error_entries:
                if not entry.strip():
                    continue

                lines = entry.split('\n')

                first_line = lines[0].strip()
                issue_type = ""
                issue_cwe = ""

                if first_line.startswith('Error:'):
                    parts = first_line.split()
                    if len(parts) >= 2:
                        issue_type = parts[1]

                    if '(CWE-' in first_line:
                        cwe_start = first_line.find('(CWE-') + 1
                        cwe_end = first_line.find(')', cwe_start)
                        if cwe_end > cwe_start:
                            issue_cwe = first_line[cwe_start:cwe_end]

                error_trace_lines = []
                justification_lines = []

                for line in lines:
                    if line.strip().startswith('#'):
                        justification_lines.append(line.strip().lstrip('#').strip())
                    else:
                        error_trace_lines.append(line)

                error_trace = '\n'.join(error_trace_lines).strip()
                human_justification = ' '.join(justification_lines).strip()

                parsed_entries.append({
                    "issue_type": issue_type,
                    "issue_cwe": issue_cwe,
                    "error_trace": error_trace,
                    "is_false_positive": True,
                    "human_justification": human_justification,
                    "comment": None,
                    "ai_prediction": None,
                    "raw_finding": error_trace
                })

            logger.debug(f"Parsed {len(parsed_entries)} entries from {ignore_err_path.name}")
            return parsed_entries

        except Exception as e:
            logger.warning(f"Failed to parse {ignore_err_path}: {e}")
            return []

    def validate_ignore_err_file(self, err_file_path: Path) -> Tuple[bool, str]:
        """
        Validate that an ignore.err file has justification comments.

        A file is valid if:
        - It has at least one line starting with '#' (justification comment)

        Args:
            err_file_path: Path to ignore.err file

        Returns:
            Tuple of (is_valid, reason_if_invalid)

        Examples:
            >>> service = IgnoreErrService()
            >>> is_valid, reason = service.validate_ignore_err_file(Path("pkg/ignore.err"))
            >>> is_valid
            True
            >>> reason
            ''
        """
        try:
            with open(err_file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            if not content or not content.strip():
                return (False, "File is empty")

            justification_lines = [line for line in content.split('\n') if line.strip().startswith('#')]

            if len(justification_lines) == 0:
                return (False, "No justification comments (no '#' lines found)")

            return (True, "")

        except Exception as e:
            return (False, f"Error reading file: {str(e)}")

    def discover_ignore_err_files(self, known_non_issue_dir: Path) -> List[Path]:
        """
        Discover all ignore.err files in known_non_issue directory.

        Args:
            known_non_issue_dir: Path to known_non_issue directory

        Returns:
            List of ignore.err file paths (sorted by package name)

        Examples:
            >>> service = IgnoreErrService()
            >>> files = service.discover_ignore_err_files(Path("data/known_non_issue"))
            >>> len(files)
            42
        """
        files = []

        if known_non_issue_dir.exists():
            for package_dir in known_non_issue_dir.iterdir():
                if package_dir.is_dir() and not package_dir.name.startswith('.'):
                    err_file = package_dir / 'ignore.err'
                    if err_file.exists() and err_file.is_file():
                        files.append(err_file)

        return sorted(files)

    def categorize_validation_failure(self, reason: str) -> str:
        """
        Map validation reason to category for reporting.

        Args:
            reason: Validation failure reason string

        Returns:
            Category string: 'no_justification', 'empty_file', 'read_error', or 'other'
        """
        if "No justification comments" in reason:
            return "no_justification"
        elif "File is empty" in reason:
            return "empty_file"
        elif "Error reading" in reason:
            return "read_error"
        else:
            return "other"
