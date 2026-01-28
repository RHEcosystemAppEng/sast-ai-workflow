"""
Pattern Loader Service - Loads SAST false positive patterns for LLM analysis.
"""

import json
import logging
from pathlib import Path
from typing import Dict, Optional

from common.pattern_constants import ISSUE_TYPE_KEYWORDS, get_patterns_directory, is_patterns_available

logger = logging.getLogger(__name__)


class PatternLoaderService:
    """Service for loading and managing SAST false positive patterns."""

    def __init__(self, patterns_dir: Optional[Path] = None):
        """
        Initialize pattern loader service.

        Args:
            patterns_dir: Optional custom patterns directory path.
                         If None, uses default from pattern_constants.
        """
        self.patterns_dir = patterns_dir or get_patterns_directory()
        self.patterns_by_issue_type: Dict[str, str] = {}
        self._patterns_loaded = False

    def load_patterns(self) -> None:
        """
        Load pattern files from directory, auto-detecting by filename.

        Supports filename conventions like:
        - RESOURCE_LEAK_step_0_patterns.json -> RESOURCE_LEAK
        - rhel_c_UNINIT_pattern_summary.json -> UNINIT
        """
        if not is_patterns_available():
            logger.warning(f"Patterns directory not found or not accessible: {self.patterns_dir}")
            logger.info("Pattern-based analysis will be disabled")
            self._patterns_loaded = True
            return

        logger.info(f"Loading patterns from: {self.patterns_dir}")

        try:
            for json_file in self.patterns_dir.glob('*.json'):
                filename_lower = json_file.name.lower()
                for issue_type, keywords in ISSUE_TYPE_KEYWORDS.items():
                    if any(kw in filename_lower for kw in keywords):
                        with open(json_file, 'r') as f:
                            self.patterns_by_issue_type[issue_type] = f.read()
                        logger.info(f"Loaded {issue_type} patterns from {json_file.name}")
                        break

            logger.info(f"Loaded patterns for {len(self.patterns_by_issue_type)} issue types: "
                       f"{list(self.patterns_by_issue_type.keys())}")
            self._patterns_loaded = True

        except Exception as e:
            logger.error(f"Error loading patterns: {e}")
            self.patterns_by_issue_type = {}
            self._patterns_loaded = True

    def get_patterns_for_issue_type(self, issue_type: str) -> Optional[str]:
        """
        Get pattern content for a specific issue type.

        Args:
            issue_type: The issue type to look up (e.g., 'RESOURCE_LEAK', 'UNINIT')

        Returns:
            Pattern file content as JSON string, or None if not available
        """
        if not self._patterns_loaded:
            self.load_patterns()

        return self.patterns_by_issue_type.get(issue_type)

    def has_patterns_for_issue_type(self, issue_type: str) -> bool:
        """
        Check if patterns are available for a specific issue type.

        Args:
            issue_type: The issue type to check

        Returns:
            True if patterns exist for this issue type, False otherwise
        """
        if not self._patterns_loaded:
            self.load_patterns()

        return issue_type in self.patterns_by_issue_type

    def get_loaded_issue_types(self) -> list:
        """
        Get list of issue types that have patterns loaded.

        Returns:
            List of issue type names with patterns available
        """
        if not self._patterns_loaded:
            self.load_patterns()

        return list(self.patterns_by_issue_type.keys())