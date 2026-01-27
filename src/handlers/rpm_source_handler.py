"""
RPM Source Handler for fetching source code from Brew RPM packages.

This handler downloads source RPMs from Red Hat Brew, extracts them,
and provides source code extraction capabilities matching the CRepoHandler interface.
"""

import os
import re
import logging
import shutil
import glob
from pathlib import Path
from typing import Dict, Optional, Set, Tuple
from collections import defaultdict
import requests
import urllib3

from Utils.rpm_utils import parse_nvr, construct_brew_url, extract_rpm_subprocess, NvrParseError, RpmExtractionError

urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

logger = logging.getLogger(__name__)


class RpmSourceException(Exception):
    """Base exception for RPM source handling."""
    pass


class RpmNotFoundException(RpmSourceException):
    """RPM package not found in Brew."""
    pass


class RpmDownloadError(RpmSourceException):
    """Failed to download RPM."""
    pass


class RpmCacheError(RpmSourceException):
    """Cache directory issues."""
    pass


class RpmSourceHandler:
    """
    Handler for fetching source code from RPM packages via Brew.

    This class provides the same interface as CRepoHandler but works with
    source RPMs instead of Git repositories.
    """

    def __init__(
        self,
        package_nvr: str,
        cache_dir: str = "/tmp/rpm_cache",
        rhel_version: Optional[int] = None
    ):
        """
        Initialize RPM source handler.

        Args:
            package_nvr: Package Name-Version-Release (e.g., "bzip2-1.0.8-18.el10")
            cache_dir: Directory for caching RPMs and extracted sources (default: /tmp/rpm_cache)
            rhel_version: RHEL major version (optional override, auto-detected from NVR if not provided)

        Raises:
            NvrParseError: If package_nvr cannot be parsed
            RpmCacheError: If cache directory cannot be created
        """
        self.package_nvr = package_nvr

        try:
            nvr_info = parse_nvr(package_nvr + ".xlsx")
        except NvrParseError as e:
            logger.error(f"Failed to parse NVR '{package_nvr}': {e}")
            raise

        self.package = nvr_info['package']
        self.version = nvr_info['version']
        self.release = nvr_info['release']

        self.rhel_version = rhel_version if rhel_version is not None else nvr_info['rhel_version']

        self.cache_dir = Path(cache_dir)
        self.rpm_cache_dir = self.cache_dir / "rpms"
        self.extract_cache_dir = self.cache_dir / "extracted"

        try:
            self.rpm_cache_dir.mkdir(parents=True, exist_ok=True)
            self.extract_cache_dir.mkdir(parents=True, exist_ok=True)
        except Exception as e:
            raise RpmCacheError(f"Failed to create cache directories: {e}")

        self.rpm_path: Optional[str] = None
        self.extract_dir: Optional[str] = None

        # Report file prefix (matches CRepoHandler behavior)
        # Error traces have format: "bzip2-1.0.8/file.c:line"
        self._report_file_prefix = f"{self.package}-{self.version}/"

        logger.debug(
            f"Initialized RpmSourceHandler: package={self.package}, version={self.version}, "
            f"release={self.release}, rhel_version={self.rhel_version}"
        )

    def get_source_code_blocks_from_error_trace(self, error_trace: str) -> dict:
        """
        Parse error trace and extract source code blocks from RPM sources.

        This method matches the interface of CRepoHandler.get_source_code_blocks_from_error_trace().

        Args:
            error_trace: Error trace string containing file paths and line numbers

        Returns:
            Dictionary mapping file paths to source code:
            {
                'file.c': 'source code with line numbers',
                ...
            }

        Raises:
            RpmNotFoundException: If RPM not found in Brew
            RpmDownloadError: If RPM download fails
            RpmExtractionError: If RPM extraction fails
        """
        try:
            # Regex matches: "path/to/file.c:123:" or "path/to/file.h:456:"
            source_files = set(re.findall(r"([^\s]+\.(?:c|h)):(\d+):", error_trace))

            if not source_files:
                logger.warning("No source files found in error trace")
                return {}

            logger.debug(f"Found {len(source_files)} source file references in error trace")

            self.rpm_path = self._download_rpm()

            self.extract_dir = self._extract_rpm(self.rpm_path)

            error_code_sources = defaultdict(set)

            for trace_path, line_number in source_files:
                try:
                    line_num = int(line_number)
                except ValueError:
                    logger.warning(f"Invalid line number: {line_number}")
                    continue

                file_path = self._map_error_trace_path_to_file(trace_path)
                if not file_path:
                    logger.debug(f"Could not find file for trace path: {trace_path}")
                    continue

                source_code = self._get_source_code_by_line_number(file_path, line_num)
                if source_code:
                    error_code_sources[trace_path].add(source_code)

            result = {
                file_path: "\n".join(code_sections)
                for file_path, code_sections in error_code_sources.items()
            }

            logger.info(
                f"Extracted source code for {len(result)} files from {self.package_nvr}"
            )
            return result

        except (RpmNotFoundException, RpmDownloadError, RpmExtractionError):
            raise
        except Exception as e:
            logger.exception(f"Unexpected error fetching source code: {e}")
            return {}

    def _download_rpm(self) -> str:
        """
        Download source RPM from Brew if not cached.

        Returns:
            Path to downloaded RPM file

        Raises:
            RpmNotFoundException: If RPM not found (404)
            RpmDownloadError: If download fails
        """
        rpm_filename = f"{self.package}-{self.version}-{self.release}.src.rpm"
        rpm_path = self.rpm_cache_dir / rpm_filename

        if rpm_path.exists():
            logger.info(f"Using cached RPM: {rpm_path}")
            return str(rpm_path)

        url = construct_brew_url(self.package, self.version, self.release, self.rhel_version)

        logger.info(f"Downloading RPM from: {url}")

        try:
            response = requests.get(url, stream=True, timeout=60, allow_redirects=True, verify=False)

            if response.status_code == 404:
                raise RpmNotFoundException(
                    f"RPM not found in Brew: {url} (HTTP 404)"
                )

            response.raise_for_status()

            with open(rpm_path, 'wb') as f:
                for chunk in response.iter_content(chunk_size=8192):
                    f.write(chunk)

            file_size_mb = rpm_path.stat().st_size / (1024 * 1024)
            logger.info(f"Downloaded RPM ({file_size_mb:.2f} MB): {rpm_path}")
            return str(rpm_path)

        except requests.exceptions.HTTPError as e:
            raise RpmDownloadError(f"HTTP error downloading RPM: {e}")
        except requests.exceptions.Timeout:
            raise RpmDownloadError(f"Timeout downloading RPM from {url}")
        except requests.exceptions.RequestException as e:
            raise RpmDownloadError(f"Failed to download RPM: {e}")
        except IOError as e:
            if rpm_path.exists():
                rpm_path.unlink()
            raise RpmDownloadError(f"Failed to write RPM to cache: {e}")

    def _extract_rpm(self, rpm_path: str) -> str:
        """
        Extract source files from .src.rpm using rpm2cpio + cpio.

        Args:
            rpm_path: Path to RPM file

        Returns:
            Path to extraction directory

        Raises:
            RpmExtractionError: If extraction fails
        """
        extract_dir = self.extract_cache_dir / self.package_nvr

        if extract_dir.exists() and any(extract_dir.iterdir()):
            logger.info(f"Using cached extraction: {extract_dir}")
            return str(extract_dir)

        extract_dir.mkdir(parents=True, exist_ok=True)

        logger.info(f"Extracting RPM to: {extract_dir}")

        try:
            extract_rpm_subprocess(rpm_path, str(extract_dir))
            return str(extract_dir)
        except RpmExtractionError:
            if extract_dir.exists():
                shutil.rmtree(extract_dir, ignore_errors=True)
            raise

    def _map_error_trace_path_to_file(self, trace_path: str) -> Optional[str]:
        """
        Map error trace path to actual extracted file path.

        Uses multi-strategy approach:
        1. Direct match with trace path
        2. Remove version prefix and search recursively
        3. Fuzzy glob match

        Args:
            trace_path: Path from error trace (e.g., "bzip2-1.0.8/bzip2recover.c")

        Returns:
            Full path to file, or None if not found

        Examples:
            trace_path="bzip2-1.0.8/bzip2recover.c" →
            "/tmp/rpm_cache/extracted/bzip2-1.0.8-18.el10/bzip2-1.0.8/bzip2recover.c"
        """
        if not self.extract_dir:
            logger.error("Extract directory not set")
            return None

        extract_path = Path(self.extract_dir)

        candidate = extract_path / trace_path
        if candidate.exists() and candidate.is_file():
            logger.debug(f"Found file (direct match): {candidate}")
            return str(candidate)

        # "bzip2-1.0.8/bzip2recover.c" → "bzip2recover.c"
        filename = os.path.basename(trace_path)

        for root, dirs, files in os.walk(extract_path):
            if filename in files:
                full_path = os.path.join(root, filename)
                logger.debug(f"Found file (recursive search): {full_path}")
                return full_path

        pattern = str(extract_path / "**" / filename)
        matches = glob.glob(pattern, recursive=True)
        if matches:
            logger.debug(f"Found file (glob match): {matches[0]}")
            return matches[0]

        logger.warning(f"Could not map trace path to file: {trace_path}")
        return None

    def _get_source_code_by_line_number(self, file_path: str, line: int) -> str:
        """
        Extract source code context around line number.

        Extracts +/-50 lines around the target line.

        Args:
            file_path: Full path to source file
            line: Target line number (1-indexed)

        Returns:
            Source code with line numbers, empty string if file not found

        Example output:
            "355| int foo() {\n356|   return 0;\n357| }\n"
        """
        if not os.path.exists(file_path):
            logger.error(f"File not found: {file_path}")
            return ""

        try:
            with open(file_path, 'r', errors='replace') as f:
                lines = f.readlines()
        except Exception as e:
            logger.error(f"Failed to read file {file_path}: {e}")
            return ""

        context_lines = 50
        start_line = max(0, line - context_lines - 1)
        end_line = min(line + context_lines, len(lines))

        numbered_lines = []
        for i in range(start_line, end_line):
            line_num = i + 1
            line_content = lines[i].rstrip('\n')
            numbered_lines.append(f"{line_num}| {line_content}")

        return "\n".join(numbered_lines)

    def cleanup(self, keep_rpm: bool = True):
        """
        Clean up extracted sources, optionally keep cached RPM.

        Args:
            keep_rpm: If True, keep RPM in cache; if False, delete it
        """
        if self.extract_dir and os.path.exists(self.extract_dir):
            try:
                shutil.rmtree(self.extract_dir)
                logger.debug(f"Cleaned up extraction: {self.extract_dir}")
            except Exception as e:
                logger.warning(f"Failed to clean up extraction {self.extract_dir}: {e}")

        if not keep_rpm and self.rpm_path and os.path.exists(self.rpm_path):
            try:
                os.remove(self.rpm_path)
                logger.debug(f"Removed RPM: {self.rpm_path}")
            except Exception as e:
                logger.warning(f"Failed to remove RPM {self.rpm_path}: {e}")
