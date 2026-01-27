"""
Utility functions for RPM source package handling.

This module provides utilities for:
- Parsing NVR (Name-Version-Release) from package filenames
- Constructing Brew RPM URLs
- Extracting source RPMs using rpm2cpio
"""

import os
import re
import logging
import subprocess
import shlex
from typing import Dict, Optional

logger = logging.getLogger(__name__)


class RpmUtilsError(Exception):
    """Base exception for RPM utilities."""
    pass


class NvrParseError(RpmUtilsError):
    """Failed to parse NVR from filename."""
    pass


class RpmExtractionError(RpmUtilsError):
    """Failed to extract RPM file."""
    pass


def parse_nvr(filename: str) -> Dict[str, any]:
    """
    Parse NVR (Name-Version-Release) from Excel filename.

    Args:
        filename: Excel filename (e.g., 'bzip2-1.0.8-18.el10.xlsx')

    Returns:
        Dictionary with:
            - package: Package name (e.g., 'bzip2')
            - version: Version number (e.g., '1.0.8')
            - release: Release string (e.g., '18.el10')
            - rhel_version: RHEL major version (e.g., 10)

    Raises:
        NvrParseError: If filename cannot be parsed

    Examples:
        >>> parse_nvr('bzip2-1.0.8-18.el10.xlsx')
        {'package': 'bzip2', 'version': '1.0.8', 'release': '18.el10', 'rhel_version': 10}

        >>> parse_nvr('systemd-257-9.el10.xlsx')
        {'package': 'systemd', 'version': '257', 'release': '9.el10', 'rhel_version': 10}
    """
    name_without_ext = filename.replace('.xlsx', '').replace('.xls', '')
    parts = name_without_ext.split('-')

    if len(parts) < 3:
        raise NvrParseError(
            f"Filename '{filename}' does not match NVR pattern (expected at least 3 parts separated by '-')"
        )

    version_idx = -1
    for i in range(len(parts) - 1, 0, -1):
        part = parts[i]
        next_part = parts[i + 1] if i < len(parts) - 1 else None

        if 'el' in part:
            continue

        if next_part and 'el' in next_part and part.isdigit():
            if len(part) == 1:
                continue
            if len(part) == 2 and next_part.startswith('el'):
                continue

        if re.match(r'^[A-Za-z0-9]+[A-Za-z0-9.]*$', part):
            version_idx = i
            break

    if version_idx == -1:
        raise NvrParseError(
            f"Could not identify version in filename '{filename}'"
        )

    package = '-'.join(parts[:version_idx])
    version = parts[version_idx]
    release = '-'.join(parts[version_idx + 1:])

    if not package or not version or not release:
        raise NvrParseError(
            f"Failed to parse NVR from '{filename}': package='{package}', version='{version}', release='{release}'"
        )

    rhel_version = extract_rhel_version(release)

    return {
        'package': package,
        'version': version,
        'release': release,
        'rhel_version': rhel_version
    }


def extract_rhel_version(release: str) -> int:
    """
    Extract RHEL major version from release string.

    Args:
        release: Release string (e.g., '18.el10', '9.el10')

    Returns:
        RHEL major version number (e.g., 10)

    Raises:
        NvrParseError: If RHEL version cannot be determined

    Examples:
        >>> extract_rhel_version('18.el10')
        10

        >>> extract_rhel_version('9.el10')
        10

        >>> extract_rhel_version('3.el9')
        9
    """
    match = re.search(r'\.el(\d+)', release)

    if match:
        rhel_version = int(match.group(1))
        logger.debug(f"Extracted RHEL version {rhel_version} from release '{release}'")
        return rhel_version

    # Fallback to environment variable
    env_rhel_version = os.getenv('RHEL_VERSION', '')
    if env_rhel_version:
        try:
            rhel_version = int(env_rhel_version)
            logger.info(
                f"Could not extract RHEL version from '{release}', using RHEL_VERSION={rhel_version} from environment"
            )
            return rhel_version
        except ValueError:
            raise NvrParseError(
                f"RHEL_VERSION environment variable '{env_rhel_version}' is not a valid integer"
            )

    raise NvrParseError(
        f"Cannot determine RHEL version from release '{release}' (no .elN pattern found) "
        f"and RHEL_VERSION environment variable is not set"
    )


def construct_brew_url(package: str, version: str, release: str, rhel_version: int) -> str:
    """
    Construct Brew RPM download URL.

    Args:
        package: Package name (e.g., 'bzip2')
        version: Version number (e.g., '1.0.8')
        release: Release string (e.g., '18.el10')
        rhel_version: RHEL major version (e.g., 10)

    Returns:
        Full URL to source RPM

    Examples:
        >>> construct_brew_url('bzip2', '1.0.8', '18.el10', 10)
        'https://download.devel.redhat.com/brewroot/vol/rhel-10/packages/bzip2/1.0.8/18.el10/src/bzip2-1.0.8-18.el10.src.rpm'
    """
    base_url = "https://download.devel.redhat.com/brewroot/vol"
    rpm_filename = f"{package}-{version}-{release}.src.rpm"

    url = f"{base_url}/rhel-{rhel_version}/packages/{package}/{version}/{release}/src/{rpm_filename}"

    logger.debug(f"Constructed Brew URL: {url}")
    return url


def extract_rpm_subprocess(rpm_path: str, extract_dir: str) -> None:
    """
    Extract source RPM using rpm2cpio and cpio, then extract any tar.gz archives.

    Args:
        rpm_path: Path to .src.rpm file
        extract_dir: Directory to extract files into

    Raises:
        RpmExtractionError: If extraction fails

    Note:
        This function:
        1. Extracts RPM using: rpm2cpio <rpm_path> | cpio -idm
        2. Finds and extracts any .tar.gz files (source tarballs)

        The extract_dir must exist before calling this function.
    """
    if not os.path.exists(rpm_path):
        raise RpmExtractionError(f"RPM file does not exist: {rpm_path}")

    if not os.path.exists(extract_dir):
        raise RpmExtractionError(f"Extract directory does not exist: {extract_dir}")

    cmd = f"rpm2cpio {shlex.quote(rpm_path)} | cpio -idm 2>&1"

    logger.debug(f"Extracting RPM: {cmd}")
    logger.debug(f"Extract directory: {extract_dir}")

    try:
        result = subprocess.run(
            cmd,
            shell=True,
            cwd=extract_dir,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            timeout=120,  # 2 minute timeout
            check=True,
            text=True
        )

        logger.debug(f"RPM extraction output: {result.stdout[:500]}")

    except subprocess.CalledProcessError as e:
        raise RpmExtractionError(
            f"Failed to extract RPM '{rpm_path}': {e.stdout if e.stdout else e.stderr}"
        )
    except subprocess.TimeoutExpired:
        raise RpmExtractionError(
            f"Timeout extracting RPM '{rpm_path}' (exceeded 120 seconds)"
        )
    except Exception as e:
        raise RpmExtractionError(
            f"Unexpected error extracting RPM '{rpm_path}': {str(e)}"
        )

    import glob as glob_module
    tar_patterns = ["*.tar.gz", "*.tar.bz2", "*.tar.xz", "*.tgz", "*.tbz2", "*.txz"]
    tar_files = []
    for pattern in tar_patterns:
        tar_files.extend(glob_module.glob(os.path.join(extract_dir, pattern)))

    if tar_files:
        logger.debug(f"Found {len(tar_files)} tarball(s) to extract")
        for tar_file in tar_files:
            try:
                tar_cmd = f"tar -xf {shlex.quote(os.path.basename(tar_file))} 2>&1"
                logger.debug(f"Extracting tarball: {tar_cmd}")

                tar_result = subprocess.run(
                    tar_cmd,
                    shell=True,
                    cwd=extract_dir,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    timeout=120,
                    check=True,
                    text=True
                )

                logger.debug(f"Tarball extraction output: {tar_result.stdout[:200]}")

            except subprocess.CalledProcessError as e:
                logger.warning(f"Failed to extract tarball {tar_file}: {e.stdout if e.stdout else e.stderr}")
            except Exception as e:
                logger.warning(f"Error extracting tarball {tar_file}: {str(e)}")

    logger.info(f"Successfully extracted RPM to {extract_dir}")
