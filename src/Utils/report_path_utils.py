"""Strip SAST report path prefixes so file paths resolve under a checkout root."""

from __future__ import annotations

import re


def segment_looks_like_project_version_archive_root(segment: str, project_name: str) -> bool:
    """Return True if *segment* looks like a scan/archive top dir, not a real package dir.

    ``project_name`` as a plain prefix is not enough: e.g. ``unzip60`` and ``tcl_api``
    must not be treated as ``unzip``/``tcl`` + version.
    """
    if not project_name or not segment.startswith(project_name):
        return False
    if len(segment) <= len(project_name):
        return False
    remainder = segment[len(project_name) :]
    if remainder[0] == "-":
        return True
    if remainder[0].isdigit():
        return bool(re.search(r"[.\-]", remainder))
    return False


def normalize_report_source_path(
    file_path: str,
    *,
    report_file_prefix: str,
    project_name: str,
) -> str:
    """Return *file_path* relative to the repo root (no leading slash).

    Strips ``report_file_prefix`` when present, otherwise may strip a first path
    segment that looks like ``{project_name}-1.2.3`` or ``{project_name}8.6.13``.
    """
    if not file_path:
        return file_path
    file_path = file_path.lstrip("/")
    if report_file_prefix and file_path.startswith(report_file_prefix):
        return file_path.removeprefix(report_file_prefix)
    root, sep, rest = file_path.partition("/")
    if sep and segment_looks_like_project_version_archive_root(root, project_name):
        return rest
    return file_path
