"""Repository primary language for handler and prompt selection."""

import logging
from enum import StrEnum
from pathlib import Path

logger = logging.getLogger(__name__)


class RepoLanguage(StrEnum):
    """Supported repository languages (handler + investigation prompts)."""

    GO = "go"
    C = "c"
    GENERIC = "generic"


def parse_repo_language(value: str | RepoLanguage | None) -> RepoLanguage | None:
    """Parse config or env value into RepoLanguage; return None if invalid or unset."""
    if value is None:
        return None
    if isinstance(value, RepoLanguage):
        return value
    try:
        return RepoLanguage(value)
    except ValueError:
        return None


def repo_language_for_investigation(value: str | RepoLanguage | None) -> RepoLanguage:
    """Language for investigation prompts and checklists; GENERIC when unset."""
    parsed = parse_repo_language(value)
    return parsed if parsed is not None else RepoLanguage.GENERIC


def detect_repository_language(repo_path: str) -> RepoLanguage:
    """
    Auto-detect the primary programming language based on file extensions.
    Supports: Go and C (.go, .c, and .h file extensions only).

    Returns RepoLanguage.GO or RepoLanguage.C (fallback when no source files are found).
    """
    try:
        repo = Path(repo_path)

        go_count = len(list(repo.rglob("*.go")))
        c_count = len(list(repo.rglob("*.c"))) + len(list(repo.rglob("*.h")))

        total_files = go_count + c_count
        logger.info(f"Language detection: Go={go_count}, C={c_count} (total: {total_files})")

        if total_files == 0:
            logger.info("No source files detected, defaulting to C")
            return RepoLanguage.C

        if go_count >= c_count:
            logger.info("Primary language: Go")
            return RepoLanguage.GO

        logger.info("Primary language: C")
        return RepoLanguage.C

    except Exception as e:
        logger.warning(f"Language detection failed: {e}, defaulting to C")
        return RepoLanguage.C


def resolve_repo_language(
    repo_local_path: str,
    configured: str | RepoLanguage | None = None,
) -> RepoLanguage:
    """Return configured language if valid, otherwise auto-detect from source files."""
    parsed = parse_repo_language(configured)
    if parsed is not None:
        return parsed
    return detect_repository_language(repo_local_path)
