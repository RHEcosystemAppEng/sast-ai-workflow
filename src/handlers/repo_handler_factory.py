import logging
from pathlib import Path

from common.config import Config
from handlers.c_repo_handler import CRepoHandler
from handlers.go_repo_handler import GoRepoHandler
from handlers.protocols import RepoHandlerProtocol
from Utils.repo_utils import download_repo

logger = logging.getLogger(__name__)


def repo_handler_factory(config: Config) -> RepoHandlerProtocol:
    """
    Factory function to create appropriate repository handler based on project type.
    Supports: Go, C, C++
    """
    if config.DOWNLOAD_REPO:
        config.REPO_LOCAL_PATH = download_repo(config.REPO_REMOTE_URL)

    detected_language = _detect_repository_language(config.REPO_LOCAL_PATH)

    if detected_language == "go":
        logger.info("Using Go repository handler (auto-detected)")
        return GoRepoHandler(config)

    logger.info(f"Using C repository handler for {detected_language} (auto-detected)")
    return CRepoHandler(config)


def _detect_repository_language(repo_path: str) -> str:
    """
    Auto-detect the primary programming language based on file extensions.
    Supports: Go, C, C++

    Returns the language with the most source files, or 'c' as fallback.
    """
    try:
        repo = Path(repo_path)

        # Count files by extension patterns (simple glob approach)
        go_count = len(list(repo.rglob("*.go")))
        c_count = len(list(repo.rglob("*.c"))) + len(list(repo.rglob("*.h")))
        cpp_count = (
            len(list(repo.rglob("*.cpp")))
            + len(list(repo.rglob("*.hpp")))
            + len(list(repo.rglob("*.cc")))
            + len(list(repo.rglob("*.cxx")))
        )

        # Log detection results
        total_files = go_count + c_count + cpp_count
        logger.info(
            f"Language detection: Go={go_count}, C={c_count}, "
            f"C++={cpp_count} (total: {total_files})"
        )

        if total_files == 0:
            logger.info("No source files detected, defaulting to 'c'")
            return "c"

        # Return language with most files
        if go_count >= c_count and go_count >= cpp_count:
            logger.info("Primary language: go")
            return "go"
        elif cpp_count >= c_count:
            logger.info("Primary language: cpp")
            return "cpp"
        else:
            logger.info("Primary language: c")
            return "c"

    except Exception as e:
        logger.warning(f"Language detection failed: {e}, defaulting to 'c'")
        return "c"
