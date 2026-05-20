import logging

from common.config import Config
from common.repo_language import RepoLanguage, resolve_repo_language
from handlers.c_repo_handler import CRepoHandler
from handlers.go_repo_handler import GoRepoHandler
from handlers.protocols import RepoHandlerProtocol
from Utils.repo_utils import download_repo

logger = logging.getLogger(__name__)


def repo_handler_factory(config: Config) -> RepoHandlerProtocol:
    """
    Factory function to create appropriate repository handler based on project type.
    Supports: Go and C.
    """
    if config.DOWNLOAD_REPO:
        config.REPO_LOCAL_PATH = download_repo(config.REPO_REMOTE_URL)

    config.REPO_LANGUAGE = resolve_repo_language(
        config.REPO_LOCAL_PATH,
        getattr(config, "REPO_LANGUAGE", None),
    )

    if config.REPO_LANGUAGE == RepoLanguage.GO:
        logger.info("Using Go repository handler (auto-detected)")
        return GoRepoHandler(config)

    logger.info("Using C repository handler (auto-detected)")
    return CRepoHandler(config)
