from .protocols import RepoHandlerProtocol
from .systemd.repo_handler import SystemdRepoHandler
from .base_repo_handler import CRepoHandler
from common.config import Config


def repo_handler_factory(config: Config) -> RepoHandlerProtocol:
    if config.PROJECT_NAME == 'systemd':
        return SystemdRepoHandler(config)
    return CRepoHandler(config)