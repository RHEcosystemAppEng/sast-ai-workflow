from common.config import Config
from handlers.c_repo_handler import CRepoHandler
from handlers.protocols import RepoHandlerProtocol
from handlers.systemd.repo_handler import SystemdRepoHandler


def repo_handler_factory(config: Config) -> RepoHandlerProtocol:
    if config.PROJECT_NAME == "systemd":
        return SystemdRepoHandler(config)
    return CRepoHandler(config)
