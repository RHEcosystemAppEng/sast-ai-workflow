from .protocols import RepoHandlerProtocol
from .systemd.repo_handler import SystemdRepoHandler
from .base_repo_handler import CRepoHandler
from common.config import Config


def repo_handler_factory(project_name: str, project_version: str, config: Config) -> RepoHandlerProtocol:
    if project_name == 'systemd':
        return SystemdRepoHandler(config, project_version)
    return CRepoHandler(config)