from common.config import Config
from handlers.c_repo_handler import CRepoHandler
from handlers.protocols import RepoHandlerProtocol


def repo_handler_factory(config: Config) -> RepoHandlerProtocol:
    return CRepoHandler(config)
