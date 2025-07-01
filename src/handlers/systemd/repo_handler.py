from common.config import Config
from handlers.c_repo_handler import CRepoHandler


class SystemdRepoHandler(CRepoHandler):
    def __init__(self, config: Config) -> None:
        super().__init__(config)
        self._report_file_prefix = f"{config.PROJECT_NAME}-{config.PROJECT_VERSION.split('-')[0]}/"
