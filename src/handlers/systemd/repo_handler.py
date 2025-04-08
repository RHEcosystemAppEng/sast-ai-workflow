from handlers.base_repo_handler import CRepoHandler
from common.config import Config



class SystemdRepoHandler(CRepoHandler):
    def __init__(self, config: Config, project_version: str) -> None:
        super().__init__(config)
        self._report_file_prefix = f"systemd-{project_version.split('-')[0]}/"