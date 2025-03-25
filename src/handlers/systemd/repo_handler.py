from handlers.base_repo_handler import RepoHandler
from Utils.config import Config



class SystemdRepoHandler(RepoHandler):
    def __init__(self, config: Config, project_version: str) -> None:
        super().__init__(config)
        self._report_file_prefix = f"systemd-{project_version.split('-')[0]}/"
        self.clang_args = [
            "-D_public_=__attribute__((visibility(\"default\")))",
            "-D_pure_=__attribute__((pure))",
            "-DENABLE_ENCRYPTION",
            "-DHAVE_OPENSSL=1"
            ]   