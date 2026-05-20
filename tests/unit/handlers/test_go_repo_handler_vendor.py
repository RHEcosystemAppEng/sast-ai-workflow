"""Tests for GoRepoHandler vendor download and indexing vendor/."""

from pathlib import Path
from unittest.mock import MagicMock, Mock, patch

from src.handlers.go_repo_handler import CompactIndex, GoRepoHandler


def make_handler():
    """Return a GoRepoHandler instance with __init__ bypassed."""
    with patch.object(GoRepoHandler, "__init__", return_value=None):
        handler = GoRepoHandler(Mock())
    handler.repo_local_path = "/repo"
    return handler


class TestFindGoFilesIncludesVendor:

    def test_vendor_go_files_are_listed(self, tmp_path: Path):
        handler = make_handler()
        (tmp_path / "main.go").write_text("package main\nfunc main() {}\n", encoding="utf-8")
        vend = tmp_path / "vendor" / "some.module" / "pkg"
        vend.mkdir(parents=True)
        (vend / "dep.go").write_text("package pkg\nfunc Dep() {}\n", encoding="utf-8")

        handler.repo_local_path = str(tmp_path)
        files = handler._find_go_files()

        dep_files = [f for f in files if "vendor" in f and f.endswith("dep.go")]
        assert len(dep_files) == 1

    def test_skips_test_go_even_under_vendor(self, tmp_path: Path):
        handler = make_handler()
        vend = tmp_path / "vendor" / "x"
        vend.mkdir(parents=True)
        (vend / "z_test.go").write_text(
            "package x\nfunc TestZ(t *testing.T) {}\n", encoding="utf-8"
        )

        handler.repo_local_path = str(tmp_path)
        files = handler._find_go_files()
        assert not any(f.endswith("z_test.go") for f in files)

    def test_skips_hidden_directories(self, tmp_path: Path):
        handler = make_handler()
        hidden = tmp_path / ".cache" / "x"
        hidden.mkdir(parents=True)
        (hidden / "a.go").write_text("package x\n", encoding="utf-8")

        handler.repo_local_path = str(tmp_path)
        files = handler._find_go_files()
        assert not any(".cache" in f for f in files)


class TestEnsureGoVendor:

    def test_skips_when_flag_false(self, tmp_path: Path):
        handler = make_handler()
        handler.repo_local_path = str(tmp_path)
        (tmp_path / "go.mod").write_text("module example.com/foo\n\ngo 1.21\n", encoding="utf-8")
        cfg = Mock()
        cfg.DOWNLOAD_VENDOR_DEPENDENCIES = False

        with patch("src.handlers.go_repo_handler.subprocess.run") as run:
            handler._ensure_go_vendor(cfg)
            run.assert_not_called()

    def test_skips_when_no_go_module_files(self, tmp_path: Path):
        handler = make_handler()
        handler.repo_local_path = str(tmp_path)
        cfg = Mock()
        cfg.DOWNLOAD_VENDOR_DEPENDENCIES = True

        with patch("src.handlers.go_repo_handler.subprocess.run") as run:
            handler._ensure_go_vendor(cfg)
            run.assert_not_called()

    def test_runs_go_mod_vendor_when_go_mod_present(self, tmp_path: Path):
        handler = make_handler()
        handler.repo_local_path = str(tmp_path)
        (tmp_path / "go.mod").write_text("module example.com/foo\n\ngo 1.21\n", encoding="utf-8")
        cfg = Mock()
        cfg.DOWNLOAD_VENDOR_DEPENDENCIES = True

        with patch("src.handlers.go_repo_handler.subprocess.run") as run:
            run.return_value = MagicMock(returncode=0, stderr="", stdout="")
            handler._ensure_go_vendor(cfg)
            run.assert_called_once()
            args, kwargs = run.call_args
            assert args[0] == ["go", "mod", "vendor"]
            assert kwargs["cwd"] == tmp_path.resolve()

    def test_runs_go_work_vendor_when_go_work_present(self, tmp_path: Path):
        handler = make_handler()
        handler.repo_local_path = str(tmp_path)
        (tmp_path / "go.work").write_text("go 1.21\n", encoding="utf-8")
        (tmp_path / "go.mod").write_text("module example.com/foo\n\ngo 1.21\n", encoding="utf-8")
        cfg = Mock()
        cfg.DOWNLOAD_VENDOR_DEPENDENCIES = True

        with patch("src.handlers.go_repo_handler.subprocess.run") as run:
            run.return_value = MagicMock(returncode=0, stderr="", stdout="")
            handler._ensure_go_vendor(cfg)
            run.assert_called_once()
            assert run.call_args[0][0] == ["go", "work", "vendor"]

    def test_nonzero_exit_does_not_raise(self, tmp_path: Path):
        handler = make_handler()
        handler.repo_local_path = str(tmp_path)
        (tmp_path / "go.mod").write_text("module example.com/foo\n\ngo 1.21\n", encoding="utf-8")
        cfg = Mock()
        cfg.DOWNLOAD_VENDOR_DEPENDENCIES = True

        with patch("src.handlers.go_repo_handler.subprocess.run") as run:
            run.return_value = MagicMock(returncode=1, stderr="fail", stdout="")
            handler._ensure_go_vendor(cfg)

    def test_missing_go_binary_does_not_raise(self, tmp_path: Path):
        handler = make_handler()
        handler.repo_local_path = str(tmp_path)
        (tmp_path / "go.mod").write_text("module example.com/foo\n\ngo 1.21\n", encoding="utf-8")
        cfg = Mock()
        cfg.DOWNLOAD_VENDOR_DEPENDENCIES = True

        with patch("src.handlers.go_repo_handler.subprocess.run") as run:
            run.side_effect = FileNotFoundError()
            handler._ensure_go_vendor(cfg)


class TestCompactIndexSingleton:

    def test_same_repo_path_reuses_one_compact_index(self, tmp_path: Path) -> None:
        (tmp_path / "main.go").write_text(
            "package main\nfunc main() {}\n",
            encoding="utf-8",
        )

        build_calls: list[int] = []
        orig_build = GoRepoHandler._build_index

        def counting_build(self: GoRepoHandler) -> None:
            build_calls.append(1)
            orig_build(self)

        cfg = Mock()
        cfg.PROJECT_NAME = "proj"
        cfg.PROJECT_VERSION = "v1"
        cfg.REPO_LOCAL_PATH = str(tmp_path)
        cfg.DOWNLOAD_VENDOR_DEPENDENCIES = False

        repo_key = str(tmp_path.resolve())
        GoRepoHandler._compact_index_cache.pop(repo_key, None)

        with patch.object(GoRepoHandler, "_build_index", counting_build):
            h1 = GoRepoHandler(cfg)
            h2 = GoRepoHandler(cfg)

        assert sum(build_calls) == 1
        assert h1.index is h2.index
        assert isinstance(h1.index, CompactIndex)

    def test_distinct_repo_paths_get_distinct_indexes(self, tmp_path: Path) -> None:
        a = tmp_path / "a"
        b = tmp_path / "b"
        a.mkdir()
        b.mkdir()
        (a / "main.go").write_text("package main\nfunc main() {}\n", encoding="utf-8")
        (b / "main.go").write_text("package main\nfunc main() {}\n", encoding="utf-8")

        cfg_a = Mock()
        cfg_a.PROJECT_NAME = "pa"
        cfg_a.PROJECT_VERSION = "v1"
        cfg_a.REPO_LOCAL_PATH = str(a)
        cfg_a.DOWNLOAD_VENDOR_DEPENDENCIES = False

        cfg_b = Mock()
        cfg_b.PROJECT_NAME = "pb"
        cfg_b.PROJECT_VERSION = "v1"
        cfg_b.REPO_LOCAL_PATH = str(b)
        cfg_b.DOWNLOAD_VENDOR_DEPENDENCIES = False

        GoRepoHandler._compact_index_cache.pop(str(a.resolve()), None)
        GoRepoHandler._compact_index_cache.pop(str(b.resolve()), None)

        ha = GoRepoHandler(cfg_a)
        hb = GoRepoHandler(cfg_b)

        assert ha.index is not hb.index
