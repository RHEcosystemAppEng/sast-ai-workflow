"""
Tests for repo_handler_factory._detect_repository_language

The factory module imports CRepoHandler which pulls in heavy dependencies
(langchain, transformers, etc.). We mock those at the module level before
importing so these tests stay fast and isolated.
"""

import sys
from unittest.mock import MagicMock

# ── Stub out heavy transitive imports before loading the factory ──────────────
for _mod in (
    "handlers.c_repo_handler",
    "handlers.go_repo_handler",
    "common.config",
    "Utils.repo_utils",
):
    if _mod not in sys.modules:
        sys.modules[_mod] = MagicMock()

from src.handlers.repo_handler_factory import _detect_repository_language  # noqa: E402


class TestDetectRepositoryLanguage:

    def test_go_only_repo_detected_as_go(self, tmp_path):
        for name in ("main.go", "pkg/util.go", "cmd/app.go"):
            p = tmp_path / name
            p.parent.mkdir(parents=True, exist_ok=True)
            p.touch()
        assert _detect_repository_language(str(tmp_path)) == "go"

    def test_c_only_repo_detected_as_c(self, tmp_path):
        (tmp_path / "src").mkdir()
        (tmp_path / "src" / "main.c").touch()
        (tmp_path / "include").mkdir()
        (tmp_path / "include" / "util.h").touch()
        assert _detect_repository_language(str(tmp_path)) == "c"

    def test_cpp_only_repo_detected_as_cpp(self, tmp_path):
        (tmp_path / "src").mkdir()
        (tmp_path / "src" / "main.cpp").touch()
        (tmp_path / "include").mkdir()
        (tmp_path / "include" / "util.hpp").touch()
        assert _detect_repository_language(str(tmp_path)) == "cpp"

    def test_empty_repo_defaults_to_c(self, tmp_path):
        assert _detect_repository_language(str(tmp_path)) == "c"

    def test_go_wins_when_it_has_most_files(self, tmp_path):
        for name in ("a.go", "b.go", "c.go", "x.c"):
            (tmp_path / name).touch()
        assert _detect_repository_language(str(tmp_path)) == "go"

    def test_c_wins_when_it_has_most_files(self, tmp_path):
        for name in ("a.c", "b.c", "c.h", "x.go"):
            (tmp_path / name).touch()
        assert _detect_repository_language(str(tmp_path)) == "c"

    def test_cpp_wins_over_c_on_tie(self, tmp_path):
        # cpp=2 (a.cpp, b.cc), c=2 (c.c, d.h) → cpp wins on tie
        for name in ("a.cpp", "b.cc", "c.c", "d.h"):
            (tmp_path / name).touch()
        assert _detect_repository_language(str(tmp_path)) == "cpp"

    def test_go_wins_on_tie_with_c(self, tmp_path):
        # go=1, c=1, cpp=0 → go wins on tie
        for name in ("a.go", "b.c"):
            (tmp_path / name).touch()
        assert _detect_repository_language(str(tmp_path)) == "go"

    def test_nonexistent_path_defaults_to_c(self):
        assert _detect_repository_language("/nonexistent/path/xyz") == "c"

    def test_returns_string(self, tmp_path):
        (tmp_path / "main.go").touch()
        assert isinstance(_detect_repository_language(str(tmp_path)), str)

    def test_ignores_non_source_files(self, tmp_path):
        for name in ("README.md", "config.yaml", "Makefile", "main.go"):
            (tmp_path / name).touch()
        assert _detect_repository_language(str(tmp_path)) == "go"

    def test_nested_go_files_detected(self, tmp_path):
        for name in ("cmd/server/main.go", "internal/api/handler.go"):
            p = tmp_path / name
            p.parent.mkdir(parents=True, exist_ok=True)
            p.touch()
        assert _detect_repository_language(str(tmp_path)) == "go"

    def test_all_cpp_extensions_counted(self, tmp_path):
        for name in ("a.cpp", "b.hpp", "c.cc", "d.cxx"):
            (tmp_path / name).touch()
        assert _detect_repository_language(str(tmp_path)) == "cpp"
