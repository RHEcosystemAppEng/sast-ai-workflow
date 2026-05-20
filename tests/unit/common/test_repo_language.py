"""Tests for common.repo_language detection and parsing."""

from common.repo_language import (
    RepoLanguage,
    detect_repository_language,
    parse_repo_language,
    repo_language_for_investigation,
    resolve_repo_language,
)


class TestParseRepoLanguage:
    def test_parses_enum_member(self):
        assert parse_repo_language(RepoLanguage.GO) == RepoLanguage.GO

    def test_parses_string_value(self):
        assert parse_repo_language("c") == RepoLanguage.C

    def test_returns_none_for_invalid(self):
        assert parse_repo_language("rust") is None

    def test_returns_none_for_none(self):
        assert parse_repo_language(None) is None


class TestRepoLanguageForInvestigation:
    def test_returns_enum_when_set(self):
        assert repo_language_for_investigation(RepoLanguage.GO) == RepoLanguage.GO

    def test_falls_back_to_generic(self):
        assert repo_language_for_investigation(None) == RepoLanguage.GENERIC

    def test_value_usable_for_prompt_paths(self):
        assert repo_language_for_investigation(RepoLanguage.GO).value == "go"
        assert repo_language_for_investigation(None).value == "generic"


class TestDetectRepositoryLanguage:
    def test_go_only_repo_detected_as_go(self, tmp_path):
        for name in ("main.go", "pkg/util.go", "cmd/app.go"):
            p = tmp_path / name
            p.parent.mkdir(parents=True, exist_ok=True)
            p.touch()
        assert detect_repository_language(str(tmp_path)) == RepoLanguage.GO

    def test_c_only_repo_detected_as_c(self, tmp_path):
        (tmp_path / "src").mkdir()
        (tmp_path / "src" / "main.c").touch()
        (tmp_path / "include").mkdir()
        (tmp_path / "include" / "util.h").touch()
        assert detect_repository_language(str(tmp_path)) == RepoLanguage.C

    def test_unsupported_extensions_default_to_c(self, tmp_path):
        """Only .go/.c/.h are counted; other extensions do not affect detection."""
        (tmp_path / "main.rs").touch()
        (tmp_path / "lib.py").touch()
        assert detect_repository_language(str(tmp_path)) == RepoLanguage.C

    def test_empty_repo_defaults_to_c(self, tmp_path):
        assert detect_repository_language(str(tmp_path)) == RepoLanguage.C

    def test_go_wins_when_it_has_most_files(self, tmp_path):
        for name in ("a.go", "b.go", "c.go", "x.c"):
            (tmp_path / name).touch()
        assert detect_repository_language(str(tmp_path)) == RepoLanguage.GO

    def test_c_wins_when_it_has_most_files(self, tmp_path):
        for name in ("a.c", "b.c", "c.h", "x.go"):
            (tmp_path / name).touch()
        assert detect_repository_language(str(tmp_path)) == RepoLanguage.C

    def test_go_wins_on_tie_with_c(self, tmp_path):
        for name in ("a.go", "b.c"):
            (tmp_path / name).touch()
        assert detect_repository_language(str(tmp_path)) == RepoLanguage.GO

    def test_nonexistent_path_defaults_to_c(self):
        assert detect_repository_language("/nonexistent/path/xyz") == RepoLanguage.C

    def test_returns_repo_language_enum(self, tmp_path):
        (tmp_path / "main.go").touch()
        result = detect_repository_language(str(tmp_path))
        assert isinstance(result, RepoLanguage)

    def test_ignores_non_source_files(self, tmp_path):
        for name in ("README.md", "config.yaml", "Makefile", "main.go"):
            (tmp_path / name).touch()
        assert detect_repository_language(str(tmp_path)) == RepoLanguage.GO

    def test_nested_go_files_detected(self, tmp_path):
        for name in ("cmd/server/main.go", "internal/api/handler.go"):
            p = tmp_path / name
            p.parent.mkdir(parents=True, exist_ok=True)
            p.touch()
        assert detect_repository_language(str(tmp_path)) == RepoLanguage.GO

    def test_header_files_count_toward_c(self, tmp_path):
        for name in ("a.c", "b.h"):
            (tmp_path / name).touch()
        assert detect_repository_language(str(tmp_path)) == RepoLanguage.C


class TestResolveRepoLanguage:
    def test_uses_configured_when_set(self, tmp_path):
        assert resolve_repo_language(str(tmp_path), RepoLanguage.GO) == RepoLanguage.GO

    def test_detects_when_not_configured(self, tmp_path):
        (tmp_path / "main.go").touch()
        assert resolve_repo_language(str(tmp_path), None) == RepoLanguage.GO
