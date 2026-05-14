"""Tests for Utils.report_path_utils."""

from Utils.report_path_utils import (
    normalize_report_source_path,
    segment_looks_like_project_version_archive_root,
)


class TestNormalizeReportSourcePath:
    def test_strips_configured_report_prefix(self):
        assert (
            normalize_report_source_path(
                "tcl-8.6.13/generic/tclZlib.c",
                report_file_prefix="tcl-8.6.13/",
                project_name="tcl",
            )
            == "generic/tclZlib.c"
        )

    def test_strips_scan_root_when_compact_version_segment(self):
        assert (
            normalize_report_source_path(
                "tcl8.6.13/generic/tclZlib.c",
                report_file_prefix="tcl-8.6.13/",
                project_name="tcl",
            )
            == "generic/tclZlib.c"
        )

    def test_leading_slash_stripped_before_join(self):
        assert (
            normalize_report_source_path(
                "/p-1/foo.c",
                report_file_prefix="p-1/",
                project_name="p",
            )
            == "foo.c"
        )

    def test_does_not_strip_repo_dir_that_extends_project_name(self):
        assert (
            normalize_report_source_path(
                "tcl_api/generic/tclZlib.c",
                report_file_prefix="tcl-8.6.13/",
                project_name="tcl",
            )
            == "tcl_api/generic/tclZlib.c"
        )

    def test_does_not_strip_versionish_repo_dir_without_separators(self):
        """``unzip60``-style dirs are real tree roots, not ``unzip`` + version."""
        assert (
            normalize_report_source_path(
                "unzip60/envargs.c",
                report_file_prefix="unzip-6.0/",
                project_name="unzip",
            )
            == "unzip60/envargs.c"
        )


class TestSegmentLooksLikeProjectVersionArchiveRoot:
    def test_tcl_digit_version_segment(self):
        assert segment_looks_like_project_version_archive_root("tcl8.6.13", "tcl") is True

    def test_tcl_hyphen_version_segment(self):
        assert segment_looks_like_project_version_archive_root("tcl-8.6.13", "tcl") is True

    def test_tcl_api_not_archive_root(self):
        assert segment_looks_like_project_version_archive_root("tcl_api", "tcl") is False

    def test_unzip60_not_archive_root(self):
        assert segment_looks_like_project_version_archive_root("unzip60", "unzip") is False
