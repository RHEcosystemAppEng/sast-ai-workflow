"""Tests for pattern extraction parsers."""

import os
import tempfile

import pytest

from pattern_extraction.parsers import (
    parse_ground_truth_file,
    parse_ignore_err_file,
)


class TestGroundTruthParser:
    def test__valid_file_extracts_all_fields(self, sample_ground_truth_content):
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".txt", delete=False, encoding="utf-8"
        ) as f:
            f.write(sample_ground_truth_content)
            f.flush()

            entries = parse_ground_truth_file(f.name)

        os.unlink(f.name)

        assert len(entries) == 2

        # First entry - FALSE POSITIVE
        fp = entries[0]
        assert fp.package_name == "test-pkg-1.0-1.el10"
        assert fp.issue_type == "INTEGER_OVERFLOW"
        assert fp.cwe == "CWE-190"
        assert fp.verdict == "FALSE POSITIVE"
        assert fp.entry_index == 1
        assert "tainted_data_argument" in fp.error_trace
        assert "lseek" in fp.error_trace
        assert fp.source_code is not None
        assert "process_file" in fp.source_code
        assert "lseek returns -1" in fp.analyst_justification

    def test__true_positive_parsed_correctly(self, sample_ground_truth_content):
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".txt", delete=False, encoding="utf-8"
        ) as f:
            f.write(sample_ground_truth_content)
            f.flush()

            entries = parse_ground_truth_file(f.name)

        os.unlink(f.name)

        tp = entries[1]
        assert tp.issue_type == "UNINIT"
        assert tp.cwe == "CWE-457"
        assert tp.verdict == "TRUE POSITIVE"
        assert tp.entry_index == 2
        assert "uninit_use" in tp.error_trace

    def test__analyst_comment_included_in_justification(self, sample_ground_truth_content):
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".txt", delete=False, encoding="utf-8"
        ) as f:
            f.write(sample_ground_truth_content)
            f.flush()

            entries = parse_ground_truth_file(f.name)

        os.unlink(f.name)

        # First entry has both Human Expert Justification and Analyst Comment
        fp = entries[0]
        assert "lseek returns -1" in fp.analyst_justification
        assert "return value of lseek" in fp.analyst_justification.lower()

    def test__single_entry_file(self, sample_ground_truth_fp_only_content):
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".txt", delete=False, encoding="utf-8"
        ) as f:
            f.write(sample_ground_truth_fp_only_content)
            f.flush()

            entries = parse_ground_truth_file(f.name)

        os.unlink(f.name)

        assert len(entries) == 1
        assert entries[0].issue_type == "RESOURCE_LEAK"
        assert entries[0].cwe == "CWE-772"
        assert entries[0].verdict == "FALSE POSITIVE"
        assert "free" in entries[0].analyst_justification.lower()

    def test__empty_file_returns_empty_list(self):
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".txt", delete=False, encoding="utf-8"
        ) as f:
            f.write("")
            f.flush()

            entries = parse_ground_truth_file(f.name)

        os.unlink(f.name)

        assert entries == []

    def test__package_name_from_header(self, sample_ground_truth_content):
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".txt", delete=False, encoding="utf-8"
        ) as f:
            f.write(sample_ground_truth_content)
            f.flush()

            entries = parse_ground_truth_file(f.name)

        os.unlink(f.name)

        assert entries[0].package_name == "test-pkg-1.0-1.el10"

    def test__package_name_fallback_to_filename(self):
        content = """---
Entry #1:
Issue Type: UNINIT
CWE: CWE-457

Error Trace:
file.c:10: uninit_use: Using uninitialized value.

Source Code (file.c):
```c
10| int x;
```

Ground Truth Classification: FALSE POSITIVE
Human Expert Justification: Initialized elsewhere.
---
"""
        with tempfile.NamedTemporaryFile(
            mode="w",
            suffix=".txt",
            delete=False,
            prefix="my-package-",
            encoding="utf-8",
        ) as f:
            f.write(content)
            f.flush()

            entries = parse_ground_truth_file(f.name)
            # Should use filename (without extension) since no header
            assert entries[0].package_name == os.path.splitext(
                os.path.basename(f.name)
            )[0]

        os.unlink(f.name)

    def test__real_test_pattern_data_file(self):
        """Parse a real file from test_pattern_data/ if it exists."""
        real_file = os.path.join(
            os.path.dirname(__file__),
            "..", "..", "..",
            "test_pattern_data",
            "chrpath-0.16-23.el10.txt",
        )
        if not os.path.exists(real_file):
            pytest.skip("test_pattern_data not available")

        entries = parse_ground_truth_file(real_file)

        assert len(entries) == 2
        assert all(e.issue_type == "INTEGER_OVERFLOW" for e in entries)
        assert all(e.cwe == "CWE-190" for e in entries)
        assert all(e.verdict == "FALSE POSITIVE" for e in entries)
        assert all(e.source_code is not None for e in entries)
        assert all(e.analyst_justification for e in entries)


class TestIgnoreErrParser:
    def test__valid_entries_extracted(self, sample_ignore_err_content):
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".err", delete=False, encoding="utf-8"
        ) as f:
            f.write(sample_ignore_err_content)
            f.flush()

            entries = parse_ignore_err_file(f.name, package_name="test-pkg")

        os.unlink(f.name)

        assert len(entries) == 2

        # First entry
        e1 = entries[0]
        assert e1.issue_type == "INTEGER_OVERFLOW"
        assert e1.cwe == "CWE-190"
        assert e1.verdict == "FALSE POSITIVE"
        assert "tainted_data_argument" in e1.error_trace
        assert e1.analyst_justification  # Has justification text

        # Second entry
        e2 = entries[1]
        assert e2.issue_type == "UNINIT"
        assert e2.cwe == "CWE-457"

    def test__all_entries_marked_false_positive(self, sample_ignore_err_content):
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".err", delete=False, encoding="utf-8"
        ) as f:
            f.write(sample_ignore_err_content)
            f.flush()

            entries = parse_ignore_err_file(f.name)

        os.unlink(f.name)

        assert all(e.verdict == "FALSE POSITIVE" for e in entries)

    def test__missing_cwe_returns_none(self, sample_ignore_err_no_cwe_content):
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".err", delete=False, encoding="utf-8"
        ) as f:
            f.write(sample_ignore_err_no_cwe_content)
            f.flush()

            entries = parse_ignore_err_file(f.name)

        os.unlink(f.name)

        assert len(entries) == 1
        assert entries[0].issue_type == "DEADCODE"
        assert entries[0].cwe is None

    def test__source_code_is_none(self, sample_ignore_err_content):
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".err", delete=False, encoding="utf-8"
        ) as f:
            f.write(sample_ignore_err_content)
            f.flush()

            entries = parse_ignore_err_file(f.name)

        os.unlink(f.name)

        assert all(e.source_code is None for e in entries)

    def test__package_name_from_arg(self, sample_ignore_err_content):
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".err", delete=False, encoding="utf-8"
        ) as f:
            f.write(sample_ignore_err_content)
            f.flush()

            entries = parse_ignore_err_file(f.name, package_name="my-pkg-1.0")

        os.unlink(f.name)

        assert all(e.package_name == "my-pkg-1.0" for e in entries)

    def test__empty_file_returns_empty_list(self):
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".err", delete=False, encoding="utf-8"
        ) as f:
            f.write("")
            f.flush()

            entries = parse_ignore_err_file(f.name)

        os.unlink(f.name)

        assert entries == []


