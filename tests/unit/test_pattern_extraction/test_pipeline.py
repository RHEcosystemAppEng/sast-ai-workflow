"""Tests for pattern extraction pipeline."""

import json
import os
import tempfile
from unittest.mock import Mock, patch

import pytest

from pattern_extraction.models import (
    ExtractedPattern,
    FalsePositiveIndicator,
    MatchingCriterion,
    PatternExtractionResponse,
)
from pattern_extraction.parsers import ParsedFalsePositive
from pattern_extraction.pipeline import PatternExtractionPipeline


@pytest.fixture
def mock_llm():
    return Mock()


@pytest.fixture
def sample_entries():
    """List of ParsedFalsePositive entries for testing."""
    return [
        ParsedFalsePositive(
            package_name="test-pkg",
            issue_type="INTEGER_OVERFLOW",
            cwe="CWE-190",
            error_trace="file.c:10: underflow: cast to signed type",
            source_code="int x = (int)offset;",
            analyst_justification="Return value is checked",
            verdict="FALSE POSITIVE",
            entry_index=1,
        ),
        ParsedFalsePositive(
            package_name="test-pkg",
            issue_type="INTEGER_OVERFLOW",
            cwe="CWE-190",
            error_trace="file.c:20: underflow: another cast",
            source_code="int y = (int)size;",
            analyst_justification="Size is bounded",
            verdict="FALSE POSITIVE",
            entry_index=2,
        ),
        ParsedFalsePositive(
            package_name="test-pkg",
            issue_type="UNINIT",
            cwe="CWE-457",
            error_trace="util.c:5: uninit_use: Using uninitialized value",
            source_code=None,
            analyst_justification="Initialized by prior call",
            verdict="FALSE POSITIVE",
            entry_index=3,
        ),
    ]


@pytest.fixture
def sample_extracted_pattern():
    return ExtractedPattern(
        issue_type="INTEGER_OVERFLOW",
        cwe="CWE-190",
        pattern_summary="Unsigned-to-signed cast with proper bounds checking",
        code_pattern="Cast to signed type where value is bounded or checked",
        file_pattern=None,
        false_positive_indicators=[
            FalsePositiveIndicator(
                indicator_type="error_handling",
                description="Return value checked",
            )
        ],
        matching_criteria=[
            MatchingCriterion(
                field="issue_type",
                value="INTEGER_OVERFLOW",
                weight=0.9,
            )
        ],
        analyst_reasoning_summary="Value is properly bounded before cast",
        confidence=0.8,
    )


@pytest.fixture
def mock_llm_response(sample_extracted_pattern):
    return PatternExtractionResponse(patterns=[sample_extracted_pattern])


@pytest.fixture
def ground_truth_dir():
    """Create a temp directory with ground truth files for pipeline testing."""
    content = """================================================================================
GROUND-TRUTH ENTRIES FOR: test-pkg
================================================================================

Package: test-pkg
Total Entries: 2
False Positives: 2
True Positives: 0

---
Entry #1:
Issue Type: INTEGER_OVERFLOW
CWE: CWE-190

Error Trace:
file.c:10: underflow: cast to signed type
#   10|->    int x = (int)offset;

Source Code (file.c):
```c
10| int x = (int)offset;
```

Ground Truth Classification: FALSE POSITIVE
Human Expert Justification: Return value is checked.
---


---
Entry #2:
Issue Type: UNINIT
CWE: CWE-457

Error Trace:
util.c:5: uninit_use: Using uninitialized value.
#    5|->    memcpy(dest, buf, len);

Source Code (util.c):
```c
5| memcpy(dest, buf, len);
```

Ground Truth Classification: FALSE POSITIVE
Human Expert Justification: Initialized by prior call.
---
"""
    tmpdir = tempfile.mkdtemp()
    with open(os.path.join(tmpdir, "test-pkg.txt"), "w", encoding="utf-8") as f:
        f.write(content)

    yield tmpdir

    # Cleanup
    import shutil

    shutil.rmtree(tmpdir, ignore_errors=True)


class TestPipelineProcessing:
    @patch("pattern_extraction.pipeline.robust_structured_output")
    def test__processes_entries_and_returns_patterns(
        self, mock_rso, mock_llm, mock_llm_response, ground_truth_dir
    ):
        mock_rso.return_value = mock_llm_response

        pipeline = PatternExtractionPipeline(
            llm=mock_llm,
            input_dir=ground_truth_dir,
            max_source_code_chars=3000,
            output_file=os.path.join(ground_truth_dir, "output.json"),
            checkpoint_dir=None,
        )

        result = pipeline.run()

        assert result["metadata"]["total_patterns_extracted"] > 0
        assert len(result["patterns"]) > 0
        assert mock_rso.called

    @patch("pattern_extraction.pipeline.robust_structured_output")
    def test__groups_entries_by_issue_type(
        self, mock_rso, mock_llm, sample_extracted_pattern, sample_entries
    ):
        mock_rso.return_value = PatternExtractionResponse(
            patterns=[sample_extracted_pattern]
        )

        pipeline = PatternExtractionPipeline(
            llm=mock_llm,
            input_dir="dummy",
            max_source_code_chars=3000,
            checkpoint_dir=None,
        )

        _, errors = pipeline._process_package("test-pkg", sample_entries)

        # Should be called once per issue_type (INTEGER_OVERFLOW + UNINIT = 2)
        assert mock_rso.call_count == 2
        assert len(errors) == 0

    @patch("pattern_extraction.pipeline.robust_structured_output")
    def test__llm_error_continues_to_next_batch(
        self, mock_rso, mock_llm, sample_extracted_pattern, sample_entries
    ):
        # First call fails, second succeeds
        mock_rso.side_effect = [
            Exception("LLM API error"),
            PatternExtractionResponse(patterns=[sample_extracted_pattern]),
        ]

        pipeline = PatternExtractionPipeline(
            llm=mock_llm,
            input_dir="dummy",
            max_source_code_chars=3000,
            checkpoint_dir=None,
        )

        patterns, errors = pipeline._process_package("test-pkg", sample_entries)

        assert len(errors) == 1
        assert "LLM API error" in errors[0]["error"]
        assert len(patterns) == 1  # Second batch succeeded

    @patch("pattern_extraction.pipeline.robust_structured_output")
    def test__filters_only_false_positives(
        self, mock_rso, mock_llm, sample_extracted_pattern
    ):
        mock_rso.return_value = PatternExtractionResponse(
            patterns=[sample_extracted_pattern]
        )

        entries_mixed = [
            ParsedFalsePositive(
                package_name="pkg",
                issue_type="UNINIT",
                cwe="CWE-457",
                error_trace="trace",
                source_code=None,
                analyst_justification="reason",
                verdict="FALSE POSITIVE",
                entry_index=1,
            ),
            ParsedFalsePositive(
                package_name="pkg",
                issue_type="UNINIT",
                cwe="CWE-457",
                error_trace="trace",
                source_code=None,
                analyst_justification="reason",
                verdict="TRUE POSITIVE",
                entry_index=2,
            ),
        ]

        pipeline = PatternExtractionPipeline(
            llm=mock_llm,
            input_dir="dummy",
            max_source_code_chars=3000,
            only_false_positives=True,
            checkpoint_dir=None,
        )

        filtered = pipeline._filter_entries({"pkg": entries_mixed})

        assert len(filtered["pkg"]) == 1
        assert filtered["pkg"][0].verdict == "FALSE POSITIVE"

    def test__format_entries_for_prompt(self, mock_llm, sample_entries):
        pipeline = PatternExtractionPipeline(
            llm=mock_llm,
            input_dir="dummy",
            max_source_code_chars=3000,
            checkpoint_dir=None,
        )

        formatted = pipeline._format_entries_for_prompt(sample_entries[:2])

        assert "--- Entry 1 ---" in formatted
        assert "--- Entry 2 ---" in formatted
        assert "INTEGER_OVERFLOW" in formatted
        assert "CWE-190" in formatted
        assert "Error Trace:" in formatted
        assert "Analyst Justification:" in formatted

    def test__format_entries_truncates_long_source_code(self, mock_llm):
        entry = ParsedFalsePositive(
            package_name="pkg",
            issue_type="UNINIT",
            cwe="CWE-457",
            error_trace="trace",
            source_code="x" * 10000,  # Very long source code
            analyst_justification="reason",
            verdict="FALSE POSITIVE",
            entry_index=1,
        )

        pipeline = PatternExtractionPipeline(
            llm=mock_llm,
            input_dir="dummy",
            max_source_code_chars=3000,
            checkpoint_dir=None,
        )

        formatted = pipeline._format_entries_for_prompt([entry])

        assert "truncated" in formatted.lower()
        assert len(formatted) < 10000


class TestCheckpointing:
    @patch("pattern_extraction.pipeline.robust_structured_output")
    def test__checkpoint_saved_at_interval(
        self, mock_rso, mock_llm, mock_llm_response, ground_truth_dir
    ):
        mock_rso.return_value = mock_llm_response

        checkpoint_dir = os.path.join(ground_truth_dir, "ckpt")

        pipeline = PatternExtractionPipeline(
            llm=mock_llm,
            input_dir=ground_truth_dir,
            max_source_code_chars=3000,
            output_file=os.path.join(ground_truth_dir, "output.json"),
            checkpoint_dir=checkpoint_dir,
            checkpoint_interval=1,  # Checkpoint after every package
        )

        pipeline.run()

        # Per-package checkpoint should exist
        pkg_files = [
            f for f in os.listdir(checkpoint_dir) if f.startswith("pkg_")
        ]
        assert len(pkg_files) >= 1

    @patch("pattern_extraction.pipeline.robust_structured_output")
    def test__resume_skips_completed_packages(
        self, mock_rso, mock_llm, mock_llm_response, ground_truth_dir
    ):
        mock_rso.return_value = mock_llm_response

        checkpoint_dir = os.path.join(ground_truth_dir, "ckpt")
        os.makedirs(checkpoint_dir)

        # Write a fake per-package checkpoint
        checkpoint_data = {
            "package": "test-pkg",
            "timestamp": "2026-01-01T00:00:00",
            "patterns": [mock_llm_response.patterns[0].model_dump()],
        }
        with open(
            os.path.join(checkpoint_dir, "pkg_test-pkg.json"), "w"
        ) as f:
            json.dump(checkpoint_data, f)

        pipeline = PatternExtractionPipeline(
            llm=mock_llm,
            input_dir=ground_truth_dir,
            max_source_code_chars=3000,
            output_file=os.path.join(ground_truth_dir, "output.json"),
            checkpoint_dir=checkpoint_dir,
        )

        result = pipeline.run(resume=True)

        # LLM should NOT have been called since the only package was checkpointed
        mock_rso.assert_not_called()
        # But the patterns from checkpoint should still be in the result
        assert result["metadata"]["total_patterns_extracted"] == 1


class TestProgressTracking:
    @patch("pattern_extraction.pipeline.robust_structured_output")
    def test__progress_callback_called(
        self, mock_rso, mock_llm, mock_llm_response, ground_truth_dir
    ):
        mock_rso.return_value = mock_llm_response
        progress_messages = []

        pipeline = PatternExtractionPipeline(
            llm=mock_llm,
            input_dir=ground_truth_dir,
            max_source_code_chars=3000,
            output_file=os.path.join(ground_truth_dir, "output.json"),
            checkpoint_dir=None,
            progress_callback=lambda msg: progress_messages.append(msg),
        )

        pipeline.run()

        assert len(progress_messages) > 0
        # Should contain parsing message
        assert any("Parsing" in msg for msg in progress_messages)
        # Should contain progress with count
        assert any("[1/" in msg for msg in progress_messages)
        # Should contain completion message
        assert any("Done" in msg for msg in progress_messages)


class TestOutputFile:
    @patch("pattern_extraction.pipeline.robust_structured_output")
    def test__output_json_structure(
        self, mock_rso, mock_llm, mock_llm_response, ground_truth_dir
    ):
        mock_rso.return_value = mock_llm_response

        output_file = os.path.join(ground_truth_dir, "output.json")

        pipeline = PatternExtractionPipeline(
            llm=mock_llm,
            input_dir=ground_truth_dir,
            max_source_code_chars=3000,
            output_file=output_file,
            checkpoint_dir=None,
        )

        pipeline.run()

        assert os.path.exists(output_file)

        with open(output_file, "r") as f:
            data = json.load(f)

        assert "metadata" in data
        assert "patterns" in data
        assert "errors" in data
        assert "timestamp" in data["metadata"]
        assert "total_packages" in data["metadata"]
        assert "total_patterns_extracted" in data["metadata"]
        assert "processing_time_seconds" in data["metadata"]
