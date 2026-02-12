"""
Tests for ground truth loader in investigation/observability/ground_truth_loader.py.

Covers: loading and converting human-verified verdicts to workflow format,
handling of yes/no options, invalid values, empty data, and error conditions.
"""

from unittest.mock import Mock, patch

import pytest

from sast_agent_workflow.investigation.observability.ground_truth_loader import (
    load_ground_truth_verdicts,
)

_MOD = "sast_agent_workflow.investigation.observability.ground_truth_loader"

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def mock_config():
    """Mock configuration object with standard test values."""
    config = Mock()
    config.HUMAN_VERIFIED_FILE_PATH = "/path/to/verified.xlsx"
    config.INPUT_REPORT_FILE_PATH = "/path/to/report.sarif"
    return config


# ---------------------------------------------------------------------------
# load_ground_truth_verdicts
# ---------------------------------------------------------------------------


class TestLoadGroundTruthVerdicts:
    """Tests for load_ground_truth_verdicts."""

    @patch(f"{_MOD}.get_human_verified_results")
    def test__converts_yes_to_false_positive(self, mock_get, mock_config):
        """'yes' and 'y' ground truth values should map to FALSE_POSITIVE."""
        mock_get.return_value = {
            "def1": "yes",
            "def2": "y",
        }

        result = load_ground_truth_verdicts(mock_config)

        assert result is not None
        assert result["def1"] == "FALSE_POSITIVE"
        assert result["def2"] == "FALSE_POSITIVE"

    @patch(f"{_MOD}.get_human_verified_results")
    def test__converts_no_to_true_positive(self, mock_get, mock_config):
        """'no' and 'n' ground truth values should map to TRUE_POSITIVE."""
        mock_get.return_value = {
            "def1": "no",
            "def2": "n",
        }

        result = load_ground_truth_verdicts(mock_config)

        assert result is not None
        assert result["def1"] == "TRUE_POSITIVE"
        assert result["def2"] == "TRUE_POSITIVE"

    @patch(f"{_MOD}.get_human_verified_results")
    def test__mixed_verdicts(self, mock_get, mock_config):
        """Should handle a mix of yes/no values correctly."""
        mock_get.return_value = {
            "def1": "yes",
            "def2": "no",
            "def3": "y",
            "def4": "n",
        }

        result = load_ground_truth_verdicts(mock_config)

        assert result is not None
        assert len(result) == 4
        assert result["def1"] == "FALSE_POSITIVE"
        assert result["def2"] == "TRUE_POSITIVE"
        assert result["def3"] == "FALSE_POSITIVE"
        assert result["def4"] == "TRUE_POSITIVE"

    @patch(f"{_MOD}.get_human_verified_results")
    def test__skips_invalid_values(self, mock_get, mock_config):
        """Invalid ground truth values should be skipped with a warning."""
        mock_get.return_value = {
            "def1": "yes",
            "def2": "maybe",
            "def3": "no",
            "def4": "unknown",
        }

        result = load_ground_truth_verdicts(mock_config)

        assert result is not None
        assert len(result) == 2
        assert "def1" in result
        assert "def3" in result
        assert "def2" not in result
        assert "def4" not in result

    @patch(f"{_MOD}.get_human_verified_results")
    def test__returns_none_when_no_ground_truth(self, mock_get, mock_config):
        """Should return None when get_human_verified_results returns empty/None."""
        mock_get.return_value = None

        result = load_ground_truth_verdicts(mock_config)

        assert result is None

    @patch(f"{_MOD}.get_human_verified_results")
    def test__returns_none_when_empty_dict(self, mock_get, mock_config):
        """Should return None when get_human_verified_results returns empty dict."""
        mock_get.return_value = {}

        result = load_ground_truth_verdicts(mock_config)

        assert result is None

    @patch(f"{_MOD}.get_human_verified_results")
    def test__returns_none_on_exception(self, mock_get, mock_config):
        """Should return None if an exception occurs during loading."""
        mock_get.side_effect = RuntimeError("file not found")

        result = load_ground_truth_verdicts(mock_config)

        assert result is None

    @patch(f"{_MOD}.get_human_verified_results")
    def test__passes_config_to_utility(self, mock_get, mock_config):
        """Should pass the config object to get_human_verified_results."""
        mock_get.return_value = None

        load_ground_truth_verdicts(mock_config)

        mock_get.assert_called_once_with(mock_config)

    @patch(f"{_MOD}.get_human_verified_results")
    def test__all_invalid_returns_empty_dict(self, mock_get, mock_config):
        """Should return empty dict when all values are invalid."""
        mock_get.return_value = {
            "def1": "maybe",
            "def2": "unsure",
        }

        result = load_ground_truth_verdicts(mock_config)

        assert result is not None
        assert len(result) == 0

    @patch(f"{_MOD}.get_human_verified_results")
    def test__single_issue(self, mock_get, mock_config):
        """Should handle a single issue correctly."""
        mock_get.return_value = {"def1": "yes"}

        result = load_ground_truth_verdicts(mock_config)

        assert result == {"def1": "FALSE_POSITIVE"}
