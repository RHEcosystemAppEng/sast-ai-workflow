"""
Integration test specific fixtures and configuration.
"""
import pytest
from unittest.mock import Mock, MagicMock

from common.config import Config


@pytest.fixture
def mock_confidence_config(tmp_path):
    """
    Create a mock Config with confidence scoring configuration.

    Shared by both integration tests (test_confidence_scoring_workflow.py)
    and E2E tests (test_confidence_scoring_e2e.py) to reduce code duplication.
    """
    config = MagicMock(spec=Config)

    # Basic config (for E2E tests)
    config.PROJECT_NAME = "test-project"
    config.PROJECT_VERSION = "1.0.0"
    config.REPO_LOCAL_PATH = str(tmp_path)
    config.INPUT_REPORT_FILE_PATH = str(tmp_path / "report.html")
    config.OUTPUT_FILE_PATH = str(tmp_path / "output.xlsx")
    config.WRITE_RESULTS_INCLUDE_NON_FINAL = True

    # LLM config
    config.LLM_URL = "http://mock-llm"
    config.LLM_MODEL_NAME = "mock-model"
    config.LLM_API_KEY = "mock-key"
    config.MAX_ANALYSIS_ITERATIONS = 3

    # Confidence scoring config (balanced 20/30/20/30)
    config.CONFIDENCE_WEIGHT_FILTER = 0.20
    config.CONFIDENCE_WEIGHT_AGENT = 0.30
    config.CONFIDENCE_WEIGHT_EVIDENCE = 0.20
    config.CONFIDENCE_WEIGHT_INVESTIGATION = 0.30

    # Evidence sub-weights
    config.EVIDENCE_WEIGHT_FAISS_SCORE = 0.40
    config.EVIDENCE_WEIGHT_FILES_FETCHED = 0.30
    config.EVIDENCE_WEIGHT_EVIDENCE_COUNT = 0.30

    # Investigation sub-weights
    config.INVESTIGATION_WEIGHT_DEPTH = 0.25
    config.INVESTIGATION_WEIGHT_TOOL_CALLS = 0.25
    config.INVESTIGATION_WEIGHT_REANALYSIS = 0.25
    config.INVESTIGATION_WEIGHT_STOP_REASON = 0.25

    # Normalization caps
    config.CONFIDENCE_MAX_FILES_FOR_NORMALIZATION = 10
    config.CONFIDENCE_MAX_EVIDENCE_ITEMS_FOR_NORMALIZATION = 5
    config.CONFIDENCE_MAX_SYMBOLS_FOR_NORMALIZATION = 5
    config.CONFIDENCE_MAX_TOOL_CALLS_FOR_NORMALIZATION = 20
    config.CONFIDENCE_MAX_REANALYSIS_FOR_NORMALIZATION = 3

    # Stop reason scores
    config.STOP_REASON_SCORE_APPROVED = 1.0
    config.STOP_REASON_SCORE_MAX_ITERATIONS = 0.6
    config.STOP_REASON_SCORE_NO_PROGRESS = 0.3
    config.STOP_REASON_SCORE_UNKNOWN = 0.2

    # Other settings
    config.USE_KNOWN_FALSE_POSITIVE_FILE = False
    config.CALCULATE_RAGAS_METRICS = False

    return config