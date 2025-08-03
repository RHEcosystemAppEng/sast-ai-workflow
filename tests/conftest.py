"""
Pytest configuration and shared fixtures for SAST-AI-Workflow tests.
"""
import pytest
from unittest.mock import Mock
# TODO: Import actual classes and services once we implement the tests
# from src.LLMService import LLMService
# from src.common.config import Config

@pytest.fixture
def mock_llm_service():
    """Mock LLM service for testing LLM-dependent functions."""
    # TODO: Implement mock LLM service with all required methods
    pass

@pytest.fixture
def mock_config():
    """Mock configuration for testing."""
    # TODO: Implement mock configuration object
    pass

@pytest.fixture
def sample_issue():
    """Sample Issue object for testing."""
    # TODO: Create sample Issue object with realistic data
    pass

@pytest.fixture
def sample_issues_list():
    """List of sample Issue objects for testing."""
    # TODO: Create list of varied Issue objects for comprehensive testing
    pass

@pytest.fixture
def mock_repo_handler():
    """Mock repository handler for testing code extraction."""
    # TODO: Implement mock repository handler
    pass

@pytest.fixture
def sample_error_trace():
    """Sample error trace for testing code extraction."""
    # TODO: Create realistic error trace data
    pass

@pytest.fixture
def sample_ground_truth_data():
    """Sample ground truth data for evaluation testing."""
    # TODO: Create sample ground truth data dictionary
    pass

@pytest.fixture
def sample_summary_data():
    """Sample summary data for testing output functions."""
    # TODO: Create sample summary data
    pass