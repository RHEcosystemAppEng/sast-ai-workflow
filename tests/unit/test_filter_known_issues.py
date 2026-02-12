"""
Tests for capture_known_issues function in FilterKnownIssues.py
Covers LLM vector search for identifying known false positives.
"""
import pytest
from unittest.mock import Mock, patch
from src.FilterKnownIssues import capture_known_issues
from dto.Issue import Issue


@pytest.fixture
def mock_config():
    """Create mock config with default settings."""
    config = Mock()
    config.KNOWN_FALSE_POSITIVE_FILE_PATH = "valid_file.txt"
    config.SIMILARITY_ERROR_THRESHOLD = 3
    return config


@pytest.fixture
def valid_known_issues():
    """Standard known issues for testing."""
    return ["Error: USE_AFTER_FREE\nSome error trace\nReason: duplicate variable"]


@pytest.fixture
def mock_document():
    """Create mock document with standard metadata."""
    doc = Mock()
    doc.page_content = "Some error trace"
    doc.metadata = {
        'reason_of_false_positive': 'duplicate variable',
        'issue_type': 'USE_AFTER_FREE',
        'issue_cwe': 'CWE-416'
    }
    return doc


@pytest.fixture
def mock_vector_store_with_results(mock_document):
    """Create mock vector store that returns similarity search results."""
    vector_db = Mock()
    vector_db.similarity_search_with_score.return_value = [
        (mock_document, 0.95)  # Returns list of (doc, score) tuples
    ]
    return vector_db


@pytest.fixture
def mock_vector_store_empty():
    """Create mock vector store that returns empty results."""
    vector_db = Mock()
    vector_db.similarity_search_with_score.return_value = []
    return vector_db


def setup_llm_service_with_vector_store(vector_store):
    """Helper to create mock LLM service with vector store configured."""
    llm_service = Mock()
    vector_service = Mock()
    vector_service.create_known_issues_vector_store.return_value = vector_store
    llm_service.vector_service = vector_service
    llm_service.embedding_llm = Mock()
    return llm_service


class TestCaptureKnownIssues:
    def test__capture_known_issues__faiss_failure_logs_error(self, caplog, mock_config, valid_known_issues):
        # preparation
        mock_llm_service = Mock()
        mock_vector_service = Mock()
        mock_vector_service.create_known_issues_vector_store.side_effect = Exception("FAISS embedding failed")
        mock_llm_service.vector_service = mock_vector_service
        mock_llm_service.embedding_llm = Mock()

        issue_list = [Issue(id="issue1")]

        # testing
        with patch('src.FilterKnownIssues.read_known_errors_file', return_value=valid_known_issues):
            with caplog.at_level('ERROR'):
                with pytest.raises(Exception, match="FAISS embedding failed"):
                    capture_known_issues(mock_llm_service, issue_list, mock_config)

                # assertion
                mock_vector_service.create_known_issues_vector_store.assert_called_once_with(
                    valid_known_issues, mock_llm_service.embedding_llm
                )

    def test__capture_known_issues__issue_error_continues_processing(
        self, caplog, mock_config, valid_known_issues, mock_vector_store_with_results
    ):
        # preparation
        mock_llm_service = setup_llm_service_with_vector_store(mock_vector_store_with_results)

        issue_list = [Issue(id="issue1"), Issue(id="issue2"), Issue(id="issue3")]

        def mock_filter_error(issue, context):
            if issue.id == "issue2":
                raise Exception("Processing failed for issue2")

            mock_response = Mock()
            mock_response.result = "no"
            mock_response.matched_error_trace = ""
            return mock_response

        mock_llm_service.filter_known_error.side_effect = mock_filter_error

        # testing
        with patch('src.FilterKnownIssues.read_known_errors_file', return_value=valid_known_issues):
            with caplog.at_level('ERROR'):
                with pytest.raises(Exception, match="Processing failed for issue2"):
                    capture_known_issues(mock_llm_service, issue_list, mock_config)

    def test__capture_known_issues__valid_data_returns_matches(
        self, mock_config, valid_known_issues, mock_vector_store_with_results
    ):
        # preparation
        mock_llm_service = setup_llm_service_with_vector_store(mock_vector_store_with_results)

        issue_list = [Issue(id="issue1"), Issue(id="issue2"), Issue(id="issue3")]

        def mock_filter_error(issue, context):
            mock_response = Mock()
            if issue.id in ["issue1", "issue3"]:
                mock_response.result = "YES"
                mock_response.matched_error_trace = "matched trace"
            else:
                mock_response.result = "NO"
                mock_response.matched_error_trace = ""
            return mock_response

        mock_llm_service.filter_known_error.side_effect = mock_filter_error

        # testing
        with patch('src.FilterKnownIssues.read_known_errors_file', return_value=valid_known_issues):
            already_seen_dict, context_dict = capture_known_issues(mock_llm_service, issue_list, mock_config)

            # assertion
            assert len(already_seen_dict) == 2
            assert "issue1" in already_seen_dict
            assert "issue3" in already_seen_dict
            assert "issue2" not in already_seen_dict

            assert len(context_dict) == 3
            for issue_id in ["issue1", "issue2", "issue3"]:
                assert issue_id in context_dict

            # Verify that filter_known_error was called for each issue
            assert mock_llm_service.filter_known_error.call_count == 3

            # Verify the vector service was properly initialized
            mock_llm_service.vector_service.create_known_issues_vector_store.assert_called_once_with(
                valid_known_issues, mock_llm_service.embedding_llm
            )

    def test__capture_known_issues__file_error_returns_empty(self, caplog):
        # preparation
        mock_llm_service = Mock()
        mock_config = Mock()
        mock_config.KNOWN_FALSE_POSITIVE_FILE_PATH = "/nonexistent/path/file.txt"

        issue_list = [Issue(id="issue1")]

        # testing
        with patch('src.FilterKnownIssues.read_known_errors_file', side_effect=FileNotFoundError("File not found")):
            with caplog.at_level('ERROR'):
                with pytest.raises(FileNotFoundError, match="File not found"):
                    capture_known_issues(mock_llm_service, issue_list, mock_config)

    def test__capture_known_issues__empty_file_returns_empty(self, mock_config, mock_vector_store_empty):
        # preparation
        mock_llm_service = setup_llm_service_with_vector_store(mock_vector_store_empty)

        issue_list = [Issue(id="issue1")]

        mock_response = Mock()
        mock_response.result = "NO"
        mock_response.matched_error_trace = ""
        mock_llm_service.filter_known_error.return_value = mock_response

        # testing
        with patch('src.FilterKnownIssues.read_known_errors_file', return_value=[]):
            already_seen_dict, context_dict = capture_known_issues(mock_llm_service, issue_list, mock_config)

            # assertion
            assert already_seen_dict == {}
            assert len(context_dict) == 1
            assert "issue1" in context_dict

            # Verify that filter_known_error was called for the single issue
            mock_llm_service.filter_known_error.assert_called_once()

            # Verify the vector service was properly initialized with empty list
            mock_llm_service.vector_service.create_known_issues_vector_store.assert_called_once_with(
                [], mock_llm_service.embedding_llm
            )