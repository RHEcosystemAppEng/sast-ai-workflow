"""
Tests for capture_known_issues function in stage/filter_known_issues.py
Covers LLM vector search for identifying known false positives.
"""
import pytest
from unittest.mock import Mock, patch
from src.stage.filter_known_issues import capture_known_issues
from dto.Issue import Issue


class TestCaptureKnownIssues:
    def test__capture_known_issues__faiss_failure_logs_error(self, caplog):
        # preparation
        mock_llm_service = Mock()
        mock_config = Mock()
        mock_config.KNOWN_FALSE_POSITIVE_FILE_PATH = "valid_file.txt"
        mock_config.SIMILARITY_ERROR_THRESHOLD = 3
        
        valid_known_issues = ["Error: USE_AFTER_FREE\nSome error trace\nReason: duplicate variable"]
        
        mock_vector_service = Mock()
        mock_vector_service.create_known_issues_vector_store.side_effect = Exception("FAISS embedding failed")
        mock_llm_service.vector_service = mock_vector_service
        mock_llm_service.embedding_llm = Mock()
        
        issue1 = Issue("issue1")
        issue_list = [issue1]
        
        # testing
        with patch('src.stage.filter_known_issues.read_known_errors_file', return_value=valid_known_issues):
            with caplog.at_level('ERROR'):
                with pytest.raises(Exception, match="FAISS embedding failed"):
                    capture_known_issues(mock_llm_service, issue_list, mock_config)
                
                # assertion
                mock_vector_service.create_known_issues_vector_store.assert_called_once_with(valid_known_issues, mock_llm_service.embedding_llm)

    def test__capture_known_issues__issue_error_continues_processing(self, caplog):
        # preparation        
        mock_llm_service = Mock()
        mock_config = Mock()
        mock_config.KNOWN_FALSE_POSITIVE_FILE_PATH = "valid_file.txt"
        mock_config.SIMILARITY_ERROR_THRESHOLD = 3
        
        valid_known_issues = ["Error: USE_AFTER_FREE\nSome error trace\nReason: duplicate variable"]
        
        mock_doc = Mock()
        mock_doc.page_content = "Some error trace"
        mock_doc.metadata = {
            'reason_of_false_positive': 'duplicate variable',
            'issue_type': 'USE_AFTER_FREE',
            'issue_cwe': 'CWE-416'
        }
        
        mock_retriever = Mock()
        mock_retriever.invoke.return_value = [mock_doc]
        
        mock_vector_db = Mock()
        mock_vector_db.as_retriever.return_value = mock_retriever
        
        mock_vector_service = Mock()
        mock_vector_service.create_known_issues_vector_store.return_value = mock_vector_db
        mock_llm_service.vector_service = mock_vector_service
        mock_llm_service.embedding_llm = Mock()
        
        issue1 = Issue("issue1")
        issue2 = Issue("issue2") 
        issue3 = Issue("issue3")
        issue_list = [issue1, issue2, issue3]
        
        def mock_filter_error(issue, context):
            if issue.id == "issue2":
                raise Exception("Processing failed for issue2")
            
            mock_response = Mock()
            mock_response.result = "no"
            mock_response.equal_error_trace = ""
            return mock_response
        
        mock_llm_service.filter_known_error.side_effect = mock_filter_error
        
        # testing
        with patch('src.stage.filter_known_issues.read_known_errors_file', return_value=valid_known_issues):
            with caplog.at_level('ERROR'):
                with pytest.raises(Exception, match="Processing failed for issue2"):
                    capture_known_issues(mock_llm_service, issue_list, mock_config)

    def test__capture_known_issues__valid_data_returns_matches(self, caplog):
        # preparation
        
        mock_llm_service = Mock()
        mock_config = Mock()
        mock_config.KNOWN_FALSE_POSITIVE_FILE_PATH = "valid_file.txt"
        mock_config.SIMILARITY_ERROR_THRESHOLD = 3
        
        valid_known_issues = ["Error: USE_AFTER_FREE\nSome error trace\nReason: duplicate variable"]
        
        # Mock the vector DB and retriever
        mock_doc = Mock()
        mock_doc.page_content = "Some error trace"
        mock_doc.metadata = {
            'reason_of_false_positive': 'duplicate variable',
            'issue_type': 'USE_AFTER_FREE',
            'issue_cwe': 'CWE-416'
        }
        
        mock_retriever = Mock()
        mock_retriever.invoke.return_value = [mock_doc]
        
        mock_vector_db = Mock()
        mock_vector_db.as_retriever.return_value = mock_retriever
        
        # Mock the vector service
        mock_vector_service = Mock()
        mock_vector_service.create_known_issues_vector_store.return_value = mock_vector_db
        mock_llm_service.vector_service = mock_vector_service
        mock_llm_service.embedding_llm = Mock()
        
        issue1 = Issue("issue1")
        issue2 = Issue("issue2") 
        issue3 = Issue("issue3")
        issue_list = [issue1, issue2, issue3]
        
        def mock_filter_error(issue, context):
            mock_response = Mock()
            
            if issue.id in ["issue1", "issue3"]:
                mock_response.result = "YES"
                mock_response.equal_error_trace = "matched trace"
            else:
                mock_response.result = "NO"
                mock_response.equal_error_trace = ""
                
            return mock_response
        
        mock_llm_service.filter_known_error.side_effect = mock_filter_error
        
        # testing
        with patch('src.stage.filter_known_issues.read_known_errors_file', return_value=valid_known_issues):
            with caplog.at_level('INFO'):
                already_seen_dict, context_dict = capture_known_issues(mock_llm_service, issue_list, mock_config)
                
                # assertion
                assert len(already_seen_dict) == 2 
                assert "issue1" in already_seen_dict
                assert "issue3" in already_seen_dict
                assert "issue2" not in already_seen_dict
                
                assert len(context_dict) == 3
                assert "issue1" in context_dict
                assert "issue2" in context_dict
                assert "issue3" in context_dict
                
                assert "LLM found issue1 error trace inside known false positives list" in caplog.text
                assert "LLM found issue3 error trace inside known false positives list" in caplog.text
                assert "Known false positives: 2 / 3" in caplog.text
                assert mock_llm_service.filter_known_error.call_count == 3

    def test__capture_known_issues__file_error_returns_empty(self, caplog):
        # preparation
        mock_llm_service = Mock()
        mock_config = Mock()
        mock_config.KNOWN_FALSE_POSITIVE_FILE_PATH = "/nonexistent/path/file.txt"
        
        issue_list = [Issue("issue1")]
        
        # testing
        with patch('src.stage.filter_known_issues.read_known_errors_file', side_effect=FileNotFoundError("File not found")):
            with caplog.at_level('ERROR'):
                with pytest.raises(FileNotFoundError, match="File not found"):
                    capture_known_issues(mock_llm_service, issue_list, mock_config)

    def test__capture_known_issues__empty_file_returns_empty(self, caplog):
        # preparation
        mock_llm_service = Mock()
        mock_config = Mock()
        mock_config.KNOWN_FALSE_POSITIVE_FILE_PATH = "empty_file.txt"
        mock_config.SIMILARITY_ERROR_THRESHOLD = 3
        
        issue_list = [Issue("issue1")]
        
        mock_vector_db = Mock()
        mock_retriever = Mock()
        mock_retriever.invoke.return_value = []
        mock_vector_db.as_retriever.return_value = mock_retriever
        
        mock_vector_service = Mock()
        mock_vector_service.create_known_issues_vector_store.return_value = mock_vector_db
        mock_llm_service.vector_service = mock_vector_service
        mock_llm_service.embedding_llm = Mock()
        
        mock_response = Mock()
        mock_response.result = "NO"
        mock_response.equal_error_trace = ""
        mock_llm_service.filter_known_error.return_value = mock_response
        
        # testing
        with patch('src.stage.filter_known_issues.read_known_errors_file', return_value=[]):
            with caplog.at_level('INFO'):
                already_seen_dict, context_dict = capture_known_issues(mock_llm_service, issue_list, mock_config)
                
                # assertion
                assert already_seen_dict == {}
                assert len(context_dict) == 1
                assert "issue1" in context_dict
                assert "Known false positives: 0 / 1" in caplog.text