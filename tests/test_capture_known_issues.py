"""
Tests for capture_known_issues function in stage/filter_known_issues.py
Covers LLM vector search for identifying known false positives.
"""
import pytest
from unittest.mock import Mock, patch
from src.stage.filter_known_issues import capture_known_issues


class TestCaptureKnownIssues:
    def test_given_vector_db_creation_failure_when_capturing_known_issues_with_mock_llm_then_handles_error_properly(self, caplog):
        # preparation
        mock_llm_service = Mock()
        mock_config = Mock()
        mock_config.KNOWN_FALSE_POSITIVE_FILE_PATH = "valid_file.txt"
        
        valid_known_issues = ["Error: USE_AFTER_FREE\nSome error trace\nReason: duplicate variable"]
        mock_llm_service.create_vdb_for_known_issues.side_effect = Exception("FAISS embedding failed")
        
        # testing
        with patch('src.stage.filter_known_issues.read_known_errors_file', return_value=valid_known_issues):
            with caplog.at_level('ERROR'):
                already_seen_dict, context_dict = capture_known_issues(mock_llm_service, [], mock_config)
                
                # assertion
                assert already_seen_dict == {}
                assert context_dict == {}
                assert "Failed to create vector database: FAISS embedding failed" in caplog.text
                mock_llm_service.create_vdb_for_known_issues.assert_called_once_with(valid_known_issues)

    def test_given_issue_processing_failure_when_capturing_known_issues_then_handles_gracefully(self, caplog):
        # preparation
        from src.dto.Issue import Issue
        
        mock_llm_service = Mock()
        mock_config = Mock()
        mock_config.KNOWN_FALSE_POSITIVE_FILE_PATH = "valid_file.txt"
        
        valid_known_issues = ["Error: USE_AFTER_FREE\nSome error trace\nReason: duplicate variable"]
        mock_vector_db = Mock()
        mock_llm_service.create_vdb_for_known_issues.return_value = mock_vector_db
        
        issue1 = Issue("issue1")
        issue2 = Issue("issue2") 
        issue3 = Issue("issue3")
        issue_list = [issue1, issue2, issue3]
        
        def mock_filter_error(db, issue):
            if issue.id == "issue2":
                raise Exception("Processing failed for issue2")
            
            mock_response = Mock()
            mock_response.result = "no"
            return mock_response, []
        
        mock_llm_service.filter_known_error.side_effect = mock_filter_error
        
        # testing
        with patch('src.stage.filter_known_issues.read_known_errors_file', return_value=valid_known_issues):
            with caplog.at_level('ERROR'):
                already_seen_dict, context_dict = capture_known_issues(mock_llm_service, issue_list, mock_config)
                
                # assertion
                assert len(context_dict) == 2  # issue1 and issue3 processed
                assert "issue1" in context_dict
                assert "issue3" in context_dict
                assert "issue2" not in context_dict
                assert "Failed to process issue issue2: Processing failed for issue2" in caplog.text
                assert mock_llm_service.filter_known_error.call_count == 3

    def test_given_valid_data_when_capturing_known_issues_then_detects_known_false_positives(self, caplog):
        # preparation
        from src.dto.Issue import Issue
        
        mock_llm_service = Mock()
        mock_config = Mock()
        mock_config.KNOWN_FALSE_POSITIVE_FILE_PATH = "valid_file.txt"
        
        valid_known_issues = ["Error: USE_AFTER_FREE\nSome error trace\nReason: duplicate variable"]
        mock_vector_db = Mock()
        mock_llm_service.create_vdb_for_known_issues.return_value = mock_vector_db
        
        issue1 = Issue("issue1")
        issue2 = Issue("issue2") 
        issue3 = Issue("issue3")
        issue_list = [issue1, issue2, issue3]
        
        def mock_filter_error(db, issue):
            mock_response = Mock()
            mock_context = [{"false_positive_error_trace": "trace data", "reason_marked_false_positive": "reason data"}]
            
            if issue.id in ["issue1", "issue3"]:
                mock_response.result = "YES"
            else:
                mock_response.result = "NO"
                
            return mock_response, mock_context
        
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
                
                # Verify specific log messages
                assert "LLM found issue1 error trace inside known false positives list" in caplog.text
                assert "LLM found issue3 error trace inside known false positives list" in caplog.text
                assert "Known false positives: 2 / 3" in caplog.text
                
                assert mock_llm_service.filter_known_error.call_count == 3

    def test_given_file_read_error_when_capturing_known_issues_then_handles_gracefully(self, caplog):
        # preparation
        from src.dto.Issue import Issue
        
        mock_llm_service = Mock()
        mock_config = Mock()
        mock_config.KNOWN_FALSE_POSITIVE_FILE_PATH = "/nonexistent/path/file.txt"
        
        issue_list = [Issue("issue1")]
        
        # testing
        with patch('src.stage.filter_known_issues.read_known_errors_file', side_effect=FileNotFoundError("File not found")):
            with caplog.at_level('ERROR'):
                already_seen_dict, context_dict = capture_known_issues(mock_llm_service, issue_list, mock_config)
                
                # assertion
                assert already_seen_dict == {}
                assert context_dict == {}
                assert "Failed to read known false positives file: File not found" in caplog.text
                mock_llm_service.create_vdb_for_known_issues.assert_not_called()
                mock_llm_service.filter_known_error.assert_not_called()

    def test_given_empty_known_issues_file_when_capturing_known_issues_then_handles_gracefully(self, caplog):
        # preparation
        mock_llm_service = Mock()
        mock_config = Mock()
        mock_config.KNOWN_FALSE_POSITIVE_FILE_PATH = "empty_file.txt"
        
        empty_issue_list = []
        
        # testing
        with patch('src.stage.filter_known_issues.read_known_errors_file', return_value=[]):
            with caplog.at_level('WARNING'):
                already_seen_dict, context_dict = capture_known_issues(mock_llm_service, empty_issue_list, mock_config)
                
                # assertion
                assert already_seen_dict == {}
                assert context_dict == {}
                assert "No known false positives found" in caplog.text
                mock_llm_service.create_vdb_for_known_issues.assert_not_called()