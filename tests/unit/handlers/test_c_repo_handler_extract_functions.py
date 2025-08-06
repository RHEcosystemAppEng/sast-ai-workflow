"""
Tests for extract_missing_functions_or_macros function in handlers/c_repo_handler.py
Covers secondary source code retrieval based on LLM requests.
"""
import pytest
from unittest.mock import Mock, patch
from src.handlers.c_repo_handler import CRepoHandler


class TestExtractMissingFunctionsOrMacros:

    def test__extract_missing_functions_or_macros__invalid_syntax_handles_errors(self):
        """Handles invalid function syntax in LLM requests."""
        # preparation
        with patch.object(CRepoHandler, '__init__', return_value=None):
            repo_handler = CRepoHandler(Mock())
            repo_handler.all_found_symbols = set()
            
            instruction = Mock()
            instruction.referring_source_code_path = None
            instruction.expression_name = "test_function"
            
            # testing
            with patch('src.handlers.c_repo_handler.logger') as mock_logger:
                result = repo_handler.extract_missing_functions_or_macros([instruction])
                
                # assertion
                assert result == ""
                assert mock_logger.warning.called

    def test__extract_missing_functions_or_macros__empty_instructions_handles_gracefully(self):
        """Handles empty or malformed LLM instructions."""
        # preparation
        with patch.object(CRepoHandler, '__init__', return_value=None):
            repo_handler = CRepoHandler(Mock())
            
            # testing
            result = repo_handler.extract_missing_functions_or_macros(None)
            
            # assertion
            assert result == ""
            
            result = repo_handler.extract_missing_functions_or_macros([])
            assert result == ""

    def test__extract_missing_functions_or_macros__valid_instructions_processes_successfully(self):
        """Successfully processes valid instructions and extracts code."""
        # preparation
        with patch.object(CRepoHandler, '__init__', return_value=None):
            repo_handler = CRepoHandler(Mock())
            repo_handler.all_found_symbols = set()
            repo_handler.repo_local_path = "/test/repo"
            repo_handler._report_file_prefix = "test-1.0/"
            
            instruction = Mock()
            instruction.referring_source_code_path = "/test/repotest-1.0/src/main.c:10"
            instruction.expression_name = "missing_function"
            
            # testing
            with patch.object(repo_handler, 'extract_definition_from_source_code') as mock_extract:
                mock_extract.return_value = (
                    {"missing_function"}, 
                    {"src/main.c": {"missing_function": "int missing_function() { return 0; }"}}
                )
                
                result = repo_handler.extract_missing_functions_or_macros([instruction])
                
                # assertion
                assert "int missing_function() { return 0; }" in result
                assert "code of src/main.c file:" in result
                mock_extract.assert_called_once()
                # Verify the correct path was processed
                call_args = mock_extract.call_args[0]
                assert call_args[0] == {"missing_function"}
                assert call_args[1] == "src/main.c"
