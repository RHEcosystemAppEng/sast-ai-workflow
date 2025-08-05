"""
Tests for get_source_code_blocks_from_error_trace function in handlers/c_repo_handler.py
Covers code extraction from repository using error trace information.
"""
import pytest
import os
import logging
from unittest.mock import Mock, patch
from src.handlers.c_repo_handler import CRepoHandler
from common.config import Config


class TestGetSourceCodeBlocksFromErrorTrace:

    def test_given_valid_error_trace_when_extracting_code_blocks_then_extracts_complete_c_functions(self):
        # preparation
        error_trace = """
        Error: BUFFER_SIZE (CWE-474):
        unzip60/envargs.c:121: overlapping_buffer: The source buffer "argstart + 1" potentially overlaps with the destination buffer "argstart", which results in undefined behavior for "strcpy".
        unzip60/envargs.c:121: remediation: Replace "strcpy(dest, src)" with "memmove(dest, src, strlen(src)+1)".
        #  119|               /* remove escape characters */
        #  120|               while ((argstart = MBSCHR(argstart, '\\\\')) != (char *)NULL) {
        #  121|->                 strcpy(argstart, argstart + 1);
        #  122|                   if (*argstart)
        #  123|                       ++argstart;
        # """

        mock_config = Mock(spec=Config)
        mock_config.REPO_REMOTE_URL = "https://github.com/madler/unzip"
        mock_config.DOWNLOAD_REPO = False
        mock_config.REPO_LOCAL_PATH = "/tmp/test_repo"
        mock_config.PROJECT_NAME = "unzip"
        mock_config.PROJECT_VERSION = "6.0-63.el10"
        mock_config.CONFIG_H_PATH = None
        mock_config.COMPILE_COMMANDS_JSON_PATH = None
        mock_config.LIBCLANG_PATH = "/usr/lib/libclang.so"

        expected_source_code = """
        119| /* remove escape characters */
        120| while ((argstart = MBSCHR(argstart, '\\\\')) != (char *)NULL) {
        121|     strcpy(argstart, argstart + 1);
        122|     if (*argstart)
        123|         ++argstart;
        124|     /* handle more escape sequences */
        125| }
        """

        # testing
        with patch('os.path.exists') as mock_exists, \
             patch('src.handlers.c_repo_handler.CRepoHandler.get_source_code_by_line_number') as mock_get_source, \
             patch('clang.cindex.Config.set_library_file'), \
             patch('clang.cindex.Index.create'):
            
            mock_exists.return_value = True
            mock_get_source.return_value = expected_source_code
            
            repo_handler = CRepoHandler(mock_config)
            result = repo_handler.get_source_code_blocks_from_error_trace(error_trace)
            
            # assertion
            assert isinstance(result, dict)
            assert "unzip60/envargs.c" in result
            assert result["unzip60/envargs.c"] == expected_source_code
            mock_get_source.assert_called_once_with("/tmp/test_repo/unzip60/envargs.c", 121)

    def test_given_missing_source_files_in_trace_when_extracting_code_blocks_then_processes_available_and_logs_warnings(self, caplog):
        # preparation
        caplog.set_level(logging.DEBUG)
        error_trace = """
        Error: BUFFER_SIZE (CWE-474):
        unzip60/envargs.c:121: overlapping_buffer: Buffer overflow detected.
        unzip60/missing.c:85: another_error: Missing file error.
        #  121|->                 strcpy(argstart, argstart + 1);
        #  85|->                  some_function();
        """

        mock_config = Mock(spec=Config)
        mock_config.REPO_REMOTE_URL = "https://github.com/madler/unzip"
        mock_config.DOWNLOAD_REPO = False
        mock_config.REPO_LOCAL_PATH = "/tmp/test_repo"
        mock_config.PROJECT_NAME = "unzip"
        mock_config.PROJECT_VERSION = "6.0-63.el10"
        mock_config.CONFIG_H_PATH = None
        mock_config.COMPILE_COMMANDS_JSON_PATH = None
        mock_config.LIBCLANG_PATH = "/usr/lib/libclang.so"

        expected_source_code = """
        119| /* remove escape characters */
        120| while ((argstart = MBSCHR(argstart, '\\\\')) != (char *)NULL) {
        121|     strcpy(argstart, argstart + 1);
        122| }
        """

        # testing
        with patch('os.path.exists') as mock_exists, \
             patch('src.handlers.c_repo_handler.CRepoHandler.get_source_code_by_line_number') as mock_get_source, \
             patch('clang.cindex.Config.set_library_file'), \
             patch('clang.cindex.Index.create'):
            
            def mock_file_exists(path):
                return "/tmp/test_repo/unzip60/envargs.c" in path
            
            mock_exists.side_effect = mock_file_exists
            mock_get_source.return_value = expected_source_code
            
            repo_handler = CRepoHandler(mock_config)
            result = repo_handler.get_source_code_blocks_from_error_trace(error_trace)
            
            # assertion
            assert isinstance(result, dict)
            assert "unzip60/envargs.c" in result
            assert "unzip60/missing.c" not in result
            assert result["unzip60/envargs.c"] == expected_source_code
            assert "Skipping missing file" in caplog.text
            assert "/tmp/test_repo/unzip60/missing.c" in caplog.text

    def test_given_empty_error_trace_when_extracting_code_blocks_then_returns_empty_dict_gracefully(self):
        # preparation
        empty_traces = ["", "   ", "\n\n"]
        
        mock_config = Mock(spec=Config)
        mock_config.REPO_REMOTE_URL = "https://github.com/madler/unzip"
        mock_config.DOWNLOAD_REPO = False
        mock_config.REPO_LOCAL_PATH = "/tmp/test_repo"
        mock_config.PROJECT_NAME = "unzip"
        mock_config.PROJECT_VERSION = "6.0-63.el10"
        mock_config.CONFIG_H_PATH = None
        mock_config.COMPILE_COMMANDS_JSON_PATH = None
        mock_config.LIBCLANG_PATH = "/usr/lib/libclang.so"

        # testing
        with patch('clang.cindex.Config.set_library_file'), \
             patch('clang.cindex.Index.create'):
            
            repo_handler = CRepoHandler(mock_config)
            
            for empty_trace in empty_traces:
                result = repo_handler.get_source_code_blocks_from_error_trace(empty_trace)
                
                # assertion
                assert result == {}

    def test_given_invalid_file_paths_in_trace_when_extracting_code_blocks_then_handles_path_errors(self, caplog):
        # preparation
        caplog.set_level(logging.WARNING, logger="src.handlers.c_repo_handler")
        error_trace = """
        Error: BUFFER_SIZE (CWE-474):
        unzip60/envargs.c:abc: invalid_line_number: Non-numeric line number.
        unzip60/valid.c:121: normal_error: Valid entry.
        """

        mock_config = Mock(spec=Config)
        mock_config.REPO_REMOTE_URL = "https://github.com/madler/unzip"
        mock_config.DOWNLOAD_REPO = False
        mock_config.REPO_LOCAL_PATH = "/tmp/test_repo"
        mock_config.PROJECT_NAME = "unzip"
        mock_config.PROJECT_VERSION = "6.0-63.el10"
        mock_config.CONFIG_H_PATH = None
        mock_config.COMPILE_COMMANDS_JSON_PATH = None
        mock_config.LIBCLANG_PATH = "/usr/lib/libclang.so"

        expected_source_code = """
        121| valid function code
        """

        # testing
        with patch('os.path.exists') as mock_exists, \
             patch('src.handlers.c_repo_handler.CRepoHandler.get_source_code_by_line_number') as mock_get_source, \
             patch('clang.cindex.Config.set_library_file'), \
             patch('clang.cindex.Index.create'):
            
            mock_exists.return_value = True
            mock_get_source.return_value = expected_source_code
            
            repo_handler = CRepoHandler(mock_config)
            result = repo_handler.get_source_code_blocks_from_error_trace(error_trace)
            
            # assertion
            assert isinstance(result, dict)
            assert "unzip60/valid.c" in result
            assert "unzip60/envargs.c" not in result
            assert result["unzip60/valid.c"] == expected_source_code

    def test_given_clang_parsing_failure_when_extracting_code_blocks_then_handles_ast_errors(self, caplog):
        # preparation
        caplog.set_level(logging.WARNING)
        error_trace = """
        Error: BUFFER_SIZE (CWE-474):
        unzip60/envargs.c:121: overlapping_buffer: Clang will fail parsing this.
        """

        mock_config = Mock(spec=Config)
        mock_config.REPO_REMOTE_URL = "https://github.com/madler/unzip"
        mock_config.DOWNLOAD_REPO = False
        mock_config.REPO_LOCAL_PATH = "/tmp/test_repo"
        mock_config.PROJECT_NAME = "unzip"
        mock_config.PROJECT_VERSION = "6.0-63.el10"
        mock_config.CONFIG_H_PATH = None
        mock_config.COMPILE_COMMANDS_JSON_PATH = None
        mock_config.LIBCLANG_PATH = "/usr/lib/libclang.so"

        # testing
        with patch('os.path.exists') as mock_exists, \
             patch('src.handlers.c_repo_handler.CRepoHandler.get_source_code_by_line_number') as mock_get_source, \
             patch('clang.cindex.Config.set_library_file'), \
             patch('clang.cindex.Index.create'):
            
            mock_exists.return_value = True
            mock_get_source.side_effect = Exception("Clang parsing failed")
            
            repo_handler = CRepoHandler(mock_config)
            result = repo_handler.get_source_code_blocks_from_error_trace(error_trace)
            
            # assertion
            assert isinstance(result, dict)
            assert result == {}
            assert "Failed to extract source code from /tmp/test_repo/unzip60/envargs.c:121" in caplog.text
            assert "Clang parsing failed" in caplog.text

    def test_given_missing_repository_when_extracting_code_blocks_then_handles_gracefully(self, caplog):
        # preparation
        caplog.set_level(logging.DEBUG)
        error_trace = """
        Error: BUFFER_SIZE (CWE-474):
        unzip60/envargs.c:121: overlapping_buffer: Repository doesn't exist.
        """

        mock_config = Mock(spec=Config)
        mock_config.REPO_REMOTE_URL = "https://github.com/madler/unzip"
        mock_config.DOWNLOAD_REPO = False
        mock_config.REPO_LOCAL_PATH = "/nonexistent/repo"
        mock_config.PROJECT_NAME = "unzip"
        mock_config.PROJECT_VERSION = "6.0-63.el10"
        mock_config.CONFIG_H_PATH = None
        mock_config.COMPILE_COMMANDS_JSON_PATH = None
        mock_config.LIBCLANG_PATH = "/usr/lib/libclang.so"

        # testing
        with patch('os.path.exists') as mock_exists, \
             patch('clang.cindex.Config.set_library_file'), \
             patch('clang.cindex.Index.create'):
            
            mock_exists.return_value = False
            
            repo_handler = CRepoHandler(mock_config)
            result = repo_handler.get_source_code_blocks_from_error_trace(error_trace)
            
            # assertion
            assert isinstance(result, dict)
            assert result == {}
            assert "Skipping missing file" in caplog.text
            assert "/nonexistent/repo/unzip60/envargs.c" in caplog.text

    def test_given_permission_denied_files_when_extracting_code_blocks_then_handles_access_errors(self, caplog):
        # preparation
        caplog.set_level(logging.WARNING)
        error_trace = """
        Error: BUFFER_SIZE (CWE-474):
        unzip60/protected.c:121: overlapping_buffer: Permission denied file.
        """

        mock_config = Mock(spec=Config)
        mock_config.REPO_REMOTE_URL = "https://github.com/madler/unzip"
        mock_config.DOWNLOAD_REPO = False
        mock_config.REPO_LOCAL_PATH = "/tmp/test_repo"
        mock_config.PROJECT_NAME = "unzip"
        mock_config.PROJECT_VERSION = "6.0-63.el10"
        mock_config.CONFIG_H_PATH = None
        mock_config.COMPILE_COMMANDS_JSON_PATH = None
        mock_config.LIBCLANG_PATH = "/usr/lib/libclang.so"

        # testing
        with patch('os.path.exists') as mock_exists, \
             patch('src.handlers.c_repo_handler.CRepoHandler.get_source_code_by_line_number') as mock_get_source, \
             patch('clang.cindex.Config.set_library_file'), \
             patch('clang.cindex.Index.create'):
            
            mock_exists.return_value = True
            mock_get_source.side_effect = PermissionError("Permission denied")
            
            repo_handler = CRepoHandler(mock_config)
            result = repo_handler.get_source_code_blocks_from_error_trace(error_trace)
            
            # assertion
            assert isinstance(result, dict)
            assert result == {}
            assert "Failed to extract source code from /tmp/test_repo/unzip60/protected.c:121" in caplog.text
            assert "Permission denied" in caplog.text