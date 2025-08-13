"""
Tests for investigate_issue function in LLMService.py
Covers core LLM analysis for vulnerability classification.
"""
import pytest
import logging
from unittest.mock import Mock, patch
from src.LLMService import LLMService
from src.services.issue_analysis_service import IssueAnalysisService
from src.dto.Issue import Issue
from src.dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from src.common.config import Config


# Constants for test responses
EXPECTED_TRUE_POSITIVE_RESULT = AnalysisResponse(
    investigation_result="TRUE POSITIVE",
    is_final="TRUE",
    justifications=["Buffer overflow vulnerability detected"],
    evaluation=["Analysis complete"],
    recommendations=["Fix buffer bounds"],
    instructions=[],
    prompt="test prompt",
    short_justifications="Buffer overflow found"
)

EXPECTED_FALSE_POSITIVE_RESULT = AnalysisResponse(
    investigation_result=" FALSE POSITIVE",
    is_final="TRUE", 
    justifications=["Input validation prevents overflow"],
    evaluation=["No action needed"],
    recommendations=["No fix required"],
    instructions=[],
    prompt="test prompt",
    short_justifications="Safe input validation"
)


class TestInvestigateIssue:

    def test__investigate_issue__valid_issue_returns_analysis_response(self):
        # preparation
        mock_config = Mock(spec=Config)
        mock_config.LLM_URL = "http://test-llm"
        mock_config.LLM_API_KEY = "test-key"
        mock_config.LLM_MODEL_NAME = "test-model"
        mock_config.EMBEDDINGS_LLM_URL = "http://test-embed"
        mock_config.EMBEDDINGS_LLM_API_KEY = "test-embed-key"
        mock_config.EMBEDDINGS_LLM_MODEL_NAME = "test-embed-model"
        mock_config.SIMILARITY_ERROR_THRESHOLD = 3
        mock_config.RUN_WITH_CRITIQUE = False
        mock_config.CRITIQUE_LLM_URL = None
        mock_config.CRITIQUE_LLM_API_KEY = None
        mock_config.CRITIQUE_LLM_MODEL_NAME = None
        mock_config.ANALYSIS_SYSTEM_PROMPT = "Test system prompt"
        mock_config.ANALYSIS_HUMAN_PROMPT = "Test human prompt"
        mock_config.FILTER_SYSTEM_PROMPT = "Test filter system"
        mock_config.FILTER_HUMAN_PROMPT = "Test filter human"
        mock_config.RECOMMENDATIONS_PROMPT = "Test recommendations prompt"
        mock_config.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT = "Test summary system"
        mock_config.JUSTIFICATION_SUMMARY_HUMAN_PROMPT = "Test summary human"
        mock_config.EVALUATION_PROMPT = "Test evaluation prompt"

        test_issue = Issue(id="test-issue-1")
        test_issue.issue_type = "BUFFER_SIZE"
        test_issue.issue_cve = "CWE-474"
        test_issue.trace = "buffer overflow at line 121"

        # testing        
        llm_service = LLMService(mock_config)
        
        with patch.object(llm_service.issue_analysis_service, 'analyze_issue') as mock_analyze:
            mock_analyze.return_value = (EXPECTED_TRUE_POSITIVE_RESULT, "")

            analysis_result, _ = llm_service.investigate_issue("test context", test_issue)

            # assertion
            assert type(analysis_result).__name__ == "AnalysisResponse"
            assert analysis_result.investigation_result == "TRUE POSITIVE"
            assert analysis_result.is_final == "TRUE"
            assert "Buffer overflow vulnerability detected" in analysis_result.justifications
            assert "Fix buffer bounds" in analysis_result.recommendations
            assert analysis_result.short_justifications == "Buffer overflow found"
            mock_analyze.assert_called_once_with(issue=test_issue, context="test context", main_llm=llm_service.main_llm, critique_llm=None)

    def test__investigate_issue__exception_returns_fallback_response(self):
        # preparation
        mock_config = Mock(spec=Config)
        mock_config.LLM_URL = "http://test-llm"
        mock_config.LLM_API_KEY = "test-key"
        mock_config.LLM_MODEL_NAME = "test-model"
        mock_config.EMBEDDINGS_LLM_URL = "http://test-embed"
        mock_config.EMBEDDINGS_LLM_API_KEY = "test-embed-key"
        mock_config.EMBEDDINGS_LLM_MODEL_NAME = "test-embed-model"
        mock_config.SIMILARITY_ERROR_THRESHOLD = 3
        mock_config.RUN_WITH_CRITIQUE = False
        mock_config.CRITIQUE_LLM_URL = None
        mock_config.CRITIQUE_LLM_API_KEY = None
        mock_config.CRITIQUE_LLM_MODEL_NAME = None
        mock_config.ANALYSIS_SYSTEM_PROMPT = "Test system prompt"
        mock_config.ANALYSIS_HUMAN_PROMPT = "Test human prompt"
        mock_config.FILTER_SYSTEM_PROMPT = "Test filter system"
        mock_config.FILTER_HUMAN_PROMPT = "Test filter human"
        mock_config.RECOMMENDATIONS_PROMPT = "Test recommendations prompt"
        mock_config.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT = "Test summary system"
        mock_config.JUSTIFICATION_SUMMARY_HUMAN_PROMPT = "Test summary human"
        mock_config.EVALUATION_PROMPT = "Test evaluation prompt"

        test_issue = Issue(id="test-issue-1")
        test_issue.issue_type = "BUFFER_SIZE"
        test_issue.issue_cve = "CWE-474"
        test_issue.trace = "buffer overflow at line 121"

        # testing
        llm_service = LLMService(mock_config)
        
        with patch.object(llm_service.issue_analysis_service, 'analyze_issue') as mock_analyze:
            mock_analyze.side_effect = Exception("LLM service unavailable")
            
            # Exception should propagate since LLMService doesn't handle exceptions
            with pytest.raises(Exception, match="LLM service unavailable"):
                llm_service.investigate_issue("test context", test_issue)

            mock_analyze.assert_called_once_with(issue=test_issue, context="test context", main_llm=llm_service.main_llm, critique_llm=None)

    def test__investigate_issue__false_positive_returns_fp_response(self):
        # preparation
        mock_config = Mock(spec=Config)
        mock_config.LLM_URL = "http://test-llm"
        mock_config.LLM_API_KEY = "test-key"
        mock_config.LLM_MODEL_NAME = "test-model"
        mock_config.EMBEDDINGS_LLM_URL = "http://test-embed"
        mock_config.EMBEDDINGS_LLM_API_KEY = "test-embed-key"
        mock_config.EMBEDDINGS_LLM_MODEL_NAME = "test-embed-model"
        mock_config.SIMILARITY_ERROR_THRESHOLD = 3
        mock_config.RUN_WITH_CRITIQUE = False
        mock_config.CRITIQUE_LLM_URL = None
        mock_config.CRITIQUE_LLM_API_KEY = None
        mock_config.CRITIQUE_LLM_MODEL_NAME = None
        mock_config.ANALYSIS_SYSTEM_PROMPT = "Test system prompt"
        mock_config.ANALYSIS_HUMAN_PROMPT = "Test human prompt"
        mock_config.FILTER_SYSTEM_PROMPT = "Test filter system"
        mock_config.FILTER_HUMAN_PROMPT = "Test filter human"
        mock_config.RECOMMENDATIONS_PROMPT = "Test recommendations prompt"
        mock_config.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT = "Test summary system"
        mock_config.JUSTIFICATION_SUMMARY_HUMAN_PROMPT = "Test summary human"
        mock_config.EVALUATION_PROMPT = "Test evaluation prompt"

        test_issue = Issue(id="test-issue-2")
        test_issue.issue_type = "BUFFER_SIZE" 
        test_issue.issue_cve = "CWE-474"
        test_issue.trace = "false positive case"

        # testing        
        llm_service = LLMService(mock_config)
        
        with patch.object(llm_service.issue_analysis_service, 'analyze_issue') as mock_analyze:
            mock_analyze.return_value = (EXPECTED_FALSE_POSITIVE_RESULT, "")

            analysis_result, _ = llm_service.investigate_issue("test context", test_issue)

            # assertion
            assert type(analysis_result).__name__ == "AnalysisResponse"
            assert analysis_result.investigation_result == " FALSE POSITIVE"
            assert analysis_result.is_final == "TRUE"
            assert "Input validation prevents overflow" in analysis_result.justifications
            assert "No fix required" in analysis_result.recommendations
            assert analysis_result.short_justifications == "Safe input validation"
            mock_analyze.assert_called_once_with(issue=test_issue, context="test context", main_llm=llm_service.main_llm, critique_llm=None)