"""
Tests for investigate_issue function in LLMService.py
Covers core LLM analysis for vulnerability classification.
"""
import pytest
import logging
from unittest.mock import Mock, patch
from src.LLMService import LLMService
from src.dto.Issue import Issue
from src.dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from src.dto.ResponseStructures import JudgeLLMResponse, RecommendationsResponse, JustificationsSummary, EvaluationResponse
from src.common.config import Config


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

        test_issue = Issue("test-issue-1")
        test_issue.issue_type = "BUFFER_SIZE"
        test_issue.issue_cve = "CWE-474"
        test_issue.trace = "buffer overflow at line 121"

        mock_analysis_response = JudgeLLMResponse(
            investigation_result="TRUE POSITIVE",
            justifications=["Buffer overflow vulnerability detected"]
        )

        mock_recommendations_response = RecommendationsResponse(
            is_final="TRUE",
            justifications=["Analysis complete"],
            recommendations=["Fix buffer bounds"],
            instructions=[]
        )

        mock_summary_response = JustificationsSummary(
            short_justifications="Buffer overflow found"
        )

        # testing
        with patch.object(LLMService, '_investigate_issue_with_retry') as mock_investigate, \
             patch.object(LLMService, '_recommend') as mock_recommend, \
             patch.object(LLMService, '_summarize_justification') as mock_summarize:
            
            mock_prompt = Mock()
            mock_prompt.to_string.return_value = "test prompt"
            mock_investigate.return_value = (mock_prompt, mock_analysis_response)
            mock_recommend.return_value = mock_recommendations_response
            mock_summarize.return_value = mock_summary_response

            llm_service = LLMService(mock_config)
            analysis_result, critique_result = llm_service.investigate_issue("test context", test_issue)

            # assertion
            assert type(analysis_result).__name__ == "AnalysisResponse"
            assert analysis_result.investigation_result == "TRUE POSITIVE"
            assert analysis_result.is_final == "TRUE"
            assert "Buffer overflow vulnerability detected" in analysis_result.justifications
            assert "Fix buffer bounds" in analysis_result.recommendations
            assert analysis_result.short_justifications == "Buffer overflow found"
            mock_investigate.assert_called_once_with(context="test context", issue=test_issue)
            mock_recommend.assert_called_once()
            mock_summarize.assert_called_once()

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

        test_issue = Issue("test-issue-1")
        test_issue.issue_type = "BUFFER_SIZE"
        test_issue.issue_cve = "CWE-474"
        test_issue.trace = "buffer overflow at line 121"

        # testing
        with patch.object(LLMService, '_investigate_issue_with_retry') as mock_investigate:
            mock_investigate.side_effect = Exception("LLM service unavailable")
            
            llm_service = LLMService(mock_config)
            analysis_result, critique_result = llm_service.investigate_issue("test context", test_issue)

            # assertion
            assert type(analysis_result).__name__ == "AnalysisResponse"
            assert analysis_result.investigation_result == "NOT A FALSE POSITIVE"
            assert analysis_result.is_final == "TRUE"
            assert "Failed during analyze process" in analysis_result.evaluation
            assert "Failed during analyze process" in analysis_result.recommendations
            assert "Failed during analyze process" in analysis_result.short_justifications
            mock_investigate.assert_called_once_with(context="test context", issue=test_issue)

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

        test_issue = Issue("test-issue-2")
        test_issue.issue_type = "BUFFER_SIZE" 
        test_issue.issue_cve = "CWE-474"
        test_issue.trace = "false positive case"

        mock_analysis_response = JudgeLLMResponse(
            investigation_result=" FALSE POSITIVE",
            justifications=["Input validation prevents overflow"]
        )

        mock_recommendations_response = RecommendationsResponse(
            is_final="TRUE",
            justifications=["No action needed"],
            recommendations=["No fix required"],
            instructions=[]
        )

        mock_summary_response = JustificationsSummary(
            short_justifications="Safe input validation"
        )

        # testing
        with patch.object(LLMService, '_investigate_issue_with_retry') as mock_investigate, \
             patch.object(LLMService, '_recommend') as mock_recommend, \
             patch.object(LLMService, '_summarize_justification') as mock_summarize:
            
            mock_prompt = Mock()
            mock_prompt.to_string.return_value = "test prompt"
            mock_investigate.return_value = (mock_prompt, mock_analysis_response)
            mock_recommend.return_value = mock_recommendations_response
            mock_summarize.return_value = mock_summary_response

            llm_service = LLMService(mock_config)
            analysis_result, critique_result = llm_service.investigate_issue("test context", test_issue)

            # assertion
            assert type(analysis_result).__name__ == "AnalysisResponse"
            assert analysis_result.investigation_result == " FALSE POSITIVE"
            assert analysis_result.is_final == "TRUE"
            assert "Input validation prevents overflow" in analysis_result.justifications
            assert "No fix required" in analysis_result.recommendations
            assert analysis_result.short_justifications == "Safe input validation"