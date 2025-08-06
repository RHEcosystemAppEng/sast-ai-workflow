import logging
from typing import Tuple

from common.config import Config
from dto.Issue import Issue
from dto.ResponseStructures import FilterResponse, EvaluationResponse
from dto.LLMResponse import AnalysisResponse
from handlers.llm_client_factory import LLMClientFactory
from services.vector_store_service import VectorStoreService
from services.issue_analysis_service import IssueAnalysisService

logger = logging.getLogger(__name__)


class LLMService:
    """
    Refactored LLMService that acts as a facade for the focused services.
    Maintains the same public interface while using focused services internally.
    """

    def __init__(self, config: Config):
        self.config = config
        
        # Initialize focused services
        self.llm_factory = LLMClientFactory()
        self.vector_service = VectorStoreService()
        self.issue_analysis_service = IssueAnalysisService(
            config=config,
            vector_service=self.vector_service
        )
        
        # Lazy-loaded LLM clients
        self._main_llm = None
        self._embedding_llm = None
        self._critique_llm = None
        


    @property
    def main_llm(self):
        """Lazy-loaded main LLM client"""
        if self._main_llm is None:
            self._main_llm = self.llm_factory.create_main_llm(self.config)
        return self._main_llm

    @property
    def embedding_llm(self):
        """Lazy-loaded embedding LLM client"""
        if self._embedding_llm is None:
            self._embedding_llm = self.llm_factory.create_embedding_llm(self.config)
        return self._embedding_llm

    @property
    def critique_llm(self):
        """Lazy-loaded critique LLM client"""
        if self._critique_llm is None:
            self._critique_llm = self.llm_factory.create_critique_llm(self.config)
        return self._critique_llm

    def filter_known_error(self, issue: Issue, similar_findings_context: str) -> FilterResponse:
        """
        Check if an issue exactly matches a known false positive.
        
        Args:
            issue: The issue object with details like the error trace and issue ID.
            similar_findings_context: The context containing similar findings.

        Returns:
            response (FilterResponse): A structured response with the analysis result.
        """
        return self.issue_analysis_service.filter_known_issues_from_context(issue, similar_findings_context, self.main_llm)

    def investigate_issue(self, context: str, issue: Issue) -> Tuple[AnalysisResponse, EvaluationResponse]:
        """
        Analyze an issue to determine if it is a false positive or not.

        Args:
            context: The context to assist in the analysis.
            issue: The issue object with details like the error trace and issue ID.

        Returns:
            tuple: A tuple containing:
                - llm_analysis_response (AnalysisResponse): A structured response with the analysis result.
                - critique_response (EvaluationResponse): The response of the critique model, if applicable.
        """
        return self.issue_analysis_service.analyze_issue(
            issue=issue,
            context=context,
            main_llm=self.main_llm,
            critique_llm=self.critique_llm if self.config.RUN_WITH_CRITIQUE else None
        )


