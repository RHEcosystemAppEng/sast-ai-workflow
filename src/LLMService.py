"""
Refactored LLMService that uses the new service architecture.

This class maintains the same interface as the original LLMService but uses
the new focused services internally. This allows for a smooth migration path.
"""

import logging
from typing import Tuple, List

from common.config import Config
from dto.Issue import Issue
from dto.ResponseStructures import FilterResponse, EvaluationResponse
from dto.LLMResponse import AnalysisResponse
from services.llm_service_manager import LLMServiceManager

logger = logging.getLogger(__name__)


class LLMService:
    """
    Refactored LLMService that maintains the same interface but uses
    the new service architecture internally.
    
    This class serves as a drop-in replacement for the original LLMService.
    """
    
    def __init__(self, config: Config):
        """
        Initialize the refactored LLMService.
        
        Args:
            config: Configuration object containing all necessary settings
        """
        self.config = config
        self.service_manager = LLMServiceManager(config)
        
        # Initialize failure counters for backward compatibility
        self.filter_retry_counter = 0
        self.judge_retry_counter = 0
        self.max_retry_limit = 3
    
    @property
    def main_llm(self):
        """Get main LLM client."""
        return self.service_manager.main_llm
    
    @property
    def embedding_llm(self):
        """Get embedding LLM client."""
        return self.service_manager.embedding_llm
    
    @property
    def critique_llm(self):
        """Get critique LLM client."""
        return self.service_manager.critique_llm
    
    @property
    def vector_db(self):
        """Get vector database."""
        return self.service_manager.vector_db
    
    @property
    def known_issues_vector_db(self):
        """Get known issues vector database."""
        return self.service_manager.known_issues_vector_db
    
    @property
    def similarity_error_threshold(self):
        """Get similarity error threshold."""
        return self.service_manager.similarity_error_threshold
    
    @property
    def run_with_critique(self):
        """Get run with critique flag."""
        return self.service_manager.run_with_critique
    
    def filter_known_error(self, database, issue: Issue) -> Tuple[FilterResponse, str]:
        """
        Check if an issue exactly matches a known false positive.
        
        Args:
            database: The vector database of known false positives.
            issue: The issue object with details like the error trace and issue ID.

        Returns:
            tuple: A tuple containing:
            - response (FilterResponse): A structured response with the analysis result.
            - examples_context_str (str): N most similar known issues of the same type of the query issue.
        """
        return self.service_manager.filter_known_error(database, issue)
    
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
        return self.service_manager.investigate_issue(context, issue)
    
    def create_vdb(self, text_data: List[str]):
        """Create vector database from text data."""
        return self.service_manager.create_vdb(text_data)
    
    def create_vdb_for_known_issues(self, text_data: List[str]):
        """Create vector database for known issues."""
        return self.service_manager.create_vdb_for_known_issues(text_data)
    
    # Additional methods for direct access to services (for advanced usage)
    
    def get_llm_client_factory(self):
        """Get LLM client factory for advanced usage."""
        return self.service_manager.llm_client_factory
    
    def get_vector_store_service(self):
        """Get vector store service for advanced usage."""
        return self.service_manager.vector_store_service
    
    def get_prompt_service(self):
        """Get prompt service for advanced usage."""
        return self.service_manager.prompt_service
    
    def get_structured_output_service(self):
        """Get structured output service for advanced usage."""
        return self.service_manager.structured_output_service
    
    def get_issue_analysis_service(self):
        """Get issue analysis service for advanced usage."""
        return self.service_manager.issue_analysis_service 