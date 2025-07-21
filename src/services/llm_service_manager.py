import logging
from typing import Tuple, List

from common.config import Config
from dto.Issue import Issue
from dto.ResponseStructures import FilterResponse, EvaluationResponse
from dto.LLMResponse import AnalysisResponse

from .llm_client_factory import LLMClientFactory
from .vector_store_service import VectorStoreService
from .prompt_service import PromptService
from .structured_output_service import StructuredOutputService
from .issue_analysis_service import IssueAnalysisService

logger = logging.getLogger(__name__)


class LLMServiceManager:
    """
    Facade service that coordinates all the refactored services.
    Maintains compatibility with the existing LLMService interface.
    """
    
    def __init__(self, config: Config):
        self.config = config
        
        # Initialize all services
        self.llm_client_factory = LLMClientFactory()
        self.prompt_service = PromptService(config)
        self.structured_output_service = StructuredOutputService(max_retries=3)
        
        # Initialize embedding LLM and vector store service
        self._embedding_llm = self.llm_client_factory.create_embedding_llm(config)
        self.vector_store_service = VectorStoreService(
            embedding_llm=self._embedding_llm,
            embedding_model_name=config.EMBEDDINGS_LLM_MODEL_NAME
        )
        
        # Store vector databases (will be set externally)
        self.vector_db = None
        self.known_issues_vector_db = None
        
        # Configuration parameters
        self.similarity_error_threshold = config.SIMILARITY_ERROR_THRESHOLD
        self.run_with_critique = config.RUN_WITH_CRITIQUE
        
        # Initialize issue analysis service (will be done after vector DB setup)
        self._issue_analysis_service = None
    
    @property
    def issue_analysis_service(self) -> IssueAnalysisService:
        """Lazy initialization of issue analysis service."""
        if self._issue_analysis_service is None:
            self._issue_analysis_service = IssueAnalysisService(
                llm_client_factory=self.llm_client_factory,
                vector_store_service=self.vector_store_service,
                prompt_service=self.prompt_service,
                structured_output_service=self.structured_output_service,
                known_issues_vector_db=self.known_issues_vector_db,
                similarity_error_threshold=self.similarity_error_threshold,
                run_with_critique=self.run_with_critique,
                config=self.config
            )
        return self._issue_analysis_service
    
    # Maintain compatibility with original LLMService interface
    
    def filter_known_error(self, database, issue: Issue) -> Tuple[FilterResponse, str]:
        """
        Check if an issue exactly matches a known false positive.
        
        Args:
            database: The vector database of known false positives (maintained for compatibility).
            issue: The issue object with details like the error trace and issue ID.

        Returns:
            tuple: A tuple containing:
            - response (FilterResponse): A structured response with the analysis result.
            - examples_context_str (str): N most similar known issues of the same type.
        """
        # Use the database parameter if provided, otherwise use the stored one
        if database is not None:
            self.known_issues_vector_db = database
            # Reset issue analysis service to use new database
            self._issue_analysis_service = None
        
        return self.issue_analysis_service.filter_known_error(issue)
    
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
        return self.issue_analysis_service.investigate_issue(context, issue)
    
    def create_vdb(self, text_data: List[str]):
        """Create vector database from text data."""
        self.vector_db = self.vector_store_service.create_vector_store(text_data)
        return self.vector_db
    
    def create_vdb_for_known_issues(self, text_data: List[str]):
        """Create vector database for known issues."""
        self.known_issues_vector_db = self.vector_store_service.create_known_issues_vector_store(text_data)
        # Reset issue analysis service to use new database
        self._issue_analysis_service = None
        return self.known_issues_vector_db
    
    # Properties for backward compatibility
    
    @property
    def main_llm(self):
        """Get main LLM client."""
        return self.llm_client_factory.create_main_llm(self.config)
    
    @property
    def embedding_llm(self):
        """Get embedding LLM client."""
        return self._embedding_llm
    
    @property
    def critique_llm(self):
        """Get critique LLM client."""
        return self.llm_client_factory.create_critique_llm(self.config) 