from typing import Protocol, List, Tuple, Any
from langchain_core.language_models.base import BaseLanguageModel
from langchain_openai import OpenAIEmbeddings
from langchain_community.vectorstores import FAISS
from dto.Issue import Issue
from dto.ResponseStructures import FilterResponse, JudgeLLMResponse, JustificationsSummary, RecommendationsResponse, EvaluationResponse
from dto.LLMResponse import AnalysisResponse
from common.config import Config


class LLMClientFactoryProtocol(Protocol):
    """Protocol for LLM client factory."""
    
    def create_main_llm(self, config: Config) -> BaseLanguageModel:
        """Create main LLM client."""
        ...
    
    def create_embedding_llm(self, config: Config) -> OpenAIEmbeddings:
        """Create embedding LLM client."""
        ...
    
    def create_critique_llm(self, config: Config) -> BaseLanguageModel:
        """Create critique LLM client."""
        ...


class VectorStoreServiceProtocol(Protocol):
    """Protocol for vector store operations."""
    
    def create_vector_store(self, text_data: List[str]) -> FAISS:
        """Create vector store from text data."""
        ...
    
    def create_known_issues_vector_store(self, known_issues_data: List[str]) -> FAISS:
        """Create vector store for known issues."""
        ...


class PromptServiceProtocol(Protocol):
    """Protocol for prompt management."""
    
    def get_analysis_prompt(self) -> str:
        """Get analysis prompt template."""
        ...
    
    def get_filter_prompt(self) -> str:
        """Get filter prompt template."""
        ...


class StructuredOutputServiceProtocol(Protocol):
    """Protocol for structured output parsing."""
    
    def parse_with_retry(self, llm: BaseLanguageModel, schema: Any, input_data: Any, prompt_chain: Any, max_retries: int) -> Any:
        """Parse LLM output to structured format with retry logic."""
        ...


class IssueAnalysisServiceProtocol(Protocol):
    """Protocol for issue analysis workflows."""
    
    def filter_known_error(self, issue: Issue) -> Tuple[FilterResponse, str]:
        """Filter known errors."""
        ...
    
    def investigate_issue(self, context: str, issue: Issue) -> Tuple[AnalysisResponse, EvaluationResponse]:
        """Investigate an issue."""
        ... 