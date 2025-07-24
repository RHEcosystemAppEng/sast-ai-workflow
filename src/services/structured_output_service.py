import logging
from typing import Any, Type
from langchain_core.language_models.base import BaseLanguageModel
from tenacity import retry, stop_after_attempt, wait_fixed, retry_if_exception_type

from Utils.llm_utils import robust_structured_output

logger = logging.getLogger(__name__)


class StructuredOutputService:
    """Service for handling structured output parsing with retry logic."""
    
    def __init__(self, max_retries: int = 3):
        self.max_retries = max_retries
    
    @retry(
        stop=stop_after_attempt(2),
        wait=wait_fixed(10),
        retry=retry_if_exception_type(Exception)
    )
    def parse_with_retry(
        self, 
        llm: BaseLanguageModel, 
        schema: Type[Any], 
        input_data: Any, 
        prompt_chain: Any
    ) -> Any:
        """
        Parse LLM output to structured format with retry logic.
        
        Args:
            llm: The language model to use
            schema: The Pydantic schema to parse to
            input_data: The input data for the LLM
            prompt_chain: The prompt chain to use
            
        Returns:
            Parsed structured output
            
        Raises:
            Exception: If parsing fails after all retries
        """
        try:
            return robust_structured_output(
                llm=llm,
                schema=schema,
                input=input_data,
                prompt_chain=prompt_chain,
                max_retries=self.max_retries
            )
        except Exception as e:
            logger.error(f"Failed to parse structured output after retries: {e}")
            raise e
    
    def parse_without_retry(
        self,
        llm: BaseLanguageModel,
        schema: Type[Any],
        input_data: Any,
        prompt_chain: Any
    ) -> Any:
        """
        Parse LLM output to structured format without retry logic.
        
        Args:
            llm: The language model to use
            schema: The Pydantic schema to parse to
            input_data: The input data for the LLM
            prompt_chain: The prompt chain to use
            
        Returns:
            Parsed structured output
            
        Raises:
            Exception: If parsing fails
        """
        return robust_structured_output(
            llm=llm,
            schema=schema,
            input=input_data,
            prompt_chain=prompt_chain,
            max_retries=self.max_retries
        ) 