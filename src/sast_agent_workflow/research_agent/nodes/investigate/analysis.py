"""Analysis node - LLM-based verdict decision."""

import logging

from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.prompts import ChatPromptTemplate

from common.config import Config
from Utils.llm_utils import robust_structured_output

from ...core import build_analysis_prompt
from .schemas import AnalysisResultOutput, InvestigationState

logger = logging.getLogger(__name__)


def create_analysis_node(llm: BaseChatModel, config: Config):
    """
    Create analysis node that makes verdict decision using structured output.
    
    This node analyzes gathered code and proposes a verdict.
    """
    
    async def analyze(state: InvestigationState) -> InvestigationState:
        """Execute analysis phase with structured output."""
        is_reanalysis = state.get('needs_reanalysis', False)
        logger.info(f"[{state['issue_id']}] Analysis phase (reanalysis={is_reanalysis})")
        
        # Log the gathered code length for debugging
        gathered_code_length = len(state.get('gathered_code', ''))
        logger.info(f"[{state['issue_id']}] Analysis received gathered_code: {gathered_code_length} chars")
        
        if gathered_code_length == 0:
            logger.warning(f"[{state['issue_id']}] WARNING: No code was gathered during research!")
        
        # Build analysis prompt using the prompts module
        analysis_prompt = build_analysis_prompt(state)
        
        # Use robust_structured_output with retry logic
        max_retries = getattr(config, 'structured_output_max_retries', 3)
        
        try:
            # Create prompt chain
            prompt_chain = ChatPromptTemplate.from_template("{input}")
            
            # Use robust_structured_output utility
            result: AnalysisResultOutput = robust_structured_output(
                llm=llm,
                schema=AnalysisResultOutput,
                input=analysis_prompt,
                prompt_chain=prompt_chain,
                max_retries=max_retries
            )
            
            logger.info(
                f"[{state['issue_id']}] Analysis complete: "
                f"verdict={result.verdict}, confidence={result.confidence}"
            )
            
        except Exception as e:
            logger.error(f"[{state['issue_id']}] Analysis error after retries: {e}", exc_info=True)
            # Fallback on error - don't store error in 'analysis' field
            # (errors aren't useful as "previous analysis" for retries)
            return {
                **state,
                'analysis': '',  # Clear - failed analysis has no useful content for retries
                'proposed_verdict': "NEEDS_REVIEW",
                'justifications': ["Analysis failed due to parsing error"],
                'confidence': "LOW",
                'required_information': [],
                'needs_reanalysis': False,  # Reset flag
            }
        
        # Extract from structured model
        return {
            **state,
            'analysis': result.reasoning,
            'analysis_prompt': analysis_prompt,  # Store prompt for summarization
            'proposed_verdict': result.verdict,
            'justifications': result.justifications,
            'confidence': result.confidence,
            'required_information': [],
            'needs_reanalysis': False,  # Reset flag after reanalysis
        }
    
    return analyze
