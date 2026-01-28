"""
Summarize Node - Summarize investigation justifications.

Extracts logic from tools/summarize_justifications.py without NAT dependency.
"""

import logging

from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.messages import HumanMessage, SystemMessage

from common.config import Config
from dto.SASTWorkflowModels import SASTWorkflowTracker

logger = logging.getLogger(__name__)


def create_summarize_node(config: Config, llm: BaseChatModel):
    """
    Create summarize node function.
    
    Args:
        config: Configuration instance
        llm: Language model for summarization
    
    Returns:
        Node function that summarizes justifications
    """
    
    async def summarize(state: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Summarize justifications for each issue.
        
        Core logic extracted from tools/summarize_justifications.py.
        
        Args:
            state: Tracker with investigated issues
        
        Returns:
            Tracker with summarized justifications
        """
        logger.info("Summarizing justifications...")
        
        for issue_id, per_issue in state.issues.items():
            if not per_issue.analysis_response or per_issue.analysis_response.is_final != "TRUE":
                continue
            
            # Skip if no justifications to summarize
            if not hasattr(per_issue, 'analysis_response') or not per_issue.analysis_response.justifications:
                logger.debug(f"No justifications to summarize for {issue_id}")
                continue
            
            try:
                # Get justifications and prompt
                justifications = per_issue.analysis_response.justifications
                verdict = per_issue.analysis_response.investigation_result
                actual_prompt = per_issue.analysis_response.prompt or "(No prompt available)"
                
                # Create summary prompt (matching template: actual_prompt, response)
                system_msg = SystemMessage(content=config.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT)
                
                # Format response as expected by template
                response_text = f"Verdict: {verdict}\n\nJustifications:\n" + "\n".join(f"- {j}" for j in justifications)
                
                human_msg = HumanMessage(content=config.JUSTIFICATION_SUMMARY_HUMAN_PROMPT.format(
                    actual_prompt=actual_prompt,
                    response=response_text
                ))
                
                # Get summary from LLM
                response = await llm.ainvoke([system_msg, human_msg])
                summary = response.content.strip()
                
                # Update short justification
                per_issue.analysis_response.short_justifications = summary
                
                logger.debug(f"Summarized justifications for {issue_id}")
                
            except Exception as e:
                logger.error(f"Failed to summarize {issue_id}: {e}", exc_info=True)
                # Keep original justifications if summarization fails
                if per_issue.analysis_response.justifications:
                    per_issue.analysis_response.short_justifications = per_issue.analysis_response.justifications[0][:200]
        
        logger.info("Summarization complete")
        return state
    
    return summarize

