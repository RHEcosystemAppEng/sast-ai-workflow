"""
Filter Node - Filter known false positives using embeddings.

Extracts logic from tools/filter.py without NAT dependency.
"""

import logging

from langchain_core.language_models.chat_models import BaseChatModel

from common.config import Config
from dto.SASTWorkflowModels import SASTWorkflowTracker
from FilterKnownIssues import capture_known_issues

logger = logging.getLogger(__name__)


def create_filter_node(config: Config, llm: BaseChatModel):
    """
    Create filter node function.
    
    Args:
        config: Configuration instance
        llm: LLM for embedding generation
    
    Returns:
        Node function that filters known false positives
    """
    
    def filter_node(state: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Filter known false positives using similarity search.
        
        Core logic extracted from tools/filter.py lines 123-190.
        
        Args:
            state: Tracker with loaded issues
        
        Returns:
            Tracker with known FPs marked as final
        """
        logger.info("Filtering known false positives...")
        
        if not config.USE_KNOWN_FALSE_POSITIVE_FILE:
            logger.info("Known false positive filtering disabled")
            return state
        
        try:
            # Convert issues dict to list for capture_known_issues
            issue_list = [per_issue.issue for per_issue in state.issues.values()]
            
            # Capture known issues using embeddings
            already_seen_issues_dict, similar_known_issues_dict = capture_known_issues(
                llm_service=None,  # Will use llm directly
                issue_list=issue_list,
                config=config,
            )
            
            # Mark known false positives as final
            for issue_id in already_seen_issues_dict:
                if issue_id in state.issues:
                    per_issue = state.issues[issue_id]
                    
                    # Set verdict to FALSE_POSITIVE (with space for legacy compatibility)
                    if per_issue.analysis_response:
                        per_issue.analysis_response.investigation_result = "FALSE POSITIVE"
                        per_issue.analysis_response.justifications = [
                            "This issue is similar to a known false positive"
                        ]
                        per_issue.analysis_response.is_final = "TRUE"
                    
                    logger.info(f"Marked {issue_id} as known false positive")
            
            # Store similar issues for context
            for issue_id, similar_text in similar_known_issues_dict.items():
                if issue_id in state.issues:
                    state.issues[issue_id].similar_known_issues = similar_text
            
            fp_count = len(already_seen_issues_dict)
            total_count = len(state.issues)
            logger.info(f"Filtered {fp_count}/{total_count} known false positives")
            
        except Exception as e:
            logger.error(f"Filter node error: {e}", exc_info=True)
            logger.warning("Continuing without filtering")
        
        return state
    
    return filter_node

