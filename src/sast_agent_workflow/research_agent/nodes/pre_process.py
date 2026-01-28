"""
Pre-process Node - Initialize workflow tracker from SAST report.

Extracts logic from tools/pre_process.py without NAT dependency.
"""

import logging

from common.config import Config
from dto.LLMResponse import AnalysisResponse, FinalStatus
from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from handlers.repo_handler_factory import repo_handler_factory
from ReportReader import read_sast_report

logger = logging.getLogger(__name__)


def create_pre_process_node(config: Config):
    """
    Create pre-process node function.
    
    Args:
        config: Configuration instance
    
    Returns:
        Node function that initializes the workflow tracker
    """
    
    def pre_process(state: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Initialize workflow tracker from SAST report.
        
        Core logic extracted from tools/pre_process.py lines 47-87.
        
        Args:
            state: Empty or partially initialized tracker
        
        Returns:
            Tracker with issues loaded from report
        """
        logger.info("Pre-processing SAST report...")
        
        # Load issues from report
        issue_list = read_sast_report(config)
        logger.info(f"Loaded {len(issue_list)} issues from report")
        
        # Initialize tracker with issues
        tracker = SASTWorkflowTracker(
            config=config,
            issues={
                issue.id: PerIssueData(
                    issue=issue,
                    analysis_response=AnalysisResponse(
                        investigation_result="",
                        is_final=FinalStatus.FALSE.value
                    )
                )
                for issue in issue_list
            }
        )
        
        # Initialize repo handler (downloads repo if needed)
        try:
            repo_handler_factory(config)
            logger.info(f"Repository initialized: {config.REPO_LOCAL_PATH}")
        except Exception as e:
            logger.error(f"Failed to initialize repository: {e}")
            raise RuntimeError(f"Repository initialization failed: {e}") from e
        
        logger.info("Pre-processing complete")
        return tracker
    
    return pre_process

