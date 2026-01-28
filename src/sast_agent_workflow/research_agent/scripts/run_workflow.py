#!/usr/bin/env python
"""
SAST Workflow Entry Point - Direct Python execution without NAT CLI.

This script runs the complete SAST workflow using the new NAT-free implementation.

Usage:
    python scripts/run_workflow.py
    
Configuration is loaded from config/default_config.yaml with environment variable overrides.
"""

import asyncio
from datetime import datetime
import logging
import os
import sys
import time
from pathlib import Path
from typing import Optional

# Add paths for imports
research_agent_path = Path(__file__).parent.parent  # research_agent/
src_path = research_agent_path.parent.parent        # src/
sys.path.insert(0, str(research_agent_path))
sys.path.insert(0, str(src_path))

from common.config import Config
from dto.SASTWorkflowModels import SASTWorkflowTracker
from core.workflow import build_workflow
from Utils.log_utils import setup_logging

logger = logging.getLogger(__name__)

# Langfuse integration (optional)
try:
    from langfuse.langchain import CallbackHandler as LangfuseCallbackHandler
    LANGFUSE_AVAILABLE = True
except ImportError:
    LANGFUSE_AVAILABLE = False
    logger.debug("Langfuse not available - workflow tracing disabled")


async def main():
    """
    Run the complete SAST workflow.
    
    Steps:
    1. Load configuration
    2. Build workflow graph
    3. Execute workflow
    4. Report results
    """
    # Setup logging
    setup_logging()
    
    logger.info("=" * 80)
    logger.info("SAST Workflow - Direct Python Execution (No NAT)")
    logger.info("=" * 80)
    
    try:
        # Load configuration (existing Config class)
        logger.info("Loading configuration...")
        config = Config()
        
        # Generate unique TEST_RUN_ID for this execution
        # Format: test_<timestamp> (e.g., test_1737398400)
        unique_run_id = f"test_{int(time.time())}"
        config.TEST_RUN_ID = unique_run_id
        logger.info(f"Generated unique TEST_RUN_ID: {unique_run_id}")
        
        # Add timestamp prefix to output file path
        # Format: YYYYMMDD_HHMMSS_original_filename.xlsx
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        output_path = Path(config.OUTPUT_FILE_PATH)
        timestamped_filename = f"{timestamp}_{output_path.name}"
        config.OUTPUT_FILE_PATH = str(output_path.parent / timestamped_filename)
        logger.info(f"Added timestamp prefix to output file: {config.OUTPUT_FILE_PATH}")
        
        logger.info(f"Input report: {config.INPUT_REPORT_FILE_PATH}")
        logger.info(f"Repository: {config.REPO_LOCAL_PATH}")
        logger.info(f"Output file: {config.OUTPUT_FILE_PATH}")
        logger.info(f"LLM: {config.LLM_API_TYPE}/{config.LLM_MODEL_NAME}")
        
        # Build workflow graph
        logger.info("\nBuilding workflow graph...")
        workflow = build_workflow(config)
        
        # Setup Langfuse tracing for entire workflow
        # NOTE: To enable per-issue separate traces, set LANGFUSE_TRACE_PER_ISSUE=true
        #       This disables workflow-level tracing and enables issue-level tracing
        langfuse_handler: Optional[LangfuseCallbackHandler] = None
        trace_per_issue = os.getenv("LANGFUSE_TRACE_PER_ISSUE", "false").lower() == "true"
        
        if LANGFUSE_AVAILABLE and os.getenv("LANGFUSE_PUBLIC_KEY") and not trace_per_issue:
            try:
                langfuse_handler = LangfuseCallbackHandler()
                logger.info("✓ Langfuse workflow tracing enabled (single trace for all issues)")
            except Exception as e:
                logger.warning(f"Failed to initialize Langfuse: {e}")
        elif trace_per_issue:
            logger.info("✓ Langfuse per-issue tracing enabled (separate traces for each issue)")
        
        # Create initial state
        logger.info("\nInitializing workflow state...")
        initial_state = SASTWorkflowTracker(
            config=config,
            issues={},
            iteration_count=0,
            metrics={}
        )
        
        # Prepare workflow config with optional Langfuse tracing
        workflow_config = {
            "recursion_limit": 100,
            "runName": f"sast_workflow_{config.PROJECT_NAME}_{config.TEST_RUN_ID}",
        }
        
        # Add Langfuse tracing to entire workflow
        if langfuse_handler:
            workflow_config["callbacks"] = [langfuse_handler]
            # Include project name in session ID to separate traces by project
            # This prevents different projects from being merged when using the same TEST_RUN_ID
            workflow_session_id = f"{config.PROJECT_NAME}_{config.TEST_RUN_ID}"
            workflow_config["metadata"] = {
                "langfuse_session_id": workflow_session_id,
                "langfuse_user_id": config.PROJECT_NAME,
                "langfuse_tags": [
                    f"project:{config.PROJECT_NAME}",
                    f"version:{config.PROJECT_VERSION}",
                    f"test_run:{config.TEST_RUN_ID}",
                    f"model:{config.LLM_MODEL_NAME}",
                    "workflow:sast_pipeline",
                ],
                "project_name": config.PROJECT_NAME,
                "project_version": config.PROJECT_VERSION,
                "test_run_id": config.TEST_RUN_ID,
                "llm_model": config.LLM_MODEL_NAME,
            }
            logger.info(f"✓ Tracing session: {workflow_session_id}")
        
        # Execute workflow
        logger.info("\nExecuting workflow...")
        logger.info("-" * 80)
        
        final_state = await workflow.ainvoke(
            initial_state,
            config=workflow_config
        )
        
        logger.info("-" * 80)
        logger.info("\nWorkflow execution complete!")
        
        # Report results
        if isinstance(final_state, dict):
            total_issues = len(final_state.get('issues', {}))
            final_issues = total_issues  # All issues should be processed
        else:
            total_issues = len(final_state.issues)
            final_issues = sum(
                1 for per_issue in final_state.issues.values()
                if per_issue.analysis_response and per_issue.analysis_response.is_final == "TRUE"
            )
        
        logger.info(f"\nResults:")
        logger.info(f"  Total issues: {total_issues}")
        logger.info(f"  Investigated: {final_issues}")
        logger.info(f"  Output file: {config.OUTPUT_FILE_PATH}")
        
        # Get metrics from state
        metrics = final_state.get('metrics') if isinstance(final_state, dict) else getattr(final_state, 'metrics', None)
        if metrics:
            logger.info(f"\nMetrics:")
            for key, value in metrics.items():
                if key != "error":
                    logger.info(f"  {key}: {value}")
        
        logger.info("\n" + "=" * 80)
        logger.info("SAST Workflow Complete")
        logger.info("=" * 80)
        
        return 0
        
    except KeyboardInterrupt:
        logger.warning("\nWorkflow interrupted by user")
        return 130
        
    except Exception as e:
        logger.error(f"\nWorkflow failed with error: {e}", exc_info=True)
        return 1


if __name__ == "__main__":
    exit_code = asyncio.run(main())
    sys.exit(exit_code)

