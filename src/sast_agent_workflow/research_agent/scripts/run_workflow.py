#!/usr/bin/env python
"""
SAST Workflow Entry Point - Direct Python execution without NAT CLI.

This script runs the complete SAST workflow using the new NAT-free implementation.

Usage:
    python scripts/run_workflow.py

Configuration is loaded from config/default_config.yaml with environment variable overrides.
"""

import asyncio
import logging
import os
import sys
import time
from datetime import datetime
from pathlib import Path

# Add paths for imports
research_agent_path = Path(__file__).parent.parent  # research_agent/
src_path = research_agent_path.parent.parent  # src/
sys.path.insert(0, str(research_agent_path))
sys.path.insert(0, str(src_path))

from core.workflow import build_workflow  # noqa: E402
from observability.langfuse_integration import (  # noqa: E402
    build_langfuse_metadata_for_workflow,
    workflow_langfuse_context,
)

from common.config import Config  # noqa: E402
from dto.SASTWorkflowModels import SASTWorkflowTracker  # noqa: E402
from Utils.log_utils import setup_logging  # noqa: E402

logger = logging.getLogger(__name__)


def _resolve_test_run_id(config: Config) -> None:
    """Set config.TEST_RUN_ID from environment or generate unique id."""
    env_test_run_id = os.getenv("TEST_RUN_ID")
    if env_test_run_id:
        config.TEST_RUN_ID = env_test_run_id
        logger.info("Using shared TEST_RUN_ID from environment: %s", env_test_run_id)
    else:
        config.TEST_RUN_ID = f"test_{int(time.time())}"
        logger.info("Generated unique TEST_RUN_ID: %s", config.TEST_RUN_ID)


def _setup_timestamped_output_path(config: Config) -> None:
    """Add timestamp prefix to config.OUTPUT_FILE_PATH."""
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_path = Path(config.OUTPUT_FILE_PATH)
    config.OUTPUT_FILE_PATH = str(output_path.parent / f"{timestamp}_{output_path.name}")
    logger.info("Added timestamp prefix to output file: %s", config.OUTPUT_FILE_PATH)


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
        logger.info("Loading configuration...")
        config = Config()
        _resolve_test_run_id(config)
        _setup_timestamped_output_path(config)

        logger.info("Input report: %s", config.INPUT_REPORT_FILE_PATH)
        logger.info("Repository: %s", config.REPO_LOCAL_PATH)
        logger.info("Output file: %s", config.OUTPUT_FILE_PATH)
        logger.info("LLM: %s/%s", config.LLM_API_TYPE, config.LLM_MODEL_NAME)

        logger.info("\nBuilding workflow graph...")
        workflow = build_workflow(config)

        # Create initial state
        logger.info("\nInitializing workflow state...")
        initial_state = SASTWorkflowTracker(config=config, issues={}, iteration_count=0, metrics={})

        # Prepare workflow config with optional Langfuse tracing (handler flushed on context exit)
        workflow_config = {
            "recursion_limit": 100,
            "runName": f"sast_workflow_{config.PROJECT_NAME}_{config.TEST_RUN_ID}",
        }
        with workflow_langfuse_context() as langfuse_handler:
            if langfuse_handler:
                workflow_config["callbacks"] = [langfuse_handler]
                workflow_session_id = f"{config.PROJECT_NAME}_{config.TEST_RUN_ID}"
                workflow_config["metadata"] = build_langfuse_metadata_for_workflow(
                    config, workflow_session_id
                )
                logger.info(f"✓ Tracing session: {workflow_session_id}")

            # Execute workflow
            logger.info("\nExecuting workflow...")
            logger.info("-" * 80)

            final_state = await workflow.ainvoke(initial_state, config=workflow_config)

        logger.info("-" * 80)
        logger.info("\nWorkflow execution complete!")

        # Report results
        if isinstance(final_state, dict):
            total_issues = len(final_state.get("issues", {}))
            final_issues = total_issues  # All issues should be processed
        else:
            total_issues = len(final_state.issues)
            final_issues = sum(
                1
                for per_issue in final_state.issues.values()
                if per_issue.analysis_response and per_issue.analysis_response.is_final == "TRUE"
            )

        logger.info("\nResults:")
        logger.info(f"  Total issues: {total_issues}")
        logger.info(f"  Investigated: {final_issues}")
        logger.info(f"  Output file: {config.OUTPUT_FILE_PATH}")

        # Get metrics from state
        metrics = (
            final_state.get("metrics")
            if isinstance(final_state, dict)
            else getattr(final_state, "metrics", None)
        )
        if metrics:
            logger.info("\nMetrics:")
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
