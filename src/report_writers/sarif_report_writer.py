import json
import logging
import os
from typing import List

from common.config import Config

logger = logging.getLogger(__name__)


def generate_sarif_report_with_ai_analysis(data: List, config: Config):
    """
    Generate SARIF report with AI analysis suppressions and properties.
    Simple, focused function - no classes needed.
    """

    logger.info(f"Adding AI analysis to SARIF: {config.OUTPUT_FILE_PATH}")

    # Load original SARIF
    with open(config.INPUT_REPORT_FILE_PATH, "r", encoding="utf-8") as f:
        original_sarif = json.load(f)

    # Add AI analysis to SARIF
    analyzed_sarif = _inject_analysis_results(original_sarif, data, config)

    # Write output
    os.makedirs(os.path.dirname(config.OUTPUT_FILE_PATH), exist_ok=True)
    with open(config.OUTPUT_FILE_PATH, "w", encoding="utf-8") as f:
        json.dump(analyzed_sarif, f, indent=4, ensure_ascii=False)

    logger.info(f"SARIF written with {len(data)} AI analysis results")


def _inject_analysis_results(sarif_data, analysis_data, config):
    """Add AI analysis suppressions and properties to SARIF results."""

    # Update tool info
    _update_tool_info(sarif_data, config)

    # Add suppressions to results
    for run in sarif_data.get("runs", []):
        results = run.get("results", [])

        # Validate that SARIF results and analysis data have matching sizes
        # This is critical since we depend on index matching
        if len(results) != len(analysis_data):
            raise ValueError(
                f"Index mismatch error: SARIF has {len(results)} results but analysis data has "
                f"{len(analysis_data)} items. Index-based matching requires equal counts."
            )

        # Process all results with matching analysis data
        for i, result in enumerate(results):
            issue, summary_info = analysis_data[i]
            _add_suppression(result, issue, summary_info)

    return sarif_data


def _update_tool_info(sarif_data, config):
    """Update tool attribution to show AI enhancement."""
    for run in sarif_data.get("runs", []):
        driver = run.get("tool", {}).get("driver", {})

        # Store original info
        original_name = driver.get("name", "unknown")
        original_version = driver.get("version", "unknown")

        # Get dynamic project version
        project_version = _get_project_version()

        # Update to show AI enhancement
        driver.update(
            {
                "name": "sast-ai",
                "version": project_version,
                "informationUri": "https://github.com/RHEcosystemAppEng/sast-ai-workflow",
                "organization": "Red Hat Ecosystem AppEng",
                "product": "SAST AI Workflow",
                "properties": {
                    "originalTool": original_name,
                    "originalVersion": original_version,
                    "workflowEngine": "sast-ai-workflow",
                    "analysisType": "enhanced-static-analysis",
                },
            }
        )


def _get_project_version():
    """
    Get SAST-AI-Workflow project version from build-time environment variable.

    Returns:
        str: The project version (e.g., "2.0.0")
    """
    version = os.getenv("SAST_AI_WORKFLOW_VERSION", "unknown")
    logger.debug(f"Retrieved static project version: {version}")
    return version


def _add_suppression(sarif_result, issue, summary_info):
    """Add AI suppression to SARIF result."""

    llm_response = summary_info.llm_response

    # Determine status
    is_false_positive = not llm_response.is_true_positive()
    status = "accepted" if is_false_positive else "rejected"

    justification = llm_response.short_justifications

    # Add suppression (setdefault creates list if not exists)
    sarif_result.setdefault("suppressions", []).append(
        {"kind": "external", "status": status, "justification": justification}
    )

    sarif_result.setdefault("properties", {})["aiAnalysis"] = {
        "investigation_result": llm_response.investigation_result,
        "recommendations": llm_response.recommendations,
    }
