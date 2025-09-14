"""
NAT Converter functions for summarize_justifications evaluation.

These functions handle the conversion between NAT evaluation format and
SASTWorkflowTracker for the summarize_justifications node.
"""

import logging
from typing import Dict, Any

# Import SASTWorkflowTracker for type annotations
from dto.SASTWorkflowModels import SASTWorkflowTracker

logger = logging.getLogger(__name__)


def convert_str_to_sast_tracker(input_str: str) -> SASTWorkflowTracker:
    """
    Convert string input to SASTWorkflowTracker for summarize_justifications evaluation.

    Args:
        input_str: The evaluation input (typically an issue ID)

    Returns:
        SASTWorkflowTracker with the loaded issue data
    """
    logger.info(f"Converting evaluation input to SASTWorkflowTracker: {input_str}")

    from dto.Issue import Issue
    from dto.LLMResponse import AnalysisResponse
    from dto.SASTWorkflowModels import PerIssueData, SASTWorkflowTracker
    from common.config import Config
    from common.constants import DEFAULT_FIELD_VALUE

    # Load the evaluation dataset to get the full justification
    try:
        from evaluation.tools.eval_utils import load_evaluation_dataset, find_dataset_entry

        dataset = load_evaluation_dataset("summarize_eval_dataset.json")
        entry = find_dataset_entry(dataset, input_str)

        if not entry:
            logger.warning(f"No matching entry found for input: {input_str}")
            return _create_empty_tracker()

        # Create real issue from evaluation dataset
        issue = Issue(
            id=entry.get('id', 'eval_issue'),
            issue_type=entry.get('issue_type', 'SECURITY_ISSUE'),
            severity=entry.get('severity', 'HIGH'),
            file_path=entry.get('source_file', 'unknown.c'),
            line_number=1,
            message=f"SAST analysis for {entry.get('id')}"
        )

        # Create real analysis response with actual justifications from dataset
        full_justification = entry.get('full_justification', '')
        justifications = [full_justification] if full_justification else []

        analysis_response = AnalysisResponse(
            investigation_result=entry.get('investigation_result', 'TRUE POSITIVE'),
            is_final="TRUE",
            justifications=justifications,
            prompt=input_str,
            short_justifications=DEFAULT_FIELD_VALUE  # This will trigger LLM summarization
        )

        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=analysis_response
        )

        # Create a config object for evaluation
        try:
            tracker_config = Config()
        except Exception as e:
            logger.info(f"Main Config failed ({e}), using minimal config for evaluation")
            tracker_config = _create_minimal_config()

        tracker = SASTWorkflowTracker(
            issues={entry.get('id', 'eval_issue'): per_issue_data},
            config=tracker_config
        )

        logger.info(f"Created tracker with real data for issue: {entry.get('id')}")
        return tracker

    except Exception as e:
        logger.error(f"Failed to load evaluation data: {e}")
        return _create_empty_tracker()


def convert_sast_tracker_to_str(tracker: SASTWorkflowTracker) -> str:
    """
    Convert SASTWorkflowTracker to string output for evaluation.

    Args:
        tracker: SASTWorkflowTracker with summarized justifications

    Returns:
        String containing the generated summary for evaluation
    """
    from common.constants import DEFAULT_FIELD_VALUE
    logger.info("Converting SASTWorkflowTracker to string output")

    if not tracker.issues:
        return "No issues processed"

    results = []
    for issue_id, per_issue_data in tracker.issues.items():
        logger.info(f"Processing issue {issue_id} for output conversion")

        if per_issue_data.analysis_response:
            logger.info(f"Issue {issue_id} has analysis_response")

            # Prefer short_justifications (the summarized version) if it's been updated
            if (per_issue_data.analysis_response.short_justifications and
                per_issue_data.analysis_response.short_justifications != DEFAULT_FIELD_VALUE):
                logger.info(f"Using short_justifications for issue {issue_id}")
                results.append(per_issue_data.analysis_response.short_justifications)
            elif per_issue_data.analysis_response.justifications:
                logger.info(f"Using original justifications for issue {issue_id}")
                results.append(" ".join(per_issue_data.analysis_response.justifications))
            else:
                logger.warning(f"Issue {issue_id} has DEFAULT_FIELD_VALUE, LLM summarization may have failed")
                results.append(f"Summarization failed for {issue_id} - LLM calls occurred but result not captured")
        else:
            logger.info(f"Issue {issue_id} has no analysis_response")

    return results[0] if results else "No summary generated"


def _create_empty_tracker():
    """Create an empty SASTWorkflowTracker for error cases."""
    from dto.SASTWorkflowModels import SASTWorkflowTracker
    from common.config import Config

    try:
        config = Config()
    except Exception:
        # If config fails, return None - let the evaluation handle it
        return None

    return SASTWorkflowTracker(issues={}, config=config)


def _create_minimal_config():
    """Create a minimal config object for evaluation when full config fails."""
    import os
    import yaml

    class MinimalConfig:
        def __init__(self):
            # Load real prompt templates from files for evaluation
            prompts_dir = os.path.join(os.path.dirname(__file__), "../../src/templates/prompts")

            try:
                # Load justification summary system prompt
                system_prompt_file = os.path.join(prompts_dir, "justification_summary_system_prompt.yaml")
                with open(system_prompt_file, "r", encoding="utf-8") as f:
                    system_prompt_data = yaml.safe_load(f)
                    self.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT = system_prompt_data.get("template", "")

                # Load justification summary human prompt
                human_prompt_file = os.path.join(prompts_dir, "justification_summary_human_prompt.yaml")
                with open(human_prompt_file, "r", encoding="utf-8") as f:
                    human_prompt_data = yaml.safe_load(f)
                    self.JUSTIFICATION_SUMMARY_HUMAN_PROMPT = human_prompt_data.get("template", "")

            except Exception as e:
                logger.warning(f"Failed to load prompt templates: {e}")
                # Set default prompts if loading fails
                self.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT = "Summarize the security analysis justification."
                self.JUSTIFICATION_SUMMARY_HUMAN_PROMPT = "Please provide a concise summary of the following analysis: {justification}"

    return MinimalConfig()