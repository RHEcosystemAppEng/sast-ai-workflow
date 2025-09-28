#!/usr/bin/env python3
"""
NAT evaluation converters for summarize_justifications function.

These converters transform between string format (used by NAT) and
SASTWorkflowTracker objects (used by summarize_justifications).
"""

import json
import logging
import os
import sys
import yaml
from typing import Dict, Any
from pathlib import Path

# Add project root to path for imports
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse
from evaluation.converter_tools.base_converter import BaseEvaluationConverter
from common.constants import DEFAULT_FIELD_VALUE

try:
    from common.config import Config
except ImportError:
    try:
        from Config import Config
    except ImportError:
        Config = None

logger = logging.getLogger(__name__)


class SummarizeConverter(BaseEvaluationConverter):
    """Summarize evaluation converter that inherits from BaseEvaluationConverter."""

    def __init__(self):
        super().__init__("summarize_justifications", "summarize_eval_dataset.json")

    def parse_input_data(self, input_str: str) -> Dict[str, Any]:
        """Parse input string for summarize evaluation."""
        try:
            data = json.loads(input_str)
            return data
        except json.JSONDecodeError:
            logger.info("Input is not JSON, loading dataset to find matching entry")
            eval_data = self.load_dataset_entry(input_str)
            if not eval_data:
                logger.warning(f"No matching entry found for input: {input_str}")
                return {"id": "unknown", "issue_type": "SECURITY_ISSUE", "severity": "HIGH",
                       "source_file": "unknown.c", "full_justification": ""}
            return eval_data

    def create_issue_objects(self, parsed_data: Dict[str, Any]) -> Dict[str, Any]:
        """Create Issue objects for summarize evaluation."""
        issue = Issue(
            id=parsed_data.get('id', 'eval_issue'),
            issue_type=parsed_data.get('issue_type', 'SECURITY_ISSUE'),
            severity=parsed_data.get('severity', 'HIGH'),
            file_path=parsed_data.get('source_file', 'unknown.c'),
            line_number=1,
            message=f"SAST analysis for {parsed_data.get('id')}"
        )

        full_justification = parsed_data.get('full_justification', '')
        justifications = [full_justification] if full_justification else []

        analysis_response = AnalysisResponse(
            investigation_result=parsed_data.get('investigation_result', 'TRUE POSITIVE'),
            is_final="TRUE",
            justifications=justifications,
            prompt=parsed_data.get('id', 'eval_issue'),
            short_justifications=DEFAULT_FIELD_VALUE  # This will trigger LLM summarization
        )

        per_issue_data = PerIssueData(
            issue=issue,
            analysis_response=analysis_response
        )

        return {parsed_data.get('id', 'eval_issue'): per_issue_data}

    def extract_output_data(self, tracker: SASTWorkflowTracker) -> str:
        """Extract output data for summarize evaluation."""
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

    def get_minimal_config(self):
        """Get summarize-specific minimal config."""
        class MinimalConfig:
            def __init__(self):
                prompts_dir = os.path.join(os.path.dirname(__file__), "../../src/templates/prompts")

                try:
                    system_prompt_file = os.path.join(prompts_dir, "justification_summary_system_prompt.yaml")
                    with open(system_prompt_file, "r", encoding="utf-8") as f:
                        system_prompt_data = yaml.safe_load(f)
                        self.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT = system_prompt_data.get("template", "")

                    human_prompt_file = os.path.join(prompts_dir, "justification_summary_human_prompt.yaml")
                    with open(human_prompt_file, "r", encoding="utf-8") as f:
                        human_prompt_data = yaml.safe_load(f)
                        self.JUSTIFICATION_SUMMARY_HUMAN_PROMPT = human_prompt_data.get("template", "")

                except Exception as e:
                    logger.warning(f"Failed to load prompt templates: {e}")
                    self.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT = "Summarize the security analysis justification."
                    self.JUSTIFICATION_SUMMARY_HUMAN_PROMPT = "Please provide a concise summary of the following analysis: {justification}"

        return MinimalConfig()

    def setup_environment(self, **kwargs):
        """Setup summarize-specific environment variables."""
        summarize_kwargs = {
            'INPUT_REPORT_FILE_PATH': '/dev/null',
            'OUTPUT_FILE_PATH': '/dev/null',
            'PROJECT_NAME': 'test-eval',
            'PROJECT_VERSION': '1.0.0',
            'REPO_LOCAL_PATH': str(self.project_root)
        }
        summarize_kwargs.update(kwargs)
        super().setup_environment(**summarize_kwargs)

    def create_config(self):
        """Create summarize-specific config."""
        try:
            self.setup_environment()
            config = Config()
            logger.info("Successfully created real Config object for evaluation")
            return config
        except Exception as e:
            logger.info(f"Main Config failed ({e}), using minimal config for evaluation")
            return self.get_minimal_config()

    def convert_sast_tracker_to_str(self, tracker: SASTWorkflowTracker) -> str:
        """
        Override the base class method to return direct string output instead of JSON.

        For summarize evaluation, we return the actual summary text rather than JSON-wrapped data.
        """
        try:
            # Extract the summary directly
            summary_text = self.extract_output_data(tracker)
            logger.info(f"Converted SASTWorkflowTracker to summarize results")
            return summary_text

        except Exception as e:
            logger.error(f"Failed to convert SASTWorkflowTracker to string: {e}")
            return f"Conversion failed: {str(e)}"


# Create converter instance for backward compatibility
_summarize_converter = SummarizeConverter()


def convert_str_to_sast_tracker(input_str: str) -> SASTWorkflowTracker:
    """Convert string to SASTWorkflowTracker (backward compatibility function)."""
    return _summarize_converter.convert_str_to_sast_tracker(input_str)


def convert_sast_tracker_to_str(tracker: SASTWorkflowTracker) -> str:
    """Convert SASTWorkflowTracker to string (backward compatibility function)."""
    return _summarize_converter.convert_sast_tracker_to_str(tracker)


def _create_minimal_config():
    """Create minimal config object for evaluation when full config fails (backward compatibility)."""
    return _summarize_converter.get_minimal_config()


__all__ = ['SummarizeConverter', 'convert_str_to_sast_tracker', 'convert_sast_tracker_to_str', '_create_minimal_config']