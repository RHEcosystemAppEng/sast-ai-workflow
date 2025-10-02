#!/usr/bin/env python3
"""
NAT evaluation converters for judge_llm_analysis function.

These converters transform between string format (used by NAT) and
SASTWorkflowTracker objects (used by judge_llm_analysis).
"""

import json
import logging
import os
import sys
import yaml
from typing import Dict, List, Any
from pathlib import Path

# Add project root to path for imports
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, FinalStatus
from evaluation.converter_tools.base_converter import BaseEvaluationConverter

try:
    from common.config import Config
except ImportError:
    try:
        from Config import Config
    except ImportError:
        Config = None

logger = logging.getLogger(__name__)

class JudgeLLMConverter(BaseEvaluationConverter):
    """Judge LLM evaluation converter that inherits from BaseEvaluationConverter."""

    def __init__(self):
        super().__init__("judge_llm_analysis", "judge_llm_eval_dataset_6.json")

    def parse_input_data(self, input_str: str) -> Dict[str, Any]:
        """Parse input string for judge LLM evaluation."""
        try:
            data = json.loads(input_str)
            return data
        except json.JSONDecodeError:
            logger.info("Input is not JSON, loading dataset to find matching entry")
            eval_data = self.load_dataset_entry(input_str)
            if not eval_data:
                logger.warning(f"No matching entry found for input: {input_str}")
                return {"id": "unknown", "issue_name": "", "error_description": "", "source_code_context": ""}
            return eval_data

    def create_issue_objects(self, parsed_data: Dict[str, Any]) -> Dict[str, Any]:
        """Create Issue objects for judge LLM evaluation."""
        issue = Issue(
            id=parsed_data.get("id", "unknown"),
            issue_type=parsed_data.get("issue_name", ""),
            issue_label=parsed_data.get("issue_name", ""),
            trace=parsed_data.get("error_description", "")
        )

        source_code = {}
        similar_known_issues = ""
        source_context = parsed_data.get("source_code_context", "")

        if source_context:
            if "*** Examples ***" in source_context:
                parts = source_context.split("*** Examples ***", 1)
                code_part = parts[0].strip()
                # Don't include the header since build_analysis_context() will add it
                examples_part = parts[1].strip() if len(parts) > 1 else ""
                similar_known_issues = examples_part
            else:
                code_part = source_context
                similar_known_issues = ""

            # Parse the code part to extract source code
            if code_part and "code of" in code_part:
                lines = code_part.split('\n')
                file_path = "unknown.c"
                code_lines = []

                for line in lines:
                    if line.startswith("code of") and "file:" in line:
                        file_path = line.split("code of")[1].split("file:")[0].strip()
                    elif line and not line.startswith("code of"):
                        code_lines.append(line)

                if code_lines:
                    source_code[file_path] = ['\n'.join(code_lines)]

        analysis_response = AnalysisResponse(
            investigation_result="",
            is_final=FinalStatus.FALSE.value,
            prompt="",
            justifications=[],
            recommendations=[],
            instructions=[],
            evaluation=[]
        )

        per_issue_data = PerIssueData(
            issue=issue,
            source_code=source_code,
            similar_known_issues=similar_known_issues,  # Examples from the dataset
            analysis_response=analysis_response,  # Empty object for function to populate
            found_symbols=set()
        )

        return {issue.id: per_issue_data}

    def extract_output_data(self, tracker: SASTWorkflowTracker) -> Dict[str, Any]:
        """Extract output data for judge LLM evaluation."""
        results = {}

        for issue_id, per_issue_data in tracker.issues.items():
            if per_issue_data.analysis_response:
                results[issue_id] = {
                    "investigation_result": per_issue_data.analysis_response.investigation_result,
                    "justifications": per_issue_data.analysis_response.justifications,
                    "is_final": per_issue_data.analysis_response.is_final,
                    "prompt": per_issue_data.analysis_response.prompt
                }
            else:
                results[issue_id] = {
                    "investigation_result": "NO_ANALYSIS",
                    "justifications": [],
                    "is_final": "FALSE",
                    "prompt": ""
                }

        return results

    def get_minimal_config(self):
        """Get judge LLM-specific minimal config."""
        class MinimalConfig:
            def __init__(self):
                self.MAX_ANALYSIS_ITERATIONS = 3
                self.project_name = "judge-llm-eval"
                self.project_version = "1.0.0"

        return MinimalConfig()

    def setup_environment(self, **kwargs):
        """Setup judge LLM-specific environment variables."""
        judge_kwargs = {
            'INPUT_REPORT_FILE_PATH': '/dev/null',
            'OUTPUT_FILE_PATH': '/dev/null',
            'KNOWN_FALSE_POSITIVE_FILE_PATH': '/dev/null',
            'PROJECT_NAME': 'judge-llm-eval',
            'PROJECT_VERSION': '1.0.0',
            'REPO_LOCAL_PATH': str(project_root)
        }
        judge_kwargs.update(kwargs)
        super().setup_environment(**judge_kwargs)

    def create_config(self):
        """Create judge LLM-specific config."""
        try:
            self.setup_environment()
            config = Config()
            logger.info("Successfully created real Config object for evaluation")
            return config
        except Exception as e:
            logger.info(f"Main Config failed ({e}), using minimal config for evaluation")
            return self.get_minimal_config()

# Create converter instance for backward compatibility
_judge_llm_converter = JudgeLLMConverter()


def convert_str_to_sast_tracker(input_str: str) -> SASTWorkflowTracker:
    """Convert string to SASTWorkflowTracker (backward compatibility function)."""
    return _judge_llm_converter.convert_str_to_sast_tracker(input_str)


def convert_sast_tracker_to_str(tracker: SASTWorkflowTracker) -> str:
    """Convert SASTWorkflowTracker to string (backward compatibility function)."""
    return _judge_llm_converter.convert_sast_tracker_to_str(tracker)


def create_minimal_config():
    """Create minimal config object for evaluation when full config fails (backward compatibility)."""
    return _judge_llm_converter.get_minimal_config()


__all__ = ['JudgeLLMConverter', 'convert_str_to_sast_tracker', 'convert_sast_tracker_to_str', 'create_minimal_config']