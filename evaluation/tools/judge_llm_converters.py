#!/usr/bin/env python3
"""
NAT evaluation converters for judge_llm_analysis function.

These converters transform between string format (used by NAT) and
SASTWorkflowTracker objects (used by judge_llm_analysis).
"""

import json
import logging
from typing import Dict, List
from pathlib import Path
import sys

# Add project root to path for imports
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, FinalStatus

try:
    from common.config import Config
except ImportError:
    try:
        from Config import Config
    except ImportError:
        Config = None

logger = logging.getLogger(__name__)

def convert_str_to_sast_tracker(input_str: str) -> SASTWorkflowTracker:
    """
    Convert JSON string input to SASTWorkflowTracker for judge_llm_analysis.

    Expected input format: JSON record from judge_llm_eval_dataset.json
    """
    try:
        # Parse JSON input
        data = json.loads(input_str)

        # Create Issue object
        issue = Issue(
            id=data.get("id", "unknown"),
            issue_type=data.get("issue_name", ""),
            issue_label=data.get("issue_name", ""),
            trace=data.get("error_description", "")
        )

        # Parse source code context and separate code from examples
        source_code = {}
        similar_known_issues = ""
        source_context = data.get("source_code_context", "")

        if source_context:
            # Split the context at "*** Examples ***" to separate code and examples
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
                        # Extract file path: "code of src/file.c file:"
                        file_path = line.split("code of")[1].split("file:")[0].strip()
                    elif line and not line.startswith("code of"):
                        code_lines.append(line)

                if code_lines:
                    source_code[file_path] = ['\n'.join(code_lines)]

        # Create empty AnalysisResponse for function to populate
        analysis_response = AnalysisResponse(
            investigation_result="",
            is_final=FinalStatus.FALSE.value,
            prompt="",
            justifications=[],
            recommendations=[],
            instructions=[],
            evaluation=[]
        )

        # Create PerIssueData
        per_issue_data = PerIssueData(
            issue=issue,
            source_code=source_code,
            similar_known_issues=similar_known_issues,  # Examples from the dataset
            analysis_response=analysis_response,  # Empty object for function to populate
            found_symbols=set()
        )

        # Create a config object for evaluation
        try:
            # Set up required environment variables for Config
            import os
            os.environ.setdefault('INPUT_REPORT_FILE_PATH', '/dev/null')
            os.environ.setdefault('OUTPUT_FILE_PATH', '/dev/null')
            os.environ.setdefault('KNOWN_FALSE_POSITIVE_FILE_PATH', '/dev/null')
            os.environ.setdefault('PROJECT_NAME', 'judge-llm-eval')
            os.environ.setdefault('PROJECT_VERSION', '1.0.0')
            os.environ.setdefault('REPO_LOCAL_PATH', str(project_root))

            tracker_config = Config()
            logger.info("Successfully created real Config object for evaluation")
        except Exception as e:
            logger.info(f"Main Config failed ({e}), using minimal config for evaluation")
            tracker_config = create_minimal_config()

        # Create SASTWorkflowTracker
        tracker = SASTWorkflowTracker(
            config=tracker_config,
            issues={issue.id: per_issue_data},
            iteration_count=0,
            metrics={}
        )

        logger.info(f"Converted input to SASTWorkflowTracker with issue: {issue.id}")
        return tracker

    except Exception as e:
        logger.error(f"Error converting string to SASTWorkflowTracker: {e}")
        raise ValueError(f"Failed to convert input: {e}")

def convert_sast_tracker_to_str(tracker: SASTWorkflowTracker) -> str:
    """
    Convert SASTWorkflowTracker output to JSON string for evaluation.

    Extracts the investigation result and justifications from analysis_response.
    """
    try:
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

        output = json.dumps(results, indent=2)
        logger.info(f"Converted SASTWorkflowTracker to output string with {len(results)} results")
        return output

    except Exception as e:
        logger.error(f"Error converting SASTWorkflowTracker to string: {e}")
        return json.dumps({"error": str(e)})

def create_minimal_config():
    """Create minimal config object for evaluation when full config fails."""
    import os
    import yaml
    from pathlib import Path

    # Create a minimal config structure similar to real Config
    class MinimalConfig:
        def __init__(self):
            self.MAX_ANALYSIS_ITERATIONS = 3
            # Add other attributes that might be needed
            self.project_name = "judge-llm-eval"
            self.project_version = "1.0.0"

    return MinimalConfig()

# Make converters available for NAT
__all__ = ['convert_str_to_sast_tracker', 'convert_sast_tracker_to_str']