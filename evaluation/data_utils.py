"""
Data utilities for preparing test data for node evaluation
"""

import json
import logging
from typing import Dict, Any
from pathlib import Path

from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.LLMResponse import AnalysisResponse

logger = logging.getLogger(__name__)


def prepare_test_data(golden_dataset_path: str, max_examples: int) -> SASTWorkflowTracker:
    if not Path(golden_dataset_path).exists():
        logger.error(f"Golden dataset not found at: {golden_dataset_path}")
        return SASTWorkflowTracker(issues={})
    
    try:
        with open(golden_dataset_path, 'r', encoding='utf-8') as f:
            golden_data = json.load(f)
    except Exception as e:
        logger.error(f"Failed to load golden dataset: {e}")
        return SASTWorkflowTracker(issues={})
    
    tracker = SASTWorkflowTracker(issues={})
    
    processed = 0
    for item in golden_data:
        if processed >= max_examples:
            break
            
        try:
            issue_id = item.get('issue_id', f"test_issue_{processed}")
            analysis_response = AnalysisResponse(
                investigation_result=item.get('investigation_result', 'TRUE POSITIVE'),
                is_final='TRUE',
                justifications=item.get('justifications', []),
                prompt=item.get('prompt', ''),
                short_justifications=""
            )
            
            per_issue_data = PerIssueData(
                analysis_response=analysis_response
            )
            
            tracker.issues[issue_id] = per_issue_data
            processed += 1
            
        except Exception as e:
            logger.error(f"Failed to process item {processed}: {e}")
            continue
    
    logger.info(f"Prepared {len(tracker.issues)} test cases from golden dataset")
    return tracker


def load_golden_dataset(dataset_path: str) -> Dict[str, Any]:
    try:
        with open(dataset_path, 'r', encoding='utf-8') as f:
            return json.load(f)
    except Exception as e:
        logger.error(f"Failed to load dataset from {dataset_path}: {e}")
        return {}


def validate_test_data(tracker: SASTWorkflowTracker) -> bool:
    if not tracker.issues:
        logger.error("No issues found in tracker")
        return False
    
    for issue_id, per_issue_data in tracker.issues.items():
        if not per_issue_data.analysis_response:
            logger.error(f"No analysis response for issue {issue_id}")
            return False
            
        if not per_issue_data.analysis_response.justifications:
            logger.error(f"No justifications for issue {issue_id}")
            return False
    
    return True