"""Ground truth loader for Langfuse integration.

This module provides utilities to load human-verified ground truth data
and convert it to the workflow's verdict format for conditional scoring.
"""

import logging
from typing import Dict, Optional

from Utils.file_utils import get_human_verified_results
from common.constants import YES_OPTIONS, NO_OPTIONS

logger = logging.getLogger(__name__)


def load_ground_truth_verdicts(config) -> Optional[Dict[str, str]]:
    """
    Load ground truth verdicts from human-verified data.

    Converts "False Positive?" column values to workflow verdict format:
    - "yes"/"y" → "FALSE_POSITIVE" (is a false positive)
    - "no"/"n" → "TRUE_POSITIVE" (is NOT a false positive, so true positive)

    Args:
        config: Workflow configuration with HUMAN_VERIFIED_FILE_PATH or INPUT_REPORT_FILE_PATH

    Returns:
        Dict mapping issue_id (def1, def2, ...) → verdict ("TRUE_POSITIVE" or "FALSE_POSITIVE")
        None if no ground truth available (production mode)

    Example:
        >>> ground_truth = load_ground_truth_verdicts(config)
        >>> if ground_truth:
        >>>     print(f"Loaded {len(ground_truth)} ground truth verdicts")
        >>>     # {'def1': 'TRUE_POSITIVE', 'def2': 'FALSE_POSITIVE', ...}
    """
    try:
        # Use existing utility function to load raw ground truth
        raw_ground_truth = get_human_verified_results(config)

        if not raw_ground_truth:
            logger.info("No ground truth available (production mode)")
            return None

        # Convert to verdict format
        ground_truth_verdicts = {}
        for issue_id, false_positive_value in raw_ground_truth.items():
            # false_positive_value is already normalized (lowercase, stripped) by get_human_verified_results
            if false_positive_value in YES_OPTIONS:
                # "yes" means it IS a false positive
                ground_truth_verdicts[issue_id] = "FALSE_POSITIVE"
            elif false_positive_value in NO_OPTIONS:
                # "no" means it is NOT a false positive (true positive)
                ground_truth_verdicts[issue_id] = "TRUE_POSITIVE"
            else:
                logger.warning(
                    f"Issue {issue_id} has invalid ground truth value: '{false_positive_value}'. "
                    f"Expected one of {YES_OPTIONS + NO_OPTIONS}. Skipping this issue."
                )
                continue

        tp_count = sum(1 for v in ground_truth_verdicts.values() if v == "TRUE_POSITIVE")
        fp_count = sum(1 for v in ground_truth_verdicts.values() if v == "FALSE_POSITIVE")

        logger.info(
            f"Loaded ground truth for {len(ground_truth_verdicts)} issues "
            f"(TP: {tp_count}, FP: {fp_count})"
        )

        return ground_truth_verdicts

    except Exception as e:
        logger.error(f"Failed to load ground truth: {e}", exc_info=True)
        return None
