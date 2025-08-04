"""
Validation utilities for input validation and data integrity checks.
"""

import logging
import re
from typing import List

from common.constants import VALIDATION_LIMITS
from dto.Issue import Issue

logger = logging.getLogger(__name__)


class ValidationError(Exception):
    """Custom exception for validation errors."""

    pass


def validate_similarity_threshold(threshold: int) -> bool:
    """
    Validate that the similarity threshold is within acceptable range.

    Args:
        threshold: The similarity threshold to validate

    Returns:
        bool: True if valid

    Raises:
        ValidationError: If threshold is out of range
    """
    if not isinstance(threshold, int):
        raise ValidationError(f"Similarity threshold must be an integer, got {type(threshold)}")

    min_threshold = VALIDATION_LIMITS["MIN_SIMILARITY_THRESHOLD"]
    max_threshold = VALIDATION_LIMITS["MAX_SIMILARITY_THRESHOLD"]

    if threshold < min_threshold or threshold > max_threshold:
        raise ValidationError(
            f"Similarity threshold {threshold} must be between {min_threshold} and {max_threshold}"
        )

    return True


def validate_issue(issue: Issue) -> None:
    """
    Validate that an Issue has complete data required for analysis.

    Args:
        issue: Issue object to validate

    Raises:
        ValidationError: If the issue is invalid or missing required data
    """
    # Check for parsing errors flag
    if issue.parsing_errors:
        raise ValidationError(f"Issue {issue.id} has parsing errors")

    # Check required fields
    if not issue.issue_type:
        raise ValidationError(f"Issue {issue.id} missing or invalid issue_type")

    if not issue.trace:
        raise ValidationError(f"Issue {issue.id} missing or invalid trace data")

    if issue.issue_cwe and not re.match(r"^CWE-\d+$", issue.issue_cwe):
        raise ValidationError(f"Issue {issue.id} has invalid CWE format: {issue.issue_cwe}")


def validate_issue_list(issues: List[Issue]) -> bool:
    """
    Validate that the issue list is not empty and contains valid issues.

    Args:
        issues: List of issue objects to validate

    Returns:
        bool: True if valid

    Raises:
        ValidationError: If issue list is invalid
    """
    if not isinstance(issues, list):
        raise ValidationError(f"Issues must be a list, got {type(issues)}")

    if not issues:
        raise ValidationError("Issue list cannot be empty")

    if not all(isinstance(issue, Issue) for issue in issues):
        raise ValidationError("All items in the issue list must be of type Issue")

    return True


def safe_validate(validation_func, *args, **kwargs) -> bool:
    """
    Safely execute a validation function with error logging.

    Args:
        validation_func: The validation function to execute
        *args: Arguments to pass to the validation function
        **kwargs: Keyword arguments to pass to the validation function

    Returns:
        bool: True if validation passes, False if it fails
    """
    try:
        return validation_func(*args, **kwargs)
    except ValidationError as e:
        logger.error(f"Validation error in {validation_func.__name__}: {str(e)}")
        return False
    except Exception as e:
        logger.error(f"Unexpected error in {validation_func.__name__}: {str(e)}")
        return False
