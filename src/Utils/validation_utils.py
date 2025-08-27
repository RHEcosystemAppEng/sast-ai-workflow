"""
Validation utilities for input validation and data integrity checks.
"""

import logging
from typing import Any, List
from dto.Issue import Issue

logger = logging.getLogger(__name__)


class ValidationError(Exception):
    """Custom exception for validation errors."""
    pass


def is_valid_int_value(value: Any, min_val: int | None = None, max_val: int | None = None) -> bool:
    """
    Validate that a value is an integer within optional min/max range.
    
    Args:
        value: The value to validate
        min_val: Minimum allowed value (optional)
        max_val: Maximum allowed value (optional)
        
    Returns:
        bool: True if valid, False otherwise
    """
    if not isinstance(value, int):
        return False
    
    if min_val is not None and value < min_val:
        return False
    
    if max_val is not None and value > max_val:
        return False
    
    return True


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


def validate_issue_dict(issues: dict[str, Any]) -> bool:
    """
    Validate that the issue dict is not empty and contains valid PerIssueData objects.
    
    Args:
        issues: Dict of PerIssueData objects to validate
        
    Returns:
        bool: True if valid
        
    Raises:
        ValidationError: If issue dict is invalid
    """
    # Import inside function to avoid circular dependency
    from dto.SASTWorkflowModels import PerIssueData
    
    if not isinstance(issues, dict):
        raise ValidationError(f"Issues must be a dict, got {type(issues)}")
    
    if not issues:
        raise ValidationError("Issue dict cannot be empty")
    
    if not all(isinstance(issue, PerIssueData) for issue in issues.values()):
        raise ValidationError("All items in the issue dict must be of type PerIssueData")

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