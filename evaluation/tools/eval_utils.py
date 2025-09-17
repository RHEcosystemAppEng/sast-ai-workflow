"""
Common utilities for evaluation tools.
"""

import logging
import json
from pathlib import Path
from typing import List, Dict, Any, Optional

logger = logging.getLogger(__name__)


def load_evaluation_dataset(dataset_name: str = "summarize_eval_dataset.json") -> List[Dict[str, Any]]:
    """
    Load evaluation dataset from standard locations.

    Args:
        dataset_name: Name of the dataset file

    Returns:
        List of evaluation entries

    Raises:
        FileNotFoundError: If dataset cannot be found
    """
    # Try multiple possible paths for the dataset
    base_paths = [
        Path.cwd(),  # Current working directory
        Path(__file__).parent.parent,  # evaluation/
        Path(__file__).parent.parent.parent,  # root
    ]

    dataset_paths = []
    for base in base_paths:
        dataset_paths.extend([
            base / "evaluation" / "dataset" / "summarize_eval" / dataset_name,  # Add summarize_eval subdir
            base / "evaluation" / "dataset" / dataset_name,
            base / "dataset" / dataset_name,
            base / dataset_name
        ])

    for path in dataset_paths:
        try:
            with open(path, 'r') as f:
                dataset = json.load(f)
                logger.info(f"Successfully loaded dataset from: {path}")
                return dataset
        except FileNotFoundError:
            continue

    raise FileNotFoundError(f"Could not find {dataset_name} in any of the expected locations: {dataset_paths}")


def find_dataset_entry(dataset: List[Dict[str, Any]],
                      entry_id: str,
                      id_fields: List[str] = None) -> Optional[Dict[str, Any]]:
    """
    Find a specific entry in the evaluation dataset.

    Args:
        dataset: The loaded dataset
        entry_id: The ID to search for
        id_fields: List of field names to check for the ID (default: ['id', 'question'])

    Returns:
        The matching dataset entry or None
    """
    if id_fields is None:
        id_fields = ['id', 'question']

    for item in dataset:
        for field in id_fields:
            if item.get(field) == entry_id:
                return item

    return None


def create_results_directory(output_dir: str = "./evaluation/results") -> Path:
    """
    Create results directory with timestamp.

    Args:
        output_dir: Base output directory

    Returns:
        Path to the created results directory
    """
    from datetime import datetime

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    results_dir = Path(output_dir) / f"run_{timestamp}"
    results_dir.mkdir(parents=True, exist_ok=True)

    logger.info(f"Created results directory: {results_dir}")
    return results_dir


def save_evaluation_results(results: Dict[str, Any],
                          output_file: str,
                          results_dir: Optional[Path] = None) -> Path:
    """
    Save evaluation results to JSON file.

    Args:
        results: Results dictionary to save
        output_file: Output filename
        results_dir: Results directory (created if None)

    Returns:
        Path to the saved file
    """
    if results_dir is None:
        results_dir = create_results_directory()

    output_path = results_dir / output_file

    with open(output_path, 'w') as f:
        json.dump(results, f, indent=2)

    logger.info(f"Saved evaluation results to: {output_path}")
    return output_path


def validate_dataset_structure(dataset: List[Dict[str, Any]],
                             required_fields: List[str]) -> bool:
    """
    Validate that dataset has required structure.

    Args:
        dataset: The dataset to validate
        required_fields: List of required field names

    Returns:
        True if dataset is valid

    Raises:
        ValueError: If dataset structure is invalid
    """
    if not dataset:
        raise ValueError("Dataset is empty")

    for i, entry in enumerate(dataset):
        missing_fields = [field for field in required_fields if field not in entry]
        if missing_fields:
            raise ValueError(f"Entry {i} missing required fields: {missing_fields}")

    logger.info(f"Dataset validated with {len(dataset)} entries")
    return True