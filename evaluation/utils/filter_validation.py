#!/usr/bin/env python3
"""
Filter Evaluation Validation Script - FAISS Matching Performance

This script validates FAISS matching performance against ground truth expectations.
It stratifies results by whether matches were expected or not.

Usage:
    python evaluation/utils/filter_validation.py [results_dir] [dataset_file]
"""

import json
import logging
import sys
from pathlib import Path

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from evaluation.constants import (
    WORKFLOW_OUTPUT_FILENAME,
    FILTER_VALIDATION_REPORT_FILENAME,
    REPORTS_FILTER_DIR,
    DATASET_FILTER_DIR,
    FILTER_DATASET_FILENAME
)

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


def main():
    """Main validation function."""
    if len(sys.argv) >= 2:
        results_dir = Path(sys.argv[1])
    else:
        results_dir = project_root / REPORTS_FILTER_DIR

    if len(sys.argv) >= 3:
        dataset_file = Path(sys.argv[2])
    else:
        dataset_file = project_root / DATASET_FILTER_DIR / FILTER_DATASET_FILENAME

    logger.info("=" * 80)
    logger.info("Filter FAISS Matching Performance Validation")
    logger.info("=" * 80)
    logger.info(f"Results directory: {results_dir}")
    logger.info(f"Dataset file: {dataset_file}")

    if not results_dir.exists():
        logger.error(f"Results directory does not exist: {results_dir}")
        sys.exit(1)

    if not dataset_file.exists():
        logger.error(f"Dataset file does not exist: {dataset_file}")
        sys.exit(1)

    # Load dataset
    try:
        with open(dataset_file, 'r') as f:
            dataset = json.load(f)
        logger.info(f"Loaded {len(dataset)} test cases from dataset")
    except Exception as e:
        logger.error(f"Failed to load dataset: {e}")
        sys.exit(1)

    # Load workflow results
    workflow_output_path = results_dir / WORKFLOW_OUTPUT_FILENAME
    try:
        with open(workflow_output_path, 'r') as f:
            workflow_results = json.load(f)
        logger.info(f"Loaded {len(workflow_results)} results from workflow output")
    except Exception as e:
        logger.error(f"Failed to load workflow results: {e}")
        sys.exit(1)

    # Build lookup from workflow results
    results_by_id = {}
    for result in workflow_results:
        test_id = result.get('id')
        if test_id:
            results_by_id[test_id] = result

    # Stratify by has_expected_matches
    with_expected = {'total': 0, 'faiss_found_matches': 0}
    without_expected = {'total': 0, 'faiss_found_matches': 0}

    for test_case in dataset:
        test_id = test_case.get('id')
        expected_output_obj = test_case.get('expected_output_obj', {})
        has_expected_matches = expected_output_obj.get('has_expected_matches', False)

        # Get generated result
        result = results_by_id.get(test_id)
        if not result:
            logger.warning(f"No result found for test case: {test_id}")
            continue

        # Parse generated answer
        generated_answer_str = result.get('generated_answer', '{}')
        try:
            generated_answer = json.loads(generated_answer_str)
        except json.JSONDecodeError:
            logger.warning(f"Failed to parse generated answer for {test_id}")
            generated_answer = {}

        # Check if FAISS found matches
        similar_known_issues = generated_answer.get('similar_known_issues', [])
        faiss_found_matches = len(similar_known_issues) > 0

        # Stratify
        if has_expected_matches:
            with_expected['total'] += 1
            if faiss_found_matches:
                with_expected['faiss_found_matches'] += 1
        else:
            without_expected['total'] += 1
            if faiss_found_matches:
                without_expected['faiss_found_matches'] += 1

    # Calculate perc_correct
    if with_expected['total'] > 0:
        with_expected['perc_correct'] = with_expected['faiss_found_matches'] / with_expected['total']
    else:
        with_expected['perc_correct'] = None

    if without_expected['total'] > 0:
        # For without_expected, correct means FAISS did NOT find matches
        without_expected['perc_correct'] = (without_expected['total'] - without_expected['faiss_found_matches']) / without_expected['total']
    else:
        without_expected['perc_correct'] = None

    # Build final report
    report = {
        "faiss_stratified_stats": {
            "with_expected_matches": with_expected,
            "without_expected_matches": without_expected
        }
    }

    # Write report
    output_path = results_dir / FILTER_VALIDATION_REPORT_FILENAME
    with open(output_path, 'w') as f:
        json.dump(report, f, indent=2)

    logger.info("\n" + "=" * 80)
    logger.info("FAISS MATCHING PERFORMANCE")
    logger.info("=" * 80)
    logger.info(f"\nWith expected matches (has_expected_matches=true):")
    logger.info(f"  Total: {with_expected['total']}")
    logger.info(f"  FAISS found matches: {with_expected['faiss_found_matches']}")
    logger.info(f"  Percent correct: {with_expected['perc_correct']:.2%}" if with_expected['perc_correct'] is not None else "  Percent correct: N/A")

    logger.info(f"\nWithout expected matches (has_expected_matches=false):")
    logger.info(f"  Total: {without_expected['total']}")
    logger.info(f"  FAISS found matches: {without_expected['faiss_found_matches']}")
    logger.info(f"  Percent correct: {without_expected['perc_correct']:.2%}" if without_expected['perc_correct'] is not None else "  Percent correct: N/A")

    logger.info(f"\nValidation report saved to: {output_path}")


if __name__ == "__main__":
    main()