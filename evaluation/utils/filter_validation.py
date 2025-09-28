#!/usr/bin/env python3
"""
Filter Evaluation Validation Script

This script validates filter evaluation results against ground truth data.
It checks:
1. Filter decision accuracy (FALSE_POSITIVE vs TRUE_POSITIVE against GT)
2. FAISS similarity matching correctness
3. Known false positive identification accuracy
4. Comprehensive metrics and statistics

Usage:
    python evaluation/utils/filter_validation.py [results_dir] [dataset_file]
"""

import json
import logging
import os
import sys
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Tuple, Optional

project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class FilterValidationReport:
    """Comprehensive validation report for filter evaluation results."""

    def __init__(self):
        self.report_timestamp = datetime.now().isoformat()
        self.summary = {
            "total_test_cases": 0,
            "total_issues": 0,
            "classification_accuracy": 0.0,
            "faiss_matching_accuracy": 0.0,
            "overall_score": 0.0
        }
        self.detailed_results = {}
        self.faiss_validation = {
            "correct_matches_found": 0,
            "incorrect_matches_found": 0,
            "missing_expected_matches": 0,
            "unexpected_matches": 0,
            "total_expected_matches": 0,
            "total_no_match_items": 0,
            "correct_no_matches": 0
        }
        self.classification_metrics = {
            "true_positives": 0,  # Correctly identified as TP
            "true_negatives": 0,  # Correctly identified as FP
            "false_positives": 0, # Incorrectly identified as TP (should be FP)
            "false_negatives": 0, # Incorrectly identified as FP (should be TP)
            "precision": 0.0,
            "recall": 0.0,
            "f1_score": 0.0
        }
        self.errors = []

    def to_dict(self) -> Dict[str, Any]:
        """Convert report to dictionary for JSON serialization."""
        return {
            "report_timestamp": self.report_timestamp,
            "summary": self.summary,
            "detailed_results": self.detailed_results,
            "faiss_validation": self.faiss_validation,
            "classification_metrics": self.classification_metrics,
            "errors": self.errors
        }


def load_ground_truth_dataset(dataset_path: Path) -> List[Dict[str, Any]]:
    """Load the ground truth dataset."""
    try:
        with open(dataset_path, 'r') as f:
            dataset = json.load(f)
        logger.info(f"Loaded ground truth dataset with {len(dataset)} test cases")
        return dataset
    except Exception as e:
        logger.error(f"Failed to load ground truth dataset: {e}")
        return []


def load_evaluation_results(results_dir: Path) -> Dict[str, Any]:
    """Load evaluation results from workflow output."""
    workflow_output_path = results_dir / "workflow_output.json"

    try:
        with open(workflow_output_path, 'r') as f:
            results = json.load(f)
        logger.info(f"Loaded evaluation results from {workflow_output_path}")
        return results
    except Exception as e:
        logger.error(f"Failed to load evaluation results: {e}")
        return {}


def extract_expected_faiss_matches(gt_issue: Dict[str, Any]) -> List[str]:
    """Extract expected FAISS matches from ground truth issue."""
    expected_output = gt_issue.get("expected_output_obj", {})
    package_analysis = expected_output.get("package_analysis", {})

    expected_matches = []
    for issue_id, analysis in package_analysis.items():
        similar_issues = analysis.get("similar_known_issues", [])
        if similar_issues:
            expected_matches.extend(similar_issues)

    return expected_matches


def validate_faiss_matching(
    actual_results: Dict[str, Any],
    expected_matches: List[str],
    issue_id: str
) -> Dict[str, Any]:
    """Validate FAISS similarity matching results."""
    actual_matches = actual_results.get("similar_known_issues", [])

    validation = {
        "expected_matches": expected_matches,
        "actual_matches": actual_matches,
        "correct_matches": [],
        "missing_matches": [],
        "unexpected_matches": [],
        "accuracy": 0.0
    }

    validation["correct_matches"] = list(set(expected_matches) & set(actual_matches))

    validation["missing_matches"] = list(set(expected_matches) - set(actual_matches))

    validation["unexpected_matches"] = list(set(actual_matches) - set(expected_matches))

    if expected_matches:
        correct_count = len(validation["correct_matches"])
        total_expected = len(expected_matches)
        validation["accuracy"] = correct_count / total_expected if total_expected > 0 else 0.0
    else:
        validation["accuracy"] = 1.0 if not actual_matches else 0.0

    return validation


def validate_classification_decision(
    actual_result: str,
    expected_result: str
) -> Tuple[bool, str]:
    """Validate filter classification decision."""
    is_correct = actual_result.upper() == expected_result.upper()

    if is_correct:
        decision_type = "correct"
    else:
        decision_type = f"incorrect (got {actual_result}, expected {expected_result})"

    return is_correct, decision_type


def analyze_test_case(
    test_case: Dict[str, Any],
    evaluation_results: Dict[str, Any],
    report: FilterValidationReport
) -> None:
    """Analyze a single test case and update the validation report."""
    test_id = test_case.get("id", "unknown")
    logger.info(f"Analyzing test case: {test_id}")

    eval_result = None
    if isinstance(evaluation_results, list):
        results_list = evaluation_results
    else:
        results_list = evaluation_results.get("results", [])

    for result in results_list:
        if result.get("id") == test_id:
            eval_result = result
            break

    if not eval_result:
        error_msg = f"No evaluation result found for test case {test_id}"
        logger.error(error_msg)
        report.errors.append(error_msg)
        return

    generated_answer_str = eval_result.get("generated_answer", "{}")
    try:
        generated_answer = json.loads(generated_answer_str) if isinstance(generated_answer_str, str) else generated_answer_str
    except json.JSONDecodeError as e:
        error_msg = f"Failed to parse generated answer for {test_id}: {e}"
        logger.error(error_msg)
        report.errors.append(error_msg)
        return

    gt_package_analysis = test_case.get("expected_output_obj", {}).get("package_analysis", {})
    gen_package_analysis = generated_answer.get("package_analysis", {})

    test_case_results = {
        "test_id": test_id,
        "issues": {},
        "classification_accuracy": 0.0,
        "faiss_accuracy": 0.0,
        "overall_accuracy": 0.0
    }

    total_issues = 0
    correct_classifications = 0
    correct_faiss_results = 0

    for issue_id in gt_package_analysis.keys():
        total_issues += 1
        report.summary["total_issues"] += 1

        gt_analysis = gt_package_analysis[issue_id]
        gen_analysis = gen_package_analysis.get(issue_id, {})

        expected_classification = gt_analysis.get("filter_result", "")
        actual_classification = gen_analysis.get("filter_result", "")

        classification_correct, classification_type = validate_classification_decision(
            actual_classification, expected_classification
        )

        if classification_correct:
            correct_classifications += 1

            if expected_classification.upper() == "TRUE_POSITIVE":
                report.classification_metrics["true_positives"] += 1
            else:  # FALSE_POSITIVE
                report.classification_metrics["true_negatives"] += 1
        else:
            if expected_classification.upper() == "TRUE_POSITIVE":
                report.classification_metrics["false_negatives"] += 1
            else:  # FALSE_POSITIVE
                report.classification_metrics["false_positives"] += 1

        expected_similar_issues = gt_analysis.get("similar_known_issues", [])
        faiss_validation = validate_faiss_matching(
            gen_analysis, expected_similar_issues, issue_id
        )

        if faiss_validation["accuracy"] == 1.0:
            correct_faiss_results += 1

        if expected_similar_issues:
            report.faiss_validation["total_expected_matches"] += len(expected_similar_issues)
            report.faiss_validation["correct_matches_found"] += len(faiss_validation["correct_matches"])
            report.faiss_validation["missing_expected_matches"] += len(faiss_validation["missing_matches"])
        else:
            report.faiss_validation["total_no_match_items"] += 1
            if faiss_validation["accuracy"] == 1.0:
                report.faiss_validation["correct_no_matches"] += 1

        report.faiss_validation["unexpected_matches"] += len(faiss_validation["unexpected_matches"])

        issue_result = {
            "classification": {
                "expected": expected_classification,
                "actual": actual_classification,
                "correct": classification_correct
            }
        }

        if expected_similar_issues or faiss_validation["accuracy"] < 1.0:
            issue_result["faiss_matching"] = {
                "expected_matches": expected_similar_issues,
                "actual_matches": faiss_validation["actual_matches"],
                "accuracy": faiss_validation["accuracy"]
            }
            if faiss_validation["missing_matches"]:
                issue_result["faiss_matching"]["missing_matches"] = faiss_validation["missing_matches"]
            if faiss_validation["unexpected_matches"]:
                issue_result["faiss_matching"]["unexpected_matches"] = faiss_validation["unexpected_matches"]

        if not classification_correct or expected_similar_issues:
            issue_result["justification"] = gen_analysis.get("justification", "")

        test_case_results["issues"][issue_id] = issue_result

    if total_issues > 0:
        test_case_results["classification_accuracy"] = correct_classifications / total_issues
        test_case_results["faiss_accuracy"] = correct_faiss_results / total_issues
        test_case_results["overall_accuracy"] = (
            test_case_results["classification_accuracy"] + test_case_results["faiss_accuracy"]
        ) / 2

    report.detailed_results[test_id] = test_case_results
    logger.info(f"Test case {test_id}: {correct_classifications}/{total_issues} correct classifications, "
               f"{correct_faiss_results}/{total_issues} correct FAISS results")


def calculate_final_metrics(report: FilterValidationReport) -> None:
    """Calculate final summary metrics for the validation report."""
    # Calculate overall accuracy metrics
    total_test_cases = len(report.detailed_results)
    total_issues = report.summary["total_issues"]

    if total_test_cases > 0:
        report.summary["total_test_cases"] = total_test_cases

        tp = report.classification_metrics["true_positives"]
        tn = report.classification_metrics["true_negatives"]
        fp = report.classification_metrics["false_positives"]
        fn = report.classification_metrics["false_negatives"]

        total_classifications = tp + tn + fp + fn
        if total_classifications > 0:
            report.summary["classification_accuracy"] = (tp + tn) / total_classifications

            if (tp + fp) > 0:
                report.classification_metrics["precision"] = tp / (tp + fp)
            if (tp + fn) > 0:
                report.classification_metrics["recall"] = tp / (tp + fn)

            precision = report.classification_metrics["precision"]
            recall = report.classification_metrics["recall"]
            if (precision + recall) > 0:
                report.classification_metrics["f1_score"] = 2 * (precision * recall) / (precision + recall)

        total_expected_matches = report.faiss_validation["total_expected_matches"]
        correct_matches = report.faiss_validation["correct_matches_found"]
        correct_no_matches = report.faiss_validation["correct_no_matches"]
        total_no_match_items = report.faiss_validation["total_no_match_items"]

        total_faiss_checks = total_expected_matches + total_no_match_items
        if total_faiss_checks > 0:
            correct_faiss_total = correct_matches + correct_no_matches
            report.summary["faiss_matching_accuracy"] = correct_faiss_total / total_faiss_checks

        report.summary["overall_score"] = (
            report.summary["classification_accuracy"] + report.summary["faiss_matching_accuracy"]
        ) / 2


def main():
    """Main validation function."""
    if len(sys.argv) >= 2:
        results_dir = Path(sys.argv[1])
    else:
        results_dir = project_root / "evaluation" / "reports" / "filter"

    if len(sys.argv) >= 3:
        dataset_file = Path(sys.argv[2])
    else:
        dataset_file = project_root / "evaluation" / "dataset" / "filter_eval" / "filter_eval_dataset.json"

    logger.info("=" * 80)
    logger.info("Filter Evaluation Validation Report")
    logger.info("=" * 80)
    logger.info(f"Results directory: {results_dir}")
    logger.info(f"Dataset file: {dataset_file}")

    if not results_dir.exists():
        logger.error(f"Results directory does not exist: {results_dir}")
        sys.exit(1)

    if not dataset_file.exists():
        logger.error(f"Dataset file does not exist: {dataset_file}")
        sys.exit(1)

    ground_truth_dataset = load_ground_truth_dataset(dataset_file)
    evaluation_results = load_evaluation_results(results_dir)

    if not ground_truth_dataset:
        logger.error("Failed to load ground truth dataset")
        sys.exit(1)

    if not evaluation_results:
        logger.error("Failed to load evaluation results")
        sys.exit(1)

    report = FilterValidationReport()

    for test_case in ground_truth_dataset:
        analyze_test_case(test_case, evaluation_results, report)

    calculate_final_metrics(report)

    logger.info("\n" + "=" * 80)
    logger.info("VALIDATION SUMMARY")
    logger.info("=" * 80)
    logger.info(f"Total test cases analyzed: {report.summary['total_test_cases']}")
    logger.info(f"Total issues analyzed: {report.summary['total_issues']}")
    logger.info(f"Classification accuracy: {report.summary['classification_accuracy']:.3f}")
    logger.info(f"FAISS matching accuracy: {report.summary['faiss_matching_accuracy']:.3f}")
    logger.info(f"Overall score: {report.summary['overall_score']:.3f}")

    cm = report.classification_metrics
    logger.info(f"\nClassification Metrics:")
    logger.info(f"  True Positives: {cm['true_positives']}")
    logger.info(f"  True Negatives: {cm['true_negatives']}")
    logger.info(f"  False Positives: {cm['false_positives']}")
    logger.info(f"  False Negatives: {cm['false_negatives']}")
    logger.info(f"  Precision: {cm['precision']:.3f}")
    logger.info(f"  Recall: {cm['recall']:.3f}")
    logger.info(f"  F1 Score: {cm['f1_score']:.3f}")

    fv = report.faiss_validation
    logger.info(f"\nFAISS Matching Metrics:")
    logger.info(f"  Expected matches found: {fv['correct_matches_found']}/{fv['total_expected_matches']}")
    logger.info(f"  Correct no-matches: {fv['correct_no_matches']}/{fv['total_no_match_items']}")
    logger.info(f"  Missing expected matches: {fv['missing_expected_matches']}")
    logger.info(f"  Unexpected matches: {fv['unexpected_matches']}")

    if report.errors:
        logger.warning(f"\nErrors encountered: {len(report.errors)}")
        for error in report.errors:
            logger.warning(f"  - {error}")

    report_path = results_dir / "filter_validation_report.json"
    try:
        with open(report_path, 'w') as f:
            json.dump(report.to_dict(), f, indent=2)
        logger.info(f"\nValidation report saved to: {report_path}")
    except Exception as e:
        logger.error(f"Failed to save validation report: {e}")
        sys.exit(1)

    logger.info("Validation completed successfully!")
    return report


if __name__ == "__main__":
    main()