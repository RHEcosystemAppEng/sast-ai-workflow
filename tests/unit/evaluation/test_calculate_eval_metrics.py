#!/usr/bin/env python3
"""
Unit tests for calculate_eval_metrics.py

Tests cover:
- Metrics calculation (precision, recall, F1, accuracy)
- Confusion matrix computation
- Length mismatch handling
- Investigation result extraction
- Workflow output processing
- Empty/invalid data handling
"""

import json
import os
import tempfile
import unittest
from pathlib import Path
from typing import Dict, List

# Add project root to path
import sys
project_root = Path(__file__).parent.parent.parent.parent
sys.path.insert(0, str(project_root))

from evaluation.utils.calculate_eval_metrics import (
    extract_investigation_result,
    calculate_metrics,
    process_workflow_output,
    calculate_metrics_from_workflow
)
from evaluation.constants import (
    CLASSIFICATION_TRUE_POSITIVE,
    CLASSIFICATION_FALSE_POSITIVE
)


class TestExtractInvestigationResult(unittest.TestCase):
    """Test cases for extract_investigation_result function."""

    def test_flat_dict_format(self):
        """Test extraction from flat dictionary format."""
        generated_answer = json.dumps({
            "investigation_result": "TRUE_POSITIVE"
        })
        result = extract_investigation_result(generated_answer)
        self.assertEqual(result, "TRUE_POSITIVE")

    def test_nested_dict_format(self):
        """Test extraction from nested dictionary format."""
        generated_answer = json.dumps({
            "analysis": {
                "investigation_result": "FALSE_POSITIVE"
            }
        })
        result = extract_investigation_result(generated_answer)
        self.assertEqual(result, "FALSE_POSITIVE")

    def test_invalid_json(self):
        """Test handling of invalid JSON."""
        generated_answer = "not valid json"
        result = extract_investigation_result(generated_answer)
        self.assertIsNone(result)

    def test_missing_field(self):
        """Test handling of missing investigation_result field."""
        generated_answer = json.dumps({
            "some_other_field": "value"
        })
        result = extract_investigation_result(generated_answer)
        self.assertIsNone(result)

    def test_empty_string(self):
        """Test handling of empty string."""
        result = extract_investigation_result("")
        self.assertIsNone(result)

    def test_none_input(self):
        """Test handling of None input."""
        result = extract_investigation_result(None)
        self.assertIsNone(result)


class TestCalculateMetrics(unittest.TestCase):
    """Test cases for calculate_metrics function."""

    def test_perfect_predictions(self):
        """Test metrics with perfect predictions."""
        predictions = [CLASSIFICATION_TRUE_POSITIVE, CLASSIFICATION_FALSE_POSITIVE, CLASSIFICATION_TRUE_POSITIVE]
        ground_truth = [CLASSIFICATION_TRUE_POSITIVE, CLASSIFICATION_FALSE_POSITIVE, CLASSIFICATION_TRUE_POSITIVE]

        metrics = calculate_metrics(predictions, ground_truth)

        self.assertEqual(metrics["accuracy"], 1.0)
        self.assertEqual(metrics["precision"], 1.0)
        self.assertEqual(metrics["recall"], 1.0)
        self.assertEqual(metrics["f1_score"], 1.0)
        self.assertEqual(metrics["total_items"], 3)
        self.assertEqual(metrics["true_positives"], 2)
        self.assertEqual(metrics["true_negatives"], 1)
        self.assertEqual(metrics["false_positives"], 0)
        self.assertEqual(metrics["false_negatives"], 0)

    def test_all_wrong_predictions(self):
        """Test metrics with all wrong predictions."""
        predictions = [CLASSIFICATION_FALSE_POSITIVE, CLASSIFICATION_TRUE_POSITIVE, CLASSIFICATION_FALSE_POSITIVE]
        ground_truth = [CLASSIFICATION_TRUE_POSITIVE, CLASSIFICATION_FALSE_POSITIVE, CLASSIFICATION_TRUE_POSITIVE]

        metrics = calculate_metrics(predictions, ground_truth)

        self.assertEqual(metrics["accuracy"], 0.0)
        self.assertEqual(metrics["true_positives"], 0)
        self.assertEqual(metrics["true_negatives"], 0)
        self.assertEqual(metrics["false_positives"], 1)
        self.assertEqual(metrics["false_negatives"], 2)

    def test_mixed_predictions(self):
        """Test metrics with mixed predictions."""
        predictions = [
            CLASSIFICATION_TRUE_POSITIVE,   # TP (correct)
            CLASSIFICATION_TRUE_POSITIVE,   # FP (wrong - should be FP)
            CLASSIFICATION_FALSE_POSITIVE,  # TN (correct)
            CLASSIFICATION_FALSE_POSITIVE,  # FN (wrong - should be TP)
        ]
        ground_truth = [
            CLASSIFICATION_TRUE_POSITIVE,
            CLASSIFICATION_FALSE_POSITIVE,
            CLASSIFICATION_FALSE_POSITIVE,
            CLASSIFICATION_TRUE_POSITIVE,
        ]

        metrics = calculate_metrics(predictions, ground_truth)

        self.assertEqual(metrics["total_items"], 4)
        self.assertEqual(metrics["true_positives"], 1)
        self.assertEqual(metrics["false_positives"], 1)
        self.assertEqual(metrics["true_negatives"], 1)
        self.assertEqual(metrics["false_negatives"], 1)
        self.assertEqual(metrics["accuracy"], 0.5)

        # Precision = TP / (TP + FP) = 1 / (1 + 1) = 0.5
        self.assertEqual(metrics["precision"], 0.5)

        # Recall = TP / (TP + FN) = 1 / (1 + 1) = 0.5
        self.assertEqual(metrics["recall"], 0.5)

        # F1 = 2 * (P * R) / (P + R) = 2 * (0.5 * 0.5) / (0.5 + 0.5) = 0.5
        self.assertEqual(metrics["f1_score"], 0.5)

    def test_length_mismatch(self):
        """Test handling of length mismatch (non-blocking)."""
        predictions = ["TRUE POSITIVE", "FALSE POSITIVE"]
        ground_truth = ["TRUE POSITIVE"]

        metrics = calculate_metrics(predictions, ground_truth)

        # Should return zero metrics with same format
        self.assertEqual(metrics["precision"], 0.0)
        self.assertEqual(metrics["recall"], 0.0)
        self.assertEqual(metrics["f1_score"], 0.0)
        self.assertEqual(metrics["accuracy"], 0.0)
        self.assertEqual(metrics["total_items"], 0)
        self.assertEqual(metrics["true_positives"], 0)
        self.assertEqual(metrics["false_positives"], 0)
        self.assertEqual(metrics["true_negatives"], 0)
        self.assertEqual(metrics["false_negatives"], 0)

    def test_empty_lists(self):
        """Test handling of empty lists."""
        predictions = []
        ground_truth = []

        metrics = calculate_metrics(predictions, ground_truth)

        self.assertEqual(metrics["accuracy"], 0.0)
        self.assertEqual(metrics["total_items"], 0)

    def test_all_true_positives(self):
        """Test case with only true positives."""
        predictions = [CLASSIFICATION_TRUE_POSITIVE, CLASSIFICATION_TRUE_POSITIVE, CLASSIFICATION_TRUE_POSITIVE]
        ground_truth = [CLASSIFICATION_TRUE_POSITIVE, CLASSIFICATION_TRUE_POSITIVE, CLASSIFICATION_TRUE_POSITIVE]

        metrics = calculate_metrics(predictions, ground_truth)

        self.assertEqual(metrics["precision"], 1.0)
        self.assertEqual(metrics["recall"], 1.0)
        self.assertEqual(metrics["f1_score"], 1.0)

    def test_all_false_positives(self):
        """Test case with only false positives."""
        predictions = [CLASSIFICATION_FALSE_POSITIVE, CLASSIFICATION_FALSE_POSITIVE]
        ground_truth = [CLASSIFICATION_FALSE_POSITIVE, CLASSIFICATION_FALSE_POSITIVE]

        metrics = calculate_metrics(predictions, ground_truth)

        # When all predictions are negative, precision/recall are 0 (no TP)
        self.assertEqual(metrics["precision"], 0.0)
        self.assertEqual(metrics["recall"], 0.0)
        self.assertEqual(metrics["f1_score"], 0.0)
        self.assertEqual(metrics["accuracy"], 1.0)  # All correct negatives

    def test_zero_division_handling(self):
        """Test that zero division is handled gracefully."""
        # All predictions are FALSE POSITIVE, all ground truth are TRUE POSITIVE
        predictions = [CLASSIFICATION_FALSE_POSITIVE, CLASSIFICATION_FALSE_POSITIVE]
        ground_truth = [CLASSIFICATION_TRUE_POSITIVE, CLASSIFICATION_TRUE_POSITIVE]

        metrics = calculate_metrics(predictions, ground_truth)

        # Precision: TP/(TP+FP) = 0/(0+0) = 0 (no positive predictions)
        # Recall: TP/(TP+FN) = 0/(0+2) = 0
        self.assertEqual(metrics["precision"], 0.0)
        self.assertEqual(metrics["recall"], 0.0)
        self.assertEqual(metrics["f1_score"], 0.0)


class TestProcessWorkflowOutput(unittest.TestCase):
    """Test cases for process_workflow_output function."""

    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.test_file = os.path.join(self.temp_dir, "workflow_output.json")

    def tearDown(self):
        """Clean up test files."""
        import shutil
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)

    def test_valid_workflow_output(self):
        """Test processing valid workflow output."""
        workflow_data = [
            {
                "id": "test-1",
                "expected_investigation_result": CLASSIFICATION_TRUE_POSITIVE,
                "generated_answer": json.dumps({
                    "investigation_result": CLASSIFICATION_TRUE_POSITIVE
                })
            },
            {
                "id": "test-2",
                "expected_investigation_result": CLASSIFICATION_FALSE_POSITIVE,
                "generated_answer": json.dumps({
                    "investigation_result": CLASSIFICATION_FALSE_POSITIVE
                })
            }
        ]

        with open(self.test_file, 'w') as f:
            json.dump(workflow_data, f)

        result = process_workflow_output(self.test_file)

        self.assertEqual(len(result["predictions"]), 2)
        self.assertEqual(len(result["ground_truth"]), 2)
        self.assertEqual(len(result["item_ids"]), 2)
        self.assertEqual(result["predictions"][0], CLASSIFICATION_TRUE_POSITIVE)
        self.assertEqual(result["ground_truth"][0], CLASSIFICATION_TRUE_POSITIVE)

    def test_missing_fields(self):
        """Test handling of items with missing required fields."""
        workflow_data = [
            {
                "id": "test-1",
                "expected_investigation_result": CLASSIFICATION_TRUE_POSITIVE,
                "generated_answer": json.dumps({
                    "investigation_result": CLASSIFICATION_TRUE_POSITIVE
                })
            },
            {
                # Missing expected_investigation_result
                "id": "test-2",
                "generated_answer": json.dumps({
                    "investigation_result": CLASSIFICATION_FALSE_POSITIVE
                })
            },
            {
                # Missing id
                "expected_investigation_result": CLASSIFICATION_TRUE_POSITIVE,
                "generated_answer": json.dumps({
                    "investigation_result": CLASSIFICATION_TRUE_POSITIVE
                })
            }
        ]

        with open(self.test_file, 'w') as f:
            json.dump(workflow_data, f)

        result = process_workflow_output(self.test_file)

        # Only first item should be processed
        self.assertEqual(len(result["predictions"]), 1)

    def test_invalid_investigation_result(self):
        """Test handling of invalid investigation results."""
        workflow_data = [
            {
                "id": "test-1",
                "expected_investigation_result": "TRUE POSITIVE",
                "generated_answer": json.dumps({
                    "investigation_result": "INVALID_RESULT"
                })
            }
        ]

        with open(self.test_file, 'w') as f:
            json.dump(workflow_data, f)

        result = process_workflow_output(self.test_file)

        # Should skip items with invalid results
        self.assertEqual(len(result["predictions"]), 0)

    def test_missing_file(self):
        """Test handling of missing workflow output file."""
        result = process_workflow_output("/nonexistent/file.json")

        self.assertEqual(result, {})

    def test_invalid_json(self):
        """Test handling of invalid JSON in workflow output."""
        with open(self.test_file, 'w') as f:
            f.write("not valid json")

        result = process_workflow_output(self.test_file)

        self.assertEqual(result, {})

    def test_non_dict_items(self):
        """Test handling of non-dictionary items in workflow output."""
        workflow_data = [
            {
                "id": "test-1",
                "expected_investigation_result": CLASSIFICATION_TRUE_POSITIVE,
                "generated_answer": json.dumps({
                    "investigation_result": CLASSIFICATION_TRUE_POSITIVE
                })
            },
            "invalid_item",
            None,
            123
        ]

        with open(self.test_file, 'w') as f:
            json.dump(workflow_data, f)

        result = process_workflow_output(self.test_file)

        # Only valid dict item should be processed
        self.assertEqual(len(result["predictions"]), 1)


class TestCalculateMetricsFromWorkflow(unittest.TestCase):
    """Test cases for calculate_metrics_from_workflow function."""

    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.test_file = os.path.join(self.temp_dir, "workflow_output.json")

    def tearDown(self):
        """Clean up test files."""
        import shutil
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)

    def test_full_workflow_metrics(self):
        """Test complete metrics calculation from workflow output."""
        workflow_data = [
            {
                "id": "test-1",
                "expected_investigation_result": CLASSIFICATION_TRUE_POSITIVE,
                "generated_answer": json.dumps({
                    "investigation_result": CLASSIFICATION_TRUE_POSITIVE
                })
            },
            {
                "id": "test-2",
                "expected_investigation_result": CLASSIFICATION_FALSE_POSITIVE,
                "generated_answer": json.dumps({
                    "investigation_result": CLASSIFICATION_FALSE_POSITIVE
                })
            },
            {
                "id": "test-3",
                "expected_investigation_result": CLASSIFICATION_TRUE_POSITIVE,
                "generated_answer": json.dumps({
                    "investigation_result": CLASSIFICATION_FALSE_POSITIVE
                })
            }
        ]

        with open(self.test_file, 'w') as f:
            json.dump(workflow_data, f)

        results = calculate_metrics_from_workflow(self.test_file)

        self.assertIn("metrics", results)
        self.assertIn("metadata", results)
        self.assertNotIn("error", results)

        metrics = results["metrics"]
        metadata = results["metadata"]

        self.assertEqual(metadata["total_items"], 3)
        self.assertEqual(metadata["processed_items"], 3)
        self.assertEqual(len(metadata["item_ids"]), 3)

        # Check metrics structure
        self.assertIn("precision", metrics)
        self.assertIn("recall", metrics)
        self.assertIn("f1_score", metrics)
        self.assertIn("accuracy", metrics)

    def test_no_valid_data(self):
        """Test handling of workflow output with no valid data."""
        workflow_data = []

        with open(self.test_file, 'w') as f:
            json.dump(workflow_data, f)

        results = calculate_metrics_from_workflow(self.test_file)

        self.assertIn("error", results)
        self.assertEqual(results["metadata"]["total_items"], 0)
        self.assertEqual(results["metadata"]["processed_items"], 0)

    def test_missing_workflow_file(self):
        """Test handling of missing workflow output file."""
        results = calculate_metrics_from_workflow("/nonexistent/file.json")

        self.assertIn("error", results)
        self.assertEqual(results["metadata"]["total_items"], 0)


class TestMetricsEdgeCases(unittest.TestCase):
    """Test cases for edge cases in metrics calculation."""

    def test_single_item_perfect(self):
        """Test metrics with single perfect prediction."""
        predictions = [CLASSIFICATION_TRUE_POSITIVE]
        ground_truth = [CLASSIFICATION_TRUE_POSITIVE]

        metrics = calculate_metrics(predictions, ground_truth)

        self.assertEqual(metrics["accuracy"], 1.0)
        self.assertEqual(metrics["precision"], 1.0)
        self.assertEqual(metrics["recall"], 1.0)

    def test_single_item_wrong(self):
        """Test metrics with single wrong prediction."""
        predictions = [CLASSIFICATION_FALSE_POSITIVE]
        ground_truth = [CLASSIFICATION_TRUE_POSITIVE]

        metrics = calculate_metrics(predictions, ground_truth)

        self.assertEqual(metrics["accuracy"], 0.0)
        self.assertEqual(metrics["false_negatives"], 1)

    def test_large_dataset(self):
        """Test metrics with large dataset."""
        # 100 items: 70 correct, 30 incorrect
        predictions = ([CLASSIFICATION_TRUE_POSITIVE] * 50 + [CLASSIFICATION_FALSE_POSITIVE] * 20 +
                      [CLASSIFICATION_TRUE_POSITIVE] * 10 + [CLASSIFICATION_FALSE_POSITIVE] * 20)
        ground_truth = ([CLASSIFICATION_TRUE_POSITIVE] * 50 + [CLASSIFICATION_FALSE_POSITIVE] * 20 +
                       [CLASSIFICATION_FALSE_POSITIVE] * 10 + [CLASSIFICATION_TRUE_POSITIVE] * 20)

        metrics = calculate_metrics(predictions, ground_truth)

        self.assertEqual(metrics["total_items"], 100)
        self.assertEqual(metrics["true_positives"], 50)
        self.assertEqual(metrics["true_negatives"], 20)
        self.assertEqual(metrics["false_positives"], 10)
        self.assertEqual(metrics["false_negatives"], 20)
        self.assertEqual(metrics["accuracy"], 0.7)


if __name__ == '__main__':
    unittest.main()
