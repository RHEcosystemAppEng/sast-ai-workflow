#!/usr/bin/env python3
"""
Script to calculate evaluation metrics (precision, recall, F1 score, accuracy)
from workflow output files containing investigation results.
"""

import json
import os
from typing import Dict, List, Any
from collections import defaultdict


def extract_investigation_result(generated_answer: str) -> str:
    """
    Extract investigation_result from the generated_answer JSON string.

    Args:
        generated_answer: JSON string containing the investigation result

    Returns:
        The investigation_result value or None if not found
    """
    try:
        # Parse the JSON
        data = json.loads(generated_answer)

        # Look for investigation_result in the top level or nested structure
        if isinstance(data, dict):
            # Check if it's directly in the data
            if "investigation_result" in data:
                return data["investigation_result"]

            # Check if it's nested under an ID key
            for key, value in data.items():
                if isinstance(value, dict) and "investigation_result" in value:
                    return value["investigation_result"]

        return None
    except (json.JSONDecodeError, KeyError, TypeError):
        return None


def calculate_metrics(predictions: List[str], ground_truth: List[str]) -> Dict[str, float]:
    """
    Calculate precision, recall, F1 score, and accuracy from predictions and ground truth.

    Args:
        predictions: List of predicted classifications ("TRUE POSITIVE" or "FALSE POSITIVE")
        ground_truth: List of ground truth classifications ("TRUE POSITIVE" or "FALSE POSITIVE")

    Returns:
        Dictionary containing calculated metrics
    """
    if len(predictions) != len(ground_truth):
        raise ValueError("Predictions and ground truth must have the same length")

    # Count true positives, false positives, true negatives, false negatives
    tp = sum(1 for p, g in zip(predictions, ground_truth)
             if p == "TRUE POSITIVE" and g == "TRUE POSITIVE")
    fp = sum(1 for p, g in zip(predictions, ground_truth)
             if p == "TRUE POSITIVE" and g == "FALSE POSITIVE")
    tn = sum(1 for p, g in zip(predictions, ground_truth)
             if p == "FALSE POSITIVE" and g == "FALSE POSITIVE")
    fn = sum(1 for p, g in zip(predictions, ground_truth)
             if p == "FALSE POSITIVE" and g == "TRUE POSITIVE")

    total = len(predictions)

    # Calculate metrics
    precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
    recall = tp / (tp + fn) if (tp + fn) > 0 else 0.0
    f1_score = 2 * (precision * recall) / (precision + recall) if (precision + recall) > 0 else 0.0
    accuracy = (tp + tn) / total if total > 0 else 0.0

    return {
        "precision": precision,
        "recall": recall,
        "f1_score": f1_score,
        "accuracy": accuracy,
        "total_items": total,
        "true_positives": tp,
        "false_positives": fp,
        "true_negatives": tn,
        "false_negatives": fn
    }


def process_workflow_output(file_path: str) -> Dict[str, Any]:
    """
    Process a single workflow output file and extract predictions and ground truth.

    Args:
        file_path: Path to the workflow_output.json file

    Returns:
        Dictionary containing predictions, ground truth, and metadata
    """
    try:
        with open(file_path, 'r') as f:
            data = json.load(f)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"Error reading {file_path}: {e}")
        return {}

    predictions = []
    ground_truth = []
    item_ids = []

    for item in data:
        if not isinstance(item, dict):
            continue

        item_id = item.get("id")
        expected_result = item.get("expected_investigation_result")
        generated_answer = item.get("generated_answer", "")

        if not item_id or not expected_result:
            continue

        # Extract the predicted result from generated_answer
        predicted_result = extract_investigation_result(generated_answer)

        if predicted_result and predicted_result in ["TRUE POSITIVE", "FALSE POSITIVE"]:
            predictions.append(predicted_result)
            ground_truth.append(expected_result)
            item_ids.append(item_id)

    return {
        "predictions": predictions,
        "ground_truth": ground_truth,
        "item_ids": item_ids,
        "file_path": file_path
    }


def calculate_metrics_from_workflow(workflow_output_file: str) -> Dict[str, Any]:
    """
    Calculate evaluation metrics from a workflow output file.

    Args:
        workflow_output_file: Path to the workflow_output.json file

    Returns:
        Dictionary containing calculated metrics and metadata
    """
    # Process the workflow output file
    processed_data = process_workflow_output(workflow_output_file)

    if not processed_data or not processed_data["predictions"]:
        return {
            "error": f"No valid data found in {workflow_output_file}",
            "metrics": {},
            "metadata": {
                "total_items": 0,
                "processed_items": 0,
                "file_path": workflow_output_file
            }
        }

    # Calculate metrics
    metrics = calculate_metrics(processed_data["predictions"], processed_data["ground_truth"])

    # Create detailed results
    results = {
        "metrics": metrics,
        "metadata": {
            "total_items": len(processed_data["item_ids"]),
            "processed_items": len(processed_data["predictions"]),
            "file_path": workflow_output_file,
            "item_ids": processed_data["item_ids"]
        }
    }

    return results


def main():
    """Main function for standalone execution."""
    import sys

    if len(sys.argv) != 2:
        print("Usage: python calculate_eval_metrics.py <workflow_output.json>")
        sys.exit(1)

    workflow_file = sys.argv[1]

    if not os.path.exists(workflow_file):
        print(f"Error: File {workflow_file} does not exist")
        sys.exit(1)

    results = calculate_metrics_from_workflow(workflow_file)

    if "error" in results:
        print(f"Error: {results['error']}")
        sys.exit(1)

    print("Evaluation Metrics Results:")
    print("=" * 50)

    metrics = results["metrics"]
    metadata = results["metadata"]

    print(f"Total Items: {metadata['total_items']}")
    print(f"Processed Items: {metadata['processed_items']}")
    print(f"File: {metadata['file_path']}")
    print()

    print("Classification Metrics:")
    print(f"  Accuracy:  {metrics['accuracy']:.4f}")
    print(f"  Precision: {metrics['precision']:.4f}")
    print(f"  Recall:    {metrics['recall']:.4f}")
    print(f"  F1 Score:  {metrics['f1_score']:.4f}")
    print()

    print("Confusion Matrix:")
    print(f"  True Positives:  {metrics['true_positives']}")
    print(f"  False Positives: {metrics['false_positives']}")
    print(f"  True Negatives:  {metrics['true_negatives']}")
    print(f"  False Negatives: {metrics['false_negatives']}")


if __name__ == "__main__":
    main()