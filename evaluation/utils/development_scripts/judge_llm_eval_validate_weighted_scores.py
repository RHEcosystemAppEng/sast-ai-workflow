#!/usr/bin/env python3
"""
Script to validate that final scores in the evaluation output match the weighted scoring formula.
Formula: (clarity_score * 0.35) + (completeness_score * 0.30) + (technical_accuracy_score * 0.25) + (logical_flow_score * 0.10)
"""

import json
import sys
from pathlib import Path
import glob
import os


def calculate_weighted_score(reasoning):
    """Calculate the weighted score based on the formula from the config."""
    # Handle cases where reasoning might be a string or have different structure
    if isinstance(reasoning, str):
        return 0  # Cannot calculate from string reasoning

    # The component scores are in a nested reasoning object within the main reasoning object
    nested_reasoning = reasoning.get("reasoning", {})

    # If nested_reasoning is a string, we can't extract scores
    if isinstance(nested_reasoning, str):
        return 0

    clarity = nested_reasoning.get("CLARITY", 0)
    completeness = nested_reasoning.get("COMPLETENESS", 0)
    technical_accuracy = nested_reasoning.get("TECHNICAL_ACCURACY", 0)
    logical_flow = nested_reasoning.get("LOGICAL_FLOW", 0)

    weighted_score = (clarity * 0.35) + (completeness * 0.30) + (technical_accuracy * 0.25) + (logical_flow * 0.10)
    return weighted_score


def validate_scores(file_path):
    """Validate all scores in the evaluation output file."""
    try:
        with open(file_path, 'r') as f:
            data = json.load(f)
    except FileNotFoundError:
        print(f"Error: File {file_path} not found.")
        return False
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON in {file_path}: {e}")
        return False

    eval_items = data.get("eval_output_items", [])
    if not eval_items:
        print("No evaluation items found in the file.")
        return True

    all_valid = True
    tolerance = 1e-6  # Small tolerance for floating point comparison

    print(f"Validating {len(eval_items)} evaluation items...")
    print("=" * 80)

    for i, item in enumerate(eval_items):
        item_id = item.get("id", f"item_{i}")
        actual_score = item.get("score", 0)
        reasoning = item.get("reasoning", {})

        expected_score = calculate_weighted_score(reasoning)
        difference = abs(actual_score - expected_score)

        if difference > tolerance:
            print(f"❌ MISMATCH - ID: {item_id}")
            print(f"   Actual score: {actual_score}")
            print(f"   Expected score: {expected_score:.6f}")
            print(f"   Difference: {difference:.6f}")
            nested_reasoning = reasoning.get("reasoning", {})
            print(f"   Component scores:")
            print(f"     CLARITY: {nested_reasoning.get('CLARITY', 'N/A')}")
            print(f"     COMPLETENESS: {nested_reasoning.get('COMPLETENESS', 'N/A')}")
            print(f"     TECHNICAL_ACCURACY: {nested_reasoning.get('TECHNICAL_ACCURACY', 'N/A')}")
            print(f"     LOGICAL_FLOW: {nested_reasoning.get('LOGICAL_FLOW', 'N/A')}")
            print()
            all_valid = False
        else:
            print(f"✅ VALID - ID: {item_id} (score: {actual_score})")

    print("=" * 80)
    if all_valid:
        print("🎉 All scores are correctly calculated!")
        average_score = data.get("average_score", 0)
        calculated_average = sum(item.get("score", 0) for item in eval_items) / len(eval_items)
        avg_difference = abs(average_score - calculated_average)

        if avg_difference > tolerance:
            print(f"⚠️  WARNING: Average score mismatch!")
            print(f"   Reported average: {average_score}")
            print(f"   Calculated average: {calculated_average:.6f}")
            print(f"   Difference: {avg_difference:.6f}")
        else:
            print(f"✅ Average score is also correct: {average_score}")
    else:
        print("❌ Some scores have discrepancies!")

    return all_valid


def find_latest_run_folder():
    """Find the latest timestamped run folder that contains evaluation output."""
    base_dir = "evaluation/reports/judge_llm_analysis"
    pattern = os.path.join(base_dir, "run_*")
    run_folders = glob.glob(pattern)

    if not run_folders:
        return None

    # Sort by folder name (which includes timestamp) and check for evaluation output
    for folder in sorted(run_folders, reverse=True):
        eval_file = os.path.join(folder, "summarization_quality_eval_output.json")
        if os.path.exists(eval_file):
            return folder

    return None

def main():
    if len(sys.argv) > 1:
        file_path = sys.argv[1]
    else:
        # Try to find the latest run folder first
        latest_folder = find_latest_run_folder()
        if latest_folder:
            file_path = os.path.join(latest_folder, "justification_quality_eval_output.json")
        else:
            # Fallback to default path
            file_path = "evaluation/reports/judge_llm_analysis/run_20250917_120835/justification_quality_eval_output.json"

    print(f"Validating weighted scores in: {file_path}")
    print(f"Using formula: (CLARITY * 0.35) + (COMPLETENESS * 0.30) + (TECHNICAL_ACCURACY * 0.25) + (LOGICAL_FLOW * 0.10)")
    print()

    is_valid = validate_scores(file_path)
    sys.exit(0 if is_valid else 1)


if __name__ == "__main__":
    main()