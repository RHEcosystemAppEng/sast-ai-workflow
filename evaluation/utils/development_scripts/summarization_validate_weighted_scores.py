#!/usr/bin/env python3
"""
Script to validate that final scores in the summarization evaluation output match the weighted scoring formula.
Formula: (semantic_similarity * 0.35) + (factual_accuracy * 0.30) + (conciseness * 0.20) + (professional_tone * 0.15)
"""

import json
import sys
from pathlib import Path
import glob
import os
import statistics


def calculate_weighted_score(reasoning):
    """Calculate the weighted score based on the formula from the summarization config."""
    # Handle cases where reasoning might be a string or have different structure
    if isinstance(reasoning, str):
        return 0  # Cannot calculate from string reasoning

    # The component scores are in a nested reasoning object within the main reasoning object
    nested_reasoning = reasoning.get("reasoning", {})

    # If nested_reasoning is a string, we can't extract scores
    if isinstance(nested_reasoning, str):
        return 0

    semantic_similarity = nested_reasoning.get("SEMANTIC_SIMILARITY", 0)
    factual_accuracy = nested_reasoning.get("FACTUAL_ACCURACY", 0)
    conciseness = nested_reasoning.get("CONCISENESS", 0)
    professional_tone = nested_reasoning.get("PROFESSIONAL_TONE", 0)

    weighted_score = (semantic_similarity * 0.35) + (factual_accuracy * 0.30) + (conciseness * 0.20) + (professional_tone * 0.15)
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
            print(f"     SEMANTIC_SIMILARITY: {nested_reasoning.get('SEMANTIC_SIMILARITY', 'N/A')}")
            print(f"     FACTUAL_ACCURACY: {nested_reasoning.get('FACTUAL_ACCURACY', 'N/A')}")
            print(f"     CONCISENESS: {nested_reasoning.get('CONCISENESS', 'N/A')}")
            print(f"     PROFESSIONAL_TONE: {nested_reasoning.get('PROFESSIONAL_TONE', 'N/A')}")
            print()
            all_valid = False
        else:
            print(f"✅ VALID - ID: {item_id} (score: {actual_score})")

    # Calculate statistics for scores and component scores
    all_scores = [item.get("score", 0) for item in eval_items]
    semantic_scores = []
    factual_scores = []
    conciseness_scores = []
    tone_scores = []

    for item in eval_items:
        reasoning = item.get("reasoning", {})
        nested_reasoning = reasoning.get("reasoning", {})
        if isinstance(nested_reasoning, dict):
            semantic_scores.append(nested_reasoning.get("SEMANTIC_SIMILARITY", 0))
            factual_scores.append(nested_reasoning.get("FACTUAL_ACCURACY", 0))
            conciseness_scores.append(nested_reasoning.get("CONCISENESS", 0))
            tone_scores.append(nested_reasoning.get("PROFESSIONAL_TONE", 0))

    print("=" * 80)
    print("\n📊 SCORING STATISTICS:")
    print("-" * 40)

    if all_scores:
        avg_score = sum(all_scores) / len(all_scores)
        std_score = statistics.stdev(all_scores) if len(all_scores) > 1 else 0.0
        print(f"Overall Scores: (avg: {avg_score:.3f}, std: {std_score:.3f})")
        print(f"  Range: {min(all_scores):.3f} - {max(all_scores):.3f}")

    print("\nComponent Score Statistics:")
    for name, scores in [
        ("Semantic Similarity", semantic_scores),
        ("Factual Accuracy", factual_scores),
        ("Conciseness", conciseness_scores),
        ("Professional Tone", tone_scores)
    ]:
        if scores:
            avg_comp = sum(scores) / len(scores)
            std_comp = statistics.stdev(scores) if len(scores) > 1 else 0.0
            print(f"  {name}: (avg: {avg_comp:.3f}, std: {std_comp:.3f})")

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
    base_dir = "evaluation/reports/summarize_justifications"
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
            file_path = os.path.join(latest_folder, "summarization_quality_eval_output.json")
        else:
            # Fallback to default path
            file_path = "evaluation/reports/summarize_justifications/run_20250917_150720/summarization_quality_eval_output.json"

    print(f"Validating weighted scores in: {file_path}")
    print(f"Using formula: (SEMANTIC_SIMILARITY * 0.35) + (FACTUAL_ACCURACY * 0.30) + (CONCISENESS * 0.20) + (PROFESSIONAL_TONE * 0.15)")
    print()

    is_valid = validate_scores(file_path)
    sys.exit(0 if is_valid else 1)


if __name__ == "__main__":
    main()