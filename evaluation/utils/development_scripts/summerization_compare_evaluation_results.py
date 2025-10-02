#!/usr/bin/env python3
"""
Script to compare all summarization_quality_eval_output.json files under
/Users/gziv/Dev/sast-ai-workflow/evaluation/reports/summarize_justifications

This script aggregates scores and component scores for each item ID across all evaluation runs.
"""

import json
import glob
import os
from pathlib import Path
from collections import defaultdict
import statistics


def find_all_eval_files():
    """Find all summarization_quality_eval_output.json files."""
    base_dir = "/Users/gziv/Dev/sast-ai-workflow/evaluation/reports/summarize_justifications"
    pattern = os.path.join(base_dir, "**/summarization_quality_eval_output.json")
    files = glob.glob(pattern, recursive=True)
    return sorted(files)


def extract_component_scores(reasoning):
    """Extract component scores from reasoning object."""
    if isinstance(reasoning, str):
        return None

    nested_reasoning = reasoning.get("reasoning", {})
    if isinstance(nested_reasoning, str):
        return None

    return {
        "semantic_similarity": nested_reasoning.get("SEMANTIC_SIMILARITY", None),
        "factual_accuracy": nested_reasoning.get("FACTUAL_ACCURACY", None),
        "conciseness": nested_reasoning.get("CONCISENESS", None),
        "professional_tone": nested_reasoning.get("PROFESSIONAL_TONE", None)
    }


def process_eval_file(file_path):
    """Process a single evaluation file and extract data."""
    try:
        with open(file_path, 'r') as f:
            data = json.load(f)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"Error reading {file_path}: {e}")
        return {}

    eval_items = data.get("eval_output_items", [])
    file_results = {}

    for item in eval_items:
        item_id = item.get("id")
        if not item_id:
            continue

        score = item.get("score", None)
        reasoning = item.get("reasoning", {})
        component_scores = extract_component_scores(reasoning)

        file_results[item_id] = {
            "score": score,
            "component_scores": component_scores
        }

    return file_results


def compare_all_evaluations():
    """Compare all evaluation files and aggregate results."""
    eval_files = find_all_eval_files()
    if not eval_files:
        print("No evaluation files found!")
        return {}

    print(f"Found {len(eval_files)} evaluation files:")
    for file_path in eval_files:
        rel_path = os.path.relpath(file_path, "/Users/gziv/Dev/sast-ai-workflow/evaluation/reports/summarize_justifications")
        print(f"  - {rel_path}")

    # Aggregate data across all files
    aggregated_data = defaultdict(lambda: {
        "scores": [],
        "semantic_similarity": [],
        "factual_accuracy": [],
        "conciseness": [],
        "professional_tone": [],
        "files": []
    })

    for file_path in eval_files:
        rel_path = os.path.relpath(file_path, "/Users/gziv/Dev/sast-ai-workflow/evaluation/reports/summarize_justifications")
        file_results = process_eval_file(file_path)

        for item_id, item_data in file_results.items():
            entry = aggregated_data[item_id]

            # Add score
            if item_data["score"] is not None:
                entry["scores"].append(item_data["score"])
            else:
                entry["scores"].append(None)

            # Add component scores
            comp_scores = item_data["component_scores"]
            if comp_scores:
                entry["semantic_similarity"].append(comp_scores["semantic_similarity"])
                entry["factual_accuracy"].append(comp_scores["factual_accuracy"])
                entry["conciseness"].append(comp_scores["conciseness"])
                entry["professional_tone"].append(comp_scores["professional_tone"])
            else:
                entry["semantic_similarity"].append(None)
                entry["factual_accuracy"].append(None)
                entry["conciseness"].append(None)
                entry["professional_tone"].append(None)

            # Add file reference
            entry["files"].append(rel_path)

    # Convert defaultdict to regular dict for JSON serialization
    result = dict(aggregated_data)

    return result


def main():
    """Main function to run the comparison."""
    print("=" * 80)
    print("Comparing all summarization_quality_eval_output.json files")
    print("=" * 80)

    results = compare_all_evaluations()

    if not results:
        print("No results to compare!")
        return

    # Write results to JSON file
    output_file = "summarization_comparison_results.json"
    with open(output_file, 'w') as f:
        json.dump(results, f, indent=2)

    print(f"\nComparison complete!")
    print(f"Results written to: {output_file}")
    print(f"Total unique items: {len(results)}")

    # Print summary statistics
    print("\nSummary:")
    for item_id, data in list(results.items()):  # Show first 5 items as example
        scores = [s for s in data["scores"] if s is not None]
        print(f"  {item_id}:")
        if scores:
            avg_score = sum(scores) / len(scores)
            std_score = statistics.stdev(scores) if len(scores) > 1 else 0.0
            print(f"    Scores: {scores} (avg: {avg_score:.3f}, std: {std_score:.3f})")
        else:
            print(f"    Scores: {scores}")
        print(f"    Files: {len(data['files'])}")

    # Print scoring dimension statistics
    print("\nScoring Dimension Analysis:")
    for item_id, data in list(results.items()):  # Show first 3 items for detail
        print(f"  {item_id}:")
        for dimension in ["semantic_similarity", "factual_accuracy", "conciseness", "professional_tone"]:
            values = [v for v in data[dimension] if v is not None]
            if values:
                avg_score = sum(values) / len(values)
                std_score = statistics.stdev(values) if len(values) > 1 else 0.0
                print(f"    {dimension.replace('_', ' ').title()}: {values} (avg: {avg_score:.3f}, std: {std_score:.3f})")


if __name__ == "__main__":
    main()