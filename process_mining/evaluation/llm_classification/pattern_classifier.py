#!/usr/bin/env python3
"""
Evaluates how well issue-type-specific false positive patterns help an LLM correctly
classify SAST findings as True Positive or False Positive.

Supports two modes:
1. Pattern-based mode: Uses a directory of issue-type-specific pattern files
2. Baseline mode: Evaluates LLM without patterns for comparison

Usage:
    # Pattern-based mode (directory of pattern files)
    python pattern_classifier.py \\
        --patterns ../../data/patterns/train_patterns/generic_patterns/issue_type_patterns_summary \\
        --validation-data ../../data/pattern_data/validation_pattern_data \\
        --output process_mining/evaluation/llm_classification/results/pattern_eval.json

    # Baseline mode (no patterns)
    python pattern_classifier.py \\
        --baseline \\
        --validation-data ../../data/pattern_data/validation_pattern_data \\
        --output process_mining/evaluation/llm_classification/results/baseline_eval.json

    # Quick test on subset
    python pattern_classifier.py \\
        --baseline \\
        --validation-data ../../data/pattern_data/validation_pattern_data \\
        --limit 10
"""

import argparse
import json
import logging
import random
import sys
from datetime import datetime
from pathlib import Path
from typing import Dict, Optional
from concurrent.futures import ThreadPoolExecutor, as_completed
from threading import Lock
from tqdm import tqdm

sys.path.insert(0, str(Path(__file__).parent))

from entry_parser import ValidationEntryParser, ValidationEntry
from llm_classifier import PatternBasedClassifier, ClassificationResult, PLATFORM_CONFIGS
from metrics import EvaluationMetrics

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)


class PatternEvaluator:
    """Main evaluator for pattern-based LLM classification."""

    def __init__(
        self,
        validation_data_dir: Path,
        output_file: Path,
        patterns_dir: Optional[Path] = None,
        verbose: bool = False,
        workers: int = 1,
        limit: int = None,
        seed: int = 42,
        baseline_mode: bool = False,
        platform: str = "local"
    ):
        """
        Initialize evaluator.

        Args:
            validation_data_dir: Directory containing validation .txt files
            output_file: Path to save evaluation results
            patterns_dir: Directory containing pattern JSON files (None for baseline mode)
            verbose: Enable verbose logging
            workers: Number of parallel workers for LLM classification
            limit: Maximum number of entries to randomly sample (None for all)
            seed: Random seed for reproducible sampling when using limit
            baseline_mode: If True, run without pattern library (baseline evaluation)
            platform: LLM platform to use ("local" or "nim")
        """
        if patterns_dir is None and not baseline_mode:
            raise ValueError("patterns_dir is required when not in baseline mode")

        self.patterns_dir = patterns_dir
        self.validation_data_dir = validation_data_dir
        self.output_file = output_file
        self.verbose = verbose
        self.workers = workers
        self.limit = limit
        self.seed = seed
        self.baseline_mode = baseline_mode
        self.platform = platform

        if verbose:
            logging.getLogger().setLevel(logging.DEBUG)

        self.parser = ValidationEntryParser()
        self.classifier = PatternBasedClassifier(platform=platform, baseline_mode=baseline_mode)
        self.metrics = EvaluationMetrics()
        self.metrics_lock = Lock()

        self.results = []
        self.results_lock = Lock()

    def run(self, dry_run: bool = False) -> Dict:
        """
        Run the full evaluation pipeline.

        Args:
            dry_run: If True, only show what would be processed without calling LLM

        Returns:
            Dictionary with evaluation results and metrics
        """
        logger.info("Starting pattern-based LLM classification evaluation")
        if self.baseline_mode:
            logger.info("Mode: BASELINE (no patterns)")
        else:
            logger.info(f"Mode: PATTERN-BASED")
            logger.info(f"Patterns directory: {self.patterns_dir}")
        logger.info(f"Validation data: {self.validation_data_dir}")

        if self.baseline_mode:
            logger.info("Running in BASELINE mode (no patterns)")
            self.classifier.load_patterns(None)
        else:
            logger.info("Loading patterns from directory...")
            self.classifier.load_patterns(self.patterns_dir)

        logger.info("Parsing validation files...")
        all_entries = self.parser.parse_directory(self.validation_data_dir)

        logger.info(f"Found {len(all_entries)} total entries")

        if self.limit is not None and self.limit > 0:
            if self.limit < len(all_entries):
                random.seed(self.seed)
                all_entries = random.sample(all_entries, self.limit)
                logger.info(f"Randomly sampled {self.limit} entries (seed={self.seed})")
            else:
                logger.info(f"Limit ({self.limit}) >= total entries ({len(all_entries)}), using all")

        logger.info(f"Processing {len(all_entries)} entries")
        logger.info(f"  TP: {sum(1 for e in all_entries if 'TRUE' in e.ground_truth_classification)}")
        logger.info(f"  FP: {sum(1 for e in all_entries if 'FALSE' in e.ground_truth_classification)}")

        if dry_run:
            logger.info("DRY RUN - Skipping LLM classification")
            return self._create_results_dict(dry_run=True)

        logger.info(f"Classifying entries with LLM (workers={self.workers})...")
        self.results = []

        if self.workers == 1:
            for entry in tqdm(all_entries, desc="Classifying"):
                result = self._classify_and_record(entry)
                self.results.append(result)
        else:
            with ThreadPoolExecutor(max_workers=self.workers) as executor:
                futures = {executor.submit(self._classify_and_record, entry): entry
                          for entry in all_entries}

                with tqdm(total=len(all_entries), desc="Classifying") as pbar:
                    for future in as_completed(futures):
                        try:
                            result = future.result()
                            with self.results_lock:
                                self.results.append(result)
                            pbar.update(1)
                        except Exception as e:
                            entry = futures[future]
                            logger.error(f"Error classifying {entry.entry_id}: {e}")
                            pbar.update(1)

        logger.info("Calculating metrics...")
        final_metrics = self.metrics.calculate_metrics()

        output = self._create_results_dict(metrics=final_metrics)

        logger.info(f"Saving results to {self.output_file}")
        self.output_file.parent.mkdir(parents=True, exist_ok=True)
        with open(self.output_file, 'w') as f:
            json.dump(output, f, indent=2)

        self.metrics.print_summary()

        logger.info(f"Evaluation complete. Results saved to: {self.output_file}")

        return output

    def _classify_and_record(self, entry: ValidationEntry) -> Dict:
        """
        Classify a single entry and record the result.

        Args:
            entry: ValidationEntry to classify

        Returns:
            Result dictionary
        """
        masked_entry = entry.get_masked_entry()

        classification_result = self.classifier.classify_entry(masked_entry)

        is_correct = (
            classification_result.predicted_classification == entry.ground_truth_classification
        )

        with self.metrics_lock:
            self.metrics.add_result(
                ground_truth=entry.ground_truth_classification,
                predicted=classification_result.predicted_classification,
                issue_type=entry.issue_type,
                cited_patterns=classification_result.cited_patterns
            )

        result = {
            "entry_id": entry.entry_id,
            "package_name": entry.package_name,
            "issue_type": entry.issue_type,
            "cwe": entry.cwe,
            "file_path": entry.file_path,
            "line_number": entry.line_number,
            "ground_truth_classification": entry.ground_truth_classification,
            "predicted_classification": classification_result.predicted_classification,
            "correct": is_correct,
            "ground_truth_justification": entry.ground_truth_justification,
            "predicted_justification": classification_result.predicted_justification,
            "cited_patterns": classification_result.cited_patterns
        }

        if self.verbose:
            logger.debug(f"Entry {entry.entry_id}: {entry.ground_truth_classification} -> "
                        f"{classification_result.predicted_classification} "
                        f"({'✓' if is_correct else '✗'})")

        return result

    def _create_results_dict(self, metrics: Dict = None, dry_run: bool = False) -> Dict:
        """Create the final results dictionary."""
        return {
            "metadata": {
                "patterns_dir": str(self.patterns_dir) if self.patterns_dir else None,
                "baseline_mode": self.baseline_mode,
                "validation_data_dir": str(self.validation_data_dir),
                "total_entries": len(self.results) if not dry_run else 0,
                "limit": self.limit,
                "seed": self.seed,
                "timestamp": datetime.now().isoformat(),
                "platform": self.platform,
                "model": self.classifier.model,
                "dry_run": dry_run
            },
            "metrics": metrics if metrics else {},
            "results": self.results
        }


def parse_args():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Evaluate LLM classification of SAST findings using issue-type-specific patterns",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Pattern-based evaluation (directory of pattern files)
  python pattern_classifier.py \\
      --patterns ../../data/patterns/train_patterns/generic_patterns/issue_type_patterns_summary \\
      --validation-data ../../data/pattern_data/validation_pattern_data \\
      --output process_mining/evaluation/llm_classification/results/pattern_eval.json

  # Baseline evaluation (no patterns)
  python pattern_classifier.py \\
      --baseline \\
      --validation-data ../../data/pattern_data/validation_pattern_data \\
      --output process_mining/evaluation/llm_classification/results/baseline_eval.json

  # Dry run to see what would be processed
  python pattern_classifier.py \\
      --patterns ../../data/patterns/train_patterns/generic_patterns/issue_type_patterns_summary \\
      --validation-data ../../data/pattern_data/validation_pattern_data \\
      --dry-run

  # With verbose logging and parallel processing
  python pattern_classifier.py \\
      --patterns ../../data/patterns/train_patterns/generic_patterns/issue_type_patterns_summary \\
      --validation-data ../../data/pattern_data/validation_pattern_data \\
      --output process_mining/evaluation/llm_classification/results/eval.json \\
      --workers 10 \\
      --verbose

  # Quick baseline test on subset of 10 entries
  python pattern_classifier.py \\
      --baseline \\
      --validation-data ../../data/pattern_data/validation_pattern_data \\
      --limit 10 \\
      --output process_mining/evaluation/llm_classification/results/baseline_test.json
        """
    )

    parser.add_argument(
        "--patterns",
        type=Path,
        required=False,
        default=None,
        help="Directory containing issue-type-specific pattern JSON files (omit for baseline mode)"
    )

    parser.add_argument(
        "--validation-data",
        type=Path,
        required=True,
        help="Directory containing validation .txt files"
    )

    parser.add_argument(
        "--baseline",
        action="store_true",
        help="Run in baseline mode without pattern files"
    )

    parser.add_argument(
        "--output",
        type=Path,
        default=Path("process_mining/evaluation/llm_classification/results") / f"evaluation_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json",
        help="Output file for evaluation results (default: process_mining/evaluation/llm_classification/results/evaluation_TIMESTAMP.json)"
    )

    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Enable verbose logging"
    )

    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be processed without calling LLM"
    )

    parser.add_argument(
        "--workers",
        type=int,
        default=1,
        help="Number of parallel workers for LLM classification (default: 1)"
    )

    parser.add_argument(
        "--limit",
        type=int,
        default=None,
        help="Maximum number of entries to randomly sample (default: all)"
    )

    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed for reproducible sampling when using --limit (default: 42)"
    )

    parser.add_argument(
        "--platform", "-p",
        choices=list(PLATFORM_CONFIGS.keys()),
        default="local",
        help="LLM platform to use: 'local' for self-hosted Llama, 'nim' for NVIDIA NIM (default: local)"
    )

    return parser.parse_args()


def main():
    """Main entry point."""
    args = parse_args()

    if args.baseline and args.patterns:
        logger.warning("Both --baseline and --patterns specified. Running in baseline mode (ignoring patterns).")
        args.patterns = None

    if not args.baseline and not args.patterns:
        logger.error("Either --patterns or --baseline must be specified")
        sys.exit(1)

    if args.patterns and not args.patterns.exists():
        logger.error(f"Patterns directory not found: {args.patterns}")
        sys.exit(1)

    if not args.validation_data.exists():
        logger.error(f"Validation data directory not found: {args.validation_data}")
        sys.exit(1)

    evaluator = PatternEvaluator(
        validation_data_dir=args.validation_data,
        output_file=args.output,
        patterns_dir=args.patterns,
        verbose=args.verbose,
        workers=args.workers,
        limit=args.limit,
        seed=args.seed,
        baseline_mode=args.baseline,
        platform=args.platform
    )

    try:
        evaluator.run(dry_run=args.dry_run)
    except KeyboardInterrupt:
        logger.warning("\nEvaluation interrupted by user")
        sys.exit(1)
    except Exception as e:
        logger.error(f"Evaluation failed: {e}", exc_info=True)
        sys.exit(1)


if __name__ == "__main__":
    main()
