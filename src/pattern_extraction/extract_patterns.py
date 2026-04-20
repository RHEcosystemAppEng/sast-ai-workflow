"""
CLI entry point for the pattern extraction pipeline.

Usage:
    python -m pattern_extraction.extract_patterns \
        --input-dir test_pattern_data/ \
        --input-format ground_truth \
        --output-file extracted_patterns.json \
        --batch-size 5 \
        --checkpoint-interval 10 \
        --resume \
        -v
"""

import argparse
import logging
import os
import sys

from handlers.llm_client_factory import LLMClientFactory

from .pipeline import PatternExtractionPipeline

logger = logging.getLogger(__name__)


def parse_args(args=None):
    parser = argparse.ArgumentParser(
        description="Extract false positive patterns from SAST data"
    )
    parser.add_argument(
        "--input-dir",
        required=True,
        help="Directory with input files (test_pattern_data/ or dir of ignore.err files)",
    )
    parser.add_argument(
        "--input-format",
        choices=["ground_truth", "ignore_err"],
        default="ground_truth",
        help="Input file format (default: ground_truth)",
    )
    parser.add_argument(
        "--output-file",
        default="extracted_patterns.json",
        help="Output JSON file path (default: extracted_patterns.json)",
    )
    parser.add_argument(
        "--batch-size",
        type=int,
        default=5,
        help="Number of packages per progress batch (default: 5)",
    )
    parser.add_argument(
        "--entries-per-call",
        type=int,
        default=3,
        help="Entries grouped per LLM invocation (default: 3)",
    )
    parser.add_argument(
        "--checkpoint-dir",
        default="checkpoints",
        help="Directory for intermediate checkpoints (default: checkpoints/)",
    )
    parser.add_argument(
        "--checkpoint-interval",
        type=int,
        default=10,
        help="Save checkpoint every N packages (default: 10)",
    )
    parser.add_argument(
        "--issue-types",
        nargs="+",
        help="Filter to specific issue types (e.g., UNINIT INTEGER_OVERFLOW)",
    )
    parser.add_argument(
        "--include-true-positives",
        action="store_true",
        help="Include TRUE POSITIVE entries (default: false positives only)",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Resume from checkpoints, skip already-processed packages",
    )
    parser.add_argument(
        "--max-retries",
        type=int,
        default=3,
        help="Max LLM API retries per call (default: 3)",
    )
    parser.add_argument(
        "--llm-url",
        default=None,
        help="LLM API URL (default: env LLM_URL)",
    )
    parser.add_argument(
        "--llm-model",
        default=None,
        help="LLM model name (default: env LLM_MODEL_NAME)",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Enable verbose/debug logging",
    )

    return parser.parse_args(args)


def main(args=None):
    parsed = parse_args(args)

    # Configure logging
    log_level = logging.DEBUG if parsed.verbose else logging.INFO
    logging.basicConfig(
        level=log_level,
        format="%(asctime)s [%(levelname)s] %(name)s: %(message)s",
        datefmt="%H:%M:%S",
    )

    # Resolve LLM config from args or env
    llm_url = parsed.llm_url or os.getenv("LLM_URL")
    llm_model = parsed.llm_model or os.getenv("LLM_MODEL_NAME")
    llm_api_key = os.getenv("LLM_API_KEY")

    if not llm_url or not llm_model or not llm_api_key:
        logger.error(
            "LLM configuration required. Set --llm-url/--llm-model or "
            "environment variables LLM_URL, LLM_MODEL_NAME, LLM_API_KEY"
        )
        sys.exit(1)

    # Create LLM client via existing factory
    factory = LLMClientFactory()
    config_stub = type("Config", (), {
        "LLM_URL": llm_url,
        "LLM_MODEL_NAME": llm_model,
        "LLM_API_KEY": llm_api_key,
    })()
    llm = factory.create_main_llm(config_stub)

    # Progress callback that prints to stdout
    def progress(msg: str):
        print(msg, flush=True)
        logger.info(msg)

    # Build and run pipeline
    pipeline = PatternExtractionPipeline(
        llm=llm,
        input_dir=parsed.input_dir,
        output_file=parsed.output_file,
        input_format=parsed.input_format,
        batch_size=parsed.batch_size,
        entries_per_llm_call=parsed.entries_per_call,
        checkpoint_dir=parsed.checkpoint_dir,
        checkpoint_interval=parsed.checkpoint_interval,
        max_retries=parsed.max_retries,
        only_false_positives=not parsed.include_true_positives,
        issue_types=parsed.issue_types,
        progress_callback=progress,
    )

    result = pipeline.run(resume=parsed.resume)

    # Print summary
    meta = result["metadata"]
    print(f"\n{'=' * 60}")
    print("Pattern Extraction Complete")
    print(f"{'=' * 60}")
    print(f"Packages processed: {meta['processed_packages']}/{meta['total_packages']}")
    print(f"Entries processed:  {meta['total_entries_processed']}")
    print(f"Patterns extracted: {meta['total_patterns_extracted']}")
    print(f"Errors:             {len(result['errors'])}")
    print(f"Time:               {meta['processing_time_seconds']}s")
    print(f"Output:             {parsed.output_file}")


if __name__ == "__main__":
    main()
