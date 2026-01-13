#!/usr/bin/env python3
"""
Batch process pattern data files using Google Cloud Vertex AI with Claude.

This script processes all package files in parallel, generating pattern analysis
JSON files for each package using the Claude model via Vertex AI.

Features:
- Parallel processing with configurable concurrency
- Progress tracking with tqdm
- Automatic retry on failures
- Resume capability (skips already-processed files)
- Detailed logging and error handling
- Cost estimation before running
- Dry-run mode

Usage:
    # Process all train pattern data files
    python scripts/batch_pattern_learning.py --dataset train

    # Process with 10 parallel workers
    python scripts/batch_pattern_learning.py --dataset train --workers 10

    # Dry run to see what would be processed
    python scripts/batch_pattern_learning.py --dataset train --dry-run

    # Resume interrupted run
    python scripts/batch_pattern_learning.py --dataset train --resume

    # Process only first 5 packages (testing)
    python scripts/batch_pattern_learning.py --dataset train --limit 5

    # Force reprocess all files
    python scripts/batch_pattern_learning.py --dataset train --force
"""

import sys
import os
from pathlib import Path
import argparse
import logging
import json
import time
from datetime import datetime
from typing import List, Dict, Optional
from concurrent.futures import ThreadPoolExecutor, as_completed
from tqdm import tqdm
import anthropic

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from process_mining.src.common.process_mining_config import ProcessMiningConfig

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)


class VertexAIPatternLearner:
    """
    Batch pattern learning using Google Cloud Vertex AI with Claude.
    """

    def __init__(
        self,
        project_id: str = "itpc-gcp-eco-eng-claude",
        region: str = "us-east5",
        model: str = "claude-sonnet-4-5@20250929",
        max_tokens: int = 16000,
        temperature: float = 0.0
    ):
        """
        Initialize Vertex AI client.

        Args:
            project_id: Google Cloud project ID
            region: GCP region for Vertex AI
            model: Claude model ID
            max_tokens: Maximum tokens in response
            temperature: Sampling temperature (0.0 = deterministic)
        """
        self.project_id = project_id
        self.region = region
        self.model = model
        self.max_tokens = max_tokens
        self.temperature = temperature

        self.client = anthropic.AnthropicVertex(
            region=region,
            project_id=project_id
        )

        logger.info(f"Initialized Vertex AI client: {project_id} / {region} / {model}")

    def load_prompt_template(self, prompt_path: Path) -> str:
        """Load the pattern learning prompt template."""
        if not prompt_path.exists():
            raise FileNotFoundError(f"Prompt template not found: {prompt_path}")
        return prompt_path.read_text()

    def load_package_data(self, package_file: Path) -> str:
        """Load package ground-truth data."""
        if not package_file.exists():
            raise FileNotFoundError(f"Package file not found: {package_file}")
        return package_file.read_text()

    def extract_package_name(self, package_file: Path) -> str:
        """Extract package name from filename (e.g., 'acl-2.3.2-1.el10.txt' -> 'acl-2.3.2-1.el10')."""
        return package_file.stem

    def estimate_tokens(self, text: str) -> int:
        """Rough token estimation (1 token ≈ 4 characters)."""
        return len(text) // 4

    def process_package(
        self,
        package_file: Path,
        prompt_template: str,
        output_dir: Path,
        model_suffix: str = "claude",
        retry_config: Optional[Dict] = None
    ) -> Dict:
        """
        Process a single package file.

        Args:
            package_file: Path to package .txt file
            prompt_template: Pattern learning prompt template
            output_dir: Output directory for JSON files
            model_suffix: Model name for output filename (e.g., 'claude')
            retry_config: Retry configuration dict (optional)

        Returns:
            Result dictionary with status and metadata
        """
        package_name = self.extract_package_name(package_file)
        output_file = output_dir / f"{package_name}_{model_suffix}.json"

        result = {
            'package_name': package_name,
            'package_file': str(package_file),
            'output_file': str(output_file),
            'status': 'unknown',
            'input_tokens': 0,
            'output_tokens': 0,
            'processing_time': 0,
            'error': None
        }

        start_time = time.time()

        try:
            package_data = self.load_package_data(package_file)

            full_prompt = f"{prompt_template}\n\n{'=' * 80}\n\n{package_data}"

            result['input_tokens'] = self.estimate_tokens(full_prompt)

            logger.debug(f"Processing {package_name} (~{result['input_tokens']:,} input tokens)")

            if retry_config is None:
                retry_config = {}

            max_retries = retry_config.get('max_retries', 3)
            retry_delay = retry_config.get('retry_delay', 10)

            for attempt in range(max_retries):
                try:
                    response = self.client.messages.create(
                        model=self.model,
                        max_tokens=self.max_tokens,
                        temperature=self.temperature,
                        messages=[
                            {
                                "role": "user",
                                "content": full_prompt
                            }
                        ]
                    )
                    break

                except Exception as api_error:
                    error_str = str(api_error)
                    if "429" in error_str or "quota exceeded" in error_str.lower():
                        if attempt < max_retries - 1:
                            wait_time = retry_delay * (2 ** attempt)
                            logger.warning(f"⚠️  {package_name}: Rate limit hit, retrying in {wait_time}s (attempt {attempt + 1}/{max_retries})")
                            time.sleep(wait_time)
                        else:
                            raise api_error
                    else:
                        raise api_error

            response_text = response.content[0].text

            json_text = response_text
            if "```json" in response_text:
                start_idx = response_text.find("```json") + 7
                end_idx = response_text.find("```", start_idx)
                if end_idx > start_idx:
                    json_text = response_text[start_idx:end_idx].strip()
            elif "```" in response_text:
                start_idx = response_text.find("```") + 3
                end_idx = response_text.find("```", start_idx)
                if end_idx > start_idx:
                    json_text = response_text[start_idx:end_idx].strip()

            pattern_data = json.loads(json_text)

            output_dir.mkdir(parents=True, exist_ok=True)
            output_file.write_text(json.dumps(pattern_data, indent=2))

            result['status'] = 'success'
            result['output_tokens'] = self.estimate_tokens(response_text)
            result['processing_time'] = time.time() - start_time

            logger.debug(f"✅ {package_name}: {result['output_tokens']:,} output tokens, {result['processing_time']:.1f}s")

        except json.JSONDecodeError as e:
            result['status'] = 'failed'
            result['error'] = f"JSON parse error: {str(e)}"
            result['processing_time'] = time.time() - start_time
            logger.error(f"❌ {package_name}: {result['error']}")

        except Exception as e:
            error_str = str(e)

            if "413" in error_str or "too long" in error_str.lower() or "prompt is too long" in error_str.lower():
                result['status'] = 'failed'
                result['error'] = f"Package too large for Vertex AI API (prompt exceeds size limit): {error_str}"
                result['processing_time'] = time.time() - start_time
                logger.error(f"❌ {package_name}: Package too large - consider processing manually or splitting the file")
            else:
                result['status'] = 'failed'
                result['error'] = error_str
                result['processing_time'] = time.time() - start_time
                logger.error(f"❌ {package_name}: {result['error']}")

        return result


def discover_package_files(pattern_data_dir: Path) -> List[Path]:
    """Discover all package .txt files in pattern_data directory."""
    if not pattern_data_dir.exists():
        raise FileNotFoundError(f"Pattern data directory not found: {pattern_data_dir}")

    files = sorted(pattern_data_dir.glob("*.txt"))
    return [f for f in files if not f.name.startswith('_')]


def load_processed_files(output_dir: Path) -> set:
    """Load set of already-processed package names from existing output files."""
    if not output_dir.exists():
        return set()

    processed = set()
    for json_file in output_dir.glob("*_claude.json"):
        package_name = json_file.stem.replace('_claude', '')
        processed.add(package_name)

    return processed


def should_skip_file(package_name: str, processed: set, force: bool) -> bool:
    """Check if file should be skipped (already processed)."""
    if force:
        return False
    return package_name in processed


def generate_summary_report(
    results: List[Dict],
    skipped: List[str],
    output_dir: Path,
    processing_time: float,
    cost_config: Optional[Dict] = None
) -> None:
    """Generate summary report JSON."""
    if cost_config is None:
        cost_config = {}
    success = [r for r in results if r['status'] == 'success']
    failed = [r for r in results if r['status'] == 'failed']

    total_input_tokens = sum(r['input_tokens'] for r in results)
    total_output_tokens = sum(r['output_tokens'] for r in results)
    total_tokens = total_input_tokens + total_output_tokens

    # Cost estimation using config values (Vertex AI pricing - model dependent)
    input_cost_per_million = cost_config.get('input_cost_per_million', 3.3)
    output_cost_per_million = cost_config.get('output_cost_per_million', 16.5)

    input_cost = (total_input_tokens / 1_000_000) * input_cost_per_million
    output_cost = (total_output_tokens / 1_000_000) * output_cost_per_million
    total_cost = input_cost + output_cost

    summary = {
        'processing_timestamp': datetime.now().isoformat(),
        'total_packages': len(results) + len(skipped),
        'processed': len(results),
        'successful': len(success),
        'failed': len(failed),
        'skipped': len(skipped),
        'skipped_files': skipped,

        'token_usage': {
            'total_input_tokens': total_input_tokens,
            'total_output_tokens': total_output_tokens,
            'total_tokens': total_tokens
        },

        'estimated_cost_usd': {
            'input_cost': round(input_cost, 2),
            'output_cost': round(output_cost, 2),
            'total_cost': round(total_cost, 2),
            'note': 'Rough estimation based on Vertex AI Claude Sonnet pricing'
        },

        'processing_time_seconds': round(processing_time, 2),
        'avg_time_per_package': round(processing_time / len(results), 2) if results else 0,

        'successful_files': [r['package_name'] for r in success],

        'failed_files': [
            {
                'package': r['package_name'],
                'error': r['error']
            } for r in failed
        ]
    }

    summary_file = output_dir / "_processing_summary.json"
    summary_file.write_text(json.dumps(summary, indent=2))
    logger.info(f"\n✅ Summary report saved to: {summary_file}")

    if failed:
        error_log_lines = []
        for r in failed:
            error_log_lines.append(f"\n{'=' * 80}")
            error_log_lines.append(f"Package: {r['package_name']}")
            error_log_lines.append(f"Error: {r['error']}")

        error_log = output_dir / "_errors.log"
        error_log.write_text("\n".join(error_log_lines))
        logger.info(f"⚠️  Error log saved to: {error_log}")

    print("\n" + "=" * 80)
    print("BATCH PATTERN LEARNING SUMMARY")
    print("=" * 80)
    print(f"Total Packages: {len(results) + len(skipped)}")
    print(f"  ✅ Successful: {len(success)}")
    print(f"  ❌ Failed: {len(failed)}")
    print(f"  ⏭️  Skipped (Already Processed): {len(skipped)}")
    print(f"\nToken Usage:")
    print(f"  Input Tokens: {total_input_tokens:,}")
    print(f"  Output Tokens: {total_output_tokens:,}")
    print(f"  Total Tokens: {total_tokens:,}")
    print(f"\nEstimated Cost: ${total_cost:.2f} USD")
    print(f"  (Input: ${input_cost:.2f}, Output: ${output_cost:.2f})")
    print(f"\nProcessing Time: {processing_time:.2f}s ({processing_time/60:.1f}m)")
    print(f"Avg Time/Package: {processing_time/len(results):.2f}s" if results else "N/A")
    print("=" * 80)


def batch_process(args):
    """Main batch processing function."""
    start_time = time.time()

    config = ProcessMiningConfig(args.config if hasattr(args, 'config') else None)
    batch_config = config.get_batch_learning_config()
    vertex_config = batch_config.get('vertex_ai', {})
    retry_config = batch_config.get('retry', {})
    cost_config = batch_config.get('cost_estimation', {})
    processing_config = batch_config.get('processing', {})
    paths_config = batch_config.get('paths', {})

    base_dir = Path(__file__).parent.parent.parent

    pattern_data_dir = base_dir / paths_config.get(
        'pattern_data_dir_template',
        'process_mining/data/pattern_data/{dataset}_pattern_data'
    ).format(dataset=args.dataset)

    output_dir = base_dir / paths_config.get(
        'output_dir_template',
        'process_mining/data/patterns/{dataset}_patterns'
    ).format(dataset=args.dataset)

    prompt_file = base_dir / paths_config.get(
        'prompt_file',
        'process_mining/prompts/pattern_learning_prompt.md'
    )

    if not pattern_data_dir.exists():
        logger.error(f"❌ Pattern data directory not found: {pattern_data_dir}")
        sys.exit(1)

    if not prompt_file.exists():
        logger.error(f"❌ Prompt file not found: {prompt_file}")
        sys.exit(1)

    package_files = discover_package_files(pattern_data_dir)
    if not package_files:
        logger.error(f"❌ No package files found in {pattern_data_dir}")
        sys.exit(1)

    logger.info(f"📁 Found {len(package_files)} package files in {pattern_data_dir.name}")
    logger.info(f"📂 Output directory: {output_dir}")

    processed_files = load_processed_files(output_dir) if args.resume else set()
    if args.resume and processed_files:
        logger.info(f"📋 Resume mode: {len(processed_files)} files already processed")

    files_to_process = []
    skipped_files = []

    for pkg_file in package_files:
        pkg_name = pkg_file.stem
        if should_skip_file(pkg_name, processed_files, args.force):
            logger.info(f"⏭️  Skipping {pkg_name} (already processed)")
            skipped_files.append(pkg_name)
            continue
        files_to_process.append(pkg_file)

    if args.limit:
        files_to_process = files_to_process[:args.limit]

    logger.info(f"🚀 Will process {len(files_to_process)} packages")

    if args.dry_run:
        print("\n🔍 DRY RUN - Would process:")
        for pkg_file in files_to_process:
            print(f"  - {pkg_file.stem}")
        print(f"\nEstimated input tokens: ~{sum(len(f.read_text()) // 4 for f in files_to_process):,}")
        return

    # CLI args override config file values
    project_id = args.project_id if args.project_id is not None else vertex_config.get('project_id', 'itpc-gcp-eco-eng-claude')
    region = args.region if args.region is not None else vertex_config.get('region', 'us-east5')
    model = args.model if args.model is not None else vertex_config.get('model', 'claude-sonnet-4-5@20250929')
    max_tokens = args.max_tokens if args.max_tokens is not None else vertex_config.get('max_tokens', 16000)
    temperature = args.temperature if args.temperature is not None else vertex_config.get('temperature', 0.0)
    workers = args.workers if args.workers is not None else processing_config.get('default_workers', 5)
    model_suffix = args.model_suffix if args.model_suffix is not None else processing_config.get('model_suffix', 'claude')

    try:
        learner = VertexAIPatternLearner(
            project_id=project_id,
            region=region,
            model=model,
            max_tokens=max_tokens,
            temperature=temperature
        )
    except Exception as e:
        logger.error(f"❌ Failed to initialize Vertex AI client: {e}")
        logger.error("Make sure you're authenticated with: gcloud auth application-default login")
        sys.exit(1)

    prompt_template = learner.load_prompt_template(prompt_file)
    logger.info(f"📄 Loaded prompt template: {prompt_file.name}")

    output_dir.mkdir(parents=True, exist_ok=True)

    results = []

    if workers > 1:
        logger.info(f"🔄 Processing with {workers} parallel workers...")

        with ThreadPoolExecutor(max_workers=workers) as executor:
            futures = {
                executor.submit(
                    learner.process_package,
                    pkg_file,
                    prompt_template,
                    output_dir,
                    model_suffix,
                    retry_config
                ): pkg_file for pkg_file in files_to_process
            }

            with tqdm(total=len(files_to_process), desc="Processing packages") as pbar:
                for future in as_completed(futures):
                    result = future.result()
                    results.append(result)
                    pbar.update(1)

                    if result['status'] == 'success':
                        pbar.set_postfix_str(f"✅ {result['package_name']}")
                    else:
                        pbar.set_postfix_str(f"❌ {result['package_name']}")

    else:
        logger.info("🔄 Processing sequentially...")

        for pkg_file in tqdm(files_to_process, desc="Processing packages"):
            result = learner.process_package(
                pkg_file,
                prompt_template,
                output_dir,
                model_suffix,
                retry_config
            )
            results.append(result)

    processing_time = time.time() - start_time
    generate_summary_report(results, skipped_files, output_dir, processing_time, cost_config)

    logger.info(f"\n✅ All done! Output files in: {output_dir}")


def main():
    parser = argparse.ArgumentParser(
        description="Batch pattern learning using Google Cloud Vertex AI with Claude",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Process all train pattern data
  python scripts/batch_pattern_learning.py --dataset train

  # Process with 10 parallel workers
  python scripts/batch_pattern_learning.py --dataset train --workers 10

  # Dry run
  python scripts/batch_pattern_learning.py --dataset train --dry-run

  # Resume interrupted run
  python scripts/batch_pattern_learning.py --dataset train --resume

  # Test with 5 packages
  python scripts/batch_pattern_learning.py --dataset train --limit 5
        """
    )

    parser.add_argument(
        "--dataset",
        choices=["train", "validation", "test"],
        required=True,
        help="Which dataset to process (train/validation/test)"
    )

    parser.add_argument(
        "--config",
        help="Path to configuration YAML file (default: auto-detect)"
    )

    parser.add_argument(
        "--workers",
        type=int,
        default=None,
        help="Number of parallel workers (default from config: 5, use 1 for sequential)"
    )

    parser.add_argument(
        "--limit",
        type=int,
        help="Limit number of packages to process (for testing)"
    )

    parser.add_argument(
        "--resume",
        action="store_true",
        help="Resume: skip already-processed files"
    )

    parser.add_argument(
        "--force",
        action="store_true",
        help="Force re-process all files (ignore existing outputs)"
    )

    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be processed without actually processing"
    )

    parser.add_argument(
        "--project-id",
        default=None,
        help="Google Cloud project ID (default from config: itpc-gcp-eco-eng-claude)"
    )

    parser.add_argument(
        "--region",
        default=None,
        help="GCP region for Vertex AI (default from config: us-east5)"
    )

    parser.add_argument(
        "--model",
        default=None,
        help="Claude model ID (default from config: claude-sonnet-4-5@20250929)"
    )

    parser.add_argument(
        "--model-suffix",
        default=None,
        help="Model suffix for output filenames (default from config: claude)"
    )

    parser.add_argument(
        "--max-tokens",
        type=int,
        default=None,
        help="Maximum tokens in response (default from config: 16000)"
    )

    parser.add_argument(
        "--temperature",
        type=float,
        default=None,
        help="Sampling temperature (default from config: 0.0 for deterministic)"
    )

    args = parser.parse_args()

    try:
        batch_process(args)
    except KeyboardInterrupt:
        logger.info("\n\n⚠️  Interrupted by user")
        sys.exit(1)
    except Exception as e:
        logger.exception(f"\n\n❌ Fatal error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
