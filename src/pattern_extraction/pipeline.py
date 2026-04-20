"""
Core pattern extraction pipeline.

Orchestrates: parse -> filter -> group by issue_type -> LLM extraction -> checkpoint -> output.
"""

import json
import logging
import os
import time
from collections import defaultdict
from datetime import datetime, timezone
from typing import Any, Callable, Dict, List, Optional

import yaml
from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.prompts import ChatPromptTemplate
from langchain_core.runnables import RunnableLambda, RunnablePassthrough

from Utils.llm_utils import robust_structured_output

from .models import ExtractedPattern, ParsedFalsePositive, PatternExtractionResponse
from .parsers import parse_directory

logger = logging.getLogger(__name__)

# Max source code characters to include per entry in the LLM prompt
_MAX_SOURCE_CODE_CHARS = 3000
_JSON_EXT = ".json"


class PatternExtractionPipeline:
    """Pipeline for extracting false positive patterns from SAST data."""

    def __init__(
        self,
        llm: BaseChatModel,
        input_dir: str,
        output_file: str = "extracted_patterns.json",
        input_format: str = "ground_truth",
        batch_size: int = 5,
        entries_per_llm_call: int = 3,
        checkpoint_dir: Optional[str] = "checkpoints",
        checkpoint_interval: int = 10,
        max_retries: int = 3,
        only_false_positives: bool = True,
        issue_types: Optional[List[str]] = None,
        progress_callback: Optional[Callable[[str], None]] = None,
    ):
        self.llm = llm
        self.input_dir = input_dir
        self.output_file = output_file
        self.input_format = input_format
        self.batch_size = batch_size
        self.entries_per_llm_call = entries_per_llm_call
        self.checkpoint_dir = checkpoint_dir
        self.checkpoint_interval = checkpoint_interval
        self.max_retries = max_retries
        self.only_false_positives = only_false_positives
        self.issue_types = issue_types
        self.progress_callback = progress_callback or (lambda msg: logger.info(msg))

        self.system_prompt = self._load_prompt("pattern_extraction_system_prompt")
        self.human_prompt = self._load_prompt("pattern_extraction_human_prompt")

    def run(self, resume: bool = False) -> Dict[str, Any]:
        """Run the full pipeline.

        Args:
            resume: If True, skip packages already in checkpoint dir.

        Returns:
            Dict with metadata, patterns, and errors.
        """
        start_time = time.time()

        # 1. Parse
        self.progress_callback("Parsing input files...")
        all_packages = parse_directory(self.input_dir, self.input_format)

        # 2. Filter
        all_packages = self._filter_entries(all_packages)

        # 3. Determine which packages to process
        completed_packages = self._load_completed_packages() if resume else set()
        packages_to_process = {
            k: v for k, v in all_packages.items() if k not in completed_packages
        }

        if completed_packages:
            self.progress_callback(
                f"Resuming: {len(completed_packages)} packages already processed, "
                f"{len(packages_to_process)} remaining"
            )

        total_packages = len(all_packages)
        all_patterns: List[ExtractedPattern] = []
        all_errors: List[Dict[str, str]] = []

        # Load patterns from previous checkpoints if resuming
        if resume:
            all_patterns = self._load_checkpoint_patterns()

        # 4. Process in batches
        package_names = list(packages_to_process.keys())
        processed_count = len(completed_packages)

        for batch_start in range(0, len(package_names), self.batch_size):
            batch_names = package_names[batch_start : batch_start + self.batch_size]

            for pkg_name in batch_names:
                processed_count += 1
                entries = packages_to_process[pkg_name]

                self.progress_callback(
                    f"[{processed_count}/{total_packages}] {pkg_name}: "
                    f"{len(entries)} entries"
                )

                patterns, errors = self._process_package(pkg_name, entries)
                all_patterns.extend(patterns)
                all_errors.extend(errors)

                if patterns:
                    self.progress_callback(
                        f"  -> {len(patterns)} patterns extracted"
                    )

                # Save per-package checkpoint for resume support
                if self.checkpoint_dir:
                    self._save_package_checkpoint(pkg_name, patterns)

            # 5. Checkpoint
            if (
                self.checkpoint_dir
                and processed_count % self.checkpoint_interval == 0
                and processed_count < total_packages
            ):
                self._save_checkpoint(all_patterns, processed_count)

        # 6. Write final output
        elapsed = time.time() - start_time
        result = {
            "metadata": {
                "timestamp": datetime.now(timezone.utc).isoformat(),
                "total_packages": total_packages,
                "processed_packages": processed_count,
                "total_entries_processed": sum(len(v) for v in all_packages.values()),
                "total_patterns_extracted": len(all_patterns),
                "processing_time_seconds": round(elapsed, 2),
            },
            "patterns": [p.model_dump() for p in all_patterns],
            "errors": all_errors,
        }

        self._write_output(result)
        self.progress_callback(
            f"Done: {len(all_patterns)} patterns extracted in {elapsed:.1f}s"
        )

        return result

    def _filter_entries(
        self, packages: Dict[str, List[ParsedFalsePositive]]
    ) -> Dict[str, List[ParsedFalsePositive]]:
        """Filter entries based on configuration."""
        filtered = {}
        for pkg_name, entries in packages.items():
            kept = entries
            if self.only_false_positives:
                kept = [e for e in kept if e.verdict == "FALSE POSITIVE"]
            if self.issue_types:
                kept = [e for e in kept if e.issue_type in self.issue_types]
            if kept:
                filtered[pkg_name] = kept
        return filtered

    def _process_package(
        self, package_name: str, entries: List[ParsedFalsePositive]
    ) -> tuple[List[ExtractedPattern], List[Dict[str, str]]]:
        """Process all entries for a single package.

        Groups entries by issue_type, then sends sub-batches to the LLM.
        """
        # Group by issue_type
        by_type: Dict[str, List[ParsedFalsePositive]] = defaultdict(list)
        for entry in entries:
            by_type[entry.issue_type].append(entry)

        patterns = []
        errors = []

        for issue_type, type_entries in by_type.items():
            # Sub-batch by entries_per_llm_call
            for i in range(0, len(type_entries), self.entries_per_llm_call):
                sub_batch = type_entries[i : i + self.entries_per_llm_call]
                try:
                    batch_patterns = self._extract_patterns_from_batch(
                        sub_batch, issue_type
                    )
                    patterns.extend(batch_patterns)
                except Exception as e:
                    error_msg = (
                        f"LLM extraction failed for {package_name}/{issue_type} "
                        f"(entries {i + 1}-{i + len(sub_batch)}): {e}"
                    )
                    logger.error(error_msg)
                    errors.append({"package": package_name, "error": error_msg})

        return patterns, errors

    def _extract_patterns_from_batch(
        self, entries: List[ParsedFalsePositive], issue_type: str
    ) -> List[ExtractedPattern]:
        """Send a batch of entries to the LLM for pattern extraction."""
        cwe = entries[0].cwe or "N/A"

        # Format entries for the prompt
        formatted_entries = self._format_entries_for_prompt(entries)

        prompt = ChatPromptTemplate.from_messages(
            [
                ("system", self.system_prompt),
                ("user", self.human_prompt),
            ]
        )

        prompt_chain = {
            "entry_count": RunnableLambda(lambda _: str(len(entries))),
            "issue_type": RunnableLambda(lambda _: issue_type),
            "cwe": RunnableLambda(lambda _: cwe),
            "entries": RunnablePassthrough(),
        } | prompt

        response = robust_structured_output(
            llm=self.llm,
            schema=PatternExtractionResponse,
            input=formatted_entries,
            prompt_chain=prompt_chain,
            max_retries=self.max_retries,
        )

        return response.patterns

    def _format_entries_for_prompt(self, entries: List[ParsedFalsePositive]) -> str:
        """Format a list of entries into text for the LLM prompt."""
        parts = []
        for i, entry in enumerate(entries, start=1):
            section = [
                f"--- Entry {i} ---",
                f"Package: {entry.package_name}",
                f"Issue Type: {entry.issue_type}",
                f"CWE: {entry.cwe or 'N/A'}",
                "",
                "Error Trace:",
                entry.error_trace,
            ]

            if entry.source_code:
                truncated = entry.source_code[:_MAX_SOURCE_CODE_CHARS]
                if len(entry.source_code) > _MAX_SOURCE_CODE_CHARS:
                    truncated += "\n... (truncated)"
                section.extend(["", "Source Code:", truncated])

            section.extend([
                "",
                "Analyst Justification:",
                entry.analyst_justification,
            ])

            parts.append("\n".join(section))

        return "\n\n".join(parts)

    def _load_prompt(self, prompt_name: str) -> str:
        """Load a prompt template from the YAML file."""
        prompts_dir = os.path.join(
            os.path.dirname(__file__), "..", "templates", "prompts"
        )
        prompt_file = os.path.join(prompts_dir, f"{prompt_name}.yaml")

        if not os.path.exists(prompt_file):
            raise FileNotFoundError(f"Prompt template not found: {prompt_file}")

        with open(prompt_file, "r", encoding="utf-8") as f:
            data = yaml.safe_load(f)
            return data.get("template", "")

    def _load_completed_packages(self) -> set:
        """Load set of package names already processed from per-package checkpoints."""
        if not self.checkpoint_dir or not os.path.isdir(self.checkpoint_dir):
            return set()

        completed = set()
        for filename in os.listdir(self.checkpoint_dir):
            if filename.startswith("pkg_") and filename.endswith(_JSON_EXT):
                pkg_name = filename[4:-5]  # strip "pkg_" and _JSON_EXT
                completed.add(pkg_name)

        return completed

    def _load_checkpoint_patterns(self) -> List[ExtractedPattern]:
        """Load previously extracted patterns from checkpoint files."""
        if not self.checkpoint_dir or not os.path.isdir(self.checkpoint_dir):
            return []

        patterns = []
        for filename in sorted(os.listdir(self.checkpoint_dir)):
            if filename.startswith("pkg_") and filename.endswith(_JSON_EXT):
                filepath = os.path.join(self.checkpoint_dir, filename)
                try:
                    with open(filepath, "r", encoding="utf-8") as f:
                        data = json.load(f)
                        patterns.extend(
                            ExtractedPattern(**p) for p in data.get("patterns", [])
                        )
                except Exception as e:
                    logger.warning(f"Failed to load checkpoint {filename}: {e}")

        if patterns:
            logger.info(f"Loaded {len(patterns)} patterns from checkpoints")
        return patterns

    def _save_checkpoint(
        self, patterns: List[ExtractedPattern], processed_count: int
    ) -> None:
        """Save intermediate results to checkpoint directory."""
        if not self.checkpoint_dir:
            return

        os.makedirs(self.checkpoint_dir, exist_ok=True)
        checkpoint_file = os.path.join(
            self.checkpoint_dir, f"checkpoint_{processed_count}{_JSON_EXT}"
        )

        data = {
            "metadata": {
                "timestamp": datetime.now(timezone.utc).isoformat(),
                "processed_packages": processed_count,
                "total_patterns": len(patterns),
            },
            "patterns": [p.model_dump() for p in patterns],
        }

        with open(checkpoint_file, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2)

        self.progress_callback(
            f"  Checkpoint saved: {processed_count} packages, "
            f"{len(patterns)} patterns"
        )

    def _save_package_checkpoint(
        self, package_name: str, patterns: List[ExtractedPattern]
    ) -> None:
        """Save per-package checkpoint for resume support."""
        if not self.checkpoint_dir:
            return

        os.makedirs(self.checkpoint_dir, exist_ok=True)
        # Sanitize package name for filename
        safe_name = package_name.replace("/", "_").replace(" ", "_")
        filepath = os.path.join(self.checkpoint_dir, f"pkg_{safe_name}{_JSON_EXT}")

        data = {
            "package": package_name,
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "patterns": [p.model_dump() for p in patterns],
        }

        with open(filepath, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2)

    def _write_output(self, result: Dict[str, Any]) -> None:
        """Write final output JSON file."""
        output_dir = os.path.dirname(self.output_file)
        if output_dir:
            os.makedirs(output_dir, exist_ok=True)

        with open(self.output_file, "w", encoding="utf-8") as f:
            json.dump(result, f, indent=2)

        logger.info(f"Output written to {self.output_file}")
