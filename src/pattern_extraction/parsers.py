"""
Parsers for SAST false positive data.

Two input formats are supported:
1. Ground truth format (test_pattern_data/*.txt) - structured with labeled sections
2. Ignore.err format - raw entries separated by double newlines
"""

import logging
import os
import re
from typing import Dict, List, Optional

from Utils.file_utils import parse_single_ignore_err_entry
from pattern_extraction.models import ParsedFalsePositive

logger = logging.getLogger(__name__)

# Ground truth format patterns
_ENTRY_SPLIT_PATTERN = re.compile(r"(?:^|\n)---[ \t]*\n[ \t]*(?:---[ \t]*\n)?Entry #(\d+):", re.MULTILINE)
_FIRST_ENTRY_PATTERN = re.compile(r"Entry #(\d+):")
_PACKAGE_HEADER_PATTERN = re.compile(r"GROUND-TRUTH ENTRIES FOR:\s*(.+)")
_SOURCE_CODE_SECTION_PATTERN = re.compile(
    r"Source Code\s*\([^)]*\):\s*\n```\w*\n(.*?)```", re.DOTALL
)


def parse_ground_truth_file(file_path: str) -> List[ParsedFalsePositive]:
    """Parse a ground truth file (test_pattern_data format) into entries.

    Expected format per entry:
        Entry #N:
        Issue Type: <TYPE>
        CWE: <CWE>

        Error Trace:
        <trace lines>

        Source Code (<filepath>):
        ```c
        <code>
        ```

        Ground Truth Classification: <FALSE POSITIVE|TRUE POSITIVE>
        Human Expert Justification: <text>
        Analyst Comment: <text>  (optional)
    """
    with open(file_path, "r", encoding="utf-8") as f:
        content = f.read()

    # Extract package name from header or filename
    header_match = _PACKAGE_HEADER_PATTERN.search(content)
    if header_match:
        package_name = header_match.group(1).strip()
    else:
        package_name = os.path.splitext(os.path.basename(file_path))[0]

    # Split into individual entries
    entry_texts = _split_ground_truth_entries(content)
    if not entry_texts:
        logger.warning(f"No entries found in {file_path}")
        return []

    results = []
    for entry_index, entry_text in entry_texts:
        try:
            parsed = _parse_single_ground_truth_entry(entry_text, package_name, entry_index)
            if parsed:
                results.append(parsed)
        except Exception as e:
            logger.warning(f"Failed to parse entry #{entry_index} in {file_path}: {e}")

    logger.info(f"Parsed {len(results)} entries from {file_path}")
    return results


def _split_ground_truth_entries(content: str) -> List[tuple]:
    """Split content into (entry_index, entry_text) tuples."""
    # Find all entry start positions
    splits = list(_ENTRY_SPLIT_PATTERN.finditer(content))

    if not splits:
        # Try matching the first entry without leading ---
        first_match = _FIRST_ENTRY_PATTERN.search(content)
        if first_match:
            entry_index = int(first_match.group(1))
            entry_text = content[first_match.start():]
            return [(entry_index, entry_text)]
        return []

    entries = []
    for i, match in enumerate(splits):
        entry_index = int(match.group(1))
        start = match.start()
        end = splits[i + 1].start() if i + 1 < len(splits) else len(content)
        entries.append((entry_index, content[start:end]))

    return entries


def _parse_single_ground_truth_entry(
    entry_text: str, package_name: str, entry_index: int
) -> Optional[ParsedFalsePositive]:
    """Parse a single ground truth entry block into a ParsedFalsePositive."""
    # Issue Type
    issue_type_match = re.search(r"Issue Type:\s*(.+)", entry_text)
    if not issue_type_match:
        logger.warning(f"Missing Issue Type in entry #{entry_index}")
        return None
    issue_type = issue_type_match.group(1).strip()

    # CWE
    cwe_match = re.search(r"CWE:\s*(CWE-\d+)", entry_text)
    cwe = cwe_match.group(1) if cwe_match else None

    # Error Trace - text between "Error Trace:" and "Source Code ("
    error_trace = _extract_section(entry_text, r"Error Trace:\s*\n", r"\nSource Code\s*\(")
    if not error_trace:
        # Fallback: text between "Error Trace:" and next known section
        error_trace = _extract_section(
            entry_text, r"Error Trace:\s*\n", r"\nGround Truth Classification:"
        )
    error_trace = error_trace.strip() if error_trace else ""

    # Source Code
    source_match = _SOURCE_CODE_SECTION_PATTERN.search(entry_text)
    source_code = source_match.group(1).strip() if source_match else None

    # Ground Truth Classification
    verdict_match = re.search(
        r"Ground Truth Classification:\s*(FALSE POSITIVE|TRUE POSITIVE)", entry_text
    )
    verdict = verdict_match.group(1) if verdict_match else "UNKNOWN"

    # Human Expert Justification + Analyst Comment
    justification_parts = []
    expert_match = re.search(r"Human Expert Justification:\s*(.+)", entry_text)
    if expert_match:
        justification_parts.append(expert_match.group(1).strip())

    # Analyst Comment may span multiple lines until next --- or end
    analyst_comment = _extract_section(entry_text, r"Analyst Comment:\s*", r"\n---")
    if analyst_comment:
        justification_parts.append(analyst_comment.strip())

    analyst_justification = "\n".join(justification_parts)

    return ParsedFalsePositive(
        package_name=package_name,
        issue_type=issue_type,
        cwe=cwe,
        error_trace=error_trace,
        source_code=source_code,
        analyst_justification=analyst_justification,
        verdict=verdict,
        entry_index=entry_index,
    )


def _extract_section(text: str, start_pattern: str, end_pattern: str) -> Optional[str]:
    """Extract text between two regex patterns."""
    start_match = re.search(start_pattern, text)
    if not start_match:
        return None
    remaining = text[start_match.end():]
    end_match = re.search(end_pattern, remaining)
    if end_match:
        return remaining[: end_match.start()]
    return remaining


def parse_ignore_err_file(
    file_path: str, package_name: Optional[str] = None
) -> List[ParsedFalsePositive]:
    """Parse a raw ignore.err file into entries.

    Reuses the same parsing logic as vector_store_service._extract_metadata_from_known_false_positives.

    Each entry is separated by double newlines and follows the format:
        Error: <ISSUE_TYPE> (<CWE>):
        <error trace lines with file paths and code blocks>
        <analyst justification text>
    """
    if package_name is None:
        package_name = os.path.splitext(os.path.basename(file_path))[0]

    with open(file_path, "r", encoding="utf-8") as f:
        plain_text = f.read()

    # Split on double newlines (same separator as KNOWN_FALSE_POSITIVE_ISSUE_SEPARATOR)
    raw_entries = [item.strip() for item in plain_text.split("\n\n") if item.strip()]

    results = []
    for entry_index, item in enumerate(raw_entries, start=1):
        try:
            parsed = _parse_single_ignore_err_entry(item, package_name, entry_index)
            if parsed:
                results.append(parsed)
        except Exception as e:
            logger.warning(f"Failed to parse ignore.err entry #{entry_index} in {file_path}: {e}")

    logger.info(f"Parsed {len(results)} entries from {file_path}")
    return results


def _parse_single_ignore_err_entry(
    item: str, package_name: str, entry_index: int
) -> Optional[ParsedFalsePositive]:
    """Parse a single ignore.err entry using shared parsing logic."""
    parsed = parse_single_ignore_err_entry(item)
    if not parsed:
        return None

    return ParsedFalsePositive(
        package_name=package_name,
        issue_type=parsed["issue_type"],
        cwe=parsed["cwe"],
        error_trace=parsed["error_trace"],
        source_code=None,
        analyst_justification=parsed["justification"],
        verdict="FALSE POSITIVE",
        entry_index=entry_index,
    )


