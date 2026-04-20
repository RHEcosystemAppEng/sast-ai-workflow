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

from pattern_extraction.models import ParsedFalsePositive

logger = logging.getLogger(__name__)

# Regex patterns reused from src/common/constants.py and src/services/vector_store_service.py
_CWE_PATTERN = re.compile(r"CWE-\d+")
_ISSUE_TYPE_PATTERN = re.compile(r"Error:\s*([^\s(]+)")
_CODE_BLOCK_LINE_PATTERN = re.compile(r"#\s*\d+\|")
_PATH_LINE_PATTERN = re.compile(r"^([^:]+):(\d+):\s?(.*)")

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
    """Parse a single ignore.err entry.

    Logic adapted from vector_store_service._extract_metadata_from_known_false_positives.
    """
    lines = item.split("\n")

    # Extract issue type from first line: "Error: <TYPE> (..."
    type_match = _ISSUE_TYPE_PATTERN.search(lines[0])
    if not type_match:
        logger.warning(f"Missing issue type in ignore.err entry #{entry_index}, skipping")
        return None
    issue_type = type_match.group(1).rstrip(":")

    # Extract CWE
    cwe_match = _CWE_PATTERN.search(lines[0])
    cwe = cwe_match.group(0) if cwe_match else None

    # Find boundary between error trace and justification
    # Scan backwards to find last code block or path line
    reason_start_line_index = len(lines) - 1
    for line_index in range(len(lines) - 1, -1, -1):
        stripped = lines[line_index].strip()
        if _CODE_BLOCK_LINE_PATTERN.match(stripped) or _PATH_LINE_PATTERN.match(stripped):
            reason_start_line_index = line_index + 1
            break

    # Extract justification (lines after the trace)
    reason_lines = [
        line.lstrip("#").strip() for line in lines[reason_start_line_index:] if line.strip()
    ]
    analyst_justification = "\n".join(reason_lines)

    # Error trace is lines 1 through reason boundary
    error_trace = "\n".join(lines[1:reason_start_line_index])

    return ParsedFalsePositive(
        package_name=package_name,
        issue_type=issue_type,
        cwe=cwe,
        error_trace=error_trace,
        source_code=None,  # ignore.err doesn't have separate source code section
        analyst_justification=analyst_justification,
        verdict="FALSE POSITIVE",  # ignore.err only contains false positives
        entry_index=entry_index,
    )


def _get_parser_config(input_format: str):
    """Return (file_match_fn, parser_fn) for the given input format."""
    if input_format == "ground_truth":
        return lambda f: f.endswith(".txt"), parse_ground_truth_file
    if input_format == "ignore_err":
        return lambda f: f == "ignore.err", parse_ignore_err_file
    raise ValueError(f"Unknown input format: {input_format}")


def _collect_matching_files(dir_path: str, file_match_fn) -> List[str]:
    """Return sorted list of file paths matching the format filter."""
    paths = []
    for filename in sorted(os.listdir(dir_path)):
        if filename.startswith("_"):
            continue
        if not file_match_fn(filename):
            continue
        file_path = os.path.join(dir_path, filename)
        if os.path.isfile(file_path):
            paths.append(file_path)
    return paths


def parse_directory(
    dir_path: str, input_format: str = "ground_truth"
) -> Dict[str, List[ParsedFalsePositive]]:
    """Parse all files in a directory, keyed by package name.

    Args:
        dir_path: Path to directory containing input files
        input_format: "ground_truth" for test_pattern_data/*.txt,
                      "ignore_err" for ignore.err files

    Returns:
        Dict mapping package name to list of parsed entries
    """
    if not os.path.isdir(dir_path):
        raise FileNotFoundError(f"Input directory not found: {dir_path}")

    file_match_fn, parser_fn = _get_parser_config(input_format)
    file_paths = _collect_matching_files(dir_path, file_match_fn)

    results = {}
    for file_path in file_paths:
        try:
            entries = parser_fn(file_path)
            if entries:
                key = entries[0].package_name
                if key in results:
                    key = os.path.splitext(os.path.basename(file_path))[0]
                results[key] = entries
        except Exception as e:
            logger.error(f"Failed to parse {file_path}: {e}")

    logger.info(
        f"Parsed {len(results)} packages with "
        f"{sum(len(v) for v in results.values())} total entries"
    )
    return results
