import re
import os
from typing import Dict, List, Optional
from pathlib import Path


class ValidationEntry:
    """Represents a single SAST finding entry from validation data."""

    def __init__(
        self,
        entry_id: str,
        package_name: str,
        issue_type: str,
        cwe: str,
        error_trace: str,
        source_code: str,
        ground_truth_classification: str,
        ground_truth_justification: str,
        analyst_comment: str = "",
        file_path: str = "",
        line_number: int = 0
    ):
        self.entry_id = entry_id
        self.package_name = package_name
        self.issue_type = issue_type
        self.cwe = cwe
        self.error_trace = error_trace
        self.source_code = source_code
        self.ground_truth_classification = ground_truth_classification
        self.ground_truth_justification = ground_truth_justification
        self.analyst_comment = analyst_comment
        self.file_path = file_path
        self.line_number = line_number

    def to_dict(self) -> Dict:
        """Convert to dictionary representation."""
        return {
            "entry_id": self.entry_id,
            "package_name": self.package_name,
            "issue_type": self.issue_type,
            "cwe": self.cwe,
            "error_trace": self.error_trace,
            "source_code": self.source_code,
            "ground_truth_classification": self.ground_truth_classification,
            "ground_truth_justification": self.ground_truth_justification,
            "analyst_comment": self.analyst_comment,
            "file_path": self.file_path,
            "line_number": self.line_number
        }

    def get_masked_entry(self) -> Dict:
        """
        Get entry with ground truth masked for LLM classification.

        Returns entry without classification and justification - only the
        information an LLM would use to make a prediction.
        """
        return {
            "entry_id": self.entry_id,
            "package_name": self.package_name,
            "issue_type": self.issue_type,
            "cwe": self.cwe,
            "error_trace": self.error_trace,
            "source_code": self.source_code,
            "file_path": self.file_path,
            "line_number": self.line_number
        }


class ValidationEntryParser:
    """Parse validation .txt files into structured ValidationEntry objects."""

    def __init__(self):
        self.entries: List[ValidationEntry] = []
        self.skipped_no_source_code: int = 0

    def parse_file(self, file_path: Path) -> List[ValidationEntry]:
        """
        Parse a single validation .txt file.

        Args:
            file_path: Path to validation .txt file

        Returns:
            List of ValidationEntry objects extracted from file
        """
        if not file_path.exists():
            raise FileNotFoundError(f"Validation file not found: {file_path}")

        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()

        package_match = re.search(r'Package:\s*([^\n]+)', content)
        package_name = package_match.group(1).strip() if package_match else file_path.stem

        self.skipped_no_source_code = 0

        entries = self._split_entries(content, package_name)

        print(f"Entries with valid source code: {len(entries)}, Skipped (no source code): {self.skipped_no_source_code}")

        return entries

    def _split_entries(self, content: str, package_name: str) -> List[ValidationEntry]:
        """Split file content into individual entries."""
        entries = []

        entry_pattern = r'---\s*\nEntry #(\d+):(.*?)(?=\n---\s*\nEntry #|\Z)'
        matches = re.finditer(entry_pattern, content, re.DOTALL)

        for match in matches:
            entry_num = match.group(1)
            entry_content = match.group(2)

            try:
                has_source_code = self._has_valid_source_code(entry_content)
                if not has_source_code:
                    self.skipped_no_source_code += 1
                    continue

                entry = self._parse_entry(
                    entry_num,
                    entry_content,
                    package_name
                )
                if entry:
                    entries.append(entry)
            except Exception as e:
                print(f"Warning: Failed to parse entry #{entry_num} in {package_name}: {e}")
                continue

        return entries

    def _has_valid_source_code(self, entry_content: str) -> bool:
        """Check if entry has valid source code available."""
        source_code_match = re.search(
            r'Source Code.*?:.*?\n```(?:c|cpp)?\n(.*?)```',
            entry_content,
            re.DOTALL
        )
        if not source_code_match:
            return False
        source_code = source_code_match.group(1).strip()
        if not source_code or "source code not available" in source_code.lower():
            return False
        return True

    def _parse_entry(
        self,
        entry_num: str,
        entry_content: str,
        package_name: str
    ) -> Optional[ValidationEntry]:
        """
        Parse a single entry from its text content.

        Args:
            entry_num: Entry number
            entry_content: Raw text content of entry
            package_name: Name of package this entry belongs to

        Returns:
            ValidationEntry object or None if parsing fails
        """
        issue_type_match = re.search(r'Issue Type:\s*([^\n]+)', entry_content)
        issue_type = issue_type_match.group(1).strip() if issue_type_match else ""

        cwe_match = re.search(r'CWE:\s*([^\n]+)', entry_content)
        cwe = cwe_match.group(1).strip() if cwe_match else ""

        error_trace_match = re.search(
            r'Error Trace:(.*?)(?=\nSource Code|\nGround Truth|$)',
            entry_content,
            re.DOTALL
        )
        error_trace = error_trace_match.group(1).strip() if error_trace_match else ""

        source_code_match = re.search(
            r'Source Code.*?:.*?\n```(?:c|cpp)?\n(.*?)```',
            entry_content,
            re.DOTALL
        )
        source_code = source_code_match.group(1).strip() if source_code_match else ""

        gt_match = re.search(
            r'Ground Truth Classification:\s*(TRUE.POSITIVE|FALSE.POSITIVE)',
            entry_content,
            re.IGNORECASE
        )
        if not gt_match:
            return None

        gt_text = gt_match.group(1).upper().replace(' ', '_').replace('.', '_')
        ground_truth_classification = gt_text

        justification_match = re.search(
            r'Human Expert Justification:\s*(.*?)(?=\nAnalyst Comment:|\n---|\Z)',
            entry_content,
            re.DOTALL
        )
        ground_truth_justification = justification_match.group(1).strip() if justification_match else ""

        comment_match = re.search(
            r'Analyst Comment:\s*(.*?)(?=\n---|\Z)',
            entry_content,
            re.DOTALL
        )
        analyst_comment = comment_match.group(1).strip() if comment_match else ""

        file_line_match = re.search(r'([^\n]+\.c[^\n]*):(\d+):', error_trace)
        file_path = file_line_match.group(1).strip() if file_line_match else ""
        line_number = int(file_line_match.group(2)) if file_line_match else 0

        entry_id = f"{package_name}_entry_{entry_num}"

        return ValidationEntry(
            entry_id=entry_id,
            package_name=package_name,
            issue_type=issue_type,
            cwe=cwe,
            error_trace=error_trace,
            source_code=source_code,
            ground_truth_classification=ground_truth_classification,
            ground_truth_justification=ground_truth_justification,
            analyst_comment=analyst_comment,
            file_path=file_path,
            line_number=line_number
        )

    def parse_directory(self, directory_path: Path) -> List[ValidationEntry]:
        """
        Parse all .txt files in a directory.

        Args:
            directory_path: Path to directory containing validation .txt files

        Returns:
            List of all ValidationEntry objects from all files
        """
        all_entries = []
        total_skipped_no_source_code = 0

        if not directory_path.exists():
            raise FileNotFoundError(f"Directory not found: {directory_path}")

        txt_files = [
            f for f in directory_path.glob("*.txt")
            if not f.name.startswith("_")
        ]

        for txt_file in sorted(txt_files):
            try:
                entries = self.parse_file(txt_file)
                all_entries.extend(entries)
                total_skipped_no_source_code += self.skipped_no_source_code
                print(f"Parsed {len(entries)} entries from {txt_file.name}")
            except Exception as e:
                print(f"Error parsing {txt_file.name}: {e}")
                continue

        print(f"\nTotal entries with valid source code: {len(all_entries)}, Total skipped (no source code): {total_skipped_no_source_code}")

        return all_entries


def main():
    """Test the parser with a sample file."""
    import sys

    if len(sys.argv) < 2:
        print("Usage: python entry_parser.py <validation_file_or_dir>")
        sys.exit(1)

    path = Path(sys.argv[1])
    parser = ValidationEntryParser()

    if path.is_file():
        entries = parser.parse_file(path)
    elif path.is_dir():
        entries = parser.parse_directory(path)
    else:
        print(f"Invalid path: {path}")
        sys.exit(1)

    print(f"\nTotal entries parsed: {len(entries)}")
    print(f"TP entries: {sum(1 for e in entries if 'TRUE' in e.ground_truth_classification)}")
    print(f"FP entries: {sum(1 for e in entries if 'FALSE' in e.ground_truth_classification)}")

    if entries:
        print("\nSample entry (masked):")
        print(entries[0].get_masked_entry())


if __name__ == "__main__":
    main()
