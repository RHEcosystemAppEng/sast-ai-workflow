from dataclasses import dataclass, field
from typing import Dict, Optional


@dataclass
class ProcessMiningEntry:
    """Represents a single ground-truth entry with human annotations and source code."""

    # Issue metadata (parsed from Finding column)
    issue_type: str
    issue_cwe: str
    error_trace: str

    # Human annotations from Excel
    is_false_positive: bool          # Parsed from 'False Positive?' column (y/n)
    human_justification: str         # From 'Hint' column
    comment: Optional[str] = None    # From 'Comment' column (optional)
    ai_prediction: Optional[str] = None  # From 'AI prediction' column (optional)

    # Source code mapping: {file_path: source_code}
    source_code: Dict[str, str] = field(default_factory=dict)

    # Raw finding text
    raw_finding: str = ""
