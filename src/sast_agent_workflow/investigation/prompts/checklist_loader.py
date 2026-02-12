"""
Checklist Loader - Load vulnerability-type specific evidence checklists.

This module provides functionality to:
- Parse CWE identifiers from issue descriptions
- Load appropriate checklist templates based on CWE
- Provide guidance and stop criteria for research agents
"""

import logging
import re
from pathlib import Path
from typing import Dict, Optional

import yaml

logger = logging.getLogger(__name__)


class ChecklistLoader:
    """Load and manage vulnerability-type specific checklists."""

    def __init__(self, checklists_dir: Optional[Path] = None):
        """
        Initialize the checklist loader.

        Args:
            checklists_dir: Path to checklists directory. If None, uses default location.
        """
        if checklists_dir is None:
            # Default to checklists/ relative to this file
            self.checklists_dir = Path(__file__).parent / "checklists"
        else:
            self.checklists_dir = Path(checklists_dir)

        # Cache for loaded templates
        self._templates: Dict[str, Dict] = {}

        # CWE to template file mapping
        self._cwe_mapping: Dict[str, str] = {}
        self._load_cwe_mappings()

    def _load_cwe_mappings(self) -> None:
        """Load CWE to template file mappings from YAML files."""
        if not self.checklists_dir.exists():
            logger.warning(f"Checklists directory not found: {self.checklists_dir}")
            return

        for yaml_file in self.checklists_dir.glob("*.yaml"):
            try:
                with open(yaml_file, "r") as f:
                    data = yaml.safe_load(f)

                template_key = yaml_file.stem
                cwe_ids = data.get("cwe_ids", [])

                # Cache the template
                self._templates[template_key] = data

                # Map each CWE to this template
                for cwe in cwe_ids:
                    # Store both with and without prefix
                    self._cwe_mapping[cwe.upper()] = template_key
                    if cwe.upper().startswith("CWE-"):
                        cwe_num = cwe.upper().replace("CWE-", "")
                        self._cwe_mapping[cwe_num] = template_key

            except Exception as e:
                logger.warning(f"Failed to load checklist from {yaml_file}: {e}")

        logger.debug(
            f"Loaded {len(self._templates)} checklist templates, "
            f"{len(self._cwe_mapping)} CWE mappings"
        )

    def _extract_cwe_from_description(self, description: str) -> Optional[str]:
        """
        Extract CWE identifier from issue description.

        Looks for patterns like:
        - "CWE-119"
        - "**CWE:** CWE-119"
        - "CWE: 119"

        Args:
            description: Issue description string

        Returns:
            CWE identifier (e.g., "CWE-119") or None if not found
        """
        # Try to find CWE-XXX pattern
        match = re.search(r"CWE-(\d+)", description, re.IGNORECASE)
        if match:
            return f"CWE-{match.group(1)}"

        # Try to find "CWE: XXX" pattern
        match = re.search(r"CWE:\s*(\d+)", description, re.IGNORECASE)
        if match:
            return f"CWE-{match.group(1)}"

        return None

    def _get_template_for_cwe(self, cwe: str) -> Dict:
        """
        Get template for a CWE identifier.

        Args:
            cwe: CWE identifier (e.g., "CWE-119" or "119")

        Returns:
            Template dictionary or generic template if CWE not found
        """
        # Normalize CWE
        cwe_upper = cwe.upper()
        cwe_num = cwe_upper.replace("CWE-", "")

        # Look up in mapping
        template_key = self._cwe_mapping.get(cwe_upper) or self._cwe_mapping.get(cwe_num)

        if template_key and template_key in self._templates:
            return self._templates[template_key]

        # Fall back to generic
        return self._templates.get("generic", self._get_default_template())

    def _get_default_template(self) -> Dict:
        """Return a minimal default template if no templates are loaded."""
        return {
            "vuln_type": "Generic",
            "cwe_ids": [],
            "guidance": "Gather evidence for the reported vulnerability.",
            "checklist": [
                "Input source identified",
                "Data flow traced",
                "Vulnerable operation documented",
                "Validation checked",
            ],
        }

    def get_checklist(self, issue_description: str) -> Dict:
        """
        Get checklist for an issue based on its description.

        Args:
            issue_description: Full issue description containing CWE info

        Returns:
            Dictionary with keys:
            - vuln_type: Human-readable vulnerability type
            - guidance: Investigation guidance text
            - checklist: List of evidence items to gather
        """
        # Extract CWE from description
        cwe = self._extract_cwe_from_description(issue_description)

        if cwe:
            template = self._get_template_for_cwe(cwe)
            logger.debug(f"Using '{template.get('vuln_type', 'unknown')}' checklist for {cwe}")
        else:
            template = self._templates.get("generic", self._get_default_template())
            logger.debug("No CWE found in description, using generic checklist")

        return {
            "vuln_type": template.get("vuln_type", "Generic"),
            "guidance": template.get("guidance", ""),
            "checklist": template.get("checklist", []),
        }

    def format_checklist_for_prompt(self, checklist_data: Dict) -> str:
        """
        Format checklist data for inclusion in a prompt.

        Args:
            checklist_data: Dictionary from get_checklist()

        Returns:
            Formatted string for prompt
        """
        vuln_type = checklist_data.get("vuln_type", "Generic")
        guidance = checklist_data.get("guidance", "")
        checklist = checklist_data.get("checklist", [])

        # Format checklist items
        checklist_items = "\n".join(f"[ ] {item}" for item in checklist)

        return f"""**EVIDENCE CHECKLIST ({vuln_type}):**
{checklist_items}

**INVESTIGATION GUIDANCE:**
{guidance}"""


# Module-level instance for convenience
_loader: Optional[ChecklistLoader] = None


def get_checklist_loader() -> ChecklistLoader:
    """Get or create the global ChecklistLoader instance."""
    global _loader
    if _loader is None:
        _loader = ChecklistLoader()
    return _loader


def get_checklist_for_issue(issue_description: str) -> Dict:
    """
    Convenience function to get checklist for an issue.

    Args:
        issue_description: Issue description containing CWE info

    Returns:
        Checklist dictionary
    """
    return get_checklist_loader().get_checklist(issue_description)


def format_checklist(issue_description: str) -> str:
    """
    Convenience function to get formatted checklist for prompt.

    Args:
        issue_description: Issue description containing CWE info

    Returns:
        Formatted checklist string for prompt
    """
    loader = get_checklist_loader()
    checklist_data = loader.get_checklist(issue_description)
    return loader.format_checklist_for_prompt(checklist_data)
