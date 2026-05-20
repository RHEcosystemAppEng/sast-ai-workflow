"""
Checklist Loader - Load vulnerability-type specific evidence checklists.

This module provides functionality to:
- Load appropriate checklist templates based on CWE identifier and repository language
- Provide guidance and stop criteria for research agents

Directory layout (language-first):
    research/
        generic/
            checklists/
                generic.yaml   — default when repo language is unset
        go/
            checklists/
                ...
        c/
            checklists/
                ...

Fallback chain per lookup:
  1. research/{language}/checklists/{cwe_template}.yaml  — exact language + CWE match
  2. research/{language}/checklists/generic.yaml          — language-specific generic
  3. hardcoded minimal template                           — last resort if YAML missing

Supported languages are declared in RepoLanguage (including GENERIC).
"""

import logging
from pathlib import Path
from typing import Dict, Optional

import yaml
from jsonschema import ValidationError, validate

from common.repo_language import RepoLanguage

logger = logging.getLogger(__name__)

# Schema for checklist YAML files
_CHECKLIST_SCHEMA = {
    "type": "object",
    "required": ["vuln_type", "cwe_ids", "guidance", "checklist"],
    "properties": {
        "vuln_type": {"type": "string"},
        "cwe_ids": {
            "type": "array",
            "items": {"type": "string"},
        },
        "guidance": {"type": "string"},
        "checklist": {
            "type": "array",
            "items": {"type": "string"},
            "minItems": 1,
        },
    },
    "additionalProperties": False,
}


class ChecklistLoader:
    """Load and manage vulnerability-type specific checklists for a given language."""

    def __init__(self, language: RepoLanguage, checklists_dir: Optional[Path] = None):
        """
        Initialize the checklist loader.

        Args:
            language: Repository language. Determines which
                      research/<language>/checklists/ directory is used.
            checklists_dir: Language checklists directory. If None, uses
                            research/<language>/checklists/ next to this module.
        """
        self.language = language
        self.checklists_dir = (
            Path(checklists_dir)
            if checklists_dir is not None
            else Path(__file__).parent / "research" / language.value / "checklists"
        )

        # Cache for loaded templates
        self._templates: Dict[str, Dict] = {}

        # CWE to template file mapping
        self._cwe_mapping: Dict[str, str] = {}
        self._load_cwe_mappings()

    def _load_cwe_mappings(self) -> None:
        """Load CWE-to-template mappings from the language-specific subdirectory.

        If the language subdirectory does not exist, logs a warning and leaves
        the loader empty (callers will fall through to _get_default_template).
        """
        if not self.checklists_dir.exists():
            logger.warning(f"Checklists directory not found: {self.checklists_dir}")
            return

        for yaml_file in self.checklists_dir.glob("*.yaml"):
            try:
                with open(yaml_file, "r") as f:
                    data = yaml.safe_load(f)

                validate(instance=data, schema=_CHECKLIST_SCHEMA)

                template_key = yaml_file.stem
                cwe_ids = data["cwe_ids"]

                self._templates[template_key] = data

                for cwe in cwe_ids:
                    self._cwe_mapping[cwe.upper()] = template_key
                    if cwe.upper().startswith("CWE-"):
                        cwe_num = cwe.upper().replace("CWE-", "")
                        self._cwe_mapping[cwe_num] = template_key

            except ValidationError as e:
                logger.warning(f"Schema validation failed for {yaml_file}: {e.message}")
            except Exception as e:
                logger.warning(f"Failed to load checklist from {yaml_file}: {e}")

        logger.debug(
            f"Loaded {len(self._templates)} checklist templates "
            f"({self.language}), {len(self._cwe_mapping)} CWE mappings"
        )

    def _get_template_for_cwe(self, cwe: str) -> Dict:
        """
        Get template for a CWE identifier.

        Args:
            cwe: CWE identifier (e.g., "CWE-119" or "119")

        Returns:
            Template dictionary or generic template if CWE not found
        """
        cwe_upper = cwe.upper()
        cwe_num = cwe_upper.replace("CWE-", "")

        template_key = self._cwe_mapping.get(cwe_upper) or self._cwe_mapping.get(cwe_num)

        if template_key and template_key in self._templates:
            return self._templates[template_key]

        return self._templates.get("generic", self._get_default_template())

    @staticmethod
    def default_template() -> Dict:
        """Language-neutral checklist used when repo language is unknown."""
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

    def _get_default_template(self) -> Dict:
        """Return a minimal default template if no templates are loaded."""
        return self.default_template()

    def get_checklist(self, issue_cwe: str) -> Dict:
        """
        Get checklist for an issue based on its CWE identifier.

        Args:
            issue_cwe: CWE identifier (e.g., "CWE-119" or "119")

        Returns:
            Dictionary with keys:
            - vuln_type: Human-readable vulnerability type
            - guidance: Investigation guidance text
            - checklist: List of evidence items to gather
        """
        if issue_cwe:
            template = self._get_template_for_cwe(issue_cwe)
            logger.debug(
                f"Using '{template.get('vuln_type', 'unknown')}' checklist "
                f"for {issue_cwe} ({self.language})"
            )
        else:
            template = self._templates.get("generic", self._get_default_template())
            logger.debug(f"No CWE found in description, using generic checklist ({self.language})")

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

        checklist_items = "\n".join(f"[ ] {item}" for item in checklist)

        return f"""**EVIDENCE CHECKLIST ({vuln_type}):**
{checklist_items}

**INVESTIGATION GUIDANCE:**
{guidance}"""


# Per-language singleton cache
_loaders: Dict[RepoLanguage, ChecklistLoader] = {}


def get_checklist_loader(language: RepoLanguage) -> ChecklistLoader:
    """Get or create a ChecklistLoader for the given language."""
    if language not in _loaders:
        _loaders[language] = ChecklistLoader(language=language)
    return _loaders[language]


def get_checklist_for_issue(issue_cwe: str, language: RepoLanguage) -> Dict:
    """
    Convenience function to get checklist for an issue.

    Args:
        issue_cwe: CWE identifier (e.g., "CWE-119" or "119")
        language: Repository language

    Returns:
        Checklist dictionary
    """
    return get_checklist_loader(language).get_checklist(issue_cwe)


def format_checklist(issue_cwe: str, language: RepoLanguage) -> str:
    """
    Convenience function to get formatted checklist for prompt.

    Args:
        issue_cwe: CWE identifier (e.g., "CWE-119" or "119")
        language: Repository language (use RepoLanguage.GENERIC when unset)

    Returns:
        Formatted checklist string for prompt
    """
    loader = get_checklist_loader(language)
    checklist_data = loader.get_checklist(issue_cwe)
    return loader.format_checklist_for_prompt(checklist_data)
