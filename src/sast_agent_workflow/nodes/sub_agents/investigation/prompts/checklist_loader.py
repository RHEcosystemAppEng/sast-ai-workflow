"""
Checklist Loader - Load vulnerability-type specific evidence checklists.

This module provides functionality to:
- Load appropriate checklist templates based on CWE identifier and repository language
- Provide guidance and stop criteria for research agents

Directory layout (Option C — language outer, CWE inner):
    checklists/
        go/
            buffer_overflow.yaml
            command_injection.yaml
            ...
        c/
            buffer_overflow.yaml
            ...

Fallback chain per lookup:
  1. checklists/{language}/{cwe_template}.yaml  — exact language + CWE match
  2. checklists/{language}/generic.yaml          — language-specific generic
  3. hardcoded minimal template                  — last resort

Supported languages are declared in SUPPORTED_LANGUAGES / SupportedLanguage.
"""

import logging
from pathlib import Path
from typing import Dict, Literal, Optional

import yaml
from jsonschema import ValidationError, validate

logger = logging.getLogger(__name__)

# Canonical set of supported repository languages.
# Add a new entry here and create the matching checklists/<lang>/ directory
# to extend support to a new language.
SupportedLanguage = Literal["c", "go"]
SUPPORTED_LANGUAGES: frozenset[str] = frozenset({"c", "go"})

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

    def __init__(self, language: SupportedLanguage, checklists_dir: Optional[Path] = None):
        """
        Initialize the checklist loader.

        Args:
            language: Repository language (e.g. "go", "c"). Determines which
                      subdirectory under checklists/ is used.
            checklists_dir: Base checklists directory. If None, uses the default
                            location next to this module file.
        """
        base_dir = (
            Path(checklists_dir)
            if checklists_dir is not None
            else Path(__file__).parent / "research" / "checklists"
        )
        self.language = language
        self.checklists_dir = base_dir

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
        lang_dir = self.checklists_dir / self.language

        if not lang_dir.exists():
            logger.warning(f"Checklists directory not found: {lang_dir}")
            return

        for yaml_file in lang_dir.glob("*.yaml"):
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
_loaders: Dict[str, ChecklistLoader] = {}


def get_checklist_loader(language: SupportedLanguage) -> ChecklistLoader:
    """Get or create a ChecklistLoader for the given language."""
    if language not in _loaders:
        _loaders[language] = ChecklistLoader(language=language)
    return _loaders[language]


def get_checklist_for_issue(issue_cwe: str, language: SupportedLanguage) -> Dict:
    """
    Convenience function to get checklist for an issue.

    Args:
        issue_cwe: CWE identifier (e.g., "CWE-119" or "119")
        language: Repository language (e.g. "go", "c")

    Returns:
        Checklist dictionary
    """
    return get_checklist_loader(language).get_checklist(issue_cwe)


def format_checklist(issue_cwe: str, language: SupportedLanguage) -> str:
    """
    Convenience function to get formatted checklist for prompt.

    Args:
        issue_cwe: CWE identifier (e.g., "CWE-119" or "119")
        language: Repository language (e.g. "go", "c")

    Returns:
        Formatted checklist string for prompt
    """
    loader = get_checklist_loader(language)
    checklist_data = loader.get_checklist(issue_cwe)
    return loader.format_checklist_for_prompt(checklist_data)
