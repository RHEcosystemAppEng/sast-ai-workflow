"""
Tests for investigation/prompts/checklist_loader.py.

Covers: ChecklistLoader initialization, CWE extraction, checklist lookup
by CWE, generic fallback, default template, prompt formatting, and
module-level convenience functions.
"""

from pathlib import Path

import pytest

import sast_agent_workflow.investigation.prompts.checklist_loader as _checklist_mod
from sast_agent_workflow.investigation.prompts.checklist_loader import (
    ChecklistLoader,
    format_checklist,
    get_checklist_for_issue,
    get_checklist_loader,
)

_MOD = "sast_agent_workflow.investigation.prompts.checklist_loader"

# Derive checklists path from the installed module rather than hardcoded parents[N]
_CHECKLISTS_DIR = Path(_checklist_mod.__file__).parent / "checklists"


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def loader():
    """ChecklistLoader initialised against the real YAML checklists."""
    return ChecklistLoader(checklists_dir=_CHECKLISTS_DIR)


@pytest.fixture(autouse=True)
def _reset_global_loader():
    """Reset the module-level singleton before each test."""
    _checklist_mod._loader = None
    yield
    _checklist_mod._loader = None


# ---------------------------------------------------------------------------
# ChecklistLoader.__init__
# ---------------------------------------------------------------------------


class TestChecklistLoaderInit:
    """Tests for ChecklistLoader initialization."""

    def test__loads_all_yaml_templates(self, loader):
        """All 8 YAML checklist files should be loaded as templates."""
        assert len(loader._templates) == 8

    def test__builds_cwe_mappings(self, loader):
        """CWE mappings should be populated from YAML cwe_ids fields."""
        assert len(loader._cwe_mapping) > 0
        # Spot-check a few known CWE entries
        assert "CWE-119" in loader._cwe_mapping
        assert "CWE-476" in loader._cwe_mapping
        assert "CWE-416" in loader._cwe_mapping

    def test__maps_cwe_with_and_without_prefix(self, loader):
        """Each CWE should be accessible both as 'CWE-119' and '119'."""
        assert "CWE-119" in loader._cwe_mapping
        assert "119" in loader._cwe_mapping
        assert loader._cwe_mapping["CWE-119"] == loader._cwe_mapping["119"]

    def test__nonexistent_dir_loads_no_templates(self, tmp_path):
        """A non-existent checklists directory should produce an empty loader."""
        loader = ChecklistLoader(checklists_dir=tmp_path / "does_not_exist")

        assert len(loader._templates) == 0
        assert len(loader._cwe_mapping) == 0

    def test__default_dir_resolves_relative_to_module(self):
        """When no dir is given, checklists_dir should point next to the module."""
        loader = ChecklistLoader()

        assert loader.checklists_dir.name == "checklists"
        assert loader.checklists_dir.parent.name == "prompts"


# ---------------------------------------------------------------------------
# get_checklist  (CWE-specific lookup)
# ---------------------------------------------------------------------------


class TestGetChecklist:
    """Tests for get_checklist returning the correct template for a CWE."""

    def test__buffer_overflow_cwe_119(self, loader):
        """CWE-119 should return the Buffer Overflow checklist."""
        result = loader.get_checklist("CWE-119")

        assert result["vuln_type"] == "Buffer Overflow"
        assert len(result["checklist"]) > 0
        assert result["guidance"] != ""

    def test__buffer_overflow_cwe_122(self, loader):
        """CWE-122 (heap overflow) should also map to Buffer Overflow."""
        result = loader.get_checklist("CWE-122")
        assert result["vuln_type"] == "Buffer Overflow"

    def test__null_pointer_cwe_476(self, loader):
        """CWE-476 should return Null Pointer Dereference checklist."""
        result = loader.get_checklist("CWE-476")
        assert result["vuln_type"] == "Null Pointer Dereference"

    def test__use_after_free_cwe_416(self, loader):
        """CWE-416 should return Use After Free checklist."""
        result = loader.get_checklist("CWE-416")
        assert result["vuln_type"] == "Use After Free"

    def test__command_injection_cwe_78(self, loader):
        """CWE-78 should return Command Injection checklist."""
        result = loader.get_checklist("CWE-78")
        assert result["vuln_type"] == "Command Injection"

    def test__format_string_cwe_134(self, loader):
        """CWE-134 should return Format String checklist."""
        result = loader.get_checklist("CWE-134")
        assert result["vuln_type"] == "Format String"

    def test__integer_overflow_cwe_190(self, loader):
        """CWE-190 should return Integer Overflow checklist."""
        result = loader.get_checklist("CWE-190")
        assert result["vuln_type"] == "Integer Overflow"

    def test__race_condition_cwe_367(self, loader):
        """CWE-367 should return Race Condition checklist."""
        result = loader.get_checklist("CWE-367")
        assert result["vuln_type"] == "Race Condition"

    def test__unknown_cwe_falls_back_to_generic(self, loader):
        """An unrecognised CWE should return the generic checklist."""
        result = loader.get_checklist("CWE-9999")
        assert result["vuln_type"] == "Generic"

    def test__no_cwe_falls_back_to_generic(self, loader):
        """Empty CWE string should return the generic checklist."""
        result = loader.get_checklist("")
        assert result["vuln_type"] == "Generic"

    def test__result_has_required_keys(self, loader):
        """Every checklist result should contain vuln_type, guidance, checklist."""
        result = loader.get_checklist("CWE-119")

        assert "vuln_type" in result
        assert "guidance" in result
        assert "checklist" in result
        assert isinstance(result["checklist"], list)


# ---------------------------------------------------------------------------
# _get_default_template
# ---------------------------------------------------------------------------


class TestGetDefaultTemplate:
    """Tests for the hardcoded fallback template."""

    def test__returns_generic_vuln_type(self, loader):
        """Default template should have vuln_type 'Generic'."""
        tmpl = loader._get_default_template()
        assert tmpl["vuln_type"] == "Generic"

    def test__has_four_checklist_items(self, loader):
        """Default template should contain 4 generic checklist items."""
        tmpl = loader._get_default_template()
        assert len(tmpl["checklist"]) == 4

    def test__used_when_no_yaml_files(self, tmp_path):
        """Default template should be used when checklists dir is empty."""
        empty_dir = tmp_path / "empty_checklists"
        empty_dir.mkdir()

        loader = ChecklistLoader(checklists_dir=empty_dir)
        result = loader.get_checklist("CWE-119")

        assert result["vuln_type"] == "Generic"
        assert len(result["checklist"]) == 4


# ---------------------------------------------------------------------------
# format_checklist_for_prompt
# ---------------------------------------------------------------------------


class TestFormatChecklistForPrompt:
    """Tests for formatting checklist data into a prompt string."""

    def test__contains_vuln_type_header(self, loader):
        """Formatted output should include the vulnerability type in the header."""
        data = loader.get_checklist("CWE-476")
        formatted = loader.format_checklist_for_prompt(data)

        assert "**EVIDENCE CHECKLIST (Null Pointer Dereference):**" in formatted

    def test__contains_checklist_items_with_checkboxes(self, loader):
        """Each checklist item should appear with a [ ] checkbox prefix."""
        data = loader.get_checklist("CWE-476")
        formatted = loader.format_checklist_for_prompt(data)

        for item in data["checklist"]:
            assert f"[ ] {item}" in formatted

    def test__contains_guidance_header(self, loader):
        """Formatted output should include the investigation guidance section."""
        data = loader.get_checklist("CWE-119")
        formatted = loader.format_checklist_for_prompt(data)

        assert "**INVESTIGATION GUIDANCE:**" in formatted

    def test__contains_guidance_content(self, loader):
        """Formatted output should include the actual guidance text."""
        data = loader.get_checklist("CWE-119")
        formatted = loader.format_checklist_for_prompt(data)

        assert "BUFFER OVERFLOW" in formatted

    def test__handles_empty_checklist_gracefully(self, loader):
        """Should handle a checklist dict with empty lists without error."""
        data = {"vuln_type": "Test", "guidance": "Test guidance", "checklist": []}
        formatted = loader.format_checklist_for_prompt(data)

        assert "**EVIDENCE CHECKLIST (Test):**" in formatted


# ---------------------------------------------------------------------------
# Module-level convenience functions
# ---------------------------------------------------------------------------


class TestConvenienceFunctions:
    """Tests for format_checklist, get_checklist_for_issue, get_checklist_loader."""

    def test__get_checklist_loader__returns_singleton(self):
        """Subsequent calls to get_checklist_loader should return the same instance."""
        loader1 = get_checklist_loader()
        loader2 = get_checklist_loader()
        assert loader1 is loader2

    def test__get_checklist_for_issue__returns_dict(self):
        """get_checklist_for_issue should return a dict with expected keys."""
        result = get_checklist_for_issue("CWE-119")

        assert isinstance(result, dict)
        assert "vuln_type" in result
        assert "checklist" in result
        assert "guidance" in result

    def test__format_checklist__returns_string(self):
        """format_checklist should return a non-empty formatted string."""
        result = format_checklist("CWE-476")

        assert isinstance(result, str)
        assert len(result) > 0
        assert "EVIDENCE CHECKLIST" in result

    def test__format_checklist__matches_loader_output(self):
        """format_checklist convenience should produce same output as manual steps."""
        loader = get_checklist_loader()
        data = loader.get_checklist("CWE-416")
        expected = loader.format_checklist_for_prompt(data)

        result = format_checklist("CWE-416")

        assert result == expected
