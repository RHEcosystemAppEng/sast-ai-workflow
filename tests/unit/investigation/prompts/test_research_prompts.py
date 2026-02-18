"""
Tests for investigation/prompts/research.py.

Covers: build_code_bank, build_research_instructions (initial and continuation),
_extract_fetch_info, _build_code_bank_files_summary, _build_tool_history.
"""

from unittest.mock import patch

from sast_agent_workflow.nodes.sub_agents.investigation.prompts.research import (
    _build_code_bank_files_summary,
    _build_tool_history,
    _extract_fetch_info,
    build_code_bank,
    build_research_instructions,
)

_MOD = "sast_agent_workflow.nodes.sub_agents.investigation.prompts.research"


# ---------------------------------------------------------------------------
# build_code_bank
# ---------------------------------------------------------------------------


class TestBuildCodeBank:
    """Tests for build_code_bank."""

    def test__empty_files_returns_placeholder(self):
        """Empty fetched_files dict should return the 'no code fetched' placeholder."""
        result = build_code_bank({})

        assert "No code fetched yet" in result

    def test__none_equivalent_returns_placeholder(self):
        """None-ish (empty) input should return placeholder."""
        result = build_code_bank({})
        assert "CODE BANK" not in result or "No code fetched" in result

    def test__single_tool_single_result(self):
        """Single tool with one result should appear in the CODE BANK."""
        files = {"fetch_code": ["int main() { return 0; }"]}
        result = build_code_bank(files)

        assert "CODE BANK" in result
        assert "int main()" in result

    def test__multiple_tools_multiple_results(self):
        """Multiple tools with results should all appear in the bank."""
        files = {
            "fetch_code": ["void foo() {}", "void bar() {}"],
            "search_codebase": ["match: line 42"],
        }
        result = build_code_bank(files)

        assert "void foo()" in result
        assert "void bar()" in result
        assert "match: line 42" in result

    def test__results_separated_by_divider(self):
        """Multiple results should be separated by a visual divider."""
        files = {"fetch_code": ["FIRST", "SECOND"]}
        result = build_code_bank(files)

        assert "FIRST" in result
        assert "SECOND" in result
        # The separator line
        assert "─" in result

    def test__has_header_and_footer(self):
        """Output should have header and footer borders."""
        files = {"fetch_code": ["code"]}
        result = build_code_bank(files)

        assert result.startswith("═")
        assert result.endswith("═")


# ---------------------------------------------------------------------------
# _extract_fetch_info
# ---------------------------------------------------------------------------


class TestExtractFetchInfo:
    """Tests for _extract_fetch_info parsing code headers."""

    def test__file_and_function(self):
        """Should extract 'file.c:func_name' from full header."""
        content = (
            "=== Fetched Code: unix_name ===\n"
            "File Path: /src/kernel/file.c\n"
            "int unix_name() {}"
        )
        assert _extract_fetch_info(content) == "file.c:unix_name"

    def test__file_path_only(self):
        """Should extract just filename when no function header."""
        content = "=== File: /usr/src/utils.c ===\ncode here"
        assert _extract_fetch_info(content) == "utils.c"

    def test__function_only(self):
        """Should extract just function name when no file path."""
        content = "=== Fetched Code: my_func ===\ncode here"
        assert _extract_fetch_info(content) == "my_func"

    def test__error_trace_format(self):
        """Should extract filename from error trace header."""
        content = "=== vfat.c (from error trace) ===\ncode"
        assert _extract_fetch_info(content) == "vfat.c"

    def test__unknown_when_no_headers(self):
        """Should return 'unknown' when content has no recognisable headers."""
        content = "just some random code without headers"
        assert _extract_fetch_info(content) == "unknown"


# ---------------------------------------------------------------------------
# _build_code_bank_files_summary
# ---------------------------------------------------------------------------


class TestBuildCodeBankFilesSummary:
    """Tests for _build_code_bank_files_summary."""

    def test__empty_returns_none_string(self):
        """Empty fetched_files should return 'None'."""
        assert _build_code_bank_files_summary({}) == "None"

    def test__includes_fetch_code_results(self):
        """Results from 'fetch_code' tool should be included."""
        files = {"fetch_code": ["=== Fetched Code: main ===\nFile Path: /src/main.c\ncode"]}
        result = _build_code_bank_files_summary(files)
        assert "main.c:main" in result

    def test__excludes_search_results(self):
        """Results from 'search_codebase' should NOT be included in summary."""
        files = {
            "search_codebase": ["=== Fetched Code: search_func ===\nFile Path: /src/a.c\ncode"]
        }
        result = _build_code_bank_files_summary(files)
        assert result == "None"

    def test__deduplicates_entries(self):
        """Same file:function appearing twice should only be listed once."""
        content = "=== Fetched Code: func ===\nFile Path: /src/a.c\ncode"
        files = {"fetch_code": [content, content]}
        result = _build_code_bank_files_summary(files)

        assert result.count("a.c:func") == 1

    def test__includes_read_file_results(self):
        """Results from 'read_file' tool should be included."""
        files = {"read_file": ["=== File: /src/utils.h ===\n#define MAX 100"]}
        result = _build_code_bank_files_summary(files)
        assert "utils.h" in result


# ---------------------------------------------------------------------------
# _build_tool_history
# ---------------------------------------------------------------------------


class TestBuildToolHistory:
    """Tests for _build_tool_history."""

    def test__empty_returns_none_yet(self):
        """Empty history should return 'None yet.'."""
        assert _build_tool_history([]) == "None yet."

    def test__successful_calls(self):
        """Entries starting with checkmark should appear under Successful."""
        history = ["✓ fetch_code(main)", "✓ search_codebase(pattern)"]
        result = _build_tool_history(history)

        assert "Successful" in result
        assert "✓ fetch_code(main)" in result

    def test__failed_calls_with_warning(self):
        """Entries starting with ✗ should appear under FAILED with retry warning."""
        history = ["✗ search_codebase(bad_pattern)"]
        result = _build_tool_history(history)

        assert "FAILED" in result
        assert "DO NOT RETRY" in result
        assert "DIFFERENT approach" in result

    def test__mixed_successful_and_failed(self):
        """Mixed history should separate successful and failed entries."""
        history = [
            "✓ fetch_code(main)",
            "✗ search_codebase(missing)",
        ]
        result = _build_tool_history(history)

        assert "Successful" in result
        assert "FAILED" in result

    def test__untagged_entries_default_to_successful(self):
        """Entries without ✓/✗ prefix should be treated as successful."""
        history = ["fetch_code(func)"]
        result = _build_tool_history(history)

        assert "Successful" in result
        assert "fetch_code(func)" in result


# ---------------------------------------------------------------------------
# build_research_instructions
# ---------------------------------------------------------------------------


class TestBuildResearchInstructions:
    """Tests for build_research_instructions."""

    @patch(f"{_MOD}.format_checklist", return_value="**EVIDENCE CHECKLIST (Generic):**\n...")
    def test__first_iteration_returns_initial_instructions(self, mock_checklist):
        """Iteration 1 should produce initial investigation instructions."""
        state = {
            "issue_description": "CWE-119 buffer overflow",
            "issue_cwe": "CWE-119",
            "fetched_files": {},
            "tool_call_history": [],
            "iteration": 1,
        }
        result = build_research_instructions(state)

        assert "code gatherer" in result
        assert "FINDING" in result
        assert "CWE-119 buffer overflow" in result
        assert "RESEARCH_COMPLETE" in result

    @patch(f"{_MOD}.format_checklist", return_value="**EVIDENCE CHECKLIST (Generic):**\n...")
    def test__first_iteration_includes_checklist(self, mock_checklist):
        """Initial instructions should include the formatted checklist."""
        state = {
            "issue_description": "CWE-119 buffer overflow",
            "issue_cwe": "CWE-119",
            "fetched_files": {},
            "tool_call_history": [],
            "iteration": 1,
        }
        result = build_research_instructions(state)

        assert "EVIDENCE CHECKLIST" in result
        mock_checklist.assert_called_once_with("CWE-119")

    def test__continuation_iteration_returns_feedback_instructions(self):
        """Iteration > 1 should produce feedback-based continuation instructions."""
        state = {
            "issue_description": "CWE-119 buffer overflow",
            "fetched_files": {},
            "tool_call_history": [],
            "iteration": 2,
            "evaluation_feedback": "Need more evidence for guard check",
            "required_information": ["validate_input", "check_bounds"],
        }
        result = build_research_instructions(state)

        assert "Gather more code based on feedback" in result
        assert "Need more evidence for guard check" in result
        assert "validate_input" in result
        assert "check_bounds" in result
        assert "RESEARCH_COMPLETE" in result

    def test__continuation_without_required_info(self):
        """Continuation with empty required_information should show 'None specified'."""
        state = {
            "issue_description": "CWE-119 buffer overflow",
            "fetched_files": {},
            "tool_call_history": [],
            "iteration": 3,
            "required_information": [],
        }
        result = build_research_instructions(state)

        assert "None specified" in result

    @patch(f"{_MOD}.format_checklist", return_value="**EVIDENCE CHECKLIST (Generic):**\n...")
    def test__includes_tool_history_in_output(self, mock_checklist):
        """Tool call history should be included in instructions."""
        state = {
            "issue_description": "CWE-476 null deref",
            "issue_cwe": "CWE-476",
            "fetched_files": {},
            "tool_call_history": ["✓ fetch_code(main)"],
            "iteration": 1,
        }
        result = build_research_instructions(state)

        assert "fetch_code(main)" in result

    @patch(f"{_MOD}.format_checklist", return_value="**EVIDENCE CHECKLIST (Generic):**\n...")
    def test__includes_code_bank_files_summary(self, mock_checklist):
        """Code bank file summary should appear in the instructions."""
        state = {
            "issue_description": "CWE-476 null deref",
            "issue_cwe": "CWE-476",
            "fetched_files": {
                "fetch_code": ["=== Fetched Code: alloc ===\nFile Path: /src/mem.c\ncode"]
            },
            "tool_call_history": [],
            "iteration": 1,
        }
        result = build_research_instructions(state)

        assert "mem.c:alloc" in result
