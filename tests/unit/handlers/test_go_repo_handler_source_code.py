"""
Tests for GoRepoHandler methods:
- get_source_code_by_line_number
- get_source_code_blocks_from_error_trace
- extract_missing_functions_or_macros
"""

from unittest.mock import Mock, patch

from src.handlers.go_repo_handler import CodeLocation, GoRepoHandler, Symbol


def make_handler():
    """Return a GoRepoHandler instance with __init__ bypassed."""
    with patch.object(GoRepoHandler, "__init__", return_value=None):
        handler = GoRepoHandler(Mock())
    handler._report_file_prefix = "myproject-1.0/"
    handler.repo_local_path = "/repo"
    handler.index = Mock()
    handler.index._file_content_cache = {}
    handler.index.symbol_data = []
    return handler


def make_symbol(
    name="MyFunc",
    kind="func",
    file_path="/repo/main.go",
    start_line=10,
    end_line=20,
    source_code="10| func MyFunc() {}",
    documentation=None,
) -> Symbol:
    return Symbol(
        name=name,
        kind=kind,
        location=CodeLocation(file_path=file_path, start_line=start_line, end_line=end_line),
        source_code=source_code,
        package_name="main",
        documentation=documentation or [],
    )


# ------------------------------------------------------------------------------
# get_source_code_by_line_number
# ------------------------------------------------------------------------------


class TestGetSourceCodeByLineNumber:

    def test_returns_symbol_source_code_when_symbol_found(self):
        handler = make_handler()
        sym = make_symbol()
        handler._get_symbol_at_line = Mock(return_value=sym)

        result = handler.get_source_code_by_line_number("/repo/main.go", 15)

        handler._get_symbol_at_line.assert_called_once_with("/repo/main.go", 15)
        assert result == sym.source_code

    def test_falls_back_to_context_when_no_symbol(self):
        handler = make_handler()
        handler._get_symbol_at_line = Mock(return_value=None)
        handler._get_context_around_line = Mock(return_value="10| some code")

        result = handler.get_source_code_by_line_number("/repo/main.go", 10)

        handler._get_context_around_line.assert_called_once_with("/repo/main.go", 10)
        assert result == "10| some code"

    def test_returns_empty_string_when_no_symbol_and_no_context(self):
        handler = make_handler()
        handler._get_symbol_at_line = Mock(return_value=None)
        handler._get_context_around_line = Mock(return_value="")

        result = handler.get_source_code_by_line_number("/nonexistent/file.go", 5)

        assert result == ""

    def test_passes_correct_args_to_symbol_lookup(self):
        handler = make_handler()
        sym = make_symbol(file_path="/repo/pkg/util.go", start_line=1, end_line=5)
        handler._get_symbol_at_line = Mock(return_value=sym)

        handler.get_source_code_by_line_number("/repo/pkg/util.go", 3)

        handler._get_symbol_at_line.assert_called_once_with("/repo/pkg/util.go", 3)

    def test_context_fallback_not_called_when_symbol_found(self):
        handler = make_handler()
        sym = make_symbol()
        handler._get_symbol_at_line = Mock(return_value=sym)
        handler._get_context_around_line = Mock()

        handler.get_source_code_by_line_number("/repo/main.go", 12)

        handler._get_context_around_line.assert_not_called()


# ------------------------------------------------------------------------------
# get_source_code_blocks_from_error_trace
# ------------------------------------------------------------------------------


class TestGetSourceCodeBlocksFromErrorTrace:

    def test_empty_trace_returns_empty_dict(self):
        handler = make_handler()
        assert handler.get_source_code_blocks_from_error_trace("") == {}

    def test_trace_with_no_go_references_returns_empty_dict(self):
        handler = make_handler()
        trace = "panic: runtime error\nat goroutine 1 [running]"
        assert handler.get_source_code_blocks_from_error_trace(trace) == {}

    def test_single_absolute_path_reference_with_symbol(self):
        handler = make_handler()
        sym = make_symbol(file_path="/repo/main.go", source_code="10| func MyFunc() {}")
        handler._get_symbol_at_line = Mock(return_value=sym)

        trace = "\t/repo/main.go:15: +0x28"
        result = handler.get_source_code_blocks_from_error_trace(trace)

        assert "/repo/main.go" in result
        assert result["/repo/main.go"] == "10| func MyFunc() {}"

    def test_absolute_path_is_passed_directly_to_symbol_lookup(self):
        handler = make_handler()
        handler._get_symbol_at_line = Mock(return_value=None)
        handler._get_context_around_line = Mock(return_value="")

        trace = "/abs/path/main.go:7: some error"
        handler.get_source_code_blocks_from_error_trace(trace)

        handler._get_symbol_at_line.assert_called_once_with("/abs/path/main.go", 7)

    def test_report_prefix_stripped_before_path_resolution(self):
        handler = make_handler()
        handler.repo_local_path = "/the/repo"
        handler._report_file_prefix = "proj-1.0/"
        handler._get_symbol_at_line = Mock(return_value=None)
        handler._get_context_around_line = Mock(return_value="")

        trace = "proj-1.0/main.go:3: some error"
        handler.get_source_code_blocks_from_error_trace(trace)

        handler._get_symbol_at_line.assert_called_once_with("/the/repo/main.go", 3)

    def test_relative_path_joined_with_repo_local_path(self):
        handler = make_handler()
        handler.repo_local_path = "/the/repo"
        handler._report_file_prefix = "proj-1.0/"
        handler._get_symbol_at_line = Mock(return_value=None)
        handler._get_context_around_line = Mock(return_value="")

        trace = "pkg/util.go:5: some error"
        handler.get_source_code_blocks_from_error_trace(trace)

        handler._get_symbol_at_line.assert_called_once_with("/the/repo/pkg/util.go", 5)

    def test_symbol_with_documentation_prepends_docs(self):
        handler = make_handler()
        sym = make_symbol(
            file_path="/repo/main.go",
            start_line=11,
            end_line=15,
            source_code="11| func MyFunc() {}",
            documentation=["MyFunc does something important."],
        )
        handler._get_symbol_at_line = Mock(return_value=sym)

        trace = "myproject-1.0/main.go:12: some error"
        result = handler.get_source_code_blocks_from_error_trace(trace)

        value = result["myproject-1.0/main.go"]
        assert "MyFunc does something important." in value
        assert "func MyFunc" in value

    def test_documentation_line_numbers_are_correct(self):
        """Doc header line numbers are placed just before the symbol start line."""
        handler = make_handler()
        sym = make_symbol(
            file_path="/repo/main.go",
            start_line=5,
            end_line=10,
            source_code="5| func F() {}",
            documentation=["First doc line.", "Second doc line."],
        )
        handler._get_symbol_at_line = Mock(return_value=sym)

        trace = "myproject-1.0/main.go:6: error"
        result = handler.get_source_code_blocks_from_error_trace(trace)

        value = result["myproject-1.0/main.go"]
        assert "3| // First doc line." in value
        assert "4| // Second doc line." in value

    def test_fallback_to_context_when_no_symbol_found(self):
        handler = make_handler()
        handler._get_symbol_at_line = Mock(return_value=None)
        handler._get_context_around_line = Mock(return_value="5| var x = broken")

        trace = "myproject-1.0/main.go:5: some error"
        result = handler.get_source_code_blocks_from_error_trace(trace)

        assert any("5| var x = broken" in v for v in result.values())

    def test_fallback_absent_when_context_is_empty(self):
        handler = make_handler()
        handler._get_symbol_at_line = Mock(return_value=None)
        handler._get_context_around_line = Mock(return_value="")

        trace = "myproject-1.0/main.go:5: some error"
        result = handler.get_source_code_blocks_from_error_trace(trace)

        assert result == {}

    def test_same_block_not_duplicated_for_same_file(self):
        handler = make_handler()
        sym = make_symbol(file_path="/repo/main.go", source_code="10| func MyFunc() {}")
        handler._get_symbol_at_line = Mock(return_value=sym)

        # Two different lines in the same file both resolve to the same symbol block
        trace = "myproject-1.0/main.go:10: error\nmyproject-1.0/main.go:11: error"
        result = handler.get_source_code_blocks_from_error_trace(trace)

        for v in result.values():
            assert v.count("func MyFunc") == 1

    def test_multiple_files_produce_separate_keys(self):
        handler = make_handler()
        sym_a = make_symbol(name="A", file_path="/repo/a.go", source_code="1| func A() {}")
        sym_b = make_symbol(name="B", file_path="/repo/b.go", source_code="1| func B() {}")

        def side_effect(path, line):
            return sym_a if "a.go" in path else sym_b

        handler._get_symbol_at_line = Mock(side_effect=side_effect)

        trace = "myproject-1.0/a.go:1: error\nmyproject-1.0/b.go:1: error"
        result = handler.get_source_code_blocks_from_error_trace(trace)

        assert len(result) == 2

    def test_multiple_symbols_in_same_file_joined(self):
        """Multiple distinct blocks for the same file are joined with double newline."""
        handler = make_handler()
        sym_a = make_symbol(
            name="A", file_path="/repo/a.go", start_line=1, end_line=5, source_code="1| func A() {}"
        )
        sym_b = make_symbol(
            name="B",
            file_path="/repo/a.go",
            start_line=10,
            end_line=15,
            source_code="10| func B() {}",
        )

        call_count = {"n": 0}

        def side_effect(path, line):
            call_count["n"] += 1
            return sym_a if call_count["n"] == 1 else sym_b

        handler._get_symbol_at_line = Mock(side_effect=side_effect)

        trace = "myproject-1.0/a.go:2: error\nmyproject-1.0/a.go:11: error"
        result = handler.get_source_code_blocks_from_error_trace(trace)

        assert len(result) == 1
        value = list(result.values())[0]
        assert "func A" in value
        assert "func B" in value

    def test_non_matching_line_number_format_is_ignored(self):
        """Lines that don't match file.go:NNN: pattern are silently ignored."""
        handler = make_handler()
        trace = "main.go:notanumber: this should not match"
        result = handler.get_source_code_blocks_from_error_trace(trace)
        assert result == {}


# ------------------------------------------------------------------------------
# extract_missing_functions_or_macros
# ------------------------------------------------------------------------------


class TestExtractMissingFunctionsOrMacros:

    def test_none_instructions_returns_empty(self):
        handler = make_handler()
        code, found = handler.extract_missing_functions_or_macros(None, set())
        assert code == ""
        assert found == set()

    def test_empty_instructions_returns_empty(self):
        handler = make_handler()
        code, found = handler.extract_missing_functions_or_macros([], set())
        assert code == ""
        assert found == set()

    def test_extracts_definition_and_adds_to_found(self):
        handler = make_handler()
        sym = make_symbol(name="ParseToken", source_code="5| func ParseToken() {}")
        handler.get_definition_by_name = Mock(return_value=[sym])

        instr = Mock()
        instr.expression_name = "ParseToken"

        code, found = handler.extract_missing_functions_or_macros([instr], set())

        assert "ParseToken" in found
        assert "func ParseToken" in code
        assert "// Definition of ParseToken (func):" in code

    def test_skips_already_found_symbols(self):
        handler = make_handler()
        handler.get_definition_by_name = Mock()

        instr = Mock()
        instr.expression_name = "AlreadyFound"

        code, _ = handler.extract_missing_functions_or_macros([instr], {"AlreadyFound"})

        handler.get_definition_by_name.assert_not_called()
        assert code == ""

    def test_instruction_without_expression_name_is_skipped(self):
        handler = make_handler()
        handler.get_definition_by_name = Mock()

        instr = Mock(spec=[])  # no expression_name attribute

        code, _ = handler.extract_missing_functions_or_macros([instr], set())

        handler.get_definition_by_name.assert_not_called()
        assert code == ""

    def test_multiple_instructions_accumulate_code(self):
        handler = make_handler()
        sym_a = make_symbol(name="FuncA", source_code="1| func FuncA() {}")
        sym_b = make_symbol(name="FuncB", source_code="10| func FuncB() {}")

        def get_def(name):
            return [sym_a] if name == "FuncA" else [sym_b]

        handler.get_definition_by_name = Mock(side_effect=get_def)

        instr_a = Mock()
        instr_a.expression_name = "FuncA"
        instr_b = Mock()
        instr_b.expression_name = "FuncB"

        code, found = handler.extract_missing_functions_or_macros([instr_a, instr_b], set())

        assert "FuncA" in found
        assert "FuncB" in found
        assert "func FuncA" in code
        assert "func FuncB" in code

    def test_duplicate_instruction_adds_definition_only_once(self):
        """Same expression_name appearing twice is fetched and added only on the first."""
        handler = make_handler()
        sym = make_symbol(name="MyFunc", source_code="1| func MyFunc() {}")
        handler.get_definition_by_name = Mock(return_value=[sym])

        instr = Mock()
        instr.expression_name = "MyFunc"

        code, _ = handler.extract_missing_functions_or_macros([instr, instr], set())

        assert code.count("func MyFunc") == 1
        assert handler.get_definition_by_name.call_count == 1

    def test_found_symbols_updated_with_pre_existing_entries_preserved(self):
        handler = make_handler()
        sym = make_symbol(name="NewFunc", source_code="1| func NewFunc() {}")
        handler.get_definition_by_name = Mock(return_value=[sym])

        instr = Mock()
        instr.expression_name = "NewFunc"

        _, found = handler.extract_missing_functions_or_macros([instr], {"ExistingFunc"})

        assert "NewFunc" in found
        assert "ExistingFunc" in found

    def test_no_definition_found_does_not_add_to_code_or_found(self):
        handler = make_handler()
        handler.get_definition_by_name = Mock(return_value=[])

        instr = Mock()
        instr.expression_name = "GhostFunc"

        code, found = handler.extract_missing_functions_or_macros([instr], set())

        assert code == ""
        assert "GhostFunc" not in found

    def test_definition_header_includes_kind(self):
        """The inserted header encodes both the symbol name and its kind."""
        handler = make_handler()
        sym = make_symbol(name="MyType", kind="type", source_code="1| type MyType struct {}")
        handler.get_definition_by_name = Mock(return_value=[sym])

        instr = Mock()
        instr.expression_name = "MyType"

        code, _ = handler.extract_missing_functions_or_macros([instr], set())

        assert "// Definition of MyType (type):" in code

    def test_mixed_valid_and_invalid_instructions(self):
        """Instructions with and without expression_name are processed correctly."""
        handler = make_handler()
        sym = make_symbol(name="ValidFunc", source_code="1| func ValidFunc() {}")
        handler.get_definition_by_name = Mock(return_value=[sym])

        good_instr = Mock()
        good_instr.expression_name = "ValidFunc"
        bad_instr = Mock(spec=[])  # no expression_name

        code, found = handler.extract_missing_functions_or_macros(
            [bad_instr, good_instr, bad_instr], set()
        )

        assert "ValidFunc" in found
        assert "func ValidFunc" in code
