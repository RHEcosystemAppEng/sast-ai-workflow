"""
Tests for GoRepoHandler reference and definition lookup methods:
- get_all_references
- get_definition_by_name
- _is_definition_context (including := short variable declarations)
"""

from collections import defaultdict
from unittest.mock import Mock, patch

import pytest

from src.handlers.go_repo_handler import CodeLocation, GoRepoHandler, Reference, Symbol


def make_handler():
    """Return a GoRepoHandler instance with __init__ bypassed."""
    with patch.object(GoRepoHandler, "__init__", return_value=None):
        handler = GoRepoHandler(Mock())
    handler._report_file_prefix = "myproject-1.0/"
    handler.repo_local_path = "/repo"
    handler.project_name = ""
    handler.index = Mock()
    handler.index._file_content_cache = {}
    handler.index.symbol_data = []
    handler.index.symbols_by_file = defaultdict(list)
    handler.index.reference_data = []
    handler.index.references = defaultdict(list)
    handler.index.symbols = defaultdict(list)
    handler.index.string_to_id = {}
    return handler


def make_symbol(
    name="MyFunc",
    kind="func",
    file_path="/repo/main.go",
    start_line=10,
    end_line=20,
    source_code="10| func MyFunc() {}",
) -> Symbol:
    return Symbol(
        name=name,
        kind=kind,
        location=CodeLocation(file_path=file_path, start_line=start_line, end_line=end_line),
        source_code=source_code,
        package_name="main",
    )


def make_reference(
    symbol_name="MyFunc",
    file_path="/repo/main.go",
    line=15,
    usage_type="call",
) -> Reference:
    return Reference(
        symbol_name=symbol_name,
        location=CodeLocation(file_path=file_path, start_line=line, end_line=line),
        context=f"result := {symbol_name}()",
        usage_type=usage_type,
    )


# ------------------------------------------------------------------------------
# get_definition_by_name
# ------------------------------------------------------------------------------


class TestGetDefinitionByName:

    def test_returns_empty_when_name_not_indexed(self):
        handler = make_handler()
        handler.index.string_to_id = {}
        assert handler.get_definition_by_name("Unknown") == []

    def test_returns_all_definitions_for_name(self):
        handler = make_handler()
        sym_a = make_symbol(name="Parse", file_path="/repo/a.go")
        sym_b = make_symbol(name="Parse", file_path="/repo/b.go")
        handler.index.string_to_id = {"Parse": 1}
        handler.index.symbols = {1: [0, 1]}
        handler.index.symbol_data = [sym_a, sym_b]

        result = handler.get_definition_by_name("Parse")

        assert len(result) == 2
        assert sym_a in result
        assert sym_b in result

    def test_filters_by_file_path(self):
        handler = make_handler()
        sym_a = make_symbol(name="Parse", file_path="/repo/a.go")
        sym_b = make_symbol(name="Parse", file_path="/repo/b.go")
        handler.index.string_to_id = {"Parse": 1}
        handler.index.symbols = {1: [0, 1]}
        handler.index.symbol_data = [sym_a, sym_b]

        result = handler.get_definition_by_name("Parse", file_path="/repo/a.go")

        assert result == [sym_a]

    def test_returns_empty_when_file_filter_matches_nothing(self):
        handler = make_handler()
        sym = make_symbol(name="Parse", file_path="/repo/a.go")
        handler.index.string_to_id = {"Parse": 1}
        handler.index.symbols = {1: [0]}
        handler.index.symbol_data = [sym]

        result = handler.get_definition_by_name("Parse", file_path="/repo/other.go")

        assert result == []

    def test_single_definition_returned_without_filter(self):
        handler = make_handler()
        sym = make_symbol(name="Init")
        handler.index.string_to_id = {"Init": 1}
        handler.index.symbols = {1: [0]}
        handler.index.symbol_data = [sym]

        result = handler.get_definition_by_name("Init")

        assert result == [sym]


# ------------------------------------------------------------------------------
# get_all_references
# ------------------------------------------------------------------------------


class TestGetAllReferences:

    def test_returns_empty_when_name_not_indexed(self):
        handler = make_handler()
        handler.index.string_to_id = {}
        assert handler.get_all_references("Unknown") == []

    def test_returns_all_references_for_name(self):
        handler = make_handler()
        ref_a = make_reference(symbol_name="Parse", file_path="/repo/a.go", line=5)
        ref_b = make_reference(symbol_name="Parse", file_path="/repo/b.go", line=10)
        handler.index.string_to_id = {"Parse": 1}
        handler.index.references = {1: [0, 1]}
        handler.index.reference_data = [ref_a, ref_b]

        result = handler.get_all_references("Parse")

        assert len(result) == 2
        assert ref_a in result
        assert ref_b in result

    def test_filters_by_file_path(self):
        handler = make_handler()
        ref_a = make_reference(symbol_name="Parse", file_path="/repo/a.go")
        ref_b = make_reference(symbol_name="Parse", file_path="/repo/b.go")
        handler.index.string_to_id = {"Parse": 1}
        handler.index.references = {1: [0, 1]}
        handler.index.reference_data = [ref_a, ref_b]

        result = handler.get_all_references("Parse", file_path="/repo/a.go")

        assert result == [ref_a]

    def test_returns_empty_when_file_filter_matches_nothing(self):
        handler = make_handler()
        ref = make_reference(symbol_name="Parse", file_path="/repo/a.go")
        handler.index.string_to_id = {"Parse": 1}
        handler.index.references = {1: [0]}
        handler.index.reference_data = [ref]

        result = handler.get_all_references("Parse", file_path="/repo/other.go")

        assert result == []

    def test_distinguishes_different_symbol_names(self):
        handler = make_handler()
        ref_parse = make_reference(symbol_name="Parse")
        ref_format = make_reference(symbol_name="Format")
        handler.index.string_to_id = {"Parse": 1, "Format": 2}
        handler.index.references = {1: [0], 2: [1]}
        handler.index.reference_data = [ref_parse, ref_format]

        assert handler.get_all_references("Parse") == [ref_parse]
        assert handler.get_all_references("Format") == [ref_format]

    def test_method_call_references_included(self):
        handler = make_handler()
        ref = make_reference(symbol_name="Write", usage_type="method_call")
        handler.index.string_to_id = {"Write": 1}
        handler.index.references = {1: [0]}
        handler.index.reference_data = [ref]

        result = handler.get_all_references("Write")

        assert len(result) == 1
        assert result[0].usage_type == "method_call"


# ------------------------------------------------------------------------------
# _is_definition_context
# ------------------------------------------------------------------------------


def make_node(node_type: str, name_child=None, left_child=None, parent=None):
    """Build a mock tree-sitter node."""
    node = Mock()
    node.type = node_type
    node.parent = parent
    node.child_by_field_name = Mock(
        side_effect=lambda field: {"name": name_child, "left": left_child}.get(field)
    )
    return node


class TestIsDefinitionContext:

    def test_identifier_with_no_relevant_parent_is_not_definition(self):
        handler = make_handler()
        node = Mock()
        node.parent = None
        assert handler._is_definition_context(node) is False

    @pytest.mark.parametrize(
        "parent_type",
        [
            "function_declaration",
            "method_declaration",
            "type_spec",
            "var_spec",
            "const_spec",
            "parameter_declaration",
        ],
    )
    def test_name_field_of_declaration_is_definition(self, parent_type):
        handler = make_handler()
        node = Mock()
        node.parent = make_node(parent_type, name_child=node)
        node.parent.parent = None
        assert handler._is_definition_context(node) is True

    @pytest.mark.parametrize(
        "parent_type",
        [
            "function_declaration",
            "method_declaration",
            "type_spec",
            "var_spec",
            "const_spec",
            "parameter_declaration",
        ],
    )
    def test_non_name_child_of_declaration_is_not_definition(self, parent_type):
        handler = make_handler()
        node = Mock()
        other_child = Mock()
        node.parent = make_node(parent_type, name_child=other_child)
        node.parent.parent = None
        assert handler._is_definition_context(node) is False

    def test_short_var_declaration_single_var_is_definition(self):
        """x := expr — identifier is directly the 'left' field."""
        handler = make_handler()
        node = Mock()
        node.parent = make_node("short_var_declaration", left_child=node)
        node.parent.parent = None
        assert handler._is_definition_context(node) is True

    def test_short_var_declaration_rhs_is_not_definition(self):
        """x := expr — identifier on the right side is a reference."""
        handler = make_handler()
        node = Mock()
        other_child = Mock()
        node.parent = make_node("short_var_declaration", left_child=other_child)
        node.parent.parent = None
        assert handler._is_definition_context(node) is False

    def test_short_var_declaration_multi_var_lhs_is_definition(self):
        """x, y := expr — identifier inside the expression_list on the left is a definition."""
        handler = make_handler()
        node = Mock()

        expr_list = Mock()
        expr_list.type = "expression_list"
        expr_list.parent = make_node("short_var_declaration", left_child=expr_list)
        expr_list.parent.parent = None

        node.parent = expr_list
        assert handler._is_definition_context(node) is True

    def test_short_var_declaration_multi_var_rhs_expression_list_is_not_definition(self):
        """Identifiers inside the RHS expression_list of := are references."""
        handler = make_handler()
        node = Mock()

        lhs_list = Mock()
        rhs_list = Mock()
        rhs_list.type = "expression_list"
        rhs_list.parent = make_node("short_var_declaration", left_child=lhs_list)
        rhs_list.parent.parent = None

        node.parent = rhs_list
        assert handler._is_definition_context(node) is False

    def test_identifier_in_call_expression_is_not_definition(self):
        """Function call arguments are references, not definitions."""
        handler = make_handler()
        node = Mock()
        node.parent = make_node("call_expression")
        node.parent.parent = None
        assert handler._is_definition_context(node) is False
