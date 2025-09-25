"""
Tests for _extract_documentation in handlers/go_repo_handler.py

Covers the godoc extraction logic:
- single-line // comments
- multi-line // comment blocks
- single-line /* */ block comments
- multi-line /* */ block comments
- blank line stops scan (godoc requires no gap)
- non-comment line stops scan
- edge cases (declaration at top of file, no comments)
"""

from unittest.mock import Mock, patch

from src.handlers.go_repo_handler import GoRepoHandler


def make_handler():
    """Return a GoRepoHandler instance with __init__ bypassed."""
    with patch.object(GoRepoHandler, "__init__", return_value=None):
        return GoRepoHandler(Mock())


def make_node(line: int) -> Mock:
    """Return a mock tree-sitter node positioned at the given 0-based line."""
    node = Mock()
    node.start_point = (line, 0)
    return node


class TestExtractDocumentation:

    def test_single_line_comment(self):
        handler = make_handler()
        content = "// DoSomething does something.\nfunc DoSomething() {}"
        node = make_node(1)

        result = handler._extract_documentation(node, content)

        assert result == ["DoSomething does something."]

    def test_multiple_consecutive_line_comments(self):
        handler = make_handler()
        content = (
            "// DoSomething does something.\n"
            "// It also does something else.\n"
            "func DoSomething() {}"
        )
        node = make_node(2)

        result = handler._extract_documentation(node, content)

        assert result == ["DoSomething does something.", "It also does something else."]

    def test_blank_line_between_comment_and_declaration_is_excluded(self):
        """Godoc requires no blank line between comment and declaration."""
        handler = make_handler()
        content = "// Unrelated comment.\n\nfunc DoSomething() {}"
        node = make_node(2)

        result = handler._extract_documentation(node, content)

        assert result == []

    def test_blank_line_within_comment_block_stops_scan(self):
        """Only comments directly adjacent to the declaration are included."""
        handler = make_handler()
        content = "// First comment.\n\n// Second comment.\nfunc DoSomething() {}"
        node = make_node(3)

        result = handler._extract_documentation(node, content)

        assert result == ["Second comment."]

    def test_non_comment_line_stops_scan(self):
        handler = make_handler()
        content = "var x = 1\n// DoSomething does something.\nfunc DoSomething() {}"
        node = make_node(2)

        result = handler._extract_documentation(node, content)

        assert result == ["DoSomething does something."]

    def test_no_comments_returns_empty(self):
        handler = make_handler()
        content = "var x = 1\nfunc DoSomething() {}"
        node = make_node(1)

        result = handler._extract_documentation(node, content)

        assert result == []

    def test_declaration_at_top_of_file_returns_empty(self):
        handler = make_handler()
        content = "func DoSomething() {}"
        node = make_node(0)

        result = handler._extract_documentation(node, content)

        assert result == []

    def test_single_line_block_comment(self):
        handler = make_handler()
        content = "/* DoSomething does something. */\nfunc DoSomething() {}"
        node = make_node(1)

        result = handler._extract_documentation(node, content)

        assert result == ["DoSomething does something."]

    def test_multiline_block_comment(self):
        handler = make_handler()
        content = (
            "/*\n"
            " * DoSomething does something.\n"
            " * It accepts no arguments.\n"
            " */\n"
            "func DoSomething() {}"
        )
        node = make_node(4)

        result = handler._extract_documentation(node, content)

        assert "DoSomething does something." in result
        assert "It accepts no arguments." in result

    def test_multiline_block_comment_order_preserved(self):
        handler = make_handler()
        content = "/*\n * First line.\n * Second line.\n */\nfunc DoSomething() {}"
        node = make_node(4)

        result = handler._extract_documentation(node, content)

        assert result.index("First line.") < result.index("Second line.")

    def test_block_comment_blank_line_gap_is_excluded(self):
        handler = make_handler()
        content = "/*\n * Unrelated.\n */\n\nfunc DoSomething() {}"
        node = make_node(4)

        result = handler._extract_documentation(node, content)

        assert result == []

    def test_line_comment_strips_leading_slashes_and_whitespace(self):
        handler = make_handler()
        content = "//   Padded comment.   \nfunc DoSomething() {}"
        node = make_node(1)

        result = handler._extract_documentation(node, content)

        assert result == ["Padded comment."]


class TestGetSymbolDocumentation:
    """Tests for get_symbol_documentation scoped to a specific file."""

    def test_returns_docs_for_correct_file(self):
        """Returns documentation only from the requested file."""
        handler = make_handler()

        from src.handlers.go_repo_handler import CodeLocation, Symbol

        sym_a = Symbol(
            name="DoSomething",
            kind="func",
            location=CodeLocation(file_path="a.go", start_line=2, end_line=5),
            source_code="",
            package_name="pkg",
            documentation=["Docs from a.go."],
        )
        sym_b = Symbol(
            name="DoSomething",
            kind="func",
            location=CodeLocation(file_path="b.go", start_line=2, end_line=5),
            source_code="",
            package_name="pkg",
            documentation=["Docs from b.go."],
        )

        handler.index = Mock()
        handler.index.string_to_id = {"DoSomething": 1}
        handler.index.symbols = {1: [0, 1]}
        handler.index.symbol_data = [sym_a, sym_b]

        result = handler.get_symbol_documentation("DoSomething", "a.go")

        assert result == ["Docs from a.go."]

    def test_returns_empty_when_symbol_not_in_file(self):
        """Returns empty list when symbol exists but not in the given file."""
        handler = make_handler()

        from src.handlers.go_repo_handler import CodeLocation, Symbol

        sym = Symbol(
            name="DoSomething",
            kind="func",
            location=CodeLocation(file_path="a.go", start_line=2, end_line=5),
            source_code="",
            package_name="pkg",
            documentation=["Some docs."],
        )

        handler.index = Mock()
        handler.index.string_to_id = {"DoSomething": 1}
        handler.index.symbols = {1: [0]}
        handler.index.symbol_data = [sym]

        result = handler.get_symbol_documentation("DoSomething", "other.go")

        assert result == []

    def test_returns_empty_when_symbol_unknown(self):
        handler = make_handler()

        handler.index = Mock()
        handler.index.string_to_id = {}

        result = handler.get_symbol_documentation("Unknown", "a.go")

        assert result == []
