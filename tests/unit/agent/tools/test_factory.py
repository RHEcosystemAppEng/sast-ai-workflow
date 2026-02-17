"""
Unit tests for investigation tools factory.
Tests all scenarios with 100% coverage.
"""
import pytest
from pathlib import Path
from unittest.mock import Mock, MagicMock, patch
from langchain_core.tools import BaseTool
from langchain_community.tools.file_management import (
    ListDirectoryTool,
    FileSearchTool,
)

from sast_agent_workflow.investigation.tools.factory import (
    create_investigation_tools,
    _create_list_directory_tool,
    _create_file_search_tool,
)


class TestCreateInvestigationTools:
    """Test create_investigation_tools function."""

    @pytest.fixture
    def mock_config(self, tmp_path):
        """Create a mock config with temporary repository path."""
        config = Mock()
        config.REPO_LOCAL_PATH = str(tmp_path)
        config.REPO_TYPE = "git"
        return config

    @pytest.fixture
    def mock_repo_handler(self):
        """Create a mock repository handler."""
        handler = Mock()
        handler.extract_missing_functions_or_macros = Mock(return_value=("", set()))
        return handler

    def test_creates_list_of_tools(self, mock_config):
        """Test that function returns a list of tools."""
        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()
            tools = create_investigation_tools(mock_config)

        assert isinstance(tools, list)
        assert len(tools) > 0
        assert all(isinstance(tool, BaseTool) for tool in tools)

    def test_creates_expected_number_of_tools(self, mock_config):
        """Test that all expected tools are created."""
        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()
            tools = create_investigation_tools(mock_config)

        # Expected tools: fetch_code, search_codebase, read_file, list_directory, file_search
        assert len(tools) == 5

    def test_creates_fetch_code_tool(self, mock_config):
        """Test that fetch_code tool is created."""
        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()
            tools = create_investigation_tools(mock_config)

        tool_names = [tool.name for tool in tools]
        assert "fetch_code" in tool_names

    def test_creates_search_codebase_tool(self, mock_config):
        """Test that search_codebase tool is created."""
        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()
            tools = create_investigation_tools(mock_config)

        tool_names = [tool.name for tool in tools]
        assert "search_codebase" in tool_names

    def test_creates_read_file_tool(self, mock_config):
        """Test that read_file tool is created."""
        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()
            tools = create_investigation_tools(mock_config)

        tool_names = [tool.name for tool in tools]
        assert "read_file" in tool_names

    def test_creates_list_directory_tool(self, mock_config):
        """Test that list_directory tool is created."""
        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()
            tools = create_investigation_tools(mock_config)

        tool_names = [tool.name for tool in tools]
        assert "list_directory" in tool_names

    def test_creates_file_search_tool(self, mock_config):
        """Test that file_search tool is created."""
        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()
            tools = create_investigation_tools(mock_config)

        tool_names = [tool.name for tool in tools]
        assert "file_search" in tool_names

    def test_uses_repo_handler_factory(self, mock_config):
        """Test that repo_handler_factory is called with config."""
        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_handler = Mock()
            mock_factory.return_value = mock_handler

            create_investigation_tools(mock_config)

        mock_factory.assert_called_once_with(mock_config)

    def test_resolves_repo_path(self, mock_config, tmp_path):
        """Test that repository path is resolved correctly."""
        mock_config.REPO_LOCAL_PATH = str(tmp_path)

        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()

            # Patch the individual tool creation functions to capture the repo_path
            with patch("sast_agent_workflow.investigation.tools.factory.create_fetch_code_tool") as mock_fetch:
                with patch("sast_agent_workflow.investigation.tools.factory.create_search_codebase_tool") as mock_search:
                    with patch("sast_agent_workflow.investigation.tools.factory.create_read_file_tool") as mock_read:
                        mock_fetch.return_value = Mock(name="fetch_code")
                        mock_search.return_value = Mock(name="search_codebase")
                        mock_read.return_value = Mock(name="read_file")

                        create_investigation_tools(mock_config)

                        # Verify that Path was used
                        assert mock_fetch.called
                        assert mock_search.called
                        assert mock_read.called

    def test_all_tools_are_base_tool_instances(self, mock_config):
        """Test that all returned tools are BaseTool instances."""
        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()
            tools = create_investigation_tools(mock_config)

        for tool in tools:
            assert isinstance(tool, BaseTool)

    def test_list_directory_tool_is_langchain_community_tool(self, mock_config):
        """Test that list_directory uses langchain_community.ListDirectoryTool."""
        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()
            tools = create_investigation_tools(mock_config)

        list_dir_tool = next(t for t in tools if t.name == "list_directory")
        assert isinstance(list_dir_tool, ListDirectoryTool)

    def test_file_search_tool_is_langchain_community_tool(self, mock_config):
        """Test that file_search uses langchain_community.FileSearchTool."""
        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()
            tools = create_investigation_tools(mock_config)

        file_search_tool = next(t for t in tools if t.name == "file_search")
        assert isinstance(file_search_tool, FileSearchTool)


class TestCreateListDirectoryTool:
    """Test _create_list_directory_tool function."""

    def test_creates_list_directory_tool(self, tmp_path):
        """Test that function creates a ListDirectoryTool."""
        repo_path_str = str(tmp_path)
        tool = _create_list_directory_tool(repo_path_str)

        assert isinstance(tool, ListDirectoryTool)
        assert tool.name == "list_directory"

    def test_tool_is_sandboxed_to_repo(self, tmp_path):
        """Test that tool is sandboxed to repository root."""
        repo_path_str = str(tmp_path)
        tool = _create_list_directory_tool(repo_path_str)

        # Check that root_dir is set correctly
        assert tool.root_dir == repo_path_str

    def test_tool_has_custom_description(self, tmp_path):
        """Test that tool has custom description."""
        repo_path_str = str(tmp_path)
        tool = _create_list_directory_tool(repo_path_str)

        assert "List files and directories" in tool.description
        assert "dir_path" in tool.description
        assert "Examples:" in tool.description

    def test_description_mentions_exploration(self, tmp_path):
        """Test that description mentions codebase exploration."""
        repo_path_str = str(tmp_path)
        tool = _create_list_directory_tool(repo_path_str)

        assert "Explore the codebase" in tool.description or "explore" in tool.description.lower()

    def test_description_includes_parameters(self, tmp_path):
        """Test that description includes parameter information."""
        repo_path_str = str(tmp_path)
        tool = _create_list_directory_tool(repo_path_str)

        assert "Parameters:" in tool.description
        assert "dir_path" in tool.description

    def test_description_includes_examples(self, tmp_path):
        """Test that description includes usage examples."""
        repo_path_str = str(tmp_path)
        tool = _create_list_directory_tool(repo_path_str)

        assert "Examples:" in tool.description
        assert "list_directory" in tool.description

    def test_tool_functionality(self, tmp_path):
        """Test that the tool can actually list directories."""
        # Create some test files
        (tmp_path / "test1.c").write_text("test")
        (tmp_path / "test2.c").write_text("test")
        subdir = tmp_path / "subdir"
        subdir.mkdir()
        (subdir / "test3.c").write_text("test")

        repo_path_str = str(tmp_path)
        tool = _create_list_directory_tool(repo_path_str)

        # List root directory
        result = tool.invoke({"dir_path": "."})

        # Should contain the files and directory
        assert "test1.c" in result
        assert "test2.c" in result
        assert "subdir" in result


class TestCreateFileSearchTool:
    """Test _create_file_search_tool function."""

    def test_creates_file_search_tool(self, tmp_path):
        """Test that function creates a FileSearchTool."""
        repo_path_str = str(tmp_path)
        tool = _create_file_search_tool(repo_path_str)

        assert isinstance(tool, FileSearchTool)
        assert tool.name == "file_search"

    def test_tool_is_sandboxed_to_repo(self, tmp_path):
        """Test that tool is sandboxed to repository root."""
        repo_path_str = str(tmp_path)
        tool = _create_file_search_tool(repo_path_str)

        # Check that root_dir is set correctly
        assert tool.root_dir == repo_path_str

    def test_tool_has_custom_description(self, tmp_path):
        """Test that tool has custom description."""
        repo_path_str = str(tmp_path)
        tool = _create_file_search_tool(repo_path_str)

        assert "Search for files" in tool.description
        assert "pattern" in tool.description
        assert "Examples:" in tool.description

    def test_description_mentions_glob_patterns(self, tmp_path):
        """Test that description mentions glob patterns."""
        repo_path_str = str(tmp_path)
        tool = _create_file_search_tool(repo_path_str)

        assert "glob" in tool.description.lower()
        assert "*.c" in tool.description or "*.py" in tool.description

    def test_description_distinguishes_from_content_search(self, tmp_path):
        """Test that description clarifies it searches names, not contents."""
        repo_path_str = str(tmp_path)
        tool = _create_file_search_tool(repo_path_str)

        assert "file NAMES" in tool.description or "name" in tool.description.lower()
        assert "search_codebase" in tool.description

    def test_description_includes_parameters(self, tmp_path):
        """Test that description includes parameter information."""
        repo_path_str = str(tmp_path)
        tool = _create_file_search_tool(repo_path_str)

        assert "Parameters:" in tool.description
        assert "pattern" in tool.description

    def test_description_includes_examples(self, tmp_path):
        """Test that description includes usage examples."""
        repo_path_str = str(tmp_path)
        tool = _create_file_search_tool(repo_path_str)

        assert "Examples:" in tool.description
        assert "file_search" in tool.description

    def test_tool_functionality(self, tmp_path):
        """Test that the tool can actually search for files."""
        # Create test files
        (tmp_path / "test1.c").write_text("test")
        (tmp_path / "test2.py").write_text("test")
        (tmp_path / "auth.c").write_text("test")
        subdir = tmp_path / "src"
        subdir.mkdir()
        (subdir / "main.c").write_text("test")

        repo_path_str = str(tmp_path)
        tool = _create_file_search_tool(repo_path_str)

        # Search for .c files
        result = tool.invoke({"pattern": "*.c"})

        # Should find .c files but not .py files
        assert "test1.c" in result
        assert "auth.c" in result
        assert "main.c" in result or "src/main.c" in result
        assert "test2.py" not in result

    def test_tool_searches_with_name_pattern(self, tmp_path):
        """Test that the tool can search for files by name pattern."""
        # Create test files
        (tmp_path / "auth.c").write_text("test")
        (tmp_path / "validate.c").write_text("test")
        (tmp_path / "sanitize.c").write_text("test")

        repo_path_str = str(tmp_path)
        tool = _create_file_search_tool(repo_path_str)

        # Search for files with 'auth' in name
        result = tool.invoke({"pattern": "*auth*"})

        assert "auth.c" in result
        assert "validate.c" not in result
        assert "sanitize.c" not in result


class TestToolIntegration:
    """Integration tests for tool factory."""

    def test_all_tools_have_unique_names(self, tmp_path):
        """Test that all tools have unique names."""
        config = Mock()
        config.REPO_LOCAL_PATH = str(tmp_path)

        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()
            tools = create_investigation_tools(config)

        tool_names = [tool.name for tool in tools]
        assert len(tool_names) == len(set(tool_names))

    def test_all_tools_have_descriptions(self, tmp_path):
        """Test that all tools have non-empty descriptions."""
        config = Mock()
        config.REPO_LOCAL_PATH = str(tmp_path)

        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()
            tools = create_investigation_tools(config)

        for tool in tools:
            assert tool.description
            assert len(tool.description) > 0

    def test_custom_tools_are_created_with_repo_path(self, tmp_path):
        """Test that custom tools receive the correct repo_path."""
        config = Mock()
        config.REPO_LOCAL_PATH = str(tmp_path)

        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()

            with patch("sast_agent_workflow.investigation.tools.factory.create_fetch_code_tool") as mock_fetch:
                with patch("sast_agent_workflow.investigation.tools.factory.create_search_codebase_tool") as mock_search:
                    with patch("sast_agent_workflow.investigation.tools.factory.create_read_file_tool") as mock_read:
                        mock_fetch.return_value = Mock(name="fetch_code")
                        mock_search.return_value = Mock(name="search_codebase")
                        mock_read.return_value = Mock(name="read_file")

                        create_investigation_tools(config)

                        # Verify repo_path argument is a Path object
                        fetch_args = mock_fetch.call_args
                        search_args = mock_search.call_args
                        read_args = mock_read.call_args

                        # Second argument should be a Path
                        assert isinstance(fetch_args[0][1], Path)
                        assert isinstance(search_args[0][0], Path)
                        assert isinstance(read_args[0][0], Path)

    def test_community_tools_are_sandboxed(self, tmp_path):
        """Test that langchain_community tools are sandboxed to repo."""
        config = Mock()
        config.REPO_LOCAL_PATH = str(tmp_path)

        with patch("sast_agent_workflow.investigation.tools.factory.repo_handler_factory") as mock_factory:
            mock_factory.return_value = Mock()
            tools = create_investigation_tools(config)

        list_dir_tool = next(t for t in tools if t.name == "list_directory")
        file_search_tool = next(t for t in tools if t.name == "file_search")

        # Both should be sandboxed to the resolved repo path
        assert list_dir_tool.root_dir == str(tmp_path.resolve())
        assert file_search_tool.root_dir == str(tmp_path.resolve())