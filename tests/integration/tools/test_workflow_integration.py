"""
End-to-end integration tests for investigation tool workflows.
Tests realistic scenarios of chaining multiple tools together.
"""
import pytest
from pathlib import Path
from unittest.mock import Mock

from sast_agent_workflow.investigation.tools.factory import create_investigation_tools
from common.config import Config


@pytest.fixture
def sample_repo_path():
    """Path to sample repository for integration testing."""
    return Path(__file__).parent / "fixtures" / "sample_repo"


@pytest.fixture
def mock_config(sample_repo_path):
    """Create mock config pointing to sample repository."""
    config = Mock(spec=Config)
    config.REPO_LOCAL_PATH = str(sample_repo_path)
    config.REPO_TYPE = "git"
    return config


@pytest.fixture
def investigation_tools(mock_config):
    """Create all investigation tools."""
    with pytest.MonkeyPatch.context() as m:
        # Mock repo_handler_factory to avoid dependencies
        mock_handler = Mock()
        mock_handler.extract_missing_functions_or_macros = Mock(return_value=("", set()))

        from sast_agent_workflow.investigation.tools import factory
        m.setattr(factory, "repo_handler_factory", lambda config: mock_handler)

        return create_investigation_tools(mock_config)


@pytest.fixture
def tool_dict(investigation_tools):
    """Dictionary of tools by name for easy access."""
    return {tool.name: tool for tool in investigation_tools}


class TestSearchAndFetchWorkflow:
    """Test search → fetch workflow (find function, then retrieve it)."""

    def test_search_for_function_then_fetch_it(self, tool_dict):
        """Test realistic workflow: search for function, then fetch its implementation."""
        # Step 1: Search for sanitize function
        search_result = tool_dict["search_codebase"].invoke({
            "pattern": "sanitize_input",
            "file_pattern": "*.c",
            "max_results": 5
        })

        assert "sanitize_input" in search_result
        assert "src/main.c" in search_result

        # Step 2: Extract file path from search results
        # In real scenario, agent would parse this
        file_path = "src/main.c"  # Found from search

        # Step 3: Fetch the function implementation
        fetch_result = tool_dict["fetch_code"].invoke({
            "file_path": file_path,
            "function_name": "sanitize_input",
            "context_lines": 2
        })

        assert "char* sanitize_input" in fetch_result
        assert "malloc" in fetch_result
        assert "Remove dangerous characters" in fetch_result

    def test_search_malloc_then_examine_allocation(self, tool_dict):
        """Test workflow: find malloc usage, then examine the function."""
        # Step 1: Search for malloc usage
        search_result = tool_dict["search_codebase"].invoke({
            "pattern": "malloc",
            "file_pattern": "*.c",
            "max_results": 10
        })

        assert "malloc" in search_result

        # Step 2: Fetch one of the functions that uses malloc
        fetch_result = tool_dict["fetch_code"].invoke({
            "file_path": "lib/config.c",
            "function_name": "allocate_buffer",
            "context_lines": 0
        })

        assert "malloc" in fetch_result
        assert "allocate_buffer" in fetch_result


class TestExploreAndReadWorkflow:
    """Test list → search → read workflow (explore structure, find files, read)."""

    def test_list_directory_then_search_files(self, tool_dict):
        """Test workflow: list directory contents, then search within."""
        # Step 1: List src directory
        list_result = tool_dict["list_directory"].invoke({"dir_path": "src"})

        assert "main.c" in list_result

        # Step 2: Search for files matching pattern
        file_search_result = tool_dict["file_search"].invoke({
            "pattern": "*.c",
            "dir_path": "src"
        })

        assert "main.c" in file_search_result

        # Step 3: Read the found file
        read_result = tool_dict["read_file"].invoke({
            "file_path": "src/main.c",
            "start_line": 1,
            "end_line": 20
        })

        assert "main.c" in read_result
        assert "#include" in read_result

    def test_explore_lib_directory_and_examine_auth(self, tool_dict):
        """Test workflow: explore lib directory, find auth file, examine it."""
        # Step 1: List lib directory to see what's available
        list_result = tool_dict["list_directory"].invoke({"dir_path": "lib"})

        assert "auth.c" in list_result or "config.c" in list_result

        # Step 2: Read auth.c to understand authentication logic
        read_result = tool_dict["read_file"].invoke({
            "file_path": "lib/auth.c",
            "start_line": None,
            "end_line": None
        })

        assert "authenticate_user" in read_result
        assert "hash_password" in read_result


class TestVulnerabilityInvestigationWorkflow:
    """Test realistic vulnerability investigation scenarios."""

    def test_investigate_input_sanitization(self, tool_dict):
        """Test investigating whether input is properly sanitized."""
        # Step 1: Search for sanitization functions
        sanitize_search = tool_dict["search_codebase"].invoke({
            "pattern": "sanitize.*",
            "file_pattern": "*.c",
            "max_results": 5
        })

        assert "sanitize_input" in sanitize_search

        # Step 2: Fetch the sanitization function
        sanitize_impl = tool_dict["fetch_code"].invoke({
            "file_path": "src/main.c",
            "function_name": "sanitize_input",
            "context_lines": 2
        })

        assert "Remove dangerous characters" in sanitize_impl
        assert ';' in sanitize_impl or '&' in sanitize_impl  # Check what's filtered

        # Step 3: Search for where sanitize_input is called
        usage_search = tool_dict["search_codebase"].invoke({
            "pattern": "process_user_data",
            "file_pattern": "*.c",
            "max_results": 5
        })

        assert "process_user_data" in usage_search

        # Step 4: Fetch the caller to verify sanitization is applied
        caller_impl = tool_dict["fetch_code"].invoke({
            "file_path": "src/main.c",
            "function_name": "process_user_data",
            "context_lines": 0
        })

        assert "sanitize_input" in caller_impl  # Verifies it's called

    def test_investigate_authentication_flow(self, tool_dict):
        """Test investigating authentication implementation."""
        # Step 1: List files to find auth-related code
        list_result = tool_dict["list_directory"].invoke({"dir_path": "lib"})

        assert "auth.c" in list_result

        # Step 2: Search for authentication functions
        auth_search = tool_dict["search_codebase"].invoke({
            "pattern": "auth.*user",
            "file_pattern": "*.c",
            "max_results": 5
        })

        assert "authenticate_user" in auth_search

        # Step 3: Fetch authentication function
        auth_impl = tool_dict["fetch_code"].invoke({
            "file_path": "lib/auth.c",
            "function_name": "authenticate_user",
            "context_lines": 1
        })

        assert "username" in auth_impl
        assert "password" in auth_impl

        # Step 4: Check password hashing
        hash_impl = tool_dict["fetch_code"].invoke({
            "file_path": "lib/auth.c",
            "function_name": "hash_password",
            "context_lines": 0
        })

        assert "hash" in hash_impl

    def test_investigate_memory_management(self, tool_dict):
        """Test investigating memory allocation and deallocation."""
        # Step 1: Search for memory allocation
        malloc_search = tool_dict["search_codebase"].invoke({
            "pattern": "malloc|calloc|realloc",
            "file_pattern": "*.c",
            "max_results": 15
        })

        assert "malloc" in malloc_search or "realloc" in malloc_search

        # Step 2: Examine allocation function
        alloc_impl = tool_dict["fetch_code"].invoke({
            "file_path": "lib/config.c",
            "function_name": "allocate_buffer",
            "context_lines": 0
        })

        assert "malloc" in alloc_impl

        # Step 3: Check if there's cleanup (free)
        free_search = tool_dict["search_codebase"].invoke({
            "pattern": "free\\s*\\(",
            "file_pattern": "*.c",
            "max_results": 10
        })

        # Should find free calls (in process_user_data)
        assert "free" in free_search or "No matches" in free_search


class TestHeaderFileInvestigation:
    """Test investigating header files and includes."""

    def test_examine_header_files_for_function_declarations(self, tool_dict):
        """Test workflow for examining header files."""
        # Step 1: List include directory
        list_result = tool_dict["list_directory"].invoke({"dir_path": "include"})

        assert "utils.h" in list_result or "auth.h" in list_result

        # Step 2: Read header to see declarations
        utils_header = tool_dict["read_file"].invoke({
            "file_path": "include/utils.h",
            "start_line": None,
            "end_line": None
        })

        assert "sanitize_input" in utils_header
        assert "validate_email" in utils_header

        # Step 3: Find implementation in source files
        impl_search = tool_dict["search_codebase"].invoke({
            "pattern": "sanitize_input.*\\{",
            "file_pattern": "*.c",
            "max_results": 5
        })

        assert "sanitize_input" in impl_search or "src/main.c" in impl_search


class TestToolChaining:
    """Test chaining all tools together in complex scenarios."""

    def test_complete_investigation_workflow(self, tool_dict):
        """Test complete investigation: list → search → fetch → read."""
        # Scenario: Investigating a SAST finding about input validation

        # Step 1: Explore codebase structure
        root_list = tool_dict["list_directory"].invoke({"dir_path": "."})
        assert "src" in root_list
        assert "lib" in root_list
        assert "include" in root_list

        # Step 2: Find validation functions
        validation_search = tool_dict["search_codebase"].invoke({
            "pattern": "validate_.*",
            "file_pattern": "*.c",
            "max_results": 10
        })
        assert "validate_email" in validation_search

        # Step 3: Fetch validation implementation
        validation_impl = tool_dict["fetch_code"].invoke({
            "file_path": "src/main.c",
            "function_name": "validate_email",
            "context_lines": 1
        })
        assert "strchr" in validation_impl

        # Step 4: Read header to see interface
        header_content = tool_dict["read_file"].invoke({
            "file_path": "include/utils.h",
            "start_line": None,
            "end_line": None
        })
        assert "validate_email" in header_content

        # Step 5: Search for usage
        usage_search = tool_dict["search_codebase"].invoke({
            "pattern": "validate_email",
            "file_pattern": "*.c",
            "max_results": 10
        })
        # Should find both definition and potential calls
        assert "validate_email" in usage_search

    def test_all_tools_work_together(self, investigation_tools):
        """Smoke test: verify all tools can be invoked without errors."""
        tools_invoked = []

        for tool in investigation_tools:
            try:
                if tool.name == "fetch_code":
                    result = tool.invoke({
                        "file_path": "src/main.c",
                        "function_name": "sanitize_input",
                        "context_lines": 0
                    })
                elif tool.name == "search_codebase":
                    result = tool.invoke({
                        "pattern": "sanitize",
                        "file_pattern": "*.c",
                        "max_results": 5
                    })
                elif tool.name == "read_file":
                    result = tool.invoke({
                        "file_path": "include/utils.h",
                        "start_line": None,
                        "end_line": None
                    })
                elif tool.name == "list_directory":
                    result = tool.invoke({"dir_path": "src"})
                elif tool.name == "file_search":
                    result = tool.invoke({
                        "pattern": "*.c",
                        "dir_path": "src"
                    })
                else:
                    continue

                tools_invoked.append(tool.name)
                assert result  # Should return something
                assert "Error" not in result or "does not exist" not in result

            except Exception as e:
                pytest.fail(f"Tool {tool.name} failed: {e}")

        # Verify we tested the main tools
        assert "fetch_code" in tools_invoked
        assert "search_codebase" in tools_invoked
        assert "read_file" in tools_invoked
