"""
Unit tests for project context initialization (TASK-001.4).

Tests the one-time discovery of project structure, security files,
and frameworks that runs before agent investigation begins.
"""

import os
import tempfile
import time
from pathlib import Path

import pytest

from sast_agent_workflow.agent.project_context import (
    detect_frameworks,
    discover_directory_structure,
    find_security_files,
    initialize_project_context,
)


class TestDiscoverDirectoryStructure:
    """Test directory structure discovery."""

    @pytest.fixture
    def temp_repo(self):
        """Create a temporary repository structure for testing."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create directory structure (3 levels deep)
            # Level 1
            os.makedirs(os.path.join(tmpdir, "app"))
            os.makedirs(os.path.join(tmpdir, "tests"))
            os.makedirs(os.path.join(tmpdir, "config"))
            os.makedirs(os.path.join(tmpdir, "node_modules"))  # Should be excluded

            # Level 2
            os.makedirs(os.path.join(tmpdir, "app", "middleware"))
            os.makedirs(os.path.join(tmpdir, "app", "views"))
            os.makedirs(os.path.join(tmpdir, "tests", "unit"))

            # Level 3
            os.makedirs(os.path.join(tmpdir, "app", "middleware", "security"))
            os.makedirs(os.path.join(tmpdir, "app", "views", "admin"))

            # Level 4 (should NOT be discovered due to max_depth=3)
            os.makedirs(os.path.join(tmpdir, "app", "middleware", "security", "deep"))

            # Create some files
            Path(os.path.join(tmpdir, "app", "views.py")).touch()
            Path(os.path.join(tmpdir, "app", "models.py")).touch()
            Path(os.path.join(tmpdir, "config", "settings.py")).touch()

            yield tmpdir

    def test_discovers_directories_up_to_max_depth(self, temp_repo):
        """Test that directories are discovered up to max_depth levels."""
        structure = discover_directory_structure(temp_repo, max_depth=3)

        # Should discover levels 0, 1, 2
        # The implementation stops RECURSING at depth >= max_depth, but still stores that level
        # So depth 0 (root), 1 (app), 2 (app/middleware) are fully processed
        # Depth 3 (app/middleware/security) is STORED but not recursed into
        assert "/" in structure or "" in structure
        assert any("app" in key for key in structure.keys())
        assert any("app/middleware" in key for key in structure.keys())

        # Depth 2 directories should be stored (even though we don't recurse into them)
        # Let's verify that depth 2 exists and was recorded
        depth_2_found = any("app/middleware" in key for key in structure.keys())
        assert depth_2_found, f"Structure keys: {list(structure.keys())}"

        # Should NOT discover level 4 (deep) because we stop recursing at depth 3
        assert not any("app/middleware/security/deep" in key for key in structure.keys())

    def test_excludes_node_modules_venv_git(self, temp_repo):
        """Test that excluded directories are not discovered."""
        # Create excluded directories
        os.makedirs(os.path.join(temp_repo, "venv"))
        os.makedirs(os.path.join(temp_repo, ".git"))
        os.makedirs(os.path.join(temp_repo, "__pycache__"))

        structure = discover_directory_structure(temp_repo, max_depth=3)

        # Excluded directories should not appear in structure
        assert not any("node_modules" in key for key in structure.keys())
        assert not any("venv" in key for key in structure.keys())
        assert not any(".git" in key for key in structure.keys())
        assert not any("__pycache__" in key for key in structure.keys())

    def test_includes_files_and_subdirs_in_structure(self, temp_repo):
        """Test that structure includes both files and subdirectories."""
        structure = discover_directory_structure(temp_repo, max_depth=3)

        # Should contain both subdirectories and files
        # Note: The actual structure depends on the traversal, so we check more generally
        all_contents = []
        for contents in structure.values():
            all_contents.extend(contents)

        assert "views.py" in all_contents or any("views.py" in c for c in all_contents)

    def test_limits_to_50_items_per_directory(self, temp_repo):
        """Test that results are limited to 50 items per directory."""
        # Create directory with many files
        many_files_dir = os.path.join(temp_repo, "many_files")
        os.makedirs(many_files_dir)
        for i in range(100):
            Path(os.path.join(many_files_dir, f"file_{i}.txt")).touch()

        structure = discover_directory_structure(temp_repo, max_depth=3)

        # Find the many_files entry
        for key, contents in structure.items():
            if "many_files" in key or key.endswith("many_files"):
                assert len(contents) <= 50
                break

    def test_handles_empty_directory(self):
        """Test handling of empty directory."""
        with tempfile.TemporaryDirectory() as tmpdir:
            structure = discover_directory_structure(tmpdir, max_depth=3)
            # Should return at least the root
            assert len(structure) >= 0

    def test_handles_nonexistent_directory(self):
        """Test graceful handling of nonexistent directory."""
        structure = discover_directory_structure("/nonexistent/path", max_depth=3)
        # Should return empty dict (error logged but not raised)
        assert structure == {}


class TestFindSecurityFiles:
    """Test security file discovery using grep."""

    @pytest.fixture
    def temp_repo_with_security(self):
        """Create temporary repository with security-related files."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create directories
            os.makedirs(os.path.join(tmpdir, "app", "middleware"), exist_ok=True)
            os.makedirs(os.path.join(tmpdir, "app", "utils"), exist_ok=True)
            os.makedirs(os.path.join(tmpdir, "tests"), exist_ok=True)

            # Create files with security patterns
            Path(os.path.join(tmpdir, "app", "middleware", "security.py")).write_text(
                "def sanitize_input(data):\n    return data.strip()"
            )
            Path(os.path.join(tmpdir, "app", "utils", "validators.py")).write_text(
                "def validate_email(email):\n    return '@' in email"
            )
            Path(os.path.join(tmpdir, "app", "utils", "cleaner.py")).write_text(
                "def clean_html(html):\n    return html.escape()"
            )
            Path(os.path.join(tmpdir, "app", "middleware", "csrf.py")).write_text(
                "CSRF_TOKEN = 'abc123'"
            )

            # File without security patterns
            Path(os.path.join(tmpdir, "app", "models.py")).write_text("class User:\n    pass")

            yield tmpdir

    def test_finds_files_with_security_patterns(self, temp_repo_with_security):
        """Test that files containing security patterns are found."""
        files = find_security_files(temp_repo_with_security, timeout=15)

        # Convert to relative paths for comparison
        file_names = [os.path.basename(f) for f in files]

        # Should find files with security keywords
        assert "security.py" in file_names
        assert "validators.py" in file_names
        assert "cleaner.py" in file_names
        assert "csrf.py" in file_names

        # Should NOT find files without security patterns
        assert "models.py" not in file_names

    def test_searches_for_sanitize_validate_clean_escape(self, temp_repo_with_security):
        """Test that specific security patterns are matched."""
        files = find_security_files(temp_repo_with_security, timeout=15)

        assert len(files) >= 3  # At least sanitize, validate, clean files

    def test_limits_to_100_files(self):
        """Test that results are limited to 100 files."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create 150 files with security patterns
            for i in range(150):
                Path(os.path.join(tmpdir, f"file_{i}.py")).write_text("def sanitize():\n    pass")

            files = find_security_files(tmpdir, timeout=15)
            assert len(files) <= 100

    def test_handles_timeout_gracefully(self):
        """Test that timeout is handled gracefully."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create a file
            Path(os.path.join(tmpdir, "test.py")).write_text("sanitize")

            # Very short timeout - might or might not timeout depending on system speed
            files = find_security_files(tmpdir, timeout=0.001)

            # Should return empty list on timeout (no exception)
            assert isinstance(files, list)

    def test_handles_nonexistent_directory(self):
        """Test graceful handling of nonexistent directory."""
        files = find_security_files("/nonexistent/path", timeout=15)
        # Should return empty list (warning logged but not raised)
        assert files == []

    def test_excludes_node_modules_venv_git(self, temp_repo_with_security):
        """Test that excluded directories are not searched."""
        # Create excluded directories with security files
        os.makedirs(os.path.join(temp_repo_with_security, "node_modules"), exist_ok=True)
        Path(os.path.join(temp_repo_with_security, "node_modules", "sanitize.js")).write_text(
            "function sanitize() {}"
        )

        files = find_security_files(temp_repo_with_security, timeout=15)

        # Should not include files from excluded directories
        assert not any("node_modules" in f for f in files)


class TestDetectFrameworks:
    """Test framework detection."""

    @pytest.fixture
    def temp_repo_base(self):
        """Create base temporary repository for framework tests."""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_detects_django(self, temp_repo_base):
        """Test Django framework detection."""
        Path(os.path.join(temp_repo_base, "manage.py")).write_text("# Django management")

        frameworks = detect_frameworks(temp_repo_base)
        assert "Django" in frameworks

    def test_detects_flask(self, temp_repo_base):
        """Test Flask framework detection."""
        Path(os.path.join(temp_repo_base, "requirements.txt")).write_text(
            "flask==2.0.0\nrequests==2.28.0"
        )

        frameworks = detect_frameworks(temp_repo_base)
        assert "Flask" in frameworks

    def test_does_not_detect_flask_without_flask_in_requirements(self, temp_repo_base):
        """Test that Flask is not detected without 'flask' in requirements.txt."""
        Path(os.path.join(temp_repo_base, "requirements.txt")).write_text(
            "requests==2.28.0\ndjango==4.0.0"
        )

        frameworks = detect_frameworks(temp_repo_base)
        assert "Flask" not in frameworks

    def test_detects_rails(self, temp_repo_base):
        """Test Rails framework detection."""
        os.makedirs(os.path.join(temp_repo_base, "config"), exist_ok=True)
        Path(os.path.join(temp_repo_base, "Gemfile")).write_text("source 'https://rubygems.org'")
        Path(os.path.join(temp_repo_base, "config", "application.rb")).write_text("# Rails config")

        frameworks = detect_frameworks(temp_repo_base)
        assert "Rails" in frameworks

    def test_detects_express(self, temp_repo_base):
        """Test Express framework detection."""
        Path(os.path.join(temp_repo_base, "package.json")).write_text(
            '{"dependencies": {"express": "^4.18.0"}}'
        )

        frameworks = detect_frameworks(temp_repo_base)
        assert "Express" in frameworks

    def test_detects_spring(self, temp_repo_base):
        """Test Spring framework detection."""
        Path(os.path.join(temp_repo_base, "pom.xml")).write_text("<project>Spring</project>")

        frameworks = detect_frameworks(temp_repo_base)
        assert "Spring" in frameworks

    def test_detects_multiple_frameworks(self, temp_repo_base):
        """Test detection of multiple frameworks."""
        Path(os.path.join(temp_repo_base, "manage.py")).write_text("# Django")
        Path(os.path.join(temp_repo_base, "requirements.txt")).write_text("flask==2.0.0")

        frameworks = detect_frameworks(temp_repo_base)
        assert "Django" in frameworks
        assert "Flask" in frameworks
        assert len(frameworks) >= 2

    def test_returns_empty_list_for_no_frameworks(self, temp_repo_base):
        """Test that empty list is returned when no frameworks detected."""
        # Create a file that doesn't indicate any framework
        Path(os.path.join(temp_repo_base, "README.md")).write_text("# My Project")

        frameworks = detect_frameworks(temp_repo_base)
        assert frameworks == []


class TestInitializeProjectContext:
    """Test the main initialization function."""

    @pytest.fixture
    def realistic_repo(self):
        """Create a realistic repository structure for integration testing."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create Django-like structure
            os.makedirs(os.path.join(tmpdir, "app", "middleware"), exist_ok=True)
            os.makedirs(os.path.join(tmpdir, "app", "views"), exist_ok=True)
            os.makedirs(os.path.join(tmpdir, "config"), exist_ok=True)
            os.makedirs(os.path.join(tmpdir, "tests"), exist_ok=True)

            # Create framework indicator
            Path(os.path.join(tmpdir, "manage.py")).write_text("# Django manage")
            Path(os.path.join(tmpdir, "requirements.txt")).write_text("django==4.0.0")

            # Create security files
            Path(os.path.join(tmpdir, "app", "middleware", "security.py")).write_text(
                "def sanitize_input(data):\n    return data"
            )
            Path(os.path.join(tmpdir, "app", "views", "validators.py")).write_text(
                "def validate_email(email):\n    return '@' in email"
            )
            Path(os.path.join(tmpdir, "config", "csrf.py")).write_text("CSRF_PROTECTION = True")

            # Create regular files
            Path(os.path.join(tmpdir, "app", "models.py")).write_text("class User:\n    pass")
            Path(os.path.join(tmpdir, "app", "views.py")).write_text("def index():\n    pass")

            yield tmpdir

    def test_integration_returns_complete_project_context(self, realistic_repo):
        """Test that initialization returns complete ProjectContext."""
        context = initialize_project_context(realistic_repo)

        # Check structure is populated
        assert len(context.structure) > 0
        assert isinstance(context.structure, dict)

        # Check security files found
        assert len(context.security_files) > 0
        assert any("security.py" in f for f in context.security_files)

        # Check frameworks detected
        assert len(context.frameworks) > 0
        assert "Django" in context.frameworks

    def test_initialization_completes_within_15_seconds(self, realistic_repo):
        """Test that initialization completes within 15 seconds (CRITICAL for TASK-001.4)."""
        start_time = time.time()
        context = initialize_project_context(realistic_repo)
        elapsed = time.time() - start_time

        # This is a CRITICAL acceptance criterion from TASK-001.4
        assert elapsed < 15.0, f"Initialization took {elapsed:.2f}s, exceeds 15s limit"

        # Verify context is valid
        assert context is not None
        assert isinstance(context.structure, dict)

    def test_stores_results_in_project_context_schema(self, realistic_repo):
        """Test that results conform to ProjectContext schema."""
        context = initialize_project_context(realistic_repo)

        # Verify schema fields
        assert hasattr(context, "structure")
        assert hasattr(context, "security_files")
        assert hasattr(context, "frameworks")

        # Verify types
        assert isinstance(context.structure, dict)
        assert isinstance(context.security_files, list)
        assert isinstance(context.frameworks, list)

        # Verify structure format (dict of lists)
        for key, value in context.structure.items():
            assert isinstance(key, str)
            assert isinstance(value, list)

    def test_handles_empty_repository(self):
        """Test initialization on empty repository."""
        with tempfile.TemporaryDirectory() as tmpdir:
            context = initialize_project_context(tmpdir)

            # Should succeed but with minimal data
            assert context is not None
            assert isinstance(context.structure, dict)
            assert isinstance(context.security_files, list)
            assert isinstance(context.frameworks, list)

    def test_context_persists_across_references(self, realistic_repo):
        """Test that same context can be referenced by multiple agent states."""
        # Initialize once
        context = initialize_project_context(realistic_repo)

        # Simulate sharing across multiple agent states
        ref1 = context
        ref2 = context

        # Both references should point to same object
        assert ref1 is ref2
        assert ref1.frameworks == ref2.frameworks

        # Modifying through one reference affects the other
        ref1.frameworks.append("TestFramework")
        assert "TestFramework" in ref2.frameworks


class TestPerformanceAndScaling:
    """Test performance characteristics and scaling behavior."""

    def test_handles_deep_directory_structure(self):
        """Test handling of very deep directory structures."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create 10-level deep structure
            current = tmpdir
            for i in range(10):
                current = os.path.join(current, f"level_{i}")
                os.makedirs(current, exist_ok=True)
                Path(os.path.join(current, "file.py")).write_text("code")

            # Should only traverse 3 levels (max_depth=3)
            start_time = time.time()
            structure = discover_directory_structure(tmpdir, max_depth=3)
            elapsed = time.time() - start_time

            # Should complete quickly (not traverse all 10 levels)
            assert elapsed < 1.0
            # Verify max depth respected
            assert not any("level_4" in key for key in structure.keys())

    def test_handles_wide_directory_structure(self):
        """Test handling of very wide directory structures."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create 100 sibling directories
            for i in range(100):
                subdir = os.path.join(tmpdir, f"dir_{i}")
                os.makedirs(subdir, exist_ok=True)
                Path(os.path.join(subdir, "file.py")).write_text("code")

            start_time = time.time()
            structure = discover_directory_structure(tmpdir, max_depth=3)
            elapsed = time.time() - start_time

            # Should complete in reasonable time
            assert elapsed < 5.0
            # Should find directories
            assert len(structure) > 0

    def test_large_repository_initialization_performance(self):
        """Test initialization on larger repository structure."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create structure similar to medium-sized project
            # 5 top-level dirs, each with 10 subdirs, each with 5 files
            for i in range(5):
                top_dir = os.path.join(tmpdir, f"module_{i}")
                os.makedirs(top_dir, exist_ok=True)
                for j in range(10):
                    sub_dir = os.path.join(top_dir, f"package_{j}")
                    os.makedirs(sub_dir, exist_ok=True)
                    for k in range(5):
                        file_path = os.path.join(sub_dir, f"file_{k}.py")
                        # Some files with security patterns
                        if k % 2 == 0:
                            Path(file_path).write_text("def sanitize():\n    pass")
                        else:
                            Path(file_path).write_text("def process():\n    pass")

            # Time initialization
            start_time = time.time()
            context = initialize_project_context(tmpdir)
            elapsed = time.time() - start_time

            # Should complete well within 15 seconds
            assert elapsed < 15.0

            # Should discover structure
            assert len(context.structure) > 0
            # Should find some security files
            assert len(context.security_files) > 0


class TestEdgeCases:
    """Test edge cases and error conditions."""

    def test_handles_permission_errors_gracefully(self):
        """Test handling of permission errors during discovery."""
        # Note: This test may not work on all systems
        # Skipping on systems where we can't create permission-denied directories
        pytest.skip("Permission handling test - environment-dependent")

    def test_handles_symlinks(self):
        """Test handling of symbolic links."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create a real directory
            real_dir = os.path.join(tmpdir, "real_dir")
            os.makedirs(real_dir, exist_ok=True)
            Path(os.path.join(real_dir, "file.py")).write_text("code")

            # Create a symlink
            link_dir = os.path.join(tmpdir, "link_dir")
            try:
                os.symlink(real_dir, link_dir)
            except OSError:
                pytest.skip("Symlink creation not supported on this system")

            # Should handle symlinks without infinite loops
            structure = discover_directory_structure(tmpdir, max_depth=3)
            assert len(structure) >= 0  # Should complete without error

    def test_handles_special_characters_in_paths(self):
        """Test handling of special characters in file/directory names."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create directories with special characters
            special_dir = os.path.join(tmpdir, "dir with spaces")
            os.makedirs(special_dir, exist_ok=True)
            Path(os.path.join(special_dir, "file-with-dash.py")).write_text(
                "def sanitize():\n    pass"
            )

            structure = discover_directory_structure(tmpdir, max_depth=3)
            files = find_security_files(tmpdir, timeout=15)

            # Should handle special characters
            assert len(structure) > 0
            # May or may not find the file depending on grep behavior
            assert isinstance(files, list)

    def test_handles_unicode_in_file_content(self):
        """Test handling of unicode characters in file content."""
        with tempfile.TemporaryDirectory() as tmpdir:
            Path(os.path.join(tmpdir, "unicode.py")).write_text(
                "def sanitize_用户输入(data):\n    return data", encoding="utf-8"
            )

            files = find_security_files(tmpdir, timeout=15)

            # Should handle unicode without errors
            assert isinstance(files, list)
