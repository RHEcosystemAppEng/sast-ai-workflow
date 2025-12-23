"""
Project context initialization - one-time discovery of project structure.

This module runs once at the start of the workflow to discover:
- Directory structure (3 levels deep)
- Security-related files (sanitizers, validators, etc.)
- Detected frameworks (Django, Rails, Express, etc.)

The discovered context is stored in the workflow tracker and referenced
by all agent investigations.
"""

import logging
import os
import subprocess
from pathlib import Path
from typing import Dict, List

from .agent_state import ProjectContext

logger = logging.getLogger(__name__)


def discover_directory_structure(repo_path: str, max_depth: int = 3) -> Dict[str, List[str]]:
    """
    Discover directory structure up to max_depth levels.

    Args:
        repo_path: Root path of the repository
        max_depth: How deep to traverse (default: 3)

    Returns:
        Dict mapping directory paths to list of files/subdirs
    """
    structure = {}
    exclude_dirs = {".git", "node_modules", "venv", "__pycache__", ".venv", "build", "dist"}

    try:
        for root, dirs, files in os.walk(repo_path):
            # Calculate depth
            depth = root[len(repo_path) :].count(os.sep)
            if depth >= max_depth:
                dirs[:] = []  # Don't recurse deeper
                continue

            # Filter excluded directories
            dirs[:] = [d for d in dirs if d not in exclude_dirs]

            # Store this directory's contents
            relative_root = os.path.relpath(root, repo_path)
            if relative_root == ".":
                relative_root = ""

            # Combine dirs and files
            contents = dirs + files
            if contents:
                structure[relative_root or "/"] = contents[:50]  # Limit to 50 items

        logger.info(f"Discovered {len(structure)} directories in project structure")

    except Exception as e:
        logger.error(f"Failed to discover directory structure: {e}")

    return structure


def find_security_files(repo_path: str, timeout: int = 15) -> List[str]:
    """
    Find files containing security-related patterns using grep.

    Uses subprocess-based grep for fast pattern matching across the repository.
    Searches for common security keywords (auth, sanitize, validate, etc.).

    Args:
        repo_path: Root path of the repository
        timeout: Maximum time to spend searching (seconds)

    Returns:
        List of file paths containing security patterns
    """
    security_patterns = r"sanitize|validate|clean|escape|csrf|xss|sql.*inject"

    try:
        result = subprocess.run(
            [
                "grep",
                "-rl",
                "-E",
                "-i",
                security_patterns,
                repo_path,
                "--exclude-dir=.git",
                "--exclude-dir=node_modules",
                "--exclude-dir=venv",
                "--exclude-dir=__pycache__",
            ],
            capture_output=True,
            text=True,
            timeout=timeout,
        )

        files = [
            os.path.relpath(f.strip(), repo_path)
            for f in result.stdout.split("\n")
            if f.strip() and os.path.isfile(f.strip())
        ]

        logger.info(f"Found {len(files)} security-related files")
        return files[:100]  # Limit to 100 files

    except subprocess.TimeoutExpired:
        logger.warning(f"Security file search timed out after {timeout}s")
        return []
    except Exception as e:
        logger.warning(f"Failed to find security files: {e}")
        return []


def detect_frameworks(repo_path: str) -> List[str]:
    """
    Detect frameworks based on common indicator files.

    Args:
        repo_path: Root path of the repository

    Returns:
        List of detected framework names
    """
    frameworks = []

    indicators = {
        "Django": ["manage.py", "settings.py"],
        "Flask": ["flask", "requirements.txt"],  # Check requirements.txt for "flask"
        "Rails": ["Gemfile", "config/application.rb"],
        "Express": ["package.json", "app.js"],  # Check package.json for "express"
        "Spring": ["pom.xml", "build.gradle"],
        "ASP.NET": ["*.csproj", "web.config"],
    }

    for framework, files in indicators.items():
        for file_pattern in files:
            matches = list(Path(repo_path).rglob(file_pattern))
            if matches:
                # For Python/Node frameworks, check file content
                if framework == "Flask" and "requirements.txt" in file_pattern:
                    for match in matches:
                        try:
                            content = match.read_text()
                            if "flask" in content.lower():
                                frameworks.append(framework)
                                break
                        except Exception:
                            pass
                elif framework == "Express" and "package.json" in file_pattern:
                    for match in matches:
                        try:
                            content = match.read_text()
                            if "express" in content.lower():
                                frameworks.append(framework)
                                break
                        except Exception:
                            pass
                else:
                    frameworks.append(framework)
                break

    logger.info(f"Detected frameworks: {frameworks if frameworks else 'None'}")
    return frameworks


def initialize_project_context(repo_path: str) -> ProjectContext:
    """
    Initialize project context with one-time discovery.

    This runs ONCE per workflow before processing any issues.
    The resulting context is stored in SASTWorkflowTracker and referenced
    by all agent investigations.

    Args:
        repo_path: Root path of the repository to analyze

    Returns:
        ProjectContext with discovered metadata
    """
    logger.info("=" * 60)
    logger.info("Initializing project context...")
    logger.info("=" * 60)

    import time

    start_time = time.time()

    # Discover structure
    structure = discover_directory_structure(repo_path, max_depth=3)

    # Find security files
    security_files = find_security_files(repo_path, timeout=15)

    # Detect frameworks
    frameworks = detect_frameworks(repo_path)

    elapsed = time.time() - start_time

    context = ProjectContext(
        structure=structure, security_files=security_files, frameworks=frameworks
    )

    logger.info("=" * 60)
    logger.info(f"Project context initialized in {elapsed:.2f}s")
    logger.info(f"  - Directories: {len(structure)}")
    logger.info(f"  - Security files: {len(security_files)}")
    logger.info(f"  - Frameworks: {frameworks}")
    logger.info("=" * 60)

    return context
