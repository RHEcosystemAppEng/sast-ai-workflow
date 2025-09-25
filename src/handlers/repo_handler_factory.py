import logging

from common.config import Config
from handlers.c_repo_handler import CRepoHandler
from handlers.go_repo_handler import GoRepoHandler
from handlers.protocols import RepoHandlerProtocol
from Utils.repo_utils import download_repo

logger = logging.getLogger(__name__)


# TODO: ARCHITECTURE EVOLUTION - MCP/Tool-Based Approach
# Current: Direct handler instantiation via factory pattern
# Future: Transform into MCP-ready tool-based architecture:
#   1. Tool Interface: Convert handlers to async tools with standardized ToolResult responses
#   2. Agent Orchestration: Create SASTAnalysisAgent to orchestrate multiple code analysis tools
#   3. MCP Server: Expose tools via MCP protocol for LLM integration
#   4. Plugin System: Handler registry for easy language extension
# Benefits: Better LLM integration, composable workflows, microservice-ready
# Migration Path: handlers → tools → agent → MCP server
def repo_handler_factory(config: Config) -> RepoHandlerProtocol:
    """
    Factory function to create appropriate repository handler based on project type.
    Supports: Go, C, C++
    """
    if config.REPO_LOCAL_PATH:
        repo_path = config.REPO_LOCAL_PATH
    else:
        repo_path = download_repo(config.REPO_REMOTE_URL)

    detected_language = _detect_primary_language(repo_path)

    if detected_language == "go":
        logger.info("Using Go repository handler (auto-detected)")
        return GoRepoHandler(config)

    logger.info(f"Using C repository handler for {detected_language} (auto-detected)")
    return CRepoHandler(config)


def _detect_primary_language(repo_path: str) -> str:
    """
    Auto-detect the primary programming language based on file extensions.
    Supports: Go, C, C++

    Returns the language with the most source files, or 'c' as fallback.
    """
    from pathlib import Path

    try:
        repo = Path(repo_path)

        # Count files by extension patterns (simple glob approach)
        go_count = len(list(repo.rglob("*.go")))
        c_count = len(list(repo.rglob("*.c"))) + len(list(repo.rglob("*.h")))
        cpp_count = (
            len(list(repo.rglob("*.cpp")))
            + len(list(repo.rglob("*.hpp")))
            + len(list(repo.rglob("*.cc")))
            + len(list(repo.rglob("*.cxx")))
        )

        # Log detection results
        total_files = go_count + c_count + cpp_count
        logger.info(
            f"Language detection: Go={go_count}, C={c_count}, "
            f"C++={cpp_count} (total: {total_files})"
        )

        if total_files == 0:
            logger.info("No source files detected, defaulting to 'c'")
            return "c"

        # Return language with most files
        if go_count >= c_count and go_count >= cpp_count:
            logger.info("Primary language: go")
            return "go"
        elif cpp_count >= c_count:
            logger.info("Primary language: cpp")
            return "cpp"
        else:
            logger.info("Primary language: c")
            return "c"

    except Exception as e:
        logger.warning(f"Language detection failed: {e}, defaulting to 'c'")
        return "c"
