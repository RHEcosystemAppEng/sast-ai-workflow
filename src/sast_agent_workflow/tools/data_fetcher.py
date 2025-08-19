import logging
import re
from typing import Dict, List

from pydantic import Field

from aiq.builder.builder import Builder
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from handlers.repo_handler_factory import repo_handler_factory
from dto.LLMResponse import FinalStatus

logger = logging.getLogger(__name__)



class DataFetcherConfig(FunctionBaseConfig, name="data_fetcher"):
    """
    Data fetcher function for SAST workflow.
    """
    description: str = Field(
        default="Data fetcher function that fetches required data for analysis",
        description="Function description"
    )




@register_function(config_type=DataFetcherConfig)
async def data_fetcher(
    config: DataFetcherConfig, builder: Builder
):
    """Register the Data_Fetcher function."""

    logger.info("Initializing Data_Fetcher function...")

    async def _data_fetcher_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Fetch required source code for each issue.

        - Initial analysis (iteration_count == 0): fetch from error trace in report
        - Subsequent loops: fetch additional data based on instructions in analysis_response
        - Verification: if instructions present but no new data is fetched, set is_final to "TRUE"
        """
        if tracker is None:
            raise ValueError("Tracker must not be None")

        logger.info("Running Data_Fetcher node - fetching data")
        logger.info(f"Data_Fetcher node processing tracker with {len(tracker.issues)} issues")

        # Initialize handler using tracker config
        repo_handler = None
        if tracker.config is not None:
            try:
                repo_handler = repo_handler_factory(tracker.config)
            except Exception as e:
                logger.error(f"Failed to initialize repository handler: {e}")
                raise RuntimeError(f"Repository handler initialization failed: {e}") from e

        for issue_id, per_issue in tracker.issues.items():
            if not isinstance(per_issue, PerIssueData):
                logger.warning(f"Skipping issue {issue_id}: unexpected data type {type(per_issue)}")
                continue

            # Ensure source_code mapping exists
            if per_issue.source_code is None:
                per_issue.source_code = {}

            # If an earlier node (e.g., filter) already marked this issue final, skip fetching
            analysis_response = per_issue.analysis_response
            if analysis_response and analysis_response.is_final == FinalStatus.TRUE.value:
                logger.info(f"Skipping issue {issue_id}: already final")
                continue

            # Initial analysis: fetch by error trace
            if tracker.iteration_count == 0:
                try:
                    if repo_handler is not None:
                        fetched = repo_handler.get_source_code_blocks_from_error_trace(per_issue.issue.trace)
                        # Convert to list structure for multiple snippets per file
                        for path, code in (fetched or {}).items():
                            if path not in per_issue.source_code:
                                per_issue.source_code[path] = []
                            per_issue.source_code[path].append(code)
                except Exception as e:
                    logger.error(f"Failed to fetch source code for issue {issue_id} from error trace: {e}")
                continue

            # Subsequent iterations: use instructions if second analysis is needed
            if not analysis_response:
                continue

            try:
                if analysis_response.is_second_analysis_needed():
                    if repo_handler is None:
                        continue
                    missing_source_codes, per_issue.found_symbols = repo_handler.extract_missing_functions_or_macros(
                        analysis_response.instructions, per_issue.found_symbols
                    )
                    # Parse response format: "code of <path> file:\n<code>"
                    additions: Dict[str, str] = {}
                    if missing_source_codes:
                        pattern = re.compile(r"code of\s+(?P<path>.+?)\s+file:\n", re.MULTILINE)
                        positions = [(m.start(), m.end(), m.group("path")) for m in pattern.finditer(missing_source_codes)]
                        for idx, (_, end, path) in enumerate(positions):
                            code_start = end
                            code_end = positions[idx + 1][0] if idx + 1 < len(positions) else len(missing_source_codes)
                            snippet = missing_source_codes[code_start:code_end].rstrip("\n")
                            if snippet.strip():
                                additions[path] = snippet
                    if additions:
                        for path, code in additions.items():
                            if path not in per_issue.source_code:
                                per_issue.source_code[path] = []
                            per_issue.source_code[path].append(code)
                    else:
                        # Verification step: no new data fetched though instructions exist
                        logger.debug(f"Issue {issue_id}: Setting is_final=TRUE manually - no new data fetched despite instructions present")
                        analysis_response.is_final = FinalStatus.TRUE.value
            except Exception as e:
                logger.error(f"Failed processing instructions for issue {issue_id}: {e}")

        logger.info("Data_Fetcher node completed")
        return tracker

    try:
        yield FunctionInfo.create(
            single_fn=_data_fetcher_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker
        )
    except GeneratorExit:
        logger.info("Data_Fetcher function exited early!")
    finally:
        logger.info("Cleaning up Data_Fetcher function.")
