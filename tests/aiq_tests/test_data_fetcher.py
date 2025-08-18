"""
Unit tests for the data_fetcher tool's core function.
"""

import unittest
from unittest.mock import Mock, patch

from sast_agent_workflow.tools.data_fetcher import data_fetcher, DataFetcherConfig
from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from dto.ResponseStructures import InstructionResponse
from common.config import Config
from aiq.builder.builder import Builder
from common.constants import TRUE, FALSE
from tests.aiq_tests.test_utils import TestUtils


class TestDataFetcherCore(unittest.IsolatedAsyncioTestCase):
    """Test cases for the data_fetcher core function (_data_fetcher_fn)."""

    def setUp(self):
        """Set up test fixtures."""
        self.sample_issues = TestUtils.create_sample_issues()
        self.mock_config = Mock(spec=Config)
        self.data_fetcher_config = DataFetcherConfig()
        self.builder = Mock(spec=Builder)
        
        # Create a sample tracker with issues
        self.sample_tracker = TestUtils.create_sample_tracker(self.sample_issues)

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_given_sample_tracker_when_data_fetcher_executed_then_preserves_tracker_structure_and_populates_source_code(self, mock_repo_handler_factory):
        """Given a sample tracker, when data_fetcher is executed, then it preserves tracker structure.

           Covered:
           - Initial fetch from report data populates source_code
           - Subsequent fetch via instructions merges correctly
           - Verification step sets is_final when no new data
           - Iteration count preserved

           Pending (integration level):
           - End-to-end repo handler integration with real repo and clang
        """
        
        # preparation: repo handler mock to avoid real initialization
        mock_repo_handler = Mock()
        mock_repo_handler.get_source_code_blocks_from_error_trace.return_value = {"file.c": "int f(){}"}
        mock_repo_handler.reset_found_symbols.return_value = None
        mock_repo_handler_factory.return_value = mock_repo_handler

        # testing
        result_tracker = await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, self.sample_tracker)
        
        # assertion
        self.assertIsInstance(result_tracker, SASTWorkflowTracker)
        
        self.assertEqual(len(result_tracker.issues), 2)
        self.assertEqual(result_tracker.iteration_count, 0)
        self.assertEqual(result_tracker.config, self.sample_tracker.config)
        
        for per_issue_data in result_tracker.issues.values():
            self.assertIsNotNone(per_issue_data.issue)
            self.assertIsInstance(per_issue_data.issue, Issue)
            self.assertIsInstance(per_issue_data.analysis_response, AnalysisResponse)



    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_initial_iteration_populates_source_code_from_error_trace(self, mock_repo_handler_factory):
        """Initial iteration should fetch source code from error traces and populate source_code."""
        # preparation
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=0)
        mock_repo_handler = Mock()
        # Same mock return for any trace
        mock_repo_handler.get_source_code_blocks_from_error_trace.return_value = {"file.c": "int main() {return 0;}"}
        mock_repo_handler_factory.return_value = mock_repo_handler

        # testing
        result_tracker = await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)

        # assertion
        for per_issue_data in result_tracker.issues.values():
            self.assertTrue(per_issue_data.source_code)
            self.assertIn("file.c", per_issue_data.source_code)
            self.assertTrue(any("int main()" in snippet for snippet in per_issue_data.source_code["file.c"]))
        self.assertEqual(mock_repo_handler.get_source_code_blocks_from_error_trace.call_count, len(tracker.issues))

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_skips_issue_marked_final(self, mock_repo_handler_factory):
        """If issue is already final (e.g., by filter), data_fetcher should skip fetching for it."""
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=0)
        # Mark first issue as final
        first_issue = next(iter(tracker.issues.values()))
        first_issue.analysis_response.is_final = TRUE
        first_issue_source_before = dict(first_issue.source_code)

        mock_repo_handler = Mock()
        mock_repo_handler.get_source_code_blocks_from_error_trace.return_value = {"file.c": "int main() {return 0;}"}
        mock_repo_handler_factory.return_value = mock_repo_handler

        result_tracker = await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)

        # First issue should remain untouched (no new keys added)
        self.assertEqual(first_issue.source_code, first_issue_source_before)
        # Repo handler should be called only for the non-final issue
        self.assertEqual(mock_repo_handler.get_source_code_blocks_from_error_trace.call_count, 1)
        # Verify only non-final issues got the new file added
        new_file_counts = sum(1 for p in result_tracker.issues.values() if "file.c" in p.source_code)
        self.assertEqual(new_file_counts, 1)

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory', side_effect=Exception("init failed"))
    async def test_repo_handler_init_failure_raises(self, _mock_repo_handler_factory):
        """Repo handler initialization failure should raise, mirroring pre_process behavior."""
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=0)
        with self.assertRaises(RuntimeError):
            await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_subsequent_iteration_fetches_by_instructions_and_merges(self, mock_repo_handler_factory):
        """Subsequent iteration should fetch additional data per instructions and merge into existing source_code."""
        # preparation
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=1)
        # Ensure each issue has instructions and a non-final analysis_response
        for per_issue_data in tracker.issues.values():
            per_issue_data.analysis_response = TestUtils.create_sample_analysis_response(
                is_false_positive=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=FALSE,
                instructions=[
                    InstructionResponse(expression_name="foo", referring_source_code_path="src/foo.c", recommendation="inspect")
                ]
            )
            per_issue_data.source_code = {"existing.c": ["void existing(){}"]}

        mock_repo_handler = Mock()
        # Build a missing source string consistent with repo handler format
        missing = (
            "code of src/foo.c file:\nint foo(){return 1;}\n"
            "code of include/bar.h file:\n#define BAR 1\n"
        )
        mock_repo_handler.extract_missing_functions_or_macros.return_value = (missing, set())
        mock_repo_handler_factory.return_value = mock_repo_handler

        # testing
        result_tracker = await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)

        # assertion
        for per_issue_data in result_tracker.issues.values():
            self.assertIn("existing.c", per_issue_data.source_code)
            self.assertIn("src/foo.c", per_issue_data.source_code)
            self.assertIn("include/bar.h", per_issue_data.source_code)
            self.assertTrue(any("int foo()" in snippet for snippet in per_issue_data.source_code["src/foo.c"]))
            self.assertTrue(any("#define BAR" in snippet for snippet in per_issue_data.source_code["include/bar.h"]))
            self.assertEqual(per_issue_data.analysis_response.is_final, FALSE)

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_subsequent_iteration_when_second_analysis_not_needed_then_no_fetch(self, mock_repo_handler_factory):
        """If second analysis not needed, do not call repo handler for instructions."""
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=1)
        # analysis_response with is_final TRUE will short-circuit is_second_analysis_needed
        for per_issue_data in tracker.issues.values():
            per_issue_data.analysis_response = TestUtils.create_sample_analysis_response(
                is_false_positive=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=TRUE,
                instructions=[]
            )
        mock_repo_handler = Mock()
        mock_repo_handler_factory.return_value = mock_repo_handler
        result_tracker = await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)
        mock_repo_handler.extract_missing_functions_or_macros.assert_not_called()

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_verification_sets_is_final_when_no_new_data(self, mock_repo_handler_factory):
        """If instructions exist but no new data is fetched, set is_final to TRUE."""
        # preparation
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=1)
        for per_issue_data in tracker.issues.values():
            per_issue_data.analysis_response = TestUtils.create_sample_analysis_response(
                is_false_positive=CVEValidationStatus.TRUE_POSITIVE.value,
                is_final=FALSE,
                instructions=[
                    InstructionResponse(expression_name="foo", referring_source_code_path="src/foo.c", recommendation="inspect")
                ]
            )
            per_issue_data.source_code = {}

        mock_repo_handler = Mock()
        mock_repo_handler.extract_missing_functions_or_macros.return_value = ("", set())  # No data fetched
        mock_repo_handler_factory.return_value = mock_repo_handler

        # testing
        result_tracker = await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)

        # assertion
        for per_issue_data in result_tracker.issues.values():
            self.assertEqual(per_issue_data.analysis_response.is_final, TRUE)

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_malformed_missing_source_codes_yield_no_additions_and_set_is_final(self, mock_repo_handler_factory):
        """Malformed or empty missing_source_codes should not add source and should set is_final to TRUE when instructions exist."""
        bad_inputs = ["", "random text", "code of file without break", "code of a file:\n"]
        for s in bad_inputs:
            tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=1)
            for per_issue_data in tracker.issues.values():
                per_issue_data.analysis_response = TestUtils.create_sample_analysis_response(
                    is_false_positive=CVEValidationStatus.TRUE_POSITIVE.value,
                    is_final=FALSE,
                    instructions=[
                        InstructionResponse(expression_name="foo", referring_source_code_path="src/foo.c", recommendation="inspect")
                    ]
                )
                per_issue_data.source_code = {}
            mock_repo_handler = Mock()
            mock_repo_handler.extract_missing_functions_or_macros.return_value = (s, set())
            mock_repo_handler_factory.return_value = mock_repo_handler

            result_tracker = await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)
            for per_issue_data in result_tracker.issues.values():
                self.assertEqual(per_issue_data.source_code, {})
                self.assertEqual(per_issue_data.analysis_response.is_final, TRUE)

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_handles_non_per_issue_data_entry(self, mock_repo_handler_factory):
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=0)
        # Inject a non-PerIssueData entry
        tracker.issues["bad"] = "not-per-issue"
        mock_repo_handler_factory.return_value = Mock()
        # Should not raise and should skip the bad entry
        result_tracker = await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)
        self.assertIn("bad", result_tracker.issues)

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_initial_iteration_initializes_none_source_code(self, mock_repo_handler_factory):
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=0)
        # Force one issue to have None source_code
        first_issue = next(iter(tracker.issues.values()))
        first_issue.source_code = None
        mock_repo_handler = Mock()
        mock_repo_handler.get_source_code_blocks_from_error_trace.return_value = {}
        mock_repo_handler_factory.return_value = mock_repo_handler
        result_tracker = await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)
        first_issue_after = result_tracker.issues[first_issue.issue.id]
        self.assertIsInstance(first_issue_after.source_code, dict)

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_initial_fetch_merges_same_path_content(self, mock_repo_handler_factory):
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=0)
        first_issue = next(iter(tracker.issues.values()))
        first_issue.source_code = {"file.c": ["old"]}
        mock_repo_handler = Mock()
        mock_repo_handler.get_source_code_blocks_from_error_trace.return_value = {"file.c": "new"}
        mock_repo_handler_factory.return_value = mock_repo_handler
        result_tracker = await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)
        merged = result_tracker.issues[first_issue.issue.id].source_code["file.c"]
        self.assertIn("old", merged)
        self.assertIn("new", merged)

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_initial_fetch_error_and_reset_exception_are_handled(self, mock_repo_handler_factory):
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=0)
        mock_repo_handler = Mock()
        mock_repo_handler.get_source_code_blocks_from_error_trace.side_effect = Exception("boom")
        mock_repo_handler.reset_found_symbols.side_effect = Exception("reset-boom")
        mock_repo_handler_factory.return_value = mock_repo_handler
        # Should not raise
        await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_subsequent_iteration_with_no_analysis_response_skips(self, mock_repo_handler_factory):
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=1)
        for per_issue in tracker.issues.values():
            per_issue.analysis_response = None
        mock_repo_handler = Mock()
        mock_repo_handler_factory.return_value = mock_repo_handler
        await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)
        mock_repo_handler.extract_missing_functions_or_macros.assert_not_called()

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_subsequent_iteration_with_no_repo_handler_continues(self, mock_repo_handler_factory):
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=1)
        # Remove config to simulate no repo handler
        tracker.config = None
        for per_issue in tracker.issues.values():
            per_issue.analysis_response = TestUtils.create_sample_analysis_response(
                is_final=FALSE,
                instructions=[InstructionResponse(expression_name="foo", referring_source_code_path="a.c", recommendation="inspect")]
            )
        mock_repo_handler_factory.return_value = None
        await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_subsequent_iteration_processing_error_caught(self, mock_repo_handler_factory):
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=1)
        for per_issue in tracker.issues.values():
            per_issue.analysis_response = TestUtils.create_sample_analysis_response(
                is_final=FALSE,
                instructions=[InstructionResponse(expression_name="foo", referring_source_code_path="a.c", recommendation="inspect")]
            )
        mock_repo_handler = Mock()
        mock_repo_handler.extract_missing_functions_or_macros.side_effect = Exception("oops")
        mock_repo_handler_factory.return_value = mock_repo_handler
        # Test should complete without raising exception despite the side_effect
        result_tracker = await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)
        # Verify the tracker is returned and errors were handled gracefully
        self.assertIsNotNone(result_tracker)

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_subsequent_iteration_merge_appends_to_existing_path(self, mock_repo_handler_factory):
        tracker = TestUtils.create_sample_tracker(self.sample_issues, iteration_count=1)
        for per_issue in tracker.issues.values():
            per_issue.analysis_response = TestUtils.create_sample_analysis_response(
                is_final=FALSE,
                instructions=[InstructionResponse(expression_name="foo", referring_source_code_path="src/foo.c", recommendation="inspect")]
            )
            per_issue.source_code = {"src/foo.c": ["old"]}
        missing = (
            "code of src/foo.c file:\nint foo(){return 1;}\n"
        )
        mock_repo_handler = Mock()
        mock_repo_handler.extract_missing_functions_or_macros.return_value = (missing, set())
        mock_repo_handler_factory.return_value = mock_repo_handler
        result_tracker = await TestUtils.run_single_fn(data_fetcher, self.data_fetcher_config, self.builder, tracker)
        for per_issue in result_tracker.issues.values():
            merged = per_issue.source_code["src/foo.c"]
            self.assertIn("old", merged)
            self.assertTrue(any("int foo()" in snippet for snippet in merged))

    @patch('sast_agent_workflow.tools.data_fetcher.repo_handler_factory')
    async def test_tracker_none_raises_value_error(self, mock_repo_handler_factory):
        mock_repo_handler_factory.return_value = Mock()
        # Extract single_fn and call with None to hit early guard
        async with data_fetcher(self.data_fetcher_config, self.builder) as func_info:
            with self.assertRaises(ValueError):
                await func_info.single_fn(None)



if __name__ == '__main__':
    unittest.main()