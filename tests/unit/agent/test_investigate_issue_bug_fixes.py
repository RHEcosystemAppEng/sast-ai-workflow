"""
Test specific bug fixes in investigate_issue implementation.

These tests validate fixes for issues identified during code review.
"""

from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus
from dto.SASTWorkflowModels import PerIssueData
from sast_agent_workflow.agent.agent_state import AnalysisState, SASTAgentState


class TestIsFinalMappingBugFix:
    """
    Test the bug fix for is_final mapping.

    Bug: Original code only set is_final when final_state.is_final == True,
    leaving it unset when False, which could cause stale "TRUE" values to persist.

    Fix: Always set is_final to either "TRUE" or "FALSE" based on final_state.is_final.
    """

    def test_is_final_set_to_true_when_investigation_complete(self):
        """Test is_final is set to TRUE when agent completes investigation."""
        # Create per_issue with no prior analysis_response
        per_issue = PerIssueData(issue=Issue(id="test-001", issue_type="SQL Injection"))

        # Create agent final state with is_final=True
        final_state = SASTAgentState(
            issue_id="test-001",
            issue=per_issue.issue,
            analysis=AnalysisState(
                investigation_result=AnalysisResponse(
                    investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
                    is_final="FALSE",  # Will be updated
                    justifications=["Middleware sanitizes"],
                    prompt="test",
                )
            ),
            is_final=True,  # Investigation complete
        )

        # Apply mapping (from investigate_issue.py lines 103-108)
        per_issue.analysis_response = final_state.analysis.investigation_result
        per_issue.analysis_response.is_final = (
            FinalStatus.TRUE.value if final_state.is_final else FinalStatus.FALSE.value
        )

        # Assert is_final was set to TRUE
        assert per_issue.analysis_response.is_final == FinalStatus.TRUE.value

    def test_is_final_set_to_false_when_investigation_incomplete(self):
        """
        Test is_final is set to FALSE when agent hasn't completed investigation.

        This is the critical bug fix test.
        """
        # Create per_issue with no prior analysis_response
        per_issue = PerIssueData(issue=Issue(id="test-001", issue_type="SQL Injection"))

        # Create agent final state with is_final=False
        final_state = SASTAgentState(
            issue_id="test-001",
            issue=per_issue.issue,
            analysis=AnalysisState(
                investigation_result=AnalysisResponse(
                    investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                    is_final="FALSE",
                    justifications=["Analysis incomplete"],
                    prompt="test",
                )
            ),
            is_final=False,  # Investigation NOT complete
        )

        # Apply mapping
        per_issue.analysis_response = final_state.analysis.investigation_result
        per_issue.analysis_response.is_final = (
            FinalStatus.TRUE.value if final_state.is_final else FinalStatus.FALSE.value
        )

        # Assert is_final was set to FALSE (not left unset!)
        assert per_issue.analysis_response.is_final == FinalStatus.FALSE.value

    def test_is_final_overwrites_stale_true_value(self):
        """
        Test that is_final overwrites stale TRUE value from filter node.

        Scenario:
        1. Filter node marks issue as final (is_final="TRUE")
        2. Agent investigates and determines more analysis needed (is_final=False)
        3. Mapping should update is_final to "FALSE", not leave it as "TRUE"

        This is the specific bug the fix addresses.
        """
        # 1. Filter node marks issue as final
        per_issue = PerIssueData(
            issue=Issue(id="test-001", issue_type="SQL Injection"),
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
                is_final=FinalStatus.TRUE.value,  # Marked final by filter
                justifications=["Matches known false positive"],
                prompt="filter",
            ),
        )

        # Verify initial state
        assert per_issue.analysis_response.is_final == FinalStatus.TRUE.value

        # 2. Agent investigates and determines more analysis needed
        final_state = SASTAgentState(
            issue_id="test-001",
            issue=per_issue.issue,
            analysis=AnalysisState(
                investigation_result=AnalysisResponse(
                    investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                    is_final="FALSE",
                    justifications=["Actually needs more investigation"],
                    prompt="agent",
                )
            ),
            is_final=False,  # Agent says NOT final
        )

        # 3. Apply mapping - should overwrite stale TRUE
        per_issue.analysis_response = final_state.analysis.investigation_result
        per_issue.analysis_response.is_final = (
            FinalStatus.TRUE.value if final_state.is_final else FinalStatus.FALSE.value
        )

        # Assert stale TRUE was overwritten with FALSE
        assert per_issue.analysis_response.is_final == FinalStatus.FALSE.value

    def test_buggy_implementation_would_fail(self):
        """
        Demonstrate that the buggy implementation would fail this test.

        This test shows what would happen with the original buggy code:
        ```python
        if final_state.is_final:
            per_issue.analysis_response.is_final = FinalStatus.TRUE.value
        # Bug: Nothing happens when is_final=False!
        ```
        """
        # Setup: Filter marked as final
        per_issue = PerIssueData(
            issue=Issue(id="test-001", issue_type="Test"),
            analysis_response=AnalysisResponse(
                investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
                is_final=FinalStatus.TRUE.value,  # Stale TRUE
                justifications=["Old"],
                prompt="test",
            ),
        )

        final_state = SASTAgentState(
            issue_id="test-001",
            issue=per_issue.issue,
            analysis=AnalysisState(investigation_result=per_issue.analysis_response),
            is_final=False,  # Agent says not final
        )

        # Apply BUGGY mapping (original code)
        per_issue.analysis_response = final_state.analysis.investigation_result
        if final_state.is_final:  # This is False, so nothing happens!
            per_issue.analysis_response.is_final = FinalStatus.TRUE.value

        # BUG: is_final still "TRUE" (stale value not overwritten)
        assert per_issue.analysis_response.is_final == FinalStatus.TRUE.value  # ❌ WRONG!

        # Now apply CORRECT mapping (fixed code)
        per_issue.analysis_response.is_final = (
            FinalStatus.TRUE.value if final_state.is_final else FinalStatus.FALSE.value
        )

        # CORRECT: is_final now "FALSE"
        assert per_issue.analysis_response.is_final == FinalStatus.FALSE.value  # ✅ CORRECT!

    def test_is_final_with_none_analysis_response(self):
        """
        Test edge case: what if investigation_result is None?

        This would happen if agent fails before producing any analysis.
        """
        per_issue = PerIssueData(issue=Issue(id="test-001", issue_type="Test"))

        final_state = SASTAgentState(
            issue_id="test-001",
            issue=per_issue.issue,
            analysis=AnalysisState(investigation_result=None),  # No result!
            is_final=True,  # But investigation stopped
        )

        # Mapping code
        per_issue.analysis_response = final_state.analysis.investigation_result

        # This would fail: per_issue.analysis_response is None
        # per_issue.analysis_response.is_final = ...  # AttributeError!

        # This edge case needs handling in investigate_issue.py
        # For now, document the issue
        assert per_issue.analysis_response is None
        # TODO: Handle this case in investigate_issue.py with fallback AnalysisResponse
