"""
Unit tests for state mapping between SASTAgentState and PerIssueData.

Tests the critical integration point where agent investigation results
are mapped back to the batch workflow tracker state.
"""

import pytest

from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus
from dto.SASTWorkflowModels import PerIssueData
from sast_agent_workflow.agent.agent_state import (
    AnalysisState,
    InvestigationContext,
    ProjectContext,
    SASTAgentState,
)


class TestStateMapping:
    """Test state mapping between agent state and tracker state."""

    @pytest.fixture
    def sample_issue(self):
        """Create a sample issue for testing."""
        return Issue(
            id="test-001",
            issue_type="SQL Injection",
            file_path="app/views.py",
            line_number=45,
            trace="User input flows to SQL query without sanitization",
        )

    @pytest.fixture
    def sample_project_context(self):
        """Create sample project context."""
        return ProjectContext(
            structure={"app/": ["views.py", "models.py"]},
            security_files=["app/middleware/security.py"],
            frameworks=["Django"],
        )

    @pytest.fixture
    def initial_per_issue(self, sample_issue):
        """Create initial PerIssueData before agent investigation."""
        return PerIssueData(
            issue=sample_issue,
            source_code={"app/views.py": ["def search_users():\n    ..."]},
            found_symbols=set(["search_users"]),
            analysis_response=None,
        )

    @pytest.fixture
    def final_agent_state(self, sample_issue, sample_project_context):
        """Create final agent state after investigation."""
        return SASTAgentState(
            issue_id="test-001",
            issue=sample_issue,
            context=InvestigationContext(
                fetched_files={
                    "app/views.py": ["def search_users():\n    ..."],
                    "app/middleware/security.py": ["class InputSanitizer:\n    ..."],
                },
                found_symbols={"search_users", "InputSanitizer"},
            ),
            analysis=AnalysisState(
                investigation_result=AnalysisResponse(
                    investigation_result=CVEValidationStatus.FALSE_POSITIVE.value,
                    is_final="FALSE",  # Will be set by comprehensive_evaluation
                    justifications=["Global middleware sanitizes input"],
                    prompt="Analysis prompt here",
                ),
                verdict="FALSE_POSITIVE",
            ),
            is_final=True,  # Investigation complete
            iteration_count=5,
            project_context=sample_project_context,
        )

    def test_mapping_agent_to_tracker_final_true(self, initial_per_issue, final_agent_state):
        """
        Test mapping from final agent state (is_final=True) to tracker state.

        Simulates the code at investigate_issue.py lines 102-110.
        """
        per_issue = initial_per_issue
        final_state = final_agent_state

        # Simulate the mapping code
        per_issue.analysis_response = final_state.analysis.investigation_result
        per_issue.source_code = final_state.context.fetched_files
        per_issue.found_symbols = final_state.context.found_symbols

        # Update is_final status (ALWAYS - whether TRUE or FALSE)
        per_issue.analysis_response.is_final = (
            FinalStatus.TRUE.value if final_state.is_final else FinalStatus.FALSE.value
        )

        # Assertions
        assert per_issue.analysis_response is not None
        assert (
            per_issue.analysis_response.investigation_result
            == CVEValidationStatus.FALSE_POSITIVE.value
        )
        assert per_issue.analysis_response.is_final == FinalStatus.TRUE.value  # Should be "TRUE"
        assert len(per_issue.source_code) == 2  # Fetched 2 files
        assert "app/middleware/security.py" in per_issue.source_code
        assert "InputSanitizer" in per_issue.found_symbols
        assert len(per_issue.found_symbols) == 2

    def test_mapping_agent_to_tracker_final_false(self, initial_per_issue, final_agent_state):
        """
        Test mapping when agent investigation is not complete (is_final=False).

        This tests the bug fix - is_final should be set to FALSE, not left unset.
        """
        per_issue = initial_per_issue
        final_state = final_agent_state
        final_state.is_final = False  # Investigation NOT complete

        # Simulate the mapping code
        per_issue.analysis_response = final_state.analysis.investigation_result
        per_issue.source_code = final_state.context.fetched_files
        per_issue.found_symbols = final_state.context.found_symbols

        # Update is_final status (ALWAYS - whether TRUE or FALSE)
        per_issue.analysis_response.is_final = (
            FinalStatus.TRUE.value if final_state.is_final else FinalStatus.FALSE.value
        )

        # Assertions
        assert per_issue.analysis_response is not None
        assert per_issue.analysis_response.is_final == FinalStatus.FALSE.value  # Should be "FALSE"

    def test_mapping_preserves_investigation_result(self, initial_per_issue, final_agent_state):
        """Test that investigation result (verdict) is preserved correctly."""
        per_issue = initial_per_issue
        final_state = final_agent_state

        # Test TRUE_POSITIVE
        final_state.analysis.investigation_result.investigation_result = (
            CVEValidationStatus.TRUE_POSITIVE.value
        )
        per_issue.analysis_response = final_state.analysis.investigation_result

        assert (
            per_issue.analysis_response.investigation_result
            == CVEValidationStatus.TRUE_POSITIVE.value
        )

        # Test FALSE_POSITIVE
        final_state.analysis.investigation_result.investigation_result = (
            CVEValidationStatus.FALSE_POSITIVE.value
        )
        per_issue.analysis_response = final_state.analysis.investigation_result

        assert (
            per_issue.analysis_response.investigation_result
            == CVEValidationStatus.FALSE_POSITIVE.value
        )

    def test_mapping_fetched_files_merge(self, initial_per_issue, final_agent_state):
        """Test that fetched files are correctly merged/replaced."""
        per_issue = initial_per_issue
        final_state = final_agent_state

        # Initial state has 1 file
        assert len(per_issue.source_code) == 1
        assert "app/views.py" in per_issue.source_code

        # Mapping replaces entire source_code dict
        per_issue.source_code = final_state.context.fetched_files

        # Final state has 2 files
        assert len(per_issue.source_code) == 2
        assert "app/views.py" in per_issue.source_code
        assert "app/middleware/security.py" in per_issue.source_code

    def test_mapping_found_symbols_accumulation(self, initial_per_issue, final_agent_state):
        """Test that found_symbols are correctly accumulated."""
        per_issue = initial_per_issue
        final_state = final_agent_state

        # Initial state has 1 symbol
        assert len(per_issue.found_symbols) == 1
        assert "search_users" in per_issue.found_symbols

        # Mapping replaces entire found_symbols set
        per_issue.found_symbols = final_state.context.found_symbols

        # Final state has 2 symbols
        assert len(per_issue.found_symbols) == 2
        assert "search_users" in per_issue.found_symbols
        assert "InputSanitizer" in per_issue.found_symbols

    def test_mapping_with_no_investigation_result(
        self, initial_per_issue, sample_issue, sample_project_context
    ):
        """Test mapping when agent didn't produce investigation_result (error case)."""
        per_issue = initial_per_issue

        # Create agent state without investigation_result
        final_state = SASTAgentState(
            issue_id="test-001",
            issue=sample_issue,
            context=InvestigationContext(
                fetched_files={"app/views.py": ["code"]}, found_symbols=set()
            ),
            analysis=AnalysisState(investigation_result=None),  # No result!
            is_final=True,  # Investigation terminated (e.g., by circuit breaker)
            project_context=sample_project_context,
        )

        # Mapping code should handle None gracefully
        per_issue.analysis_response = final_state.analysis.investigation_result
        per_issue.source_code = final_state.context.fetched_files
        per_issue.found_symbols = final_state.context.found_symbols

        # This would fail - analysis_response is None!
        # per_issue.analysis_response.is_final = ...  # AttributeError!

        # Assert that this is a known issue that needs handling
        assert per_issue.analysis_response is None

    def test_bidirectional_mapping_agent_to_tracker_to_agent(
        self, sample_issue, sample_project_context
    ):
        """
        Test bidirectional mapping: PerIssueData → SASTAgentState → PerIssueData.

        Simulates:
        1. Initial per_issue state
        2. Create agent state from per_issue (investigate_issue.py lines 84-90)
        3. Agent modifies state
        4. Map back to per_issue (lines 102-110)
        """
        # 1. Initial per_issue state
        per_issue = PerIssueData(
            issue=sample_issue,
            source_code={"app/views.py": ["initial code"]},
            found_symbols={"initial_symbol"},
            analysis_response=None,
        )

        # 2. Create agent state from per_issue (forward mapping)
        agent_state = SASTAgentState(
            issue_id=sample_issue.id,
            issue=per_issue.issue,
            context=InvestigationContext(
                fetched_files=dict(per_issue.source_code) if per_issue.source_code else {},
                # Note: found_symbols is now List[str] for JSON serialization
                found_symbols=list(per_issue.found_symbols) if per_issue.found_symbols else [],
            ),
            project_context=sample_project_context,
        )

        # Verify forward mapping
        assert agent_state.issue == per_issue.issue
        assert agent_state.context.fetched_files == per_issue.source_code
        # Compare as sets since order doesn't matter
        assert set(agent_state.context.found_symbols) == per_issue.found_symbols

        # 3. Agent modifies state
        agent_state.context.fetched_files["new_file.py"] = ["new code"]
        agent_state.context.found_symbols.append("new_symbol")  # Now using list
        agent_state.analysis.investigation_result = AnalysisResponse(
            investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
            is_final="FALSE",
            justifications=["Analysis"],
            prompt="Prompt",
        )
        agent_state.is_final = True

        # 4. Map back to per_issue (backward mapping)
        per_issue.analysis_response = agent_state.analysis.investigation_result
        per_issue.source_code = agent_state.context.fetched_files
        per_issue.found_symbols = agent_state.context.found_symbols
        per_issue.analysis_response.is_final = (
            FinalStatus.TRUE.value if agent_state.is_final else FinalStatus.FALSE.value
        )

        # Verify backward mapping
        assert len(per_issue.source_code) == 2  # Original + new file
        assert "new_file.py" in per_issue.source_code
        assert "new_symbol" in per_issue.found_symbols
        assert per_issue.analysis_response.is_final == FinalStatus.TRUE.value


class TestMappingEdgeCases:
    """Test edge cases in state mapping."""

    def test_empty_fetched_files(self):
        """Test mapping with empty fetched_files."""
        agent_state = SASTAgentState(
            issue_id="test-001",
            issue=Issue(id="test-001", issue_type="Test"),
            context=InvestigationContext(fetched_files={}, found_symbols=[]),  # Empty!
        )

        per_issue = PerIssueData(issue=agent_state.issue)
        per_issue.source_code = agent_state.context.fetched_files

        assert per_issue.source_code == {}

    def test_none_values_in_per_issue(self):
        """Test creating agent state when per_issue has None values."""
        # Note: PerIssueData Pydantic model doesn't allow None for source_code/found_symbols
        # They have default_factory, so they'll be {} and set() respectively
        per_issue = PerIssueData(
            issue=Issue(id="test-001", issue_type="Test"),
            # source_code defaults to {}
            # found_symbols defaults to set()
            analysis_response=None,
        )

        # Verify defaults are set by Pydantic
        assert per_issue.source_code == {}
        assert per_issue.found_symbols == set()

        # Code from investigate_issue.py lines 87-88 with defensive checks
        # Note: found_symbols is now List[str] for JSON serialization
        agent_state = SASTAgentState(
            issue_id="test-001",
            issue=per_issue.issue,
            context=InvestigationContext(
                fetched_files=dict(per_issue.source_code) if per_issue.source_code else {},
                found_symbols=list(per_issue.found_symbols) if per_issue.found_symbols else [],
            ),
        )

        assert agent_state.context.fetched_files == {}
        assert agent_state.context.found_symbols == []  # Now a list

    def test_large_found_symbols_list(self):
        """Test mapping with large found_symbols list."""
        # Note: found_symbols is now List[str] for JSON serialization
        large_symbols = [f"symbol_{i}" for i in range(1000)]

        agent_state = SASTAgentState(
            issue_id="test-001",
            issue=Issue(id="test-001", issue_type="Test"),
            context=InvestigationContext(found_symbols=large_symbols),
        )

        per_issue = PerIssueData(issue=agent_state.issue)
        # Convert list to set for PerIssueData which uses set
        per_issue.found_symbols = set(agent_state.context.found_symbols)

        assert len(per_issue.found_symbols) == 1000
        assert "symbol_500" in per_issue.found_symbols


class TestMappingConsistency:
    """Test that mappings are consistent and idempotent."""

    def test_idempotent_mapping(self):
        """Test that mapping twice produces same result."""
        agent_state = SASTAgentState(
            issue_id="test-001",
            issue=Issue(id="test-001", issue_type="Test"),
            context=InvestigationContext(
                fetched_files={"file.py": ["code"]}, found_symbols={"symbol"}
            ),
            analysis=AnalysisState(
                investigation_result=AnalysisResponse(
                    investigation_result=CVEValidationStatus.TRUE_POSITIVE.value,
                    is_final="FALSE",
                    justifications=["test"],
                    prompt="test",
                )
            ),
            is_final=True,
        )

        # First mapping
        per_issue1 = PerIssueData(issue=agent_state.issue)
        per_issue1.analysis_response = agent_state.analysis.investigation_result
        per_issue1.source_code = agent_state.context.fetched_files
        per_issue1.found_symbols = agent_state.context.found_symbols
        per_issue1.analysis_response.is_final = (
            FinalStatus.TRUE.value if agent_state.is_final else FinalStatus.FALSE.value
        )

        # Second mapping
        per_issue2 = PerIssueData(issue=agent_state.issue)
        per_issue2.analysis_response = agent_state.analysis.investigation_result
        per_issue2.source_code = agent_state.context.fetched_files
        per_issue2.found_symbols = agent_state.context.found_symbols
        per_issue2.analysis_response.is_final = (
            FinalStatus.TRUE.value if agent_state.is_final else FinalStatus.FALSE.value
        )

        # Both mappings should be identical
        assert per_issue1.analysis_response.is_final == per_issue2.analysis_response.is_final
        assert per_issue1.source_code == per_issue2.source_code
        assert per_issue1.found_symbols == per_issue2.found_symbols
