"""
Common test utilities for SAST Agent Workflow tests.

This module provides reusable functions, mocks, and test data generators
to simplify and standardize testing across all workflow tools.
"""

import asyncio
from unittest.mock import Mock
from typing import Dict, List, Optional

from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus
from common.config import Config


class TestUtils:
    """Common utilities for testing SAST workflow tools."""
    
    @staticmethod
    async def run_single_fn(tool_function, config, builder, input_data):
        """
        Extract and run the core function from any SAST workflow tool.
        
        Args:
            tool_function: The main tool function (e.g., pre_process, filter)
            config: The tool's configuration object
            builder: The Builder mock object
            input_data: Input data to pass to the core function (SASTWorkflowTracker object)
            
        Returns:
            The result of executing the core function (SASTWorkflowTracker object)
        """
        async with tool_function(config, builder) as func_info:
            inner_fn = func_info.single_fn
            assert asyncio.iscoroutinefunction(inner_fn), "single_fn should be a coroutine function"
            result = await inner_fn(input_data)
            print("Result of single_fn: ", result)
            return result

    @staticmethod
    def create_sample_issues(count: int = 2) -> List[Issue]:
        """Create sample issues for testing."""
        issues = []
        for i in range(count):
            issue = Issue(
                id=f"def{i+1}",
                issue_type="OVERRUN" if i % 2 == 0 else "VARARGS",
                issue_label="",
                issue_cve="CWE-119" if i % 2 == 0 else "CWE-237",
                issue_cve_link=f"https://cwe.mitre.org/data/definitions/{"CWE-119" if i % 2 == 0 else "CWE-237"}.html",
                trace=f"sample trace {i+1}"
            )
            issues.append(issue)
        return issues

    @staticmethod
    def create_sample_issue(issue_id: str, issue_type: str = "BUFFER_OVERFLOW") -> Issue:
        """Create a single sample issue for testing."""
        return Issue(
            id=issue_id,
            issue_type=issue_type,
            issue_label="",
            issue_cve="CWE-119",
            issue_cve_link="https://cwe.mitre.org/data/definitions/CWE-119.html",
            trace=f"sample trace for {issue_id}"
        )

    @staticmethod
    def create_sample_per_issue_data_dict(issues: List[Issue], is_false_positive: str = CVEValidationStatus.TRUE_POSITIVE.value, 
                                          is_final: str = FinalStatus.FALSE.value, instructions: list = [], 
                                          justifications: list = None, short_justifications: str = None) -> Dict[str, PerIssueData]:
        """Create a dictionary of PerIssueData from issues."""
        issues_dict = {}
        for i, issue in enumerate(issues):
            per_issue_data = PerIssueData(
                issue=issue,
                source_code={f"file{i+1}.c": [f"int vulnerable_function_{i+1}():\n    //vulnerable code"]},
                similar_known_issues="Example-1: Known False Positive: \nExample Trace\nExample Reason Marked as False Positive\n\nExample-2: Known False Positive: \nExample Trace\nExample Reason Marked as False Positive\n",
                analysis_response=TestUtils.create_sample_analysis_response(
                    is_false_positive=is_false_positive, 
                    is_final=is_final, 
                    instructions=instructions,
                    justifications=justifications or ["Test justification"],
                    short_justifications=short_justifications if short_justifications is not None else "Short justification"
                ),
                found_symbols=set()
            )
            issues_dict[issue.id] = per_issue_data
        return issues_dict
    
    @staticmethod
    def create_sample_analysis_response(is_false_positive: str = CVEValidationStatus.TRUE_POSITIVE.value, is_final: str = FinalStatus.FALSE.value, 
                                        instructions: list = [], justifications: list = ["Test justification"], 
                                        short_justifications: str = "Short justification", 
                                        recommendations: list = ["Test recommendation"], evaluation: list = ["Test evaluation"], prompt: str = "Test prompt") -> AnalysisResponse:
        """Create a sample AnalysisResponse for testing with default values."""
        return AnalysisResponse(
            investigation_result=is_false_positive,
            is_final=is_final,
            justifications=justifications,
            short_justifications=short_justifications,
            recommendations=recommendations,
            instructions=instructions,
            evaluation=evaluation,
            prompt=prompt
        )

    @staticmethod
    def create_sample_tracker(issues: Optional[List[Issue]] = None, 
                            issues_dict: Optional[Dict[str, PerIssueData]] = None,
                            config: Optional[Config] = None,
                            iteration_count: int = 0,
                            metrics: Optional[Dict] = None) -> SASTWorkflowTracker:
        if issues_dict is not None:
            final_issues_dict = issues_dict
        else:
            if issues is None:
                issues = TestUtils.create_sample_issues()
            final_issues_dict = TestUtils.create_sample_per_issue_data_dict(issues)
        
        return SASTWorkflowTracker(
            issues=final_issues_dict,
            config=config or Mock(spec=Config),
            iteration_count=iteration_count,
            metrics=metrics or {}
        )

    @staticmethod
    def create_clean_filter_tracker(issues: Optional[List[Issue]] = None, 
                                  config: Optional[Mock] = None,
                                  iteration_count: int = 0) -> SASTWorkflowTracker:
        """ 
        Create a clean tracker that represents the state after pre_process but before filter execution.
        """
        if issues is None:
            issues = TestUtils.create_sample_issues(count=2)
        
        if config is None:
            config = Mock(spec=Config)
            config.USE_KNOWN_FALSE_POSITIVE_FILE = True
        
        issues_dict = {}
        for issue in issues:
            issues_dict[issue.id] = PerIssueData(
                issue=issue,
                similar_known_issues="",
                source_code={},
                analysis_response=AnalysisResponse(investigation_result=CVEValidationStatus.TRUE_POSITIVE.value, is_final=FinalStatus.FALSE.value)
            )
        
        return SASTWorkflowTracker(
            config=config,
            iteration_count=iteration_count,
            issues=issues_dict,
            metrics={}
        )
