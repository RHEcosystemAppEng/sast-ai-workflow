"""
Data models for SAST Agent Workflow.

Contains all data structures used by the SAST agent workflow including
the central tracker object and per-issue data models.
"""

from typing import Dict, List, Optional, Set
from pydantic import BaseModel, Field, ConfigDict

from common.config import Config
from dto.Issue import Issue
from dto.LLMResponse import AnalysisResponse


class PerIssueData(BaseModel):
    """Per-issue data object stored in the issues dictionary."""
    
    model_config = ConfigDict(arbitrary_types_allowed=True)
    
    issue: Issue = Field(description="The issue object")
    source_code: Dict[str, List[str]] = Field(default_factory=dict, description="Dictionary mapping file paths to lists of relevant code snippets")
    similar_known_issues: str = Field(default="", description="Raw text containing the N most similar known issues from the Vector DB")
    analysis_response: Optional[AnalysisResponse] = Field(default=None, description="AnalysisResponse object containing the core analysis state for a single issue")
    found_symbols: Set[str] = Field(default_factory=set, description="Set of symbols already found for this issue to avoid re-fetching")


class SASTWorkflowTracker(BaseModel):
    """Central tracker object that manages the entire batch of issues from a single SAST report and workflow configuration."""
    
    model_config = ConfigDict(arbitrary_types_allowed=True)
    
    # Workflow configuration and tracking
    config: Config|None = Field(default=None, description="Configuration object containing settings for the entire workflow")
    iteration_count: int = Field(default=0, description="Number of analysis cycles the entire report has been through")
    
    # Data and metrics
    issues: Dict[str, PerIssueData] = Field(default_factory=dict, description="Dictionary where key is issue_id and value is per-issue data")
    metrics: dict = Field(default_factory=dict, description="Dictionary to hold calculated metrics about the workflow run")
