"""
Data models for SAST Agent Workflow.

Contains all data structures used by the SAST agent workflow including
the central tracker object and per-issue data models.
"""

from typing import Dict, Optional
from pydantic import BaseModel, Field, ConfigDict

from common.config import Config
from dto.LLMResponse import AnalysisResponse


class PerIssueData(BaseModel):
    """Per-issue data object stored in the issues dictionary."""
    
    model_config = ConfigDict(arbitrary_types_allowed=True)
    
    original_report_data: str = Field(default="", description="The raw data for the issue from the initial report")
    source_code: dict = Field(default_factory=dict, description="Dictionary mapping file paths to relevant code snippets")
    similar_known_issues: str = Field(default="", description="Raw text containing the N most similar known issues from the Vector DB")
    analysis_response: Optional[AnalysisResponse] = Field(default=None, description="AnalysisResponse object containing the core analysis state for a single issue")


class SASTWorkflowTracker(BaseModel):
    """Central tracker object that manages the entire batch of issues from a single SAST report and workflow configuration."""
    
    model_config = ConfigDict(arbitrary_types_allowed=True)
    
    # Workflow configuration and tracking
    config: Config = Field(description="Configuration object containing settings for the entire workflow")
    max_iterations: int = Field(default=5, description="Maximum number of analysis loops allowed for any single issue")
    iteration_count: int = Field(default=0, description="Number of analysis cycles the entire report has been through")
    
    # Batch data
    issues: Dict[str, PerIssueData] = Field(default_factory=dict, description="Dictionary where key is issue_id and value is per-issue data")
    metrics: dict = Field(default_factory=dict, description="Dictionary to hold calculated metrics about the workflow run") 