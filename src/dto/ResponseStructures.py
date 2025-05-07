from pydantic import BaseModel, Field
from typing import List


class FilterResponse(BaseModel):
    """
    Structured response for filtering known issues based on error traces.
    """

    issue_id: str = Field(description="Unique ID of the issue.")
    equal_error_trace: List[str] = Field(description="Matching error trace lines.")
    justifications: str = Field(description="Reasons for classification.")
    result: str = Field(description="'YES' if it matches a known false positive, otherwise 'NO'.")


class JudgeLLMResponse(BaseModel):
    """
    Structured response for analyzing and classifying issues as false positive or not a false positive.
    """

    investigation_result: str = Field(
        description="The result of the investigation. Possible values are 'FALSE POSITIVE' or 'NOT A FALSE POSITIVE'."
    )
    justifications: List[str] = Field(
        description="A list of reasons explaining the conclusion of the investigation."
    )
    recommendations: List[str] = Field(
        description="A list of recommended actions based on the investigation result."
    )


class JustificationsSummary(BaseModel):
    
        short_justifications: str = Field(
        description="A clear, concise summary of the justification written in an engineer-style tone, highlighting the most impactful point."
    )

class JudgeLLMResponseWithSummary(JustificationsSummary, JudgeLLMResponse):
    """
    This model extends `JudgeLLMResponse` by including a `short_justifications` field, 
    which provides a clear and concise summary of the justifications in an engineer-style tone.
    """
   