from pydantic import BaseModel, Field
from typing import List, Literal

class FilterResponse(BaseModel):
    """
    Structured response for filtering known issues based on error traces.
    """

    equal_error_trace: List[str] = Field(description="Matching error trace lines.If there are no matching lines, return an empty list [].")
    justifications: str = Field(description="Reasons for classification.")
    result: Literal["YES", "NO"] = Field(description="'YES' if it matches a known false positive, otherwise 'NO'.")

class JudgeLLMResponse(BaseModel):
    """
    Structured response for analyzing and classifying issues as false positive or not a false positive.
    """

    investigation_result: str = Field(
        description="The result of the investigation. Possible values are 'FALSE POSITIVE' or 'TRUE POSITIVE'."
    )
    justifications: List[str] = Field(
        description="The reasoning that led to the investigation_result decision."
    )


class InstructionResponse(BaseModel):
    expression_name: str = Field("The exact name of the function or macro that requires further inspection.")
    reffering_source_code_path: str = Field("The file path of the source code file where the 'expression_name' is called or used.")
    recommendation: str = Field("TA clear, concise, and actionable instruction specifying the exact aspect of the 'expression_name's implementation needs to be examined and the specific reason for this examination in the context of the reported CVE")


class RecommendationsResponse(BaseModel):
    is_final: str = Field(
        description="An indicator if the model response is final or additional query is needed. Possible values TRUE/FALSE"
    )
    justifications: List[str] = Field(
        description="A list of reasons explaining the conclusion of the investigation."
    )
    recommendations: List[str] = Field(
        description="A list of recommended actions based on the investigation result."
    )
    instructions: List[InstructionResponse] = Field('list of InstructionResponse instances, each instruction represents an expression to retrieve from the source code')


class JustificationsSummary(BaseModel):
    
        short_justifications: str = Field(
        description="A clear, concise summary of the justification written in an engineer-style tone, highlighting the most impactful point."
    )

class JudgeLLMResponseWithSummary(JustificationsSummary, JudgeLLMResponse):
    """
    This model extends `JudgeLLMResponse` by including a `short_justifications` field, 
    which provides a clear and concise summary of the justifications in an engineer-style tone.
    """
       

class EvaluationResponse(BaseModel):
    """
    Structured response for analyzing and classifying issues as false positive or not a false positive.
    """

    critique_result: str = Field(
        description="The result of the investigation. Possible values are 'FALSE POSITIVE' or 'NOT A FALSE POSITIVE'."
        )
    justifications: List[str] = Field(
        description="A list of reasons explaining the conclusion of the investigation."
        )

