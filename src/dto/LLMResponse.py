import json
from dataclasses import asdict, dataclass, field
from enum import Enum

from dto.ResponseStructures import InstructionResponse


class CVEValidationStatus(Enum):
    TRUE_POSITIVE = "TRUE POSITIVE"
    FALSE_POSITIVE = "FALSE POSITIVE"
    NEEDS_REVIEW = "NEEDS REVIEW"


class FinalStatus(Enum):
    TRUE = "TRUE"
    FALSE = "FALSE"


@dataclass
class AnalysisResponse:
    investigation_result: str
    is_final: str
    prompt: str = ""
    justifications: list[str] = field(default_factory=list)
    short_justifications: str = ""
    recommendations: list[str] = field(default_factory=list)
    instructions: list[InstructionResponse] = field(default_factory=list)
    evaluation: list = field(default_factory=list)
    filter_confidence: float | None = None
    faiss_similarity_score: float | None = None

    def is_true_positive(self) -> bool:
        return self.investigation_result in (
            CVEValidationStatus.TRUE_POSITIVE.value,
            CVEValidationStatus.NEEDS_REVIEW.value,
        )

    def is_second_analysis_needed(self):
        return (
            self.is_final == FinalStatus.FALSE.value
            and self.instructions
            and self.is_true_positive()
        )

    def to_dict(self):
        return asdict(self)

    def to_json(self, indent=None):
        data = asdict(self)
        if self.instructions:
            data["instructions"] = [instr.__dict__ for instr in self.instructions]
        return json.dumps(data, indent=indent)
