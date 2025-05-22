import json
from enum import Enum
from dataclasses import dataclass, field, asdict


class CVEValidationStatus(Enum):
    TRUE_POSITIVE = "TRUE POSITIVE"
    FALSE_POSITIVE = " FALSE POSITIVE"


@dataclass
class AnalysisResponse:
    investigation_result: str
    is_final: str
    prompt: str = ''
    justifications: list[str] = field(default_factory=list)
    short_justifications: str = ''
    recommendations: list[str] = field(default_factory=list)
    instructions: list[dict] = field(default_factory=list)
    evaluation: list = field(default_factory=list)

    def is_true_positive(self) -> bool:
        return self.investigation_result == CVEValidationStatus.TRUE_POSITIVE.value

    def is_secound_analysis_needed(self):
        return self.is_final == 'FALSE' and self.instructions and self.is_true_positive()
    
    def to_dict(self):
        return asdict(self)

    def to_json(self, indent=None):
        data = asdict(self)
        if self.instructions:
            data['instructions'] = [instr.__dict__ for instr in self.instructions]
        return json.dumps(data, indent=indent)
    