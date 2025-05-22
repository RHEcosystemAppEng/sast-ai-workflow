from dto.LLMResponse import AnalysisResponse


class SummaryInfo:
    def __init__(self, response:AnalysisResponse, metrics, critique_response, context):
        self.llm_response = response
        self.metrics = metrics
        self.critique_response = critique_response
        self.context = context  # context used only for debug and analyze