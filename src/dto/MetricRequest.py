from dto.LLMResponse import AnalysisResponse


class MetricRequest:
    def __init__(self, user_input, response_obj: AnalysisResponse, retrieved_contexts):
        self.user_input = user_input
        self.response = response_obj.to_json()
        self.retrieved_contexts = retrieved_contexts
