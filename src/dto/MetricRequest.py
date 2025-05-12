from dto.ResponseStructures import JudgeLLMResponseWithSummary


class MetricRequest:
    def __init__(self, user_input, response_obj:JudgeLLMResponseWithSummary, retrieved_contexts):
        self.user_input = user_input
        self.response = response_obj.model_dump_json()
        self.retrieved_contexts = retrieved_contexts