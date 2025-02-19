class MetricRequest:
    def __init__(self, user_input, response_prompt, retrieved_contexts):
        self.user_input = user_input
        self.response = response_prompt
        self.retrieved_contexts = retrieved_contexts