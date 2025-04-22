from ragas import EvaluationDataset, evaluate
from ragas.metrics import ResponseRelevancy
from ragas.llms import LangchainLLMWrapper
from ragas.embeddings import LangchainEmbeddingsWrapper

from dto.MetricRequest import MetricRequest
from dto.ResponseStructures import FinalJudgeResponseWithSummary


class MetricHandler:
    def __init__(self, main_llm, embedding_llm):
        self.wrapped_evaluator_llm = LangchainLLMWrapper(main_llm)
        self.wrapped_evaluator_embeddings = LangchainEmbeddingsWrapper(embedding_llm)
        self.ragasMetricNameList = [ResponseRelevancy({})]

    def evaluate_datasets(self, metric_request):
        evaluation_dataset = EvaluationDataset.from_list([metric_request.__dict__])
        results = evaluate(
            dataset=evaluation_dataset,
            metrics=self.ragasMetricNameList,
            llm=self.wrapped_evaluator_llm,
            embeddings=self.wrapped_evaluator_embeddings,
            # run_config
            # batch_size
        )
        return results.scores[0]


def metric_request_from_prompt(prompt_txt, llm_response:FinalJudgeResponseWithSummary):
    retrieved_contexts_str_list = parse_context_from_prompt(prompt_txt)
    return MetricRequest(prompt_txt, llm_response, retrieved_contexts_str_list)

def parse_context_from_prompt(prompt_txt):
    after_context = prompt_txt[prompt_txt.index("Context:"):]
    context_str = after_context[:after_context.index("Human:")]
    s = context_str.split(':', 1)[1]
    # print(s)
    retrieved_contexts_str_list = [s]
    return retrieved_contexts_str_list