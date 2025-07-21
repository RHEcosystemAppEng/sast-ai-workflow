import logging
from typing import Tuple, List

from dto.Issue import Issue
from dto.ResponseStructures import FilterResponse, JudgeLLMResponse, JustificationsSummary, RecommendationsResponse, EvaluationResponse
from dto.LLMResponse import AnalysisResponse
from common.constants import FALLBACK_JUSTIFICATION_MESSAGE, RED_ERROR_FOR_LLM_REQUEST
from .protocols import LLMClientFactoryProtocol, VectorStoreServiceProtocol, PromptServiceProtocol, StructuredOutputServiceProtocol

logger = logging.getLogger(__name__)


def _format_context_from_response(resp):
    """Format context from vector store response."""
    context_list = []
    for doc in resp:
        context_list.append({
            "false_positive_error_trace": doc.page_content,
            "reason_marked_false_positive": doc.metadata['reason_of_false_positive']
        })
    return context_list


class IssueAnalysisService:
    """Main service for issue analysis workflows."""
    
    def __init__(
        self,
        llm_client_factory: LLMClientFactoryProtocol,
        vector_store_service: VectorStoreServiceProtocol,
        prompt_service: PromptServiceProtocol,
        structured_output_service: StructuredOutputServiceProtocol,
        known_issues_vector_db,
        similarity_error_threshold: int,
        run_with_critique: bool,
        config
    ):
        self.llm_client_factory = llm_client_factory
        self.vector_store_service = vector_store_service
        self.prompt_service = prompt_service
        self.structured_output_service = structured_output_service
        self.known_issues_vector_db = known_issues_vector_db
        self.similarity_error_threshold = similarity_error_threshold
        self.run_with_critique = run_with_critique
        self.config = config
    
    def filter_known_error(self, issue: Issue) -> Tuple[FilterResponse, str]:
        """
        Check if an issue exactly matches a known false positive.
        
        Args:
            issue: The issue object with details like the error trace and issue ID.

        Returns:
            tuple: A tuple containing:
            - response (FilterResponse): A structured response with the analysis result.
            - examples_context_str (str): N (N=SIMILARITY_ERROR_THRESHOLD) most similar known issues of the same type of the query issue.
        """
        retriever = self.known_issues_vector_db.as_retriever(
            search_kwargs={
                "k": self.similarity_error_threshold,
                'filter': {'issue_type': issue.issue_type}
            }
        )
        resp = retriever.invoke(issue.trace)
        examples_context_str = _format_context_from_response(resp)
        
        logger.debug(f"[issue-ID - {issue.id}] Found This context:\n{examples_context_str}")
        
        if not examples_context_str:
            response = FilterResponse(
                equal_error_trace=[],
                justifications=(f"No identical error trace found in the provided context. "
                               f"The context empty because no issue of type {issue.issue_type} in known issue DB."),
                result="NO"
            )
            return response, examples_context_str

        answer_template = self.prompt_service.get_filter_answer_template()
        pattern_matching_prompt_chain = self.prompt_service.create_filter_prompt_chain(
            examples_context_str, answer_template
        )
        
        actual_prompt = pattern_matching_prompt_chain.invoke(issue.trace)
        logger.debug(f"\n\n\nFiltering prompt:\n{actual_prompt.to_string()}")
        
        try:
            main_llm = self.llm_client_factory.create_main_llm(self.config)
            response = self.structured_output_service.parse_with_retry(
                llm=main_llm,
                schema=FilterResponse,
                input_data=issue.trace,
                prompt_chain=pattern_matching_prompt_chain
            )
        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(
                max_retry_limit=3, 
                function_name="filter_known_error", 
                issue_id=issue.id, 
                error=e
            ))
            response = FilterResponse(
                equal_error_trace=[],
                justifications="An error occurred twice during model output parsing. Defaulting to: NO",
                result="NO"
            )
        
        return response, examples_context_str
    
    def investigate_issue(self, context: str, issue: Issue) -> Tuple[AnalysisResponse, EvaluationResponse]:
        """
        Analyze an issue to determine if it is a false positive or not.

        Args:
            context: The context to assist in the analysis.
            issue: The issue object with details like the error trace and issue ID.

        Returns:
            tuple: A tuple containing:
                - llm_analysis_response (AnalysisResponse): A structured response with the analysis result.
                - critique_response (EvaluationResponse): The response of the critique model, if applicable.
        """
        analysis_prompt = analysis_response = recommendations_response = short_justifications_response = None

        try:
            analysis_prompt, analysis_response = self._investigate_issue_with_retry(context=context, issue=issue)
            recommendations_response = self._recommend(issue=issue, context=context, analysis_response=analysis_response)
            short_justifications_response = self._summarize_justification(analysis_prompt.to_string(), analysis_response, issue.id)

            llm_analysis_response = AnalysisResponse(
                investigation_result=analysis_response.investigation_result,
                is_final=recommendations_response.is_final,
                justifications=analysis_response.justifications,
                evaluation=recommendations_response.justifications,
                recommendations=recommendations_response.recommendations,
                instructions=recommendations_response.instructions,
                prompt=analysis_prompt.to_string(),
                short_justifications=short_justifications_response.short_justifications
            )
        except Exception as e:
            failed_message = "Failed during analyze process"
            logger.error(f"{failed_message}, set default values for the fields it failed on. Error is: {e}")
            llm_analysis_response = AnalysisResponse(
                investigation_result="NOT A FALSE POSITIVE" if analysis_response is None else analysis_response.investigation_result,
                is_final="TRUE" if recommendations_response is None else recommendations_response.is_final,
                justifications=FALLBACK_JUSTIFICATION_MESSAGE if analysis_response is None else analysis_response.justifications,
                evaluation=[failed_message] if recommendations_response is None else recommendations_response.justifications,
                recommendations=[failed_message] if recommendations_response is None else recommendations_response.recommendations,
                instructions=[] if recommendations_response is None else recommendations_response.instructions,
                prompt=failed_message if analysis_prompt is None else analysis_prompt.to_string(),
                short_justifications=f"{failed_message}. Please check the full justifications."
                                   if short_justifications_response is None
                                   else short_justifications_response.short_justifications
            )

        try:
            critique_response = self._evaluate(analysis_prompt.to_string(), llm_analysis_response, issue.id) if self.run_with_critique and analysis_response is not None else ""
        except Exception as e:
            logger.error(f"Failed during evaluation process, set default values. Error is: {e}")
            critique_response = EvaluationResponse(
                critique_result=analysis_response.investigation_result,
                justifications=["Failed during evaluation process. Defaulting to first analysis_response"]
            )

        return llm_analysis_response, critique_response
    
    def _investigate_issue_with_retry(self, context: str, issue: Issue):
        """
        Analyze an issue to determine if it is a false positive or not.

        Args:
            context: The context to assist in the analysis.
            issue: The issue object with details like the error trace and issue ID.

        Returns:
            tuple: A tuple containing:
                - actual_prompt (str): The prompt sent to the model.
                - response (JudgeLLMResponse): A structured response with the analysis result.
        """
        user_input = "Investigate if the following problem needs to be fixed or can be considered false positive. " + issue.trace
        
        analysis_prompt_chain = self.prompt_service.create_analysis_prompt_chain(context, issue.trace)
        actual_prompt = analysis_prompt_chain.invoke(user_input)
        logger.debug(f"Analysis prompt: {actual_prompt.to_string()}")

        try:
            main_llm = self.llm_client_factory.create_main_llm(self.config)
            analysis_response = self.structured_output_service.parse_with_retry(
                llm=main_llm,
                schema=JudgeLLMResponse,
                input_data=user_input,
                prompt_chain=analysis_prompt_chain
            )
        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(
                max_retry_limit=3,
                function_name="_analyze",
                issue_id=issue.id,
                error=e
            ))
            raise e

        logger.debug(f"{analysis_response=}")
        return actual_prompt, analysis_response
    
    def _summarize_justification(self, actual_prompt: str, response: JudgeLLMResponse, issue_id: str) -> JustificationsSummary:
        """
        Summarize the justifications into a concise, engineer-style comment.

        Args:
            actual_prompt (str): The query prompt sent to the LLM, including the context.
            response (JudgeLLMResponse): A structured response with the analysis result.

        Returns:
            response (JustificationsSummary): A structured response with summary of the justifications.
        """
        examples_str = self.prompt_service.get_justification_examples()
        justification_summary_prompt_chain = self.prompt_service.create_justification_summary_prompt_chain(
            actual_prompt, examples_str
        )

        try:
            main_llm = self.llm_client_factory.create_main_llm(self.config)
            short_justification = self.structured_output_service.parse_with_retry(
                llm=main_llm,
                schema=JustificationsSummary,
                input_data=response,
                prompt_chain=justification_summary_prompt_chain
            )
        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(
                max_retry_limit=3,
                function_name="_summarize_justification",
                issue_id=issue_id,
                error=e
            ))
            raise e

        return short_justification
    
    def _recommend(self, issue: Issue, context: str, analysis_response: JudgeLLMResponse) -> RecommendationsResponse:
        """
        Evaluates a given CVE analysis and generates recommendations for further investigation, if necessary.

        Args:
            issue (Issue): An object representing the reported CVE.
            context (str): The data used for the CVE analysis.
            analysis_response (JudgeLLMResponse): An object containing the analysis of the CVE.

        Returns:
            recommendations_response (RecommendationsResponse): An object containing the LM's evaluation and recommendations.
        """
        recommendation_prompt_chain = self.prompt_service.create_recommendations_prompt_chain(
            issue.trace, analysis_response.justifications, context
        )
        
        try:
            main_llm = self.llm_client_factory.create_main_llm(self.config)
            recommendations_response = self.structured_output_service.parse_with_retry(
                llm=main_llm,
                schema=RecommendationsResponse,
                input_data={},
                prompt_chain=recommendation_prompt_chain
            )
            logger.debug(f"recommendations_response: {recommendations_response=}")

        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(
                max_retry_limit=3,
                function_name="_recommend",
                issue_id=issue.id,
                error=e
            ))
            raise e

        return recommendations_response
    
    def _evaluate(self, actual_prompt: str, response, issue_id: str) -> EvaluationResponse:
        """Evaluate the analysis using the critique model."""
        evaluation_prompt_chain = self.prompt_service.create_evaluation_prompt_chain(actual_prompt)
        
        try:
            main_llm = self.llm_client_factory.create_main_llm(self.config)
            critique_response = self.structured_output_service.parse_with_retry(
                llm=main_llm,
                schema=EvaluationResponse,
                input_data=response,
                prompt_chain=evaluation_prompt_chain
            )
            logger.debug(f"{critique_response=}")

        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(
                max_retry_limit=3,
                function_name="_evaluate",
                issue_id=issue_id,
                error=e
            ))
            raise e

        return critique_response 