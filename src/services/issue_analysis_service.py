"""
IssueAnalysisService - Responsible for issue analysis workflows.
Handles filtering known issues, analyzing issues, and generating recommendations.
"""

import os
import logging
from typing import Tuple
from langchain_core.prompts import ChatPromptTemplate, SystemMessagePromptTemplate, HumanMessagePromptTemplate
from langchain_core.runnables import RunnablePassthrough, RunnableLambda
from langchain_core.language_models import BaseLLM
from langchain_community.vectorstores import FAISS
from tenacity import retry, stop_after_attempt, wait_fixed, retry_if_exception_type

from common.constants import FALLBACK_JUSTIFICATION_MESSAGE, RED_ERROR_FOR_LLM_REQUEST
from dto.Issue import Issue
from dto.ResponseStructures import FilterResponse, JudgeLLMResponse, JustificationsSummary, RecommendationsResponse, EvaluationResponse
from dto.LLMResponse import AnalysisResponse
from Utils.file_utils import read_answer_template_file
from Utils.llm_utils import robust_structured_output
from .vector_store_service import VectorStoreService

logger = logging.getLogger(__name__)


def _format_context_from_response(resp):
    """Helper function to format context from vector search response"""
    context_list = []
    for doc in resp:
        context_list.append({
            "false_positive_error_trace": doc.page_content,
            "reason_marked_false_positive": doc.metadata['reason_of_false_positive']
        })
    return context_list


class IssueAnalysisService:
    """Service for analyzing issues and determining false positives"""
    
    def __init__(self, config, vector_service: VectorStoreService):
        self.config = config
        self.vector_service = vector_service
        self.max_retry_limit = 3
        
        # Store prompt templates from config
        self.analysis_system_prompt = config.ANALYSIS_SYSTEM_PROMPT
        self.analysis_human_prompt = config.ANALYSIS_HUMAN_PROMPT
        self.filter_system_prompt = config.FILTER_SYSTEM_PROMPT
        self.filter_human_prompt = config.FILTER_HUMAN_PROMPT
        self.recommendations_prompt = config.RECOMMENDATIONS_PROMPT
        self.justification_summary_system_prompt = config.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT
        self.justification_summary_human_prompt = config.JUSTIFICATION_SUMMARY_HUMAN_PROMPT
        self.evaluation_prompt = config.EVALUATION_PROMPT
        self.similarity_error_threshold = config.SIMILARITY_ERROR_THRESHOLD
    
    def filter_known_issues(self, issue: Issue, vector_store: FAISS, main_llm: BaseLLM) -> Tuple[FilterResponse, str]:
        """
        Check if an issue exactly matches a known false positive.
        
        Args:
            issue: The issue object with details like the error trace and issue ID
            vector_store: The vector database of known false positives
            main_llm: The main LLM for filtering
            
        Returns:
            tuple: A tuple containing:
            - response (FilterResponse): A structured response with the analysis result
            - examples_context_str: Most similar known issues of the same type
        """
        resp = self.vector_service.similarity_search(
            vector_store=vector_store,
            query=issue.trace,
            k=self.similarity_error_threshold,
            filter_criteria={'issue_type': issue.issue_type}
        )
        
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

        template_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), "templates", "known_issue_filter_resp.json")
        answer_template = read_answer_template_file(template_path)

        prompt = ChatPromptTemplate.from_messages([
            ("system", self.filter_system_prompt),
            ("user", self.filter_human_prompt)
        ])
        
        pattern_matching_prompt_chain = (
            {
                "context": RunnableLambda(lambda _: examples_context_str),
                "answer_template": RunnableLambda(lambda _: answer_template),
                "user_error_trace": RunnablePassthrough()
            }
            | prompt
        )
        
        actual_prompt = pattern_matching_prompt_chain.invoke(issue.trace)
        logger.debug(f"\n\n\nFiltering prompt:\n{actual_prompt.to_string()}")
        
        try:
            response = robust_structured_output(
                llm=main_llm,
                schema=FilterResponse,
                input=issue.trace,
                prompt_chain=pattern_matching_prompt_chain,
                max_retries=self.max_retry_limit
            )
        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(
                max_retry_limit=self.max_retry_limit, 
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
    
    def analyze_issue(self, issue: Issue, context: str, main_llm: BaseLLM, 
                     critique_llm: BaseLLM = None) -> Tuple[AnalysisResponse, EvaluationResponse]:
        """
        Analyze an issue to determine if it is a false positive or not.
        
        Args:
            issue: The issue object with details
            context: The context to assist in the analysis
            main_llm: The main LLM for analysis
            critique_llm: Optional critique LLM for evaluation
            
        Returns:
            tuple: (llm_analysis_response, critique_response)
        """
        analysis_prompt = analysis_response = recommendations_response = short_justifications_response = None

        try:
            analysis_prompt, analysis_response = self._investigate_issue_with_retry(
                context=context, issue=issue, main_llm=main_llm
            )
            recommendations_response = self._recommend(
                issue=issue, context=context, analysis_response=analysis_response, main_llm=main_llm
            )
            short_justifications_response = self._summarize_justification(
                analysis_prompt.to_string(), analysis_response, issue.id, main_llm
            )

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
            critique_response = self._evaluate(
                analysis_prompt.to_string(), llm_analysis_response, issue.id, critique_llm
            ) if self.config.RUN_WITH_CRITIQUE and analysis_response is not None and critique_llm is not None else ""
        except Exception as e:
            logger.error(f"Failed during evaluation process, set default values. Error is: {e}")
            critique_response = EvaluationResponse(
                critique_result=analysis_response.investigation_result,
                justifications=["Failed during evaluation process. Defaulting to first analysis_response"]
            )

        return llm_analysis_response, critique_response

    @retry(stop=stop_after_attempt(2), wait=wait_fixed(10), retry=retry_if_exception_type(Exception))
    def _investigate_issue_with_retry(self, context: str, issue: Issue, main_llm: BaseLLM):
        """Analyze an issue to determine if it is a false positive or not."""
        user_input = "Investigate if the following problem needs to be fixed or can be considered false positive. " + issue.trace
        
        analysis_prompt = ChatPromptTemplate.from_messages([
            SystemMessagePromptTemplate.from_template(self.analysis_system_prompt),
            HumanMessagePromptTemplate.from_template(self.analysis_human_prompt)
        ])

        analysis_prompt_chain = (
            {
                "context": RunnableLambda(lambda _: context),
                "cve_error_trace": RunnableLambda(lambda _: issue.trace),
                "question": RunnablePassthrough()
            }
            | analysis_prompt
        )
        
        actual_prompt = analysis_prompt_chain.invoke(user_input)
        logger.debug(f"Analysis prompt: {actual_prompt.to_string()}")

        try:
            analysis_response = robust_structured_output(
                llm=main_llm,
                schema=JudgeLLMResponse,
                input=user_input,
                prompt_chain=analysis_prompt_chain,
                max_retries=self.max_retry_limit
            )
        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(
                max_retry_limit=self.max_retry_limit, 
                function_name="_analyze", 
                issue_id=issue.id, 
                error=e
            ))
            raise e

        logger.debug(f"{analysis_response=}")
        return actual_prompt, analysis_response

    @retry(stop=stop_after_attempt(2), wait=wait_fixed(10), retry=retry_if_exception_type(Exception))
    def _summarize_justification(self, actual_prompt, response: JudgeLLMResponse, 
                                issue_id: str, main_llm: BaseLLM) -> JustificationsSummary:
        """Summarize the justifications into a concise, engineer-style comment."""
        examples_str = ('[{"short_justifications": "t is reassigned so previously freed value is replaced by malloced string"}, '
                       '{"short_justifications": "There is a check for k<0"}, '
                       '{"short_justifications": "i is between 1 and BMAX, line 1623 checks that j < i, array C is of the size BMAX+1"}, '
                       '{"short_justifications": "C is an array of size BMAX+1, i is between 1 and BMAX (inclusive)"}]')

        prompt = ChatPromptTemplate.from_messages([
            ("system", self.justification_summary_system_prompt),
            ("user", self.justification_summary_human_prompt)
        ])

        justification_summary_prompt_chain = (
            {
                "actual_prompt": RunnableLambda(lambda _: actual_prompt),
                "examples_str": RunnableLambda(lambda _: examples_str),
                "response": RunnablePassthrough()
            }
            | prompt
        )

        try:
            short_justification = robust_structured_output(
                llm=main_llm,
                schema=JustificationsSummary,
                input=response,
                prompt_chain=justification_summary_prompt_chain,
                max_retries=self.max_retry_limit
            )
        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(
                max_retry_limit=self.max_retry_limit, 
                function_name="_summarize_justification", 
                issue_id=issue_id, 
                error=e
            ))
            raise e

        return short_justification

    @retry(stop=stop_after_attempt(2), wait=wait_fixed(10), retry=retry_if_exception_type(Exception))
    def _recommend(self, issue: Issue, context: str, analysis_response: JudgeLLMResponse, 
                  main_llm: BaseLLM) -> RecommendationsResponse:
        """Generate recommendations for further investigation, if necessary."""
        recommendations_prompt = ChatPromptTemplate.from_messages([
            HumanMessagePromptTemplate.from_template(self.recommendations_prompt)
        ])

        try:
            recommendation_prompt_chain = (
                {
                    "cve_error_trace": RunnableLambda(lambda _: issue.trace),
                    "analysis": RunnableLambda(lambda _: analysis_response.justifications),
                    "context": RunnableLambda(lambda _: context),
                }
                | recommendations_prompt
            )
            
            recommendations_response = robust_structured_output(
                llm=main_llm,
                schema=RecommendationsResponse,
                input={},
                prompt_chain=recommendation_prompt_chain,
                max_retries=self.max_retry_limit
            )
            logger.debug(f"recommendations_response: {recommendations_response=}")

        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(
                max_retry_limit=self.max_retry_limit, 
                function_name="_recommend", 
                issue_id=issue.id, 
                error=e
            ))
            raise e

        return recommendations_response

    @retry(stop=stop_after_attempt(2), wait=wait_fixed(10), retry=retry_if_exception_type(Exception))
    def _evaluate(self, actual_prompt, response, issue_id, critique_llm: BaseLLM) -> EvaluationResponse:
        """Evaluate an analysis using critique model."""
        prompt = ChatPromptTemplate.from_messages([
            ("user", self.evaluation_prompt)
        ])

        evaluation_prompt_chain = (
            {
                "actual_prompt": RunnableLambda(lambda _: actual_prompt),
                "response": RunnablePassthrough()
            }
            | prompt
        )
        
        try:
            critique_response = robust_structured_output(
                llm=critique_llm,
                schema=EvaluationResponse,
                input=response,
                prompt_chain=evaluation_prompt_chain,
                max_retries=self.max_retry_limit
            )
            logger.debug(f"{critique_response=}")

        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(
                max_retry_limit=self.max_retry_limit, 
                function_name="_evaluate", 
                issue_id=issue_id, 
                error=e
            ))
            raise e

        return critique_response 