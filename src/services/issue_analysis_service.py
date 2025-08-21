"""
IssueAnalysisService - Responsible for issue analysis workflows.
Handles filtering known issues, analyzing issues, and generating recommendations.
"""

import os
import logging
from typing import Tuple, Union
from langchain_core.prompts import ChatPromptTemplate, SystemMessagePromptTemplate, HumanMessagePromptTemplate
from langchain_core.runnables import RunnablePassthrough, RunnableLambda
from langchain_core.language_models.chat_models import BaseChatModel
from langchain_community.vectorstores import FAISS
from tenacity import retry, stop_after_attempt, wait_fixed, retry_if_exception_type

from common.constants import FALLBACK_JUSTIFICATION_MESSAGE, RED_ERROR_FOR_LLM_REQUEST
from dto.Issue import Issue
from dto.ResponseStructures import FilterResponse, JudgeLLMResponse, JustificationsSummary, RecommendationsResponse, EvaluationResponse
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus, FinalStatus
from Utils.file_utils import read_answer_template_file
from Utils.llm_utils import robust_structured_output
from .vector_store_service import VectorStoreService

logger = logging.getLogger(__name__)


class IssueAnalysisService:
    """Service for analyzing issues and determining false positives"""
    
    def __init__(self, config, vector_service: VectorStoreService):
        self.config = config
        self.vector_service = vector_service
        self.max_retry_limit = 3
    
    def filter_known_issues_from_context(self, issue: Issue, similar_findings_context: str, main_llm: BaseChatModel) -> FilterResponse:
        """
        Check if an issue exactly matches a known false positive using provided context.
        
        Args:
            issue: The issue object with details like the error trace and issue ID
            similar_findings_context: The context containing similar findings
            main_llm: The main LLM for filtering
            
        Returns:
            response (FilterResponse): A structured response with the analysis result
        """
        logger.debug(f"[issue-ID - {issue.id}] Found This context:\n{similar_findings_context}")
        
        if not similar_findings_context:
            response = FilterResponse(
                equal_error_trace=[],
                justifications=(f"No identical error trace found in the provided context. "
                              f"The context empty because no issue of type {issue.issue_type} in known issue DB."),
                result="NO"
            )
            return response

        template_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), "templates", "known_issue_filter_resp.json")
        answer_template = read_answer_template_file(template_path)

        # Should not use 'system' for deepseek-r1
        prompt = ChatPromptTemplate.from_messages([
            ("system", self.config.FILTER_SYSTEM_PROMPT),
            ("user", self.config.FILTER_HUMAN_PROMPT)
        ])
        
        pattern_matching_prompt_chain = (
            {
                "context": RunnableLambda(lambda _: similar_findings_context),
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
        
        return response
    
    def analyze_issue(self, issue: Issue, context: str, main_llm: BaseChatModel, 
                     critique_llm: BaseChatModel = None) -> Tuple[AnalysisResponse, EvaluationResponse]:
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
            analysis_prompt, analysis_response = self._analyze_issue_with_retry(
                context=context, issue=issue, main_llm=main_llm
            )
            recommendations_response = self.recommend(
                issue=issue, context=context, analysis_response=analysis_response, main_llm=main_llm
            )
            short_justifications_response = self.summarize_justification(
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
                investigation_result=CVEValidationStatus.TRUE_POSITIVE.value if analysis_response is None else analysis_response.investigation_result,
                is_final=FinalStatus.TRUE.value if recommendations_response is None else recommendations_response.is_final,
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
    def _analyze_issue_with_retry(self, context: str, issue: Issue, main_llm: BaseChatModel):
        """Analyze an issue to determine if it is a false positive or not."""
        user_input = "Investigate if the following problem needs to be fixed or can be considered false positive. " + issue.trace
        
        # Should not use 'system' for deepseek-r1
        analysis_prompt = ChatPromptTemplate.from_messages([
            SystemMessagePromptTemplate.from_template(self.config.ANALYSIS_SYSTEM_PROMPT),
            HumanMessagePromptTemplate.from_template(self.config.ANALYSIS_HUMAN_PROMPT)
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
    def summarize_justification(self, actual_prompt, response: JudgeLLMResponse, 
                               issue_id: str, main_llm: BaseChatModel) -> JustificationsSummary:
        """Summarize the justifications into a concise, engineer-style comment."""
        examples_str = ('[{"short_justifications": "t is reassigned so previously freed value is replaced by malloced string"}, '
                       '{"short_justifications": "There is a check for k<0"}, '
                       '{"short_justifications": "i is between 1 and BMAX, line 1623 checks that j < i, array C is of the size BMAX+1"}, '
                       '{"short_justifications": "C is an array of size BMAX+1, i is between 1 and BMAX (inclusive)"}]')

        # Should not use 'system' for deepseek-r1
        prompt = ChatPromptTemplate.from_messages([
            ("system", self.config.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT),
            ("user", self.config.JUSTIFICATION_SUMMARY_HUMAN_PROMPT)
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
                function_name="summarize_justification", 
                issue_id=issue_id, 
                error=e
            ))
            raise e

        return short_justification

    @retry(stop=stop_after_attempt(2), wait=wait_fixed(10), retry=retry_if_exception_type(Exception))
    def recommend(self, issue: Issue, context: str, analysis_response: JudgeLLMResponse, 
                  main_llm: BaseChatModel) -> RecommendationsResponse:
        """Generate recommendations for further investigation, if necessary."""
        recommendations_prompt = ChatPromptTemplate.from_messages([
            HumanMessagePromptTemplate.from_template(self.config.RECOMMENDATIONS_PROMPT)
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
                function_name="recommend", 
                issue_id=issue.id, 
                error=e
            ))
            raise e

        return recommendations_response

    @retry(stop=stop_after_attempt(2), wait=wait_fixed(10), retry=retry_if_exception_type(Exception))
    def _evaluate(self, actual_prompt, response, issue_id, critique_llm: BaseChatModel) -> EvaluationResponse:
        """Evaluate an analysis using critique model."""
        prompt = ChatPromptTemplate.from_messages([
            ("user", self.config.EVALUATION_PROMPT)
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