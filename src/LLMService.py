import os
import faiss
import httpx
import logging
import re

from langchain_openai import OpenAIEmbeddings
from langchain_openai.chat_models.base import ChatOpenAI
from langchain_community.vectorstores import FAISS
from langchain_community.docstore.in_memory import InMemoryDocstore
from langchain_nvidia_ai_endpoints import ChatNVIDIA
from langchain_core.prompts import ChatPromptTemplate, SystemMessagePromptTemplate, HumanMessagePromptTemplate
from langchain_core.runnables import RunnablePassthrough, RunnableLambda

from Utils.llm_utils import robust_structured_output
from Utils.file_utils import read_answer_template_file
from Utils.embedding_utils import check_text_size_before_embedding
from common.config import Config
from common.constants import FALLBACK_JUSTIFICATION_MESSAGE, RED_ERROR_FOR_LLM_REQUEST
from dto.Issue import Issue
from dto.ResponseStructures import FilterResponse, JudgeLLMResponse, JustificationsSummary, RecommendationsResponse, EvaluationResponse
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from tenacity import retry, stop_after_attempt, wait_fixed, retry_if_exception_type

logger = logging.getLogger(__name__)

def _format_context_from_response(resp):
    context_list = []
    for doc in resp:
        context_list.append({"false_positive_error_trace":doc.page_content,
                             "reason_marked_false_positive":doc.metadata['reason_of_false_positive']
                             })
    return context_list


class LLMService:

    def __init__(self, config:Config):
        self.llm_url = config.LLM_URL
        self.llm_api_key = config.LLM_API_KEY
        self.llm_model_name = config.LLM_MODEL_NAME
        self.embedding_llm_url = config.EMBEDDINGS_LLM_URL
        self.embedding_api_key = config.EMBEDDINGS_LLM_API_KEY
        self.embedding_llm_model_name = config.EMBEDDINGS_LLM_MODEL_NAME

        self._main_llm = None
        self._embedding_llm = None
        self.vector_db = None
        self.known_issues_vector_db = None
        self.similarity_error_threshold = config.SIMILARITY_ERROR_THRESHOLD
        self.run_with_critique = config.RUN_WITH_CRITIQUE
        self._critique_llm = None
        self._critique_llm_model_name = config.CRITIQUE_LLM_MODEL_NAME
        self._critique_base_url = config.CRITIQUE_LLM_URL
        self.critique_api_key = getattr(config, "CRITIQUE_LLM_API_KEY", None)

        # Store prompt templates from config
        self.analysis_system_prompt = config.ANALYSIS_SYSTEM_PROMPT
        self.analysis_human_prompt = config.ANALYSIS_HUMAN_PROMPT
        self.filter_system_prompt = config.FILTER_SYSTEM_PROMPT
        self.filter_human_prompt = config.FILTER_HUMAN_PROMPT
        self.recommendations_prompt = config.RECOMMENDATIONS_PROMPT
        self.justification_summary_system_prompt = config.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT
        self.justification_summary_human_prompt = config.JUSTIFICATION_SUMMARY_HUMAN_PROMPT
        self.evaluation_prompt = config.EVALUATION_PROMPT

        # Initialize failure counters
        self.filter_retry_counter = 0
        self.judge_retry_counter = 0
        self.max_retry_limit = 3


    @property
    def main_llm(self):
        if self._main_llm is None:
            main_llm_http_client = httpx.Client(verify=False) # If self.llm_url also needs it

            # Decide which LLM to use based on the base_url
            if "nvidia" in self.llm_url.lower():
                self._main_llm = ChatNVIDIA(
                    base_url=self.llm_url,
                    model=self.llm_model_name,
                    api_key=self.llm_api_key,
                    temperature=0,
                    # http_client=main_llm_http_client, # if ChatNVIDIA supports it and if needed
                )
            else:
                self._main_llm = ChatOpenAI(
                    base_url=self.llm_url,
                    model=self.llm_model_name,
                    api_key=self.llm_api_key,
                    temperature=0,
                    http_client=main_llm_http_client, # Pass client if ChatOpenAI supports it and if needed
                    # top_p=0.01  # Todo: Try a different top_p, 0.01 gave bad results. Right now we're using the default (1.0) for ChatNVIDIA & ChatOpenAI, which is better, but maybe not the best.
                )
        return self._main_llm

    @property
    def embedding_llm(self):
        if self._embedding_llm is None:
            # Create a custom httpx client with SSL verification disabled
            custom_embedding_http_client = httpx.Client(verify=False) # <--- DISABLES SSL VERIFICATION

            self._embedding_llm = OpenAIEmbeddings(
                openai_api_base=self.embedding_llm_url,
                openai_api_key=self.embedding_api_key,
                model=self.embedding_llm_model_name,
                tiktoken_enabled=False,
                show_progress_bar=True,
                http_client=custom_embedding_http_client # <--- CUSTOM CLIENT
            )
        return self._embedding_llm

    @property
    def critique_llm(self):
        if self._critique_llm is None:
            critique_llm_http_client = httpx.Client(verify=False) # If self._critique_base_url also needs it

            # Decide which LLM to use based on the base_url
            if "nvidia" in self._critique_base_url.lower():
                self._critique_llm = ChatNVIDIA(
                    base_url=self._critique_base_url,
                    model=self._critique_llm_model_name,
                    api_key=self.critique_api_key,
                    temperature=0.6,
                    # http_client=critique_llm_http_client, # If needed and supported
                )
            else:
                self._critique_llm = ChatOpenAI(
                    base_url=self._critique_base_url,
                    model=self._critique_llm_model_name,
                    api_key="dummy_key",
                    temperature=0,
                    top_p=0.01,
                    http_client=critique_llm_http_client, # If needed
                )
        return self._critique_llm

    def filter_known_error(self, database, issue: Issue):
        """
        Check if an issue exactly matches a known false positive.
        
        Args:
            database: The vector database of known false positives.
            issue: The issue object with details like the error trace and issue ID.

        Returns:
            tuple: A tuple containing:
            - response (FilterResponse): A structured response with the analysis result.
            - examples_context_str (str): N (N=SIMILARITY_ERROR_THRESHOLD) most similar known issues of the same type of the query issue.
        """
        prompt = ChatPromptTemplate.from_messages([
            ("system", self.filter_system_prompt),
            ("user", self.filter_human_prompt)
        ])
        
        retriever = database.as_retriever(search_kwargs={"k": self.similarity_error_threshold,
                                                         'filter': {'issue_type': issue.issue_type}})
        resp = retriever.invoke(issue.trace)
        examples_context_str= _format_context_from_response(resp)
        logger.debug(f"[issue-ID - {issue.id}] Found This context:\n{examples_context_str}")
        if not examples_context_str:
            # logger.info(f"Not find any relevant context for issue id {issue.id}")
            response = FilterResponse(
                                    equal_error_trace=[],
                                    justifications=(f"No identical error trace found in the provided context. "
                                                    f"The context empty because no issue of type {issue.issue_type} in knonw isseu DB."),
                                    result="NO"
                                )
            return response, examples_context_str

        template_path = os.path.join(os.path.dirname(__file__), "templates", "known_issue_filter_resp.json")
        answer_template = read_answer_template_file(template_path)

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
            response = robust_structured_output(llm=self.main_llm, schema=FilterResponse, input=issue.trace, prompt_chain=pattern_matching_prompt_chain, max_retries=self.max_retry_limit)
        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(max_retry_limit=self.max_retry_limit, function_name="filter_known_error", issue_id=issue.id, error=e))
            response = FilterResponse(
                                    equal_error_trace=[],
                                    justifications="An error occurred twice during model output parsing. Defaulting to: NO",
                                    result="NO"
                                )
        return response, examples_context_str



    def investigate_issue(self, context: str, issue: Issue) -> tuple[AnalysisResponse, EvaluationResponse]:
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

            llm_analysis_response = AnalysisResponse(investigation_result=analysis_response.investigation_result,
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
            logger.error(f"{failed_message}, set default values for the fields it failed on. Error is: {e}" )
            llm_analysis_response = AnalysisResponse(investigation_result="NOT A FALSE POSITIVE" if analysis_response is None else analysis_response.investigation_result,
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
            logger.error(f"Failed during evaluation process, set default values. Error is: {e}" )
            critique_response = EvaluationResponse(critique_result=analysis_response.investigation_result,
                                                   justifications=["Failed during evaluation process. Defaulting to first analysis_response"]
                                                   )

        return llm_analysis_response, critique_response


    @retry(stop=stop_after_attempt(2),
           wait=wait_fixed(10),
           retry=retry_if_exception_type(Exception)
           )
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
        logger.debug(f"Analysis prompt:   {actual_prompt.to_string()}")

        try:
            analysis_response = robust_structured_output(llm=self.main_llm,
                                                schema=JudgeLLMResponse,
                                                input=user_input,
                                                prompt_chain=analysis_prompt_chain,
                                                max_retries=self.max_retry_limit
                                                )
        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(max_retry_limit=self.max_retry_limit, function_name="_analyze", issue_id=issue.id, error=e))
            raise e

        logger.debug(f"{analysis_response=}")
        return actual_prompt, analysis_response



    @retry(stop=stop_after_attempt(2),
           wait=wait_fixed(10),
           retry=retry_if_exception_type(Exception)
           )
    def _summarize_justification(self, actual_prompt, response: JudgeLLMResponse, issue_id: str) -> JustificationsSummary:
        """
        Summarize the justifications into a concise, engineer-style comment.

        Args:
            actual_prompt (str): The query prompt sent to the LLM, including the context.
            response (JudgeLLMResponse): A structured response with the analysis result.

        Returns:
            response (JustificationsSummary): A structured response with summary of the justifications.
        """

        examples_str = ('[{"short_justifications": "t is reassigned so previously freed value is replaced by malloced string"}, '
                        '{"short_justifications": "There is a check for k<0"}, '
                        '{"short_justifications": "i is between 1 and BMAX, line 1623 checks that j < i, array C is of the size BMAX+1"}, '
                        '{"short_justifications": "C is an array of size BMAX+1, i is between 1 and BMAX (inclusive)"}]'
                    )

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
            short_justification = robust_structured_output(llm=self.main_llm,
                                                           schema=JustificationsSummary,
                                                           input=response,
                                                           prompt_chain=justification_summary_prompt_chain,
                                                           max_retries=self.max_retry_limit
                                                           )
        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(max_retry_limit=self.max_retry_limit, function_name="_summarize_justification", issue_id=issue_id, error=e))
            raise e

        return short_justification



    @retry(stop=stop_after_attempt(2),
           wait=wait_fixed(10),
           retry=retry_if_exception_type(Exception)
           )
    def _recommend(self, issue: Issue, context: str, analysis_response: JudgeLLMResponse) -> RecommendationsResponse:
        """
        Evaluates a given CVE analysis and generates recommendations for further investigation, if necessary.

        Args:
            issue (Issue): An object representing the reported CVE. The object must have a 'trace' attribute containing the error trace associated with the CVE.
            context (str): The data used for the CVE analysis (e.g., source code snippets, error traces). This is the raw data that the analysis is based on.
            analysis_response (JudgeLLMResponse): An object containing the analysis of the CVE.  This object provides the initial assessment and reasoning.

        Returns:
            recommendations_response (RecommendationsResponse): An object containing the language model's evaluation and recommendations.
        """

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
            recommendations_response = robust_structured_output(llm=self.main_llm,
                                                                schema=RecommendationsResponse,
                                                                input={},
                                                                prompt_chain=recommendation_prompt_chain,
                                                                max_retries=self.max_retry_limit
                                                                )
            logger.debug(f"recommendations_response: {recommendations_response=}")

        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(max_retry_limit=self.max_retry_limit, function_name="_recommand", issue_id=issue.id, error=e))
            raise e

        return recommendations_response



    @retry(stop=stop_after_attempt(2),
           wait=wait_fixed(10),
           retry=retry_if_exception_type(Exception)
           )
    def _evaluate(self, actual_prompt, response, issue_id) -> EvaluationResponse:
        from langchain_core.prompts import ChatPromptTemplate
        from langchain_core.runnables import RunnablePassthrough

        prompt = ChatPromptTemplate.from_messages([
            # Should not use 'system' for deepseek-r1
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
            critique_response = robust_structured_output(llm=self.main_llm,
                                                         schema=EvaluationResponse,
                                                         input=response,
                                                         prompt_chain=evaluation_prompt_chain,
                                                         max_retries=self.max_retry_limit
                                                        )
            logger.debug(f"{critique_response=}")

        except Exception as e:
            logger.error(RED_ERROR_FOR_LLM_REQUEST.format(max_retry_limit=self.max_retry_limit, function_name="_evaluate", issue_id=issue_id, error=e))
            raise e

        return critique_response

    def create_vdb(self, text_data):
        self.vector_db = FAISS.from_texts(text_data, self.embedding_llm)
        return self.vector_db

    def create_vdb_for_known_issues(self, text_data):
        """
        This function creates a FAISS vector database for known issues.

        If there are error traces in the text_data, it creates a populated FAISS database.
        Otherwise, it returns an empty FAISS database.
        """
        metadata_list, error_trace_list = self._extract_metadata_from_known_false_positives(text_data)

        if not error_trace_list:
            logger.info(f"Note: No known issues were found. The investigation will be based solely on the source code.")
            # Create an empty FAISS index
            # The dimension of the index must match the embedding model's output dimension.
            # We get this by embedding a dummy text.
            embedding_dimension = len(self.embedding_llm.embed_query("dummy"))
            empty_index = faiss.IndexFlatL2(embedding_dimension)
            
            # Create an empty FAISS vector store
            self.known_issues_vector_db = FAISS(
                embedding_function=self.embedding_llm,
                index=empty_index,
                docstore=InMemoryDocstore(),
                index_to_docstore_id={}   
            )
        else:
            self.known_issues_vector_db = FAISS.from_texts(texts=error_trace_list, embedding=self.embedding_llm, metadatas=metadata_list)
        return self.known_issues_vector_db

    def _extract_metadata_from_known_false_positives(self, known_issues_list):
        """
        Returns:
            tuple: A tuple containing:
                - metadata_list (list[dict]): List of metadata dictionaries, indicating for each issue the issue type and the reason it is marked as False Positive.
                - error_trace_list (list[str]): List of known issues.
        """
        metadata_list = []
        error_trace_list = []
        for item in known_issues_list:
            try:
                lines = item.split("\n")

                 # Extract the issue type (next word after "Error:")
                match = re.search(r"Error:\s*([^\s(]+)", lines[0])
                if match:
                    issue_type = match.group(1)
                else:
                    logger.warning(f"Missing issue_type, skipping known False positive {item}")
                    continue

                # Extract the lines after the error trace as 'reason_of_false_positive'
                reason_start_line_index = len(lines) - 1
                code_block_line_pattern = re.compile(r'#\s*\d+\|')
                path_line_pattern = re.compile(r'^(.+/)+(.+):(\d+):\s?(.*)')
                for line_index in range(len(lines)-1, -1, -1):
                    if code_block_line_pattern.match(lines[line_index].strip()) or path_line_pattern.match(lines[line_index].strip()):
                        reason_start_line_index = line_index + 1
                        break
                reason_lines = [line.lstrip('#').strip() for line in lines[reason_start_line_index:] if line.strip()]
                reason_of_false_positive = "\n".join(reason_lines)

                metadata_list.append({
                    "reason_of_false_positive": reason_of_false_positive,
                    "issue_type": issue_type
                })
                error_trace = "\n".join(lines[:reason_start_line_index])
                check_text_size_before_embedding(error_trace, self.embedding_llm_model_name)
                # Add the item without the last line
                error_trace_list.append(error_trace)
            except Exception as e:
                logger.error(f"Error occurred during process this known issue: {item}\nError: {e}")
                raise e

        return metadata_list, error_trace_list