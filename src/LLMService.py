import os

from langchain_openai import OpenAIEmbeddings
from langchain_openai.chat_models.base import ChatOpenAI
from langchain_community.vectorstores import FAISS
from langchain_nvidia_ai_endpoints import ChatNVIDIA
from langchain_core.prompts import ChatPromptTemplate, SystemMessagePromptTemplate, HumanMessagePromptTemplate
from langchain_core.runnables import RunnablePassthrough, RunnableLambda

from Utils.file_utils import read_answer_template_file
from Utils.embedding_utils import check_text_size_before_embedding
from common.config import Config
from dto.Issue import Issue
from dto.ResponseStructures import FilterResponse, JudgeLLMResponse, JustificationsSummary, RecommendationsResponse, EvaluationResponse
from dto.LLMResponse import AnalysisResponse, CVEValidationStatus
from tenacity import retry, stop_after_attempt, wait_fixed, retry_if_exception_type


class LLMService:

    def __init__(self, config:Config):
        self.llm_url = config.LLM_URL
        self.llm_api_key = config.LLM_API_KEY
        self.llm_model_name = config.LLM_MODEL_NAME
        self.embedding_llm_url = config.EMBEDDINGS_LLM_URL
        self.embedding_api_key = config.EMBEDDINGS_API_KEY
        self.embedding_llm_model_name = config.EMBEDDINGS_LLM_MODEL_NAME

        self._main_llm = None
        self._embedding_llm = None
        self.vector_db = None
        self.knonw_issues_vector_db = None
        self.similarity_error_threshold = config.SIMILARITY_ERROR_THRESHOLD
        self.run_with_critique = config.RUN_WITH_CRITIQUE
        self._critique_llm = None
        self._critique_llm_model_name = config.CRITIQUE_LLM_MODEL_NAME
        self._critique_base_url = config.CRITIQUE_LLM_URL
        self.critique_api_key = getattr(config, "CRITIQUE_LLM_API_KEY", None)
        
        # Initialize failure counters
        self.filter_retry_counter = 0
        self.judge_retry_counter = 0
        self.max_retry_limit = 3
        

    @property
    def main_llm(self):
        if self._main_llm is None:
            # Decide which LLM to use based on the base_url
            if "nvidia" in self.llm_url.lower():
                self._main_llm = ChatNVIDIA(
                    base_url=self.llm_url,
                    model=self.llm_model_name,
                    api_key=self.llm_api_key,
                    temperature=0
                )
            else:
                self._main_llm = ChatOpenAI(
                    base_url=self.llm_url,
                    model=self.llm_model_name,
                    api_key="dummy_key",
                    temperature=0,
                    top_p=0.01
                )
        return self._main_llm

    @property
    def embedding_llm(self):
        if self._embedding_llm is None:
            self._embedding_llm = OpenAIEmbeddings(
                openai_api_base=self.embedding_llm_url,
                openai_api_key=self.embedding_api_key,
                model=self.embedding_llm_model_name,
                tiktoken_enabled=False,
                show_progress_bar=True
            )
        return self._embedding_llm

    @property
    def critique_llm(self):
        if self._critique_llm is None:
            # Decide which LLM to use based on the base_url
            if "nvidia" in self._critique_base_url.lower():
                self._critique_llm = ChatNVIDIA(
                    base_url=self._critique_base_url,
                    model=self._critique_llm_model_name,
                    api_key=self.critique_api_key,
                    temperature=0.6
                )
            else:
                self._critique_llm = ChatOpenAI(
                    base_url=self._critique_base_url,
                    model=self._critique_llm_model_name,
                    api_key="dummy_key",
                    temperature=0,
                    top_p=0.01
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
            ("system",
            "You are an expert in identifying similar error stack traces.\n"
            "You are provided with:\n"
            "1. A list of known false positive issues (context_false_positives):\n"
            "Each issue in the list includes two key elements:\n"
            "false_positive_error_trace - the issue error trace.\n"
            "reason_marked_false_positive - A reason for its classification as a false positive.\n"
            "2. A new user error trace (user_error_trace).\n\n"
            "Your task is to determine whether the user error trace exactly matches any of the false positives.\n"
            "When comparing issues, you may ignore differences in line numbers and package version details. "
            "However, the error trace in the query must exactly match the error trace in the context, "
            "including the same method names and the same order of method calls. "
            "Answer the question using only the provided context.\n"
            "Your response must strictly follow the provided answer response template. "
            "Do not include any additional text outside the answer template.\n"
            "Answer response template:\n{answer_template}\n"
            "context_false_positives: {context}"
            ),
            ("user", "Does the error trace of user_error_trace match any of the context_false_positives errors?\n"
            "user_error_trace: {user_error_trace}")
        ])
        retriever = database.as_retriever(search_kwargs={"k": self.similarity_error_threshold, 
                                                         'filter': {'issue_type': issue.issue_type}})
        resp = retriever.invoke(issue.trace)
        examples_context_str= self._format_context_from_response(resp)
        print(f"[issue-ID - {issue.id}] Found This context:\n{examples_context_str}")
        if not examples_context_str:
            # print(f"Not find any relevant context for issue id {issue.id}")
            response = FilterResponse(
                                    issue_id=issue.id,
                                    equal_error_trace=[],
                                    justifications=(f"No identical error trace found in the provided context. "
                                                    f"The context empty because no issue of type {issue.issue_type} in knonw isseu DB."),
                                    result="NO"
                                )     
            return response, examples_context_str

        template_path = os.path.join(os.path.dirname(__file__), "templates", "known_issue_filter_resp.json")
        answer_template = read_answer_template_file(template_path)

        structured_llm = self.main_llm.with_structured_output(FilterResponse, method="json_mode")

        chain1 = (
                {
                    "context": RunnableLambda(lambda _: examples_context_str),
                    "answer_template": RunnableLambda(lambda _: answer_template),
                    "user_error_trace": RunnablePassthrough()
                }
                | prompt
        )
        # actual_prompt = chain1.invoke(issue.trace)
        # print(f"\n\n\nFiltering prompt:\n{actual_prompt.to_string()}")
        chain2 = (
                chain1
                | structured_llm
        )
        response = chain2.invoke(issue.trace)
        if not response:
            # If the response is insufficient to construct the object -
            # response will be None and we'll give it another try
            if self.filter_retry_counter >= self.max_retry_limit:
                raise Exception(
                    f"LLM output parsing has failed {self.filter_retry_counter} / {self.max_retry_limit} times in filter_known_error process. "
                    f"This indicates a persistent issue with the model or the input data. "
                    f"Please investigate the root cause to resolve this problem."
                )
            print(f"\033[91mWARNING: An error occurred during model output parsing. retrying now. \033[0m")
            response = chain2.invoke(issue.trace)
            if not response:
                print(f"\033[91mWARNING: An error occurred twice during model output parsing. Please try again and check this Issue-id {issue.id}. \033[0m")
                self.filter_retry_counter += 1
                response = FilterResponse(
                                        issue_id=issue.id,
                                        equal_error_trace=[],
                                        justifications="An error occurred twice during model output parsing. Defaulting to: NO",
                                        result="NO"
                                    )
        return response, examples_context_str

    def _format_context_from_response(self, resp):
        context_list = []
        for doc in resp:
            context_list.append({"false_positive_error_trace":doc.page_content, 
                                 "reason_marked_false_positive":doc.metadata['reason_of_false_positive']
                                 })
        return context_list
    
    def analayze(self, context: str, issue: Issue) -> tuple[AnalysisResponse, EvaluationResponse]:
        """
        Analyze an issue to determine if it is a false positive or not.

        Args:
            user_input: Query with error trace to analyze.
            context: The context to assist in the analysis.
            issue: The issue object with details like the error trace and issue ID.

        Returns:
            tuple: A tuple containing:
                - llm_analysis_response (AnalysisResponse): A structured response with the analysis result.
                - critique_response (EvaluationResponse): The response of the critique model, if applicable.
        """
        analysis_prompt, analysis_response = self._analyze(context=context, issue=issue)
        recommendations_response = self._recommand(issue=issue, context=context, analysis_response=analysis_response)
        short_justifications_response = self._summarize_justification(analysis_prompt.to_string(), analysis_response)

        llm_analysis_response = AnalysisResponse(investigation_result=analysis_response.investigation_result,
                                   is_final=recommendations_response.is_final,
                                   justifications=analysis_response.justifications,
                                   evaluation=recommendations_response.justifications,
                                   recommendations=recommendations_response.recommendations,
                                   instructions=recommendations_response.instructions,
                                   prompt=analysis_prompt.to_string(),
                                   short_justifications=short_justifications_response.short_justifications)
        
        
        critique_response = self._evaluate(analysis_prompt.to_string(), llm_analysis_response) if self.run_with_critique else ""
        return llm_analysis_response, critique_response

    @retry(stop=stop_after_attempt(2),
           wait=wait_fixed(10),
           retry=retry_if_exception_type(Exception)
    )
    def _analyze(self, context: str, issue: Issue):
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
            SystemMessagePromptTemplate.from_template("You are an expert security analyst tasked with determining if a reported CVE (Common Vulnerabilities and Exposures) is a FALSE POSITIVE or a TRUE POSITIVE.\n"
            "You will be provided with a CVE report snippet, the source code of the function(s) mentioned in the CVE's error trace and examples of verified CVEs with the same CWE as the reported CVE.\n"
            "Your task is to analyze step-by-step the code of the reported CVE issue to identify if it is FALSE POSITIVE or TRUE POSITIVE.\n"
            "A finding of **TRUE POSITIVE** should be made if **any** execution path within the provided source code potentially leads to the vulnerability described in the CVE.\n\n"
            "**Crucially, you must base your analysis solely on the explicit behavior of the provided source code and the description in the CVE report.\n"
            "Do not make any assumptions about the code's behavior based on function names, variable names, or any implied functionality.**\n"
            "Your output must include the following 2 sections:\n"
            "1. **investigation_result** [FALSE POSITIVE/TRUE POSITIVE].\n"
            "2. **Justifications:** [The reasoning that led to the investigation_result decision]\n"
            "**Here is the information for your analysis:**\n"
            "**CVE Report Snippet:**\n{cve_error_trace}\n\n"
            "{context}\n\n"
            "**Your analysis must adhere to the following strict guidelines:**\n"
            "* Provide evidence or context strictly based on the provided information.* You must explicitly reference lines of code. Do not provide justifications based on what you *infer* the code might do or how it is *typically* used.\n"
            "* If there are any uncertainties or lack of explicit proof within the provided code that *all* execution paths are safe with respect to the CVE description, you **must not** conclude FALSE POSITIVE. Clearly state the uncertainty\n"
            "* **No Implicit Behavior:** Analyze the code exactly as written. Do not assume what a function *might* do based on its name or common programming patterns. Focus only on the explicit operations performed within the provided code.\n"
            "* **No Clear False Positive Evidence Implies True Positive:** A conclusion of FALSE POSITIVE requires definitive proof within the provided CVE report and source code that the described vulnerability cannot occur under any circumstances within the analyzed code. Lack of such definitive proof should lean towards TRUE POSITIVE\n"
            "* **Single Vulnerable Path is Sufficient:** If you identify even one specific sequence of execution within the provided code that potentially triggers the vulnerability described in the CVE, the result should be **TRUE POSITIVE**\n"
            "* **Direct Correlation:** Ensure a direct and demonstrable link between the code's behavior and the vulnerability described in the CVE.\n"
            "* **Focus on Provided Information:** Your analysis and justifications must be solely based on the text of the CVE report snippet and the provided source code. Do not make assumptions about the broader system or environment.\n"
            "* Check that all of the justifications are based on code that its implementation is provided in the context.\n"
            "**Begin your analysis.**\n"),
            HumanMessagePromptTemplate.from_template("{question}")
        ])
        
        structured_llm = self.main_llm.with_structured_output(JudgeLLMResponse, method="json_mode")

        chain1 = (
                {
                    "context": RunnableLambda(lambda _: context),
                    "cve_error_trace": RunnableLambda(lambda _: issue.trace),
                    "question": RunnablePassthrough()
                }
                | analysis_prompt
        )
        actual_prompt = chain1.invoke(user_input)
        print(f"Analysis prompt:   {actual_prompt.to_string()}")
        chain2 = (
                chain1
                | structured_llm
        )
        analysis_response = chain2.invoke(user_input)

        print(f"{analysis_response=}")

        if not analysis_response:
            # If the response is insufficient to construct the object -
            # response will be None and we'll give it another try
            if self.judge_retry_counter >= self.max_retry_limit:
                raise Exception(
                    f"LLM output parsing has failed {self.judge_retry_counter} / {self.max_retry_limit} times in final_judge process. "
                    f"This indicates a persistent issue with the model or the input data. "
                    f"Please investigate the root cause to resolve this problem."
                )
            print(f"\033[91mWARNING: An error occurred during model output parsing. retrying now. \033[0m")
            response = chain2.invoke(user_input)
            if not response:
                print(f"\033[91mWARNING: An error occurred twice during model output parsing. Please try again and check this Issue-id {issue.id}. \033[0m")
                self.judge_retry_counter += 1
                response = JudgeLLMResponse(
                        investigation_result="NOT A FALSE POSITIVE",
                        justifications=["Unable to parse the result from the model. Defaulting to: NOT A FALSE POSITIVE."],
                        )
        return actual_prompt, analysis_response
    
    @retry(stop=stop_after_attempt(2),
           wait=wait_fixed(10),
           retry=retry_if_exception_type(Exception)
    )
    def _summarize_justification(self, actual_prompt, response: JudgeLLMResponse) -> JustificationsSummary:
        """
        Summarize the justifications into a concise, engineer-style comment.

        Args:
            actual_prompt (str): The query prompt sent to the LLM, including the context.
            response (JudgeLLMResponse): A structured response with the analysis result.

        Returns:
            response (JustificationsSummary): A structured response with summary of the justifications.
        """
        examples = ["t is reassigned so previously freed value is replaced by malloced string",
                    "There is a check for k<0",
                    "i is between 1 and BMAX, line 1623 checks that j < i, array C is of the size BMAX+1",
                    "C is an array of size BMAX+1, i is between 1 and BMAX (inclusive)",
                    ]
        examples_str = "\n".join(f"{i}. {example}" for i, example in enumerate(examples, start=1))
        
        prompt = ChatPromptTemplate.from_messages([
            ("system",
            "You are an experienced software engineer tasked with summarizing justifications for an investigation result. "
            "You are provided with the response of another model's analysis, which includes an investigation_result and justifications. " 
            "Your goal is to create a concise summary of the justifications provided in the response. "
            "Use the Query and the Response to ensure your summary is accurate and professional. "
            "Focus on the key technical reasons or evidence that support the investigation result. "
            "Write the summary in a clear, concise, and professional style, as if it were a comment in a code review or technical report. "
            "Limit the summary to a single sentence or two at most."
            "\n\nHere are examples of short justifications written by engineers:"
            "{examples_str}"
            ),
            ("user",
            "Summarize the justifications provided in the following response into a concise, professional comment:"
            "\n\nQuery: {actual_prompt}"
            "\n\nResponse: {response}"
            )
        ])
        structured_llm = self.main_llm.with_structured_output(JustificationsSummary, method="json_mode")
        
        chain = (
                {
                    "actual_prompt": RunnableLambda(lambda _: actual_prompt),
                    "examples_str": RunnableLambda(lambda _: examples_str),
                    "response": RunnablePassthrough()
                }
                | prompt
                | structured_llm
        )

        short_justification = chain.invoke(response)
        # print(f"{short_justification=}")
        return short_justification
    
    @retry(stop=stop_after_attempt(2),
           wait=wait_fixed(10),
           retry=retry_if_exception_type(Exception)
    )
    def _recommand(self, issue: Issue, context: str, analysis_response: JudgeLLMResponse) -> RecommendationsResponse: 
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
        HumanMessagePromptTemplate.from_template(
        "You are an expert security analyst tasked with rigorously evaluating a provided analysis of a reported CVE (Common Vulnerabilities and Exposures) to determine if it's a FALSE POSITIVE or a TRUE POSITIVE.\n"
        "You will be given the reported CVE, an analysis of the CVE, and the data the analysis is based on (source code snippets, error traces, etc.), along with examples of validated CVEs for context.\n"
        "Your primary goal is to critically assess the provided analysis for completeness, accuracy, and relevance to the reported CVE. Determine if the analysis provides sufficient evidence for a conclusive TRUE or FALSE POSITIVE determination.\n"
        "If the initial analysis is insufficient, identify the specific gaps and recommend the necessary data or steps required for a thorough evaluation.\n"
        "Only provide recommendations that are directly crucial for validating the reported CVE and reaching a definitive conclusion.\n"
        "If the analysis fails to cover all relevant execution paths or potential conditions, explain the shortcomings and specify the additional data needed for a complete assessment.\n"
        "Any recommendation that necessitates inspecting the implementation of a referenced function or macro MUST be formatted as an entry in the 'Instructions' list.\n"
        "Your output MUST adhere to the following structure:\n"
        "1. is_final: Indicate whether further investigation is needed. If clear and irrefutable evidence for a TRUE or FALSE POSITIVE is found within the evaluated analysis, set this value to true; otherwise, set it to false.\n"
        "2. justifications: Provide a detailed explanation of why the evaluated analysis is sound and complete, or clearly articulate its deficiencies and why it's insufficient for a final determination.\n"
        "3. recommendations (optional): If further analysis is required, provide a concise list of the specific data or steps needed to reach a conclusive TRUE or FALSE POSITIVE determination. Only include essential recommendations.\n"
        "4. Instructions (optional): A list of dictionaries, where each dictionary represents a recommendation to examine the implementation of a function or macro referenced in the source code context. Include this list ONLY if such investigations are necessary.\n"
        "   Each dictionary in the 'Instructions' list must have the following keys:\n"
        '   - "expression_name": The exact name of the missing function or macro (not the full declaration).\n'
        '   - "reffering_source_code_path": The precise file path where the "expression_name" is called from (include ONLY the file path without any surrounding text).\n'
        '   - "recommendation": A clear and actionable recommendation related to this "expression_name" (e.g., "Verify the implementation of `memcpy` to ensure no out-of-bounds write occurs.").\n'
        "**The reported CVE:**\n{cve_error_trace}\n\n"
        "**The Analysis:**\n{analysis}\n\n"
        "**The Data used for the analysis:**\n{context}")
        ])

        try:
            recommendations_llm = self.main_llm.with_structured_output(RecommendationsResponse, method="json_mode")
            recommendations_chain = ({
                "cve_error_trace": RunnableLambda(lambda _: issue.trace),
                "analysis": RunnableLambda(lambda _: analysis_response.justifications),
                "context": RunnableLambda(lambda _: context),
            } | recommendations_prompt | recommendations_llm
            )

            recommendations_response = recommendations_chain.invoke({})
            print(f"recommendations_response: {recommendations_response=}")
        except Exception as e:
            print(f"Failed to run recommendation prompt, ERROR: {e}")
            raise

        return recommendations_response     
    
    @retry(stop=stop_after_attempt(2),
           wait=wait_fixed(10),
           retry=retry_if_exception_type(Exception)
    )
    def _evaluate(self, actual_prompt, response) -> EvaluationResponse:      
        from langchain_core.prompts import ChatPromptTemplate
        from langchain_core.runnables import RunnablePassthrough
        from langchain_core.output_parsers import StrOutputParser

        prompt = ChatPromptTemplate.from_messages([
            # Should not use 'system' for deepseek-r1
            ("user", 
            "You are an experienced C developer tasked with analyzing code to identify potential flaws. "
            "You understand programming language control structures. Therefore, you are capable of verifying the "
            "call-hierarchy of a given source code. You can observe the runtime workflows. "
            "You understand the question has line numbers of the source code. "
            "Your goal is to critique the response of another model's analysis. "
            "First step is to see if the model justified its results by stating that Red Hat engineers have manually verified it as a false positive error. "
            "If so, check if the context really has the same error stack trace (you can ignore line numbers and code versions differences). If it does, it's a false positive. If not, this justification is incorrect. "
            "Your responses should be precise and no longer than two sentences. Provide justifications for your answers. "
            "Start you answer with '<think>\\n' and at the end add the json results"
            "Based on the context, the query, and the 'justifications' (from the response), your main goal is to check if the 'investigation_result' (from the response) is right. "
            "\nAssess it with the following parameters (give each one score 0,1,2 - 2 is the higher):"
            "\n1. Does the 'justifications' make sense given the data you have?"
            "\n2. Does the 'recommendations' make sense given the data you have?"
            "\n3. Factual accuracy (Does it match the context?)."
            "\n4. Completeness (Does it address all aspects of the query?)."
            "\nEventually decide whether the 'investigation_result' was right (is it really false positive or not false positive). "
            "Give it a overall confidence score 0,1,2 (2 is the higher)."
            "\nProvide detailed justifications for your answers and ensure your responses are clear and concise. "
            "Structure your output into JSON format with sections: 'critique_result' (which contain 'false positive' or 'not a false positive'), 'justifications'."
            "\nPerform an independent verification to determine the 'critique_result'. "
            "If the 'justifications' score is low, you can still use the same result as the 'investigation_result' for the 'critique_result', but only if you find another valid justification."
            "\n\nQuery and Context:{actual_prompt}"
            "\n\nResponse:{response}"
             )
        ])

        structured_llm = self.main_llm.with_structured_output(EvaluationResponse, method="json_mode")
        chain = (
                {
                    "actual_prompt": RunnableLambda(lambda _: actual_prompt),
                    "response": RunnablePassthrough()
                }
                | prompt
                | structured_llm
        )
        critique_response = chain.invoke({
                                "actual_prompt": actual_prompt,
                                "response": response
                            })
        print(f"{critique_response=}")
        return critique_response

    def create_vdb(self, text_data):
        self.vector_db = FAISS.from_texts(text_data, self.embedding_llm)
        return self.vector_db

    def create_vdb_for_known_issues(self, text_data):
        metadata_list, error_trace_list = self._process_known_issues(text_data)
        self.knonw_issues_vector_db = FAISS.from_texts(texts=error_trace_list, embedding=self.embedding_llm, metadatas=metadata_list)
        return self.knonw_issues_vector_db
    
    def _process_known_issues(self, known_issues_list):
        """
        Returns:
            tuple: A tuple containing:
                - metadata_list (list[dict]): List of metadata dictionaries.
                - error_trace_list (list[str]): List of known issues without the last line.
        """
        metadata_list = []
        error_trace_list = []
        for item in known_issues_list:
            try:
                lines = item.split("\n")            
                # Extract the last line as `reason_of_false_positive`
                reason_of_false_positive = lines[-1] if lines else ""
                # Extract the issue type (next word after "Error:")
                issue_type = lines[0].split("Error:")[1].strip().split()[0]
                metadata_list.append({
                    "reason_of_false_positive": reason_of_false_positive,
                    "issue_type": issue_type
                })
                error_trace = "\n".join(lines[:-1])
                check_text_size_before_embedding(error_trace, self.embedding_llm_model_name)
                # Add the item without the last line
                error_trace_list.append(error_trace)
            except Exception as e:
                print(f"Error occurred during process this known issue: {item}\nError: {e}")
                raise e

        return metadata_list, error_trace_list