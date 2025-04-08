import os

from langchain_openai.chat_models.base import ChatOpenAI
from langchain_huggingface import HuggingFaceEmbeddings
from langchain_community.vectorstores import FAISS
from langchain_nvidia_ai_endpoints import ChatNVIDIA
from langchain_openai import OpenAI
from langchain_core.prompts import ChatPromptTemplate
from langchain_core.runnables import RunnablePassthrough, RunnableLambda

from Utils.file_utils import read_answer_template_file
from Utils.embedding_utils import check_text_size_before_embedding
from Utils.system_utils import get_device
from common.config import Config
from model.Issue import Issue
from templates.response_structures import FilterResponse, FinalJudgeResponse


class LLMService:

    def __init__(self, config:Config):
        self.base_url = config.LLM_URL
        self.api_key = config.LLM_API_KEY
        self.llm_model_name = config.LLM_MODEL_NAME
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
        self.filter_failure_counter = 0
        self.judge_failure_counter = 0
        self.failure_tolerance = 3
        

    @property
    def main_llm(self):
        if self._main_llm is None:
            # Decide which LLM to use based on the base_url
            if "nvidia" in self.base_url.lower():
                self._main_llm = ChatNVIDIA(
                    base_url=self.base_url,
                    model=self.llm_model_name,
                    api_key=self.api_key,
                    temperature=0
                )
            else:
                self._main_llm = ChatOpenAI(
                    base_url=self.base_url,
                    model=self.llm_model_name,
                    api_key="dummy_key",
                    temperature=0,
                    top_p=0.01
                )
        return self._main_llm

    @property
    def embedding_llm(self):
        if self._embedding_llm is None:
            device = get_device()
            print(
                f"Embedding LLM model: {self.embedding_llm_model_name} || device: {device}".center(80, '-'))
            self._embedding_llm = HuggingFaceEmbeddings(
                model_name=self.embedding_llm_model_name,
                model_kwargs={'device': device},
                encode_kwargs={'normalize_embeddings': False},
                show_progress=True
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
                self._critique_llm = OpenAI(
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
            FilterResponse: A structured response with the analysis result.
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
        context= self._format_context_from_response(resp)
        print(f"[issue-ID - {issue.id}] Found This context:\n{context}")
        if not context:
            # print(f"Not find any relevant context for issue id {issue.id}")
            response = FilterResponse(
                                    issue_id=issue.id,
                                    equal_error_trace=[],
                                    justifications=(f"No identical error trace found in the provided context. "
                                                    f"The context empty because no issue of type {issue.issue_type} in knonw isseu DB."),
                                    result="NO"
                                )     
            return response, context

        template_path = os.path.join(os.path.dirname(__file__), "templates", "known_issue_filter_resp.json")
        answer_template = read_answer_template_file(template_path)

        structured_llm = self.main_llm.with_structured_output(FilterResponse, method="json_mode")

        chain1 = (
                {
                    "context": RunnableLambda(lambda _: context),
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
            if self.filter_failure_counter >= self.failure_tolerance:
                raise Exception(
                    f"Model output parsing has failed {self.filter_failure_counter} times as part of the filter_known_error process, "
                    f"which exceeds the allowed failure tolerance of {self.failure_tolerance}. "
                    f"This indicates a persistent issue with the model or the input data. "
                    f"Please investigate the root cause to resolve this problem."
                )
            print(f"\033[91mWARNING: An error occurred during model output parsing. retrying now. \033[0m")
            response = chain2.invoke(issue.trace)
            if not response:
                print(f"\033[91mWARNING: An error occurred twice during model output parsing. Please try again and check this Issu-id {issue.id}. \033[0m")
                self.filter_failure_counter += 1
                response = FilterResponse(
                                        issue_id=issue.id,
                                        equal_error_trace=[],
                                        justifications="An error occurred twice during model output parsing. Defaulting to: NO",
                                        result="NO"
                                    )
        return response, context

    def _format_context_from_response(self, resp):
        context_list = []
        for doc in resp:
            context_list.append({"false_positive_error_trace":doc.page_content, 
                                 "reason_marked_false_positive":doc.metadata['reason_of_false_positive']
                                 })
        return context_list
    def final_judge(self, user_input: str, context: str):
        """
        Analyze an issue to determine if it is a false positive or not.

        Args:
            user_input: Query with error trace to analyze.
            context: The context to assist in the analysis.

        Returns:
            tuple: A tuple containing:
                - actual_prompt (str): The prompt sent to the model.
                - response (FinalJudgeResponse): A structured response with the analysis result.
                - critique_response (str): The response of the critique model, if applicable.
        """
        prompt = ChatPromptTemplate.from_messages([
            ("system",
             "You are an experienced C developer tasked with analyzing code to identify potential flaws. "
             "You understand programming language control structures. Therefore, you are capable of verifying the "
             "call-hierarchy of a given source code. You can observe the runtime workflows."
             "You understand the question has line numbers of the source code."
             "Your responses should be precise and no longer than two sentences. "
             # "Do not hallucinate. Say you don't know if you don't have this information." # LLM doesn't know it is hallucinating
             # "Answer the question using only the context"  # this line can be optional
             # "First step is to see if the context has the same error stack trace. If so, it is a false positive. "
             # "For the justification you can mention that Red Hat engineers have manually verified it as false positive error."
             # "If you do not find exact error in the Context, you must perform an independent verification,"
             "Your task is to analyze the issue step-by-step. Follow these steps to guide your thinking: "
             "Step 1: Analyze the examples provided in the context. "
             "   - Check if any of the examples are relevant to the current issue. "
             "   - Use the error trace and the reason for classification as a false positive to determine relevance. "
             "   - If relevant examples exist, explain how they relate to the current issue and what you can learn from them. "
             "Step 2: Analyze the source code context. "
             "   - Investigate the source code to determine if the issue is a false positive or not. "
             "Step 3: Provide justifications for your conclusion based on the examples and source code analysis. "
             "   - Tell precisely if the error is a false positive or not. "
             "\n\nAnswer must have ONLY the following 3 sections:"
             "investigation_result, justifications, recommendations. "
             "investigation_result should only contain, FALSE POSITIVE or NOT A FALSE POSITIVE."
             "\n\nIn the context, you have two parts: "
             "1. Examples: These are already classified issues that you can use as a reference for analyzing the issue. "
             "   Each example includes two key elements: "
             "   - An error trace (called 'Known False Positive'). "
             "   - A reason for its classification as a false positive (called 'Reason Marked as False Positive'). "
             "2. Source Code Context: This contains the relevant source code extracted from the repository to help you analyze the issue. "
             "Use both parts of the context to provide your analysis. "
             "\n\nContext:{context}"
             ),
            ("user", "{question}")
        ])
        
        structured_llm = self.main_llm.with_structured_output(FinalJudgeResponse, method="json_mode")

        chain1 = (
                {
                    "context": RunnableLambda(lambda _: context),
                    "question": RunnablePassthrough()
                }
                | prompt
        )
        actual_prompt = chain1.invoke(user_input)
        # print(f"Evaluation prompt:   {actual_prompt.to_string()}")
        chain2 = (
                chain1
                | structured_llm
        )
        response = chain2.invoke(user_input)
        print(f"{response=}")
        if not response:
            # If the response is insufficient to construct the object -
            # response will be None and we'll give it another try
            if self.judge_failure_counter >= self.failure_tolerance:
                raise Exception(
                    f"Model output parsing has failed {self.judge_failure_counter} times as part of the final_judge process, "
                    f"which exceeds the allowed failure tolerance of {self.failure_tolerance}. "
                    f"This indicates a persistent issue with the model or the input data. "
                    f"Please investigate the root cause to resolve this problem."
                )
            print(f"\033[91mWARNING: An error occurred during model output parsing. retrying now. \033[0m")
            response = chain2.invoke(user_input)
            if not response:
                print(f"\033[91mWARNING: An error occurred twice during model output parsing. Please try again. \033[0m")
                self.judge_failure_counter += 1
                response = FinalJudgeResponse(
                        investigation_result="NOT A FALSE POSITIVE",
                        recommendations=[""],
                        justifications=[f"Unable to parse the result from the model. Defaulting to: NOT A FALSE POSITIVE."]
                        )
        critique_response = self._evaluate(actual_prompt.to_string(), response) if self.run_with_critique else ""
        return actual_prompt.to_string(), response, critique_response
    
    def _evaluate(self, actual_prompt, response):      
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


        chain = (
                {
                    "actual_prompt": RunnablePassthrough(),
                    "response": RunnablePassthrough()
                }
                | prompt
                | self.critique_llm
                | StrOutputParser()
        )
        critique_response = chain.invoke({
                                "actual_prompt": actual_prompt,
                                "response": response
                            })
        # print(f"{critique_response=}")
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
