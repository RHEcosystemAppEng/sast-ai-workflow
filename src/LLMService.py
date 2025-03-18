import os
from langchain_community.embeddings import HuggingFaceEmbeddings
from langchain_community.vectorstores import FAISS
from langchain_nvidia_ai_endpoints import ChatNVIDIA
from langchain_openai import OpenAI
from Utils.config_utils import load_config
from Utils.system_utils import get_device

from langchain_core.prompts import ChatPromptTemplate
from langchain_core.runnables import RunnablePassthrough, RunnableLambda
from langchain_core.output_parsers import StrOutputParser
import os

from Utils.file_utils import read_answer_template_file

config = load_config()  # Take configuration variables from default_config.yaml
RUN_WITH_CRITIQUE = config["RUN_WITH_CRITIQUE"]
CRITIQUE_LLM_API_KEY = os.environ.get("CRITIQUE_LLM_API_KEY")

class LLMService:

    def __init__(self, base_url, llm_model_name, embedding_llm_model_name, api_key, critique_llm_model_name, critique_base_url):
        self.base_url = base_url
        self.api_key = api_key
        self.llm_model_name = llm_model_name
        self.embedding_llm_model_name = embedding_llm_model_name

        self._main_llm = None
        self._embedding_llm = None
        self.vector_db = None
        self._critique_llm = None
        self._critique_llm_model_name = critique_llm_model_name
        self._critique_base_url = critique_base_url

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
                self._main_llm = OpenAI(
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
                encode_kwargs={'normalize_embeddings': False}
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
                    api_key=CRITIQUE_LLM_API_KEY,
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

    def filter_known_error(self, database, user_input):

        prompt = ChatPromptTemplate.from_messages([
            ("system",
             "You are an expert in identifying similar error stack traces. "
             "Inside the context, you are given existing set of error traces."
             "Look very precisely into the context and tell us if you find similar error trace."
             "Error traces should have exact number of lines. Same method names and order of method calls "
             "must be identical."
             "Answer the question using only the context."
             "Generate a answer response template provided. No additional text outside the "
             "answer template."
             "\nAnswer response template:{answer_template}"
             "\n\nContext:{context}"
             ),
            ("user", "{question}")
        ])
        retriever = database.as_retriever()
        resp = retriever.invoke(user_input)
        context_str = "".join(doc.page_content for doc in resp)

        template_path = os.path.join(os.path.dirname(__file__), "templates", "known_issue_resp.json")
        answer_template = read_answer_template_file(template_path)
        
        chain1 = (
                {
                    "context": RunnableLambda(lambda _: context_str),
                    "answer_template": RunnableLambda(lambda _: answer_template),
                    "question": RunnablePassthrough()
                }
                | prompt
        )
        actual_prompt = chain1.invoke(user_input)
        # print(f"Filtering prompt:   {actual_prompt.to_string()}")
        chain2 = (
                chain1
                | self.main_llm
                | StrOutputParser()
        )
        return actual_prompt.to_string(), chain2.invoke(user_input)

    def final_judge(self, database, user_input):

        prompt = ChatPromptTemplate.from_messages([
            ("system",
             "You are an experienced C developer tasked with analyzing code to identify potential flaws. "
             "You understand programming language control structures. Therefore, you are capable of verifying the "
             "call-hierarchy of a given source code. You can observe the runtime workflows."
             "You understand the question has line numbers of the source code."
             "Your responses should be precise and no longer than two sentences. Provide justifications for your answers."
             # "Do not hallucinate. Say you don't know if you don't have this information." # LLM doesn't know it is hallucinating
             # "Answer the question using only the context"  # this line can be optional
             # "First step is to see if the context has the same error stack trace. If so, it is a false positive. "
             # "For the justification you can mention that Red Hat engineers have manually verified it as false positive error."
             # "If you do not find exact error in the Context, you must perform an independent verification,"
             "Tell precisely if the error is a false positive or not. "
             "Answer must have ONLY the following 3 sections:"
             "Investigation Result, Justifications, Recommendations. "
             "Investigation Result should only contain, FALSE POSITIVE or NOT A FALSE POSITIVE."
             "\n\nContext:{context}"
             ),
            ("user", "{question}")
        ])
        retriever = database.as_retriever()
        resp = retriever.invoke(user_input)
        context_str = "".join(doc.page_content for doc in resp)

        chain1 = (
                {
                    "context": RunnableLambda(lambda _: context_str),
                    "question": RunnablePassthrough()
                }
                | prompt
        )
        actual_prompt = chain1.invoke(user_input)
        # print(f"Evaluation prompt:   {actual_prompt.to_string()}")
        chain2 = (
                chain1
                | self.main_llm
                | StrOutputParser()
        )
        response = chain2.invoke(user_input)
        # print(f"{response=}")
        critique_response = self._evaluate(actual_prompt.to_string(), response) if RUN_WITH_CRITIQUE else ""
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
            "Based on the context, the query, and the 'Justifications' (from the response), your main goal is to check if the 'Investigation Result' (from the response) is right. "
            "\nAssess it with the following parameters (give each one score 0,1,2 - 2 is the higher):"
            "\n1. Does the 'Justifications' make sense givet the data you have?"
            "\n2. Does the 'Recommendations' make sense givet the data you have?"
            "\n3. Factual accuracy (Does it match the context?)."
            "\n4. Completeness (Does it address all aspects of the query?)."
            "\nEventialy decide whether the 'Investigation Result' was right (is it really false positive or not false positive). "
            "Give it a overall confidence score 0,1,2 (2 is the higher)."
            "\nProvide detailed justifications for your answers and ensure your responses are clear and concise. "
            "Structure your output into JSON format with sections: 'Critique Result' (which contain 'false positive' or 'not a false positive'), 'Justifications'."
            "\nPerform an independent verification to determine the 'Critique Result'. "
            "If the 'Justifications' score is low, you can still use the same result as the 'Investigation Result' for the 'Critique Result', but only if you find another valid justification."
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
        critque_response = chain.invoke({
                                "actual_prompt": actual_prompt,
                                "response": response
                            })
        # print(f"{critque_response=}")
        return critque_response

    def create_vdb(self, text_data):
        self.embedding_llm.embed_documents(text_data)
        self.vector_db = FAISS.from_texts(text_data, self.embedding_llm)
        # self.vector_db.add_texts(text_data)
        return self.vector_db




