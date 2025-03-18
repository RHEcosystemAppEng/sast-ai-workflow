from langchain_community.embeddings import HuggingFaceEmbeddings
from langchain_community.vectorstores import FAISS
from langchain_nvidia_ai_endpoints import ChatNVIDIA
from langchain_openai import OpenAI
from Utils.utils import get_device
from langchain_core.prompts import ChatPromptTemplate
from langchain_core.runnables import RunnablePassthrough, RunnableLambda
from langchain_core.output_parsers import StrOutputParser

from src.Utils.utils import read_answer_template_file


class LLMService:

    def __init__(self, base_url, llm_model_name, embedding_llm_model_name, api_key):
        self.base_url = base_url
        self.api_key = api_key
        self.llm_model_name = llm_model_name
        self.embedding_llm_model_name = embedding_llm_model_name

        self.main_llm = None
        self.embedding_llm = None
        self.vector_db = None

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

        answer_template = read_answer_template_file('./templates/known_issue_resp.json')
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
        return actual_prompt.to_string(), chain2.invoke(user_input)

    def create_embedding_llm(self):
        device = get_device()
        print(
            f"Embedding LLM model: {self.embedding_llm_model_name} || device: {device}".center(80, '-'))
        self.embedding_llm = HuggingFaceEmbeddings(
            model_name=self.embedding_llm_model_name,
            model_kwargs={'device': device},
            encode_kwargs={'normalize_embeddings': False}
        )
        return self.embedding_llm

    def get_main_llm(self):
        def create_main_llm():
            # Decide which LLM to use based on the base_url
            if "nvidia" in self.base_url.lower():
                self.main_llm = ChatNVIDIA(
                    base_url=self.base_url,
                    model=self.llm_model_name,
                    api_key=self.api_key,
                    temperature=0
                )
            else:
                self.main_llm = OpenAI(
                    base_url=self.base_url,
                    model=self.llm_model_name,
                    api_key="dummy_key",
                    temperature=0,
                    top_p=0.01
                )
            return self.main_llm

        if self.main_llm is None:
            self.main_llm = create_main_llm()
        return self.main_llm

    def get_embedding_llm(self):
        if self.embedding_llm is None:
            self.embedding_llm = self.create_embedding_llm()
        return self.embedding_llm

    def create_vdb(self, text_data):
        self.embedding_llm.embed_documents(text_data)
        self.vector_db = FAISS.from_texts(text_data, self.embedding_llm)
        # self.vector_db.add_texts(text_data)
        return self.vector_db




