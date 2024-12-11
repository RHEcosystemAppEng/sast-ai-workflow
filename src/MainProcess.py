from langchain_community.embeddings import HuggingFaceEmbeddings
from langchain_community.vectorstores import FAISS
from langchain_nvidia_ai_endpoints import ChatNVIDIA


class MainProcess:

    def __init__(self, base_url, llm_model_name, embedding_llm_model_name, api_key):
        self.base_url = base_url
        self.llm_model_name = llm_model_name
        self.embedding_llm_model_name = embedding_llm_model_name
        self.api_key = api_key
        self.embedding_llm_model = HuggingFaceEmbeddings(
            model_name="/Users/jnirosha/Projects/morpheus/all-mpnet-base-v2",
            model_kwargs={'device': 'mps'},
            encode_kwargs={'normalize_embeddings': False}
        )
        self.vector_db = None

    def populate_db(self, text_data):
        self.embedding_llm_model.embed_documents(text_data)
        self.vector_db = FAISS.from_texts(text_data, self.embedding_llm_model)
        # self.vector_db.add_texts(text_data)
        return self.vector_db

    def query(self, database, user_input):
        from langchain_core.prompts import ChatPromptTemplate
        from langchain_core.runnables import RunnablePassthrough
        from langchain_core.output_parsers import StrOutputParser

        llm_model = ChatNVIDIA(base_url=self.base_url, model=self.llm_model_name, api_key=self.api_key, temperature=0)

        prompt = ChatPromptTemplate.from_messages([
            ("system",
             "You are an experienced C developer tasked with analyzing code to identify potential flaws. "
             "You understand programming language control structures. Therefore, you are capable of verifying the "
             "call-hierarchy of a given source code. You can observe the runtime workflows."
             "You understand the question has line numbers of the source code."
             "Your responses should be precise and no longer than two sentences. Provide justifications for your answers."
             "Do not hallucinate. Say you don't know if you don't have this information."
             # "Answer the question using only the context"  # this line can be optional
             "First step is to see if the context has the same error stack trace. If so, it is a false positive. "
             "For the justification you can mention that Red Hat engineers have manually verified it as false positive error."
             "If you do not find exact error in the context, you must perform an independent verification,"
             "and tell us precisely if the error is a false positive or not."
             "\n\nQuestion:{question}\n\nContext:{context}"
             ),
            ("user", "{question}")
        ])

        chain = (
                {
                    "context": database.as_retriever(),
                    "question": RunnablePassthrough()
                }
                | prompt
                | llm_model
                | StrOutputParser()
        )
        return chain.invoke(user_input)