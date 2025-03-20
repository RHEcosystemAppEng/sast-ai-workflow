import os
import time
from langchain_community.vectorstores import FAISS
from Utils.file_utils import read_all_source_code_files


def generate_code_embeddings(llm_service):
    if os.path.exists("./../faiss_index/index.faiss"):
        print("Loading source code embeddings from file index")
        src_db = FAISS.load_local("./../faiss_index", llm_service.embedding_llm,
                                  allow_dangerous_deserialization=True)
    else:
        code_text = read_all_source_code_files()
        src_text = code_text if len(code_text) > 0 else []
        start = time.time()
        print("Creating embeddings for source code...")
        src_db = llm_service.create_vdb(src_text)
        src_db.save_local("./../faiss_index")
        end = time.time()
        print(f"Src project files have embedded completely. It took : {end - start} seconds")

    return src_db