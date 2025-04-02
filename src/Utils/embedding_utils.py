import os
import time
from langchain_community.vectorstores import FAISS
from transformers import AutoTokenizer
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

def check_text_size_before_embedding(text: str, model_name: str):
    """
    Checks if the text exceeds the maximum allowed tokens for the embedding model.
    Print a warning if the text is too long.
    """
    # Load the tokenizer for the embedding model
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    max_tokens = tokenizer.model_max_length
    
    # Count tokens
    tokens = tokenizer(text)
    token_count = len(tokens["input_ids"])
    
    if token_count > max_tokens:
        print(
            f"\033[91mWARNING: Text length is {token_count} tokens, exceeding the max allowed ({max_tokens}). \033[0m"
            f"\nFirst 20 words of the text: {text.split()[:20]}"
        )
    # else:
    #     print(f"Text is within limit: {token_count} tokens.")