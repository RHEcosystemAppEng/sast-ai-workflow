import logging
import os
import time

from langchain_community.vectorstores import FAISS
from transformers import AutoTokenizer

from Utils.file_utils import read_all_source_code_files

logger = logging.getLogger(__name__)


def generate_code_embeddings(vector_service, embedding_llm):
    if os.path.exists("./../faiss_index/index.faiss"):
        logger.info("Loading source code embeddings from file index")
        src_db = FAISS.load_local(
            "./../faiss_index", embedding_llm, allow_dangerous_deserialization=True
        )
    else:
        code_text = read_all_source_code_files()
        src_text = code_text if len(code_text) > 0 else []
        start = time.time()
        logger.info("Creating embeddings for source code...")
        src_db = vector_service.create_vector_store(src_text, embedding_llm)
        src_db.save_local("./../faiss_index")
        end = time.time()
        logger.info(f"Src project files have embedded completely. It took : {end - start} seconds")

    return src_db


def truncate_text_to_token_limit(text: str, model_name: str, max_tokens: int) -> str:
    """
    Truncates text to fit within the embedding model's server-side token limit.

    max_tokens must be passed explicitly — the HuggingFace tokenizer's model_max_length
    (512 for MPNet) differs from the sentence-transformers max_seq_length (384 for
    all-mpnet-base-v2) that the TEI server enforces. Use EMBEDDINGS_MAX_INPUT_TOKENS
    from config.

    Returns the original text unchanged if it is within the limit.
    """
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    input_ids = tokenizer(text)["input_ids"]
    if len(input_ids) <= max_tokens:
        return text
    logger.warning(
        f"Text is {len(input_ids)} tokens, exceeding server limit ({max_tokens}). "
        f"Truncating. First 20 words: {text.split()[:20]}"
    )
    truncated_ids = tokenizer(text, truncation=True, max_length=max_tokens)["input_ids"]
    return tokenizer.decode(truncated_ids, skip_special_tokens=True)


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
        logger.warning(
            f"WARNING: Text length is {token_count} tokens, \
                exceeding the max allowed ({max_tokens}). "
            f"\nFirst 20 words of the text: {text.split()[:20]}"
        )
