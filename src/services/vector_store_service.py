"""
VectorStoreService - Responsible for vector database operations.
Handles creation of vector stores and similarity searches.
"""

import logging
from typing import Dict, List, Tuple

import faiss
from langchain_community.docstore.in_memory import InMemoryDocstore
from langchain_community.vectorstores import FAISS
from langchain_core.embeddings import Embeddings

from Utils.embedding_utils import truncate_text_to_token_limit
from Utils.file_utils import parse_single_ignore_err_entry

logger = logging.getLogger(__name__)


class VectorStoreService:
    """Service for managing vector store operations"""

    def create_vector_store(self, documents: List[str], embedding_llm: Embeddings) -> FAISS:
        """Create a vector store from documents"""
        return FAISS.from_texts(documents, embedding_llm)

    def create_known_issues_vector_store(
        self, known_issues: List[str], embedding_llm: Embeddings, max_input_tokens: int
    ) -> FAISS:
        """
        Create a FAISS vector database for known issues.
        If there are error traces in the known_issues, it creates a populated FAISS database.
        Otherwise, it returns an empty FAISS database.

        Embeddings are computed from truncated traces (to respect token limits), but the full
        traces are stored as page_content so retrieval returns the complete text.
        """
        metadata_list, full_trace_list, truncated_trace_list = (
            self._extract_metadata_from_known_false_positives(
                known_issues, embedding_llm, max_input_tokens
            )
        )

        if not full_trace_list:
            logger.info(
                "Note: No known issues were found. "
                "The investigation will be based solely on the source code."
            )
            embedding_dimension = len(embedding_llm.embed_query("dummy"))
            empty_index = faiss.IndexFlatL2(embedding_dimension)
            return FAISS(
                embedding_function=embedding_llm,
                index=empty_index,
                docstore=InMemoryDocstore(),
                index_to_docstore_id={},
            )
        else:
            embedding_vectors = embedding_llm.embed_documents(truncated_trace_list)
            text_embeddings = list(zip(full_trace_list, embedding_vectors))
            return FAISS.from_embeddings(
                text_embeddings=text_embeddings,
                embedding=embedding_llm,
                metadatas=metadata_list,
            )

    def _extract_metadata_from_known_false_positives(
        self, known_issues_list: List[str], embedding_llm: Embeddings, max_input_tokens: int
    ) -> Tuple[List[Dict], List[str], List[str]]:
        """
        Extract metadata and error traces from known false positives.

        Returns:
            tuple: A tuple containing:
                - metadata_list (list[dict]): List of metadata dictionaries
                - full_trace_list (list[str]): Full (untruncated) error traces for storage
                - truncated_trace_list (list[str]): Truncated error traces for embedding only
        """
        metadata_list = []
        full_trace_list = []
        truncated_trace_list = []

        for item in known_issues_list:
            try:
                parsed = parse_single_ignore_err_entry(item)
                if not parsed:
                    continue

                metadata_list.append(
                    {
                        "reason_of_false_positive": parsed["justification"],
                        "issue_type": parsed["issue_type"],
                        "issue_cwe": parsed["cwe"],
                    }
                )

                full_trace_list.append(parsed["error_trace"])
                truncated_trace_list.append(
                    truncate_text_to_token_limit(
                        parsed["error_trace"], embedding_llm.model, max_input_tokens
                    )
                )

            except Exception as e:
                logger.error(f"Error occurred during process this known issue: {item}\nError: {e}")
                raise e

        return metadata_list, full_trace_list, truncated_trace_list
