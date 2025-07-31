"""
VectorStoreService - Responsible for vector database operations.
Handles creation of vector stores and similarity searches.
"""

import re
import logging
import faiss
from typing import List, Dict, Optional, Tuple
from langchain_community.vectorstores import FAISS
from langchain_community.docstore.in_memory import InMemoryDocstore
from langchain_core.documents import Document
from langchain_core.embeddings import Embeddings

from Utils.embedding_utils import check_text_size_before_embedding
from common.constants import REGEX_PATTERNS

logger = logging.getLogger(__name__)


class VectorStoreService:
    """Service for managing vector store operations"""
    
    def create_vector_store(self, documents: List[str], embedding_llm: Embeddings) -> FAISS:
        """Create a vector store from documents"""
        return FAISS.from_texts(documents, embedding_llm)
    
    def create_known_issues_vector_store(self, known_issues: List[str], embedding_llm: Embeddings) -> FAISS:
        """
        Create a FAISS vector database for known issues.
        If there are error traces in the known_issues, it creates a populated FAISS database.
        Otherwise, it returns an empty FAISS database.
        """
        metadata_list, error_trace_list = self._extract_metadata_from_known_false_positives(
            known_issues, embedding_llm
        )
        
        if not error_trace_list:
            logger.info("Note: No known issues were found. The investigation will be based solely on the source code.")
            # Create an empty FAISS index
            embedding_dimension = len(embedding_llm.embed_query("dummy"))
            empty_index = faiss.IndexFlatL2(embedding_dimension)
            
            # Create an empty FAISS vector store
            return FAISS(
                embedding_function=embedding_llm,
                index=empty_index,
                docstore=InMemoryDocstore(),
                index_to_docstore_id={}   
            )
        else:
            return FAISS.from_texts(
                texts=error_trace_list, 
                embedding=embedding_llm, 
                metadatas=metadata_list
            )
    
    def similarity_search(self, vector_store: FAISS, query: str, k: int, 
                         filter_criteria: Optional[Dict] = None) -> List[Document]:
        """Perform similarity search in vector store"""
        retriever = vector_store.as_retriever(
            search_kwargs={"k": k, 'filter': filter_criteria} if filter_criteria else {"k": k}
        )
        return retriever.invoke(query)
    
    def _extract_metadata_from_known_false_positives(self, known_issues_list: List[str], 
                                                   embedding_llm: Embeddings) -> Tuple[List[Dict], List[str]]:
        """
        Extract metadata and error traces from known false positives.
        
        Returns:
            tuple: A tuple containing:
                - metadata_list (list[dict]): List of metadata dictionaries
                - error_trace_list (list[str]): List of known issues
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

                match_cwe = re.search(f"({REGEX_PATTERNS['CWE_PATTERN']})", lines[0])
                cwe_str = match_cwe.group(1) if match_cwe else None

                # Extract the lines after the error trace as 'reason_of_false_positive'
                reason_start_line_index = len(lines) - 1
                code_block_line_pattern = re.compile(r'#\s*\d+\|')
                path_line_pattern = re.compile(r'^(.+/)+(.+):(\d+):\s?(.*)')
                
                for line_index in range(len(lines)-1, -1, -1):
                    if (code_block_line_pattern.match(lines[line_index].strip()) or 
                        path_line_pattern.match(lines[line_index].strip())):
                        reason_start_line_index = line_index + 1
                        break
                
                reason_lines = [line.lstrip('#').strip() for line in lines[reason_start_line_index:] if line.strip()]
                reason_of_false_positive = "\n".join(reason_lines)

                metadata_list.append({
                    "reason_of_false_positive": reason_of_false_positive,
                    "issue_type": issue_type,
                    "issue_cwe": cwe_str
                })
                
                error_trace = "\n".join(lines[1:reason_start_line_index])
                check_text_size_before_embedding(error_trace, embedding_llm.model)
                error_trace_list.append(error_trace)
                
            except Exception as e:
                logger.error(f"Error occurred during process this known issue: {item}\nError: {e}")
                raise e

        return metadata_list, error_trace_list 