import faiss
import re
import logging
from typing import List, Tuple, Dict
from langchain_openai import OpenAIEmbeddings
from langchain_community.vectorstores import FAISS
from langchain_community.docstore.in_memory import InMemoryDocstore

from Utils.embedding_utils import check_text_size_before_embedding

logger = logging.getLogger(__name__)


class VectorStoreService:
    """Service for handling vector store operations."""
    
    def __init__(self, embedding_llm: OpenAIEmbeddings, embedding_model_name: str):
        self.embedding_llm = embedding_llm
        self.embedding_model_name = embedding_model_name
    
    def create_vector_store(self, text_data: List[str]) -> FAISS:
        """Create FAISS vector store from text data."""
        return FAISS.from_texts(text_data, self.embedding_llm)
    
    def create_known_issues_vector_store(self, known_issues_data: List[str]) -> FAISS:
        """
        Create a FAISS vector database for known issues.
        
        If there are error traces in the known_issues_data, it creates a populated FAISS database.
        Otherwise, it returns an empty FAISS database.
        """
        metadata_list, error_trace_list = self._extract_metadata_from_known_false_positives(known_issues_data)
        
        if not error_trace_list:
            logger.info("Note: No known issues were found. The investigation will be based solely on the source code.")
            return self._create_empty_vector_store()
        else:
            return FAISS.from_texts(
                texts=error_trace_list, 
                embedding=self.embedding_llm, 
                metadatas=metadata_list
            )
    
    def _create_empty_vector_store(self) -> FAISS:
        """Create an empty FAISS vector store."""
        # The dimension of the index must match the embedding model's output dimension.
        # We get this by embedding a dummy text.
        embedding_dimension = len(self.embedding_llm.embed_query("dummy"))
        empty_index = faiss.IndexFlatL2(embedding_dimension)
        
        # Create an empty FAISS vector store
        return FAISS(
            embedding_function=self.embedding_llm,
            index=empty_index,
            docstore=InMemoryDocstore(),
            index_to_docstore_id={}   
        )
    
    def _extract_metadata_from_known_false_positives(self, known_issues_list: List[str]) -> Tuple[List[Dict], List[str]]:
        """
        Extract metadata and error traces from known false positives.
        
        Returns:
            tuple: A tuple containing:
                - metadata_list (List[Dict]): List of metadata dictionaries, indicating for each issue 
                  the issue type and the reason it is marked as False Positive.
                - error_trace_list (List[str]): List of known issues.
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
                    "issue_type": issue_type
                })
                
                error_trace = "\n".join(lines[:reason_start_line_index])
                check_text_size_before_embedding(error_trace, self.embedding_model_name)
                error_trace_list.append(error_trace)
                
            except Exception as e:
                logger.error(f"Error occurred during process this known issue: {item}\nError: {e}")
                raise e

        return metadata_list, error_trace_list 