from langchain.text_splitter import RecursiveCharacterTextSplitter

def create_text_splitter(config=None):
    # Provide default values if config is not given or keys are missing
    chunk_size = config.get("CHUNK_SIZE", 500) if config else 500
    chunk_overlap = config.get("CHUNK_OVERLAP", 0) if config else 0
    separators = config.get("CHUNK_SEPARATORS", ["\n\n", "\n", ".", ";", ",", " ", ""]) if config else ["\n\n", "\n", ".", ";", ",", " ", ""]
    
    return RecursiveCharacterTextSplitter(
        separators=separators,
        chunk_size=chunk_size,
        chunk_overlap=chunk_overlap
    )
