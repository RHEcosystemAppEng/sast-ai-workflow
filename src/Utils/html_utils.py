import requests
from bs4 import BeautifulSoup
import requests
from bs4.element import Comment
import textwrap
from Utils.text_processing_utils import create_text_splitter 


def format_cwe_context(raw_text_chunks, line_width=80):
    """
    Takes a list of raw text chunks (strings) and returns a nicely formatted
    plain text context suitable for handing off to the LLM.
    """
    wrapped_chunks = []
    for chunk in raw_text_chunks:
        wrapped = textwrap.fill(chunk, width=line_width)
        wrapped_chunks.append(wrapped)

    combined_text = "\n\n".join(wrapped_chunks)  # ensures blank line between chunks
    
    formatted_text = f"=== CWE Context Start ===\n{combined_text}\n=== CWE Context End ==="
    return formatted_text

def read_cve_html_file(path, config=None):
    text_splitter = create_text_splitter(config)
    res = requests.get(path)
    soup = BeautifulSoup(res.content, 'html.parser')

    tags_to_collect = [
        "Description",
        "Demonstrative_Examples",
        "Observed_Examples",
        " Weakness_Ordinalities",
        "Detection_Methods",
        "Affected_Resources"
    ]

    doc_text_chunks = []
    for tag in tags_to_collect:
        div = soup.find("div", {"id": tag})
        if not div:
            continue
        
        raw_strings = div.find_all(string=True)
        visible_text = list(filter(remove_html_tags, raw_strings))
        visible_text = [t.strip() for t in visible_text if t.strip()]
        if not visible_text:
            continue
        
        section_text = " ".join(visible_text)
        titled_section_text = f"{tag}:\n\n{section_text}"
        section_chunks = text_splitter.split_text(titled_section_text)
        doc_text_chunks.extend(section_chunks)

        formatted_cwe_texts = format_cwe_context(doc_text_chunks)

    return formatted_cwe_texts

def remove_html_tags(element):
    if element.parent.name in ['style', 'script', 'head', 'title', 'meta', '[document]']:
        return False
    if isinstance(element, Comment):
        return False
    return True

def text_from_html(body):
    soup = BeautifulSoup(body, 'html.parser')
    texts = soup.findAll(string=True)
    visible_texts = filter(remove_html_tags, texts)
    return u" ".join(t.strip() for t in visible_texts)