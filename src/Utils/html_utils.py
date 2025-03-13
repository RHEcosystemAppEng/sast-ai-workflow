import requests
from bs4 import BeautifulSoup
import requests
from langchain.text_splitter import RecursiveCharacterTextSplitter, Language
from bs4.element import Comment


def read_html_file(path):
    text_splitter = RecursiveCharacterTextSplitter(separators=["\n\n", "\n", ".", ";", ",", " ", ""],
                                                   chunk_size=500, chunk_overlap=0)
    if path.strip().startswith("https://"):
        res = requests.get(path)
        doc_text = text_splitter.split_text(text_from_html(res.content))
        return doc_text
    else:
        with open(path, "r", encoding='utf-8') as f:
            doc_text = text_splitter.split_text(text_from_html(f.read()))
            return doc_text

def read_cve_html_file(path):
    text_splitter = RecursiveCharacterTextSplitter(separators=["\n\n", "\n", ".", ";", ",", " ", ""],
                                                   chunk_size=500, chunk_overlap=0)
    res = requests.get(path)
    soup = BeautifulSoup(res.content, 'html.parser')
    tags_to_collect = ["Description", "Alternate_Terms", "Common_Consequences", "Potential_Mitigations",
                       "Modes_Of_Introduction", "Likelihood_Of_Exploit", "Demonstrative_Examples",
                       "Observed_Examples", "Weakness_Ordinalities", "Detection_Methods", "Affected_Resources",
                       "Memberships", "Vulnerability_Mapping_Notes", "Taxonomy_Mappings"]
    visible_text_list = []
    for t in tags_to_collect:
        texts = soup.find("div", {"id": t})
        if texts is None:
            continue
        texts = texts.findAll(string=True)
        visible_text = list(filter(remove_html_tags, texts))
        for v in visible_text:
            visible_text_list.append(str(v.strip()))

    doc_text = text_splitter.split_text(" ".join(visible_text_list))
    return doc_text

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