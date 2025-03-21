import requests
from bs4 import BeautifulSoup
import requests
from bs4.element import Comment
from Utils.text_processing_utils import create_text_splitter 

def read_cve_html_file(path, config=None):
    text_splitter = create_text_splitter(config)
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