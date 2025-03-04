import glob
import os
import re
import sys

import git
import requests
import torch
from bs4 import BeautifulSoup
from bs4.element import Comment
from langchain.text_splitter import RecursiveCharacterTextSplitter, Language


def get_device():
    if sys.platform == "darwin":
        return "mps" if torch.backends.mps.is_available() else "cpu"

    return "cuda" if torch.cuda.is_available() else "cpu"

def download_repo(repo_url):
    try:
        # Identify if the URL has a branch or tag with "/tree/"
        if "/tree/" in repo_url:
            # Split URL to separate repository URL and branch/tag
            repo_url, branch_or_tag = re.split(r'/tree/', repo_url, maxsplit=1)
        else:
            branch_or_tag = None

        # Extract the project name (the last part before "/tree/")
        repo_name = repo_url.rstrip('/').split('/')[-1]
        if repo_name.endswith('.git'):
            repo_name = repo_name[:-4]

        # Set the destination path to the current directory
        destination_path = os.path.join(os.getcwd(), repo_name)

        # Clone the repo
        print(f"Cloning {repo_url} into {destination_path}...")
        repo = git.Repo.clone_from(repo_url, destination_path)

        # Checkout the specified branch or tag if provided
        if branch_or_tag:
            print(f"Checking out {branch_or_tag}...")
            repo.git.checkout(branch_or_tag)

        print("Repository cloned successfully!")

    except Exception as e:
        print(f"An error occurred: {e}")


def create_embeddings_for_all_project_files():
    res_list = []
    # reading project src folder
    src_dir_path = os.path.join(os.getcwd(), "systemd-rhel9/src/")
    count = 0
    for src_filename in glob.iglob(src_dir_path + '/**/**', recursive=True):

        if (src_filename.endswith(".c") or src_filename.endswith(".h")) and os.path.isfile(src_filename):
            count = count + 1
            for k in read_source_code_file(src_filename):
                res_list.append(k.page_content)  # adding source code file as text to embeddings
    print(f"Total files: {count}")
    return res_list

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


def read_source_code_file(path):
    with open(path, "r", encoding='utf-8') as f:
        text_splitter = RecursiveCharacterTextSplitter.from_language(language=Language.C,
                                                                     chunk_size=100, chunk_overlap=0)
        plain_text = f.read()
        doc_list = text_splitter.create_documents([plain_text])
        return doc_list


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


def extract_file_path(input_str):
    # Using regex to extract only the file path
    match = re.search(r"(src/[^:]+):(\d+):(\d+):", input_str)

    if match:
        file_path = match.group(1)
        return file_path
    else:
        print("No match found.")

def read_known_errors_file(path):
    with open(path, "r", encoding='utf-8') as f:
        text_splitter = RecursiveCharacterTextSplitter(separators=["\n\n", "\n"],
                                                       chunk_size=500, chunk_overlap=0)
        plain_text = f.read()
        doc_list = text_splitter.create_documents([plain_text])
        return doc_list
    
