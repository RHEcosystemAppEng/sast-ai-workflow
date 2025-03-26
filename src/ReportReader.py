import re

from bs4 import BeautifulSoup
from typing import List

from model.Issue import Issue


def read_sast_report_html(file_path) -> List[Issue]:
    issue_list = []
    print(f"Reading => {file_path}")
    with open(file_path, "r", encoding='utf-8') as f:
        soup = BeautifulSoup(f.read(), 'html.parser')
        all_pre_tags = soup.findAll('pre')
        cur_issue = Issue(-1)
        for tag in all_pre_tags[0].children:
            if tag.name == 'a' and tag.has_attr('id'):
                if cur_issue.id != -1:
                    issue_list.append(cur_issue)
                cur_issue = Issue(tag['id'])
            else:
                if tag.name == 'b' and tag.find('span') and tag.find('a'):
                    try:
                        cur_issue.issue_type = tag.text.split(':')[0]
                        cur_issue.issue_name = tag.find('span').text
                        cur_issue.issue_cve = tag.find('a').text
                        cur_issue.issue_cve_link = tag.find('a')['href']
                    except AttributeError:
                        print(f"Exception when parsing tag: {tag}")
                else:
                    cur_issue.trace += tag.text

    return issue_list


def get_report_project_info(file_path: str) -> tuple[(str, str)]:
    with open(file_path, "r", encoding='utf-8') as f:
        soup = BeautifulSoup(f.read(), 'html.parser')
        h1_tag = soup.find("h1")
        if not h1_tag:
            raise ValueError("No <h1> tag found in input report html")
        
        pkg_str = h1_tag.text
        match = match = re.match(r'^(.*)-(\d[\w\.]*-\d+)(?:[._].*)?$', pkg_str)
        if not match:
            raise ValueError(f"Could not identify target project's package string. Provided string: {pkg_str}")
        
        return match.groups()



