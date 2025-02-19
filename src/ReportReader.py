from bs4 import BeautifulSoup
from typing import List

from src.model.Issue import Issue


def read_sast_report_html(file_path) -> List[Issue]:
    issue_list = []
    with open(file_path, "r", encoding='utf-8') as f:
        soup = BeautifulSoup(f.read(), 'html.parser')
        all_pre_tags = soup.findAll('pre')
        cur_issue = Issue(-1)
        for tag in all_pre_tags[0].children:
            if tag.name == 'a' and tag.has_attr('id'):
                # print("Found an <a> tag:", tag)
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
                        print("An exception occurred when trying to parse the issue subject line")
                else:
                    cur_issue.trace += tag.text

    return issue_list



