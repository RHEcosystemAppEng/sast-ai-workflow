from bs4 import BeautifulSoup
from typing import List


class Issue:
    def __init__(self, issue_id):
        self.id = issue_id
        self.issue_type = ''
        self.issue_name = ''
        self.issue_label = ''
        self.issue_cve = ''
        self.issue_cve_link = ''
        self.trace = ''

    def __repr__(self):
        return (f"id ={self.id}\n"
                f"type ={self.issue_type}\n"
                f"name ={self.issue_name}\n"
                f"label ={self.issue_label}\n"
                f"cve ={self.issue_cve}\n"
                f"URL ={self.issue_cve_link}\n"
                f"Trace ={self.trace}")


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



