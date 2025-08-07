import logging
import os
import re
from typing import List

from bs4 import BeautifulSoup

from common.config import Config
from dto.Issue import Issue
from report_readers.base_reader import BaseReportReader

logger = logging.getLogger(__name__)


class HtmlReportReader(BaseReportReader):
    """
    Reader for Local HTML SAST report files.
    """

    def can_handle(self, file_path: str, config: Config) -> bool:
        """
        Check if the provided file_path is an HTML file.
        """
        try:
            # Support only local files
            if file_path.startswith("https://") or file_path.startswith("http://"):
                return False

            # Check file extension
            if not file_path.lower().endswith((".html", ".htm")):
                return False

            # Check if file exists
            return os.path.exists(file_path)

        except Exception as e:
            logger.debug(f"Error checking HTML file: {e}")
            return False

    def read_report(self, file_path: str, config: Config) -> List[Issue]:
        """
        Read and parse HTML SAST report file.
        """
        logger.info(f"Reading HTML report from: {file_path}")

        try:
            return self._read_sast_report_local_html(file_path)
        except Exception as e:
            logger.error(f"Error reading HTML report {file_path}: {e}")
            raise

    def _read_sast_report_local_html(self, file_path: str) -> List[Issue]:
        """
        Parse HTML SAST report file.
        """
        issue_list = []

        # Check if file exists
        if not os.path.exists(file_path):
            logger.error(f"File not found: {file_path}")
            raise FileNotFoundError(f"File not found: {file_path}")

        with open(file_path, "r", encoding="utf-8") as f:
            content = f.read()

            # Check for empty file
            if len(content.strip()) == 0:
                logger.warning(f"Empty HTML file detected: {file_path}")
                return []

            try:
                soup = BeautifulSoup(content, "html.parser")
            except Exception as e:
                logger.error(f"BeautifulSoup parsing failed for {file_path}: {e}")
                raise ValueError(f"HTML parsing failed: {e}")

            all_pre_tags = soup.find_all("pre")

            # Check if no <pre> tags found (corrupted/invalid structure)
            if not all_pre_tags:
                logger.warning(
                    f"No <pre> tags found in HTML file - possibly corrupted: {file_path}"
                )
                return []

            cur_issue = None
            tags_processed = 0
            valid_tags_found = 0

            for tag in all_pre_tags[0].children:
                tags_processed += 1

                if tag.name == "a" and tag.has_attr("id"):
                    valid_tags_found += 1
                    if cur_issue is not None:
                        # Clean the first line of the trace before adding the issue
                        cur_issue.trace = self._clean_first_line(cur_issue.trace)
                        # Normalize trailing whitespace for consistency
                        cur_issue.trace = cur_issue.trace.rstrip()
                        issue_list.append(cur_issue)
                    cur_issue = Issue(id=tag["id"])
                else:
                    if tag.name == "b" and tag.find("span") and tag.find("a"):
                        valid_tags_found += 1
                        if cur_issue is not None:
                            try:
                                cur_issue.issue_type = tag.find("span").text
                                cur_issue.issue_cwe = tag.find("a").text
                                cur_issue.issue_cwe_link = tag.find("a")["href"]
                            except AttributeError:
                                logger.error(f"Exception when parsing tag: {tag}")
                    else:
                        if cur_issue is not None:
                            cur_issue.trace += tag.text

            # Add the last issue if it exists
            if cur_issue is not None:
                # Clean the first line of the trace before adding the issue
                cur_issue.trace = self._clean_first_line(cur_issue.trace)
                # Normalize trailing whitespace for consistency
                cur_issue.trace = cur_issue.trace.rstrip()
                issue_list.append(cur_issue)

            # Log if very few valid tags were found (possible corruption)
            if tags_processed > 0 and valid_tags_found == 0:
                logger.warning(
                    f"No valid SAST tags found in {tags_processed} "
                    f"processed tags - file may be corrupted: {file_path}"
                )
            elif tags_processed > 10 and valid_tags_found < (tags_processed * 0.1):
                logger.warning(
                    f"Very few valid SAST tags found ({valid_tags_found}/{tags_processed}) "
                    f"- file may be corrupted: {file_path}"
                )

        logger.info(f"Successfully parsed {len(issue_list)} issues from HTML file")
        return issue_list

    def _clean_first_line(self, text: str) -> str:
        """
        Remove the first line if it contains unwanted patterns.
        Removes entire first line if it contains [#def1], [important], etc.
        """
        if not text:
            return text

        lines = text.split("\n")
        if lines:
            first_line = lines[0]

            # Check if first line contains unwanted patterns
            if re.search(r"\[#def\d+\]", first_line) or re.search(r"\[important\]", first_line):
                # Remove the first line entirely
                lines = lines[1:]

            return "\n".join(lines)

        return text
