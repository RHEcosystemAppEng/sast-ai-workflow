import glob
import json
import logging
import os

import gspread
import pandas as pd
from langchain.text_splitter import Language, RecursiveCharacterTextSplitter
from oauth2client.service_account import ServiceAccountCredentials
from tenacity import retry, retry_if_exception_type, stop_after_attempt, wait_fixed

from common.config import Config
from common.constants import ALL_VALID_OPTIONS, KNOWN_FALSE_POSITIVE_ISSUE_SEPARATOR
from Utils.log_utils import log_attempt_number

logger = logging.getLogger(__name__)


def read_source_code_file(path):
    with open(path, "r", encoding="utf-8") as f:
        text_splitter = RecursiveCharacterTextSplitter.from_language(
            language=Language.C, chunk_size=100, chunk_overlap=0
        )
        plain_text = f.read()
        doc_list = text_splitter.create_documents([plain_text])
        return doc_list


def read_known_errors_file(path):
    with open(path, "r", encoding="utf-8") as f:
        plain_text = f.read()
        doc_list = [item.strip() for item in plain_text.split(KNOWN_FALSE_POSITIVE_ISSUE_SEPARATOR) if item.strip()!='']
        return doc_list


def get_human_verified_results(config: Config):
    if config.HUMAN_VERIFIED_FILE_PATH:
        return get_human_verified_results_local_excel(config.HUMAN_VERIFIED_FILE_PATH)
    elif config.SERVICE_ACCOUNT_JSON_PATH and config.INPUT_REPORT_FILE_PATH:
        return get_human_verified_results_google_sheet(
            config.SERVICE_ACCOUNT_JSON_PATH, config.INPUT_REPORT_FILE_PATH
        )
    return None


def get_human_verified_results_local_excel(filename):
    try:
        df = pd.read_excel(filename, header=get_header_row(filename))
    except Exception as e:
        raise ValueError(f"Failed to read Excel file at {filename}: {e}")

    df.columns = df.columns.str.strip().str.lower()
    expected_issue_id = "issue id"
    expected_false_positive = "false positive?"

    if expected_issue_id not in df.columns:
        raise KeyError(
            f"Expected column '{expected_issue_id}' not found in the file. \
                Found columns: {list(df.columns)}"
        )
    if expected_false_positive not in df.columns:
        raise KeyError(
            f"Expected column '{expected_false_positive}' not found in the file. \
                Found columns: {list(df.columns)}"
        )

    ground_truth = dict(zip(df[expected_issue_id], df[expected_false_positive]))
    logger.info(f"Successfully loaded ground truth from {filename}")
    return ground_truth


def get_human_verified_results_google_sheet(service_account_file_path, google_sheet_url):
    """
    Reads a Google Sheet and creates a ground truth dictionary,
    based on the 'False Positive?' column.

    NOTE: Assumes the data is in the first sheet (sheet name doesn't matter),
          and that if human-verified data is filled, it is filled for all rows.

    :param config: Config object containing configuration details, including:
        - INPUT_REPORT_FILE_PATH: URL of the Google Sheet.
        - SERVICE_ACCOUNT_JSON_PATH: Path to the service account JSON file for authentication.
     :return: Dictionary of ground truth with generated IDs (e.g., 'def1', 'def2', ...).
    """
    sheet = get_google_sheet(google_sheet_url, service_account_file_path, ignore_error=False)
    rows = sheet.get_all_records()

    # Create ground truth dict in case of human verified data already filled in the google sheet
    if rows[0].get("False Positive?"):
        ground_truth = {}
        for idx, row in enumerate(rows, start=1):  # start=1 to get def1, def2, ...
            is_false_positive = row.get("False Positive?", "").strip().lower()
            if is_false_positive.lower() not in ALL_VALID_OPTIONS:
                logger.warning(
                    f"Warning: def{idx} has invalid value '{is_false_positive}' \
                        in 'False Positive?' column."
                )

            ground_truth[f"def{idx}"] = is_false_positive

    else:
        ground_truth = None

    logger.info(f"Successfully loaded ground truth from {google_sheet_url}")
    return ground_truth


def read_all_source_code_files():
    res_list = []
    # reading project src folder
    src_dir_path = os.path.join(os.getcwd(), "systemd-rhel10/src/")
    count = 0
    for src_filename in glob.iglob(src_dir_path + "/**/**", recursive=True):

        if (src_filename.endswith(".c") or src_filename.endswith(".h")) and os.path.isfile(
            src_filename
        ):
            count = count + 1
            for k in read_source_code_file(src_filename):
                res_list.append(k.page_content)  # adding source code file as text to embeddings
    logger.info(f"Total files: {count}")
    return res_list


def read_answer_template_file(path):
    with open(path, "r", encoding="utf-8") as f:
        return f.read()


def get_header_row(filename):
    # Locate the header row containing 'Issue ID'
    preview = pd.read_excel(filename, header=None, nrows=5)
    header_row = next(
        (
            i
            for i, row in preview.iterrows()
            if any(str(cell).strip().lower() == "issue id" for cell in row.values)
        ),
        None,
    )
    return header_row


def load_json_file(file_path):
    with open(file_path, "r", encoding="utf-8") as f:
        return json.load(f)


@retry(
    stop=stop_after_attempt(5),
    wait=wait_fixed(30),
    retry=retry_if_exception_type(gspread.exceptions.APIError),
    before_sleep=log_attempt_number,
)
def get_google_sheet(
    sheet_url: str, service_account_json_path: str, ignore_error: bool = True
) -> None | gspread.Worksheet:
    """NOTE: Assumes the data is in the first sheet (sheet name doesn't matter)."""
    # Define the scope for Google Sheets API
    scope = ["https://spreadsheets.google.com/feeds", "https://www.googleapis.com/auth/drive"]
    try:
        # Authenticate using the service account JSON file
        credentials = ServiceAccountCredentials.from_json_keyfile_name(
            service_account_json_path, scope
        )
        client = gspread.authorize(credentials)
        sheet = client.open_by_url(sheet_url).sheet1  # Assumes the data is in the first sheet
        return sheet
    except Exception as e:
        logger.error(f"Failed to authenticate or open Google Sheet ({sheet_url}).\nError: {e}")
        if ignore_error:
            return None
        raise e
