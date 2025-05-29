import sys
import time

import xlsxwriter
from tqdm import tqdm
import gspread
from oauth2client.service_account import ServiceAccountCredentials
from tornado.gen import sleep
from datetime import datetime

from Utils.file_utils import get_google_sheet
from Utils.metrics_utils import get_metrics, get_percentage_value
from Utils.output_utils import cell_formatting
from common.config import Config
from dto.EvaluationSummary import EvaluationSummary



def write_to_excel_file(data:list, evaluation_summary:EvaluationSummary, config:Config):
    print(f" Writing to {config.OUTPUT_FILE_PATH} ".center(80, '*'))
    
    try:
        with tqdm(total=len(data), file=sys.stdout, desc="Writing to " + config.OUTPUT_FILE_PATH + ": ") as pbar:
            workbook = xlsxwriter.Workbook(config.OUTPUT_FILE_PATH)

            write_ai_report_worksheet(data, workbook, config)
            if config.INPUT_REPORT_FILE_PATH.startswith("https"):
                write_ai_report_google_sheet(data, config)
            write_confusion_matrix_worksheet(workbook, evaluation_summary)
            if config.AGGREGATE_RESULTS_G_SHEET:
                write_summary_results_to_aggregate_google_sheet(config, evaluation_summary)

            workbook.close()

            pbar.update(1)
            sleep(1)
    except Exception as e:
        print("Error occurred during Excel writing:", e)

def write_summary_results_to_aggregate_google_sheet(config:Config, evaluation_summary:EvaluationSummary) -> None:
    """
    Appends evaluation summary results to the Google Sheet defined in 'config.AGGREGATE_RESULTS_G_SHEET'.
    Includes a retry mechanism for API quota errors (status_code 429).

    Args:
        config: A Config object containing project settings, including the Google Sheet URL
                and service account credentials path.
        evaluation_summary: An EvaluationSummary object containing the metrics to be written.
    """
    # Prepare the row data to append
    nvr = config.PROJECT_NAME + "-" + config.PROJECT_VERSION
    date = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    row_data = [
        date,
        nvr,   # Package NVR (Name Version Release)
        evaluation_summary.tp,
        evaluation_summary.fp,
        evaluation_summary.tn,
        evaluation_summary.fn,
        evaluation_summary.accuracy,
        evaluation_summary.recall,
        evaluation_summary.precision,
        evaluation_summary.f1_score
    ]
    max_retries = 5
    delay = 30
    
    sheet = get_google_sheet(config.AGGREGATE_RESULTS_G_SHEET, config.SERVICE_ACCOUNT_JSON_PATH)
    if not sheet:
        return

    for attempt in range(max_retries):
        try:
            # Append the row at the bottom of the sheet
            sheet.append_row(row_data, value_input_option='RAW')
            print("Results added successfully to aggregate Google Sheet.")
            return
        except gspread.exceptions.APIError as e:
            if e.response.status_code == 429 and attempt < max_retries:
                print(f"Quota exceeded for Google Sheet ({config.AGGREGATE_RESULTS_G_SHEET}). Retrying in {delay:.2f} seconds... (Attempt {attempt}/{max_retries})")
                time.sleep(delay)
            else:
                print(f"Failed to write results to aggregate Google Sheet ({config.AGGREGATE_RESULTS_G_SHEET}) after {attempt + 1} attempts or due to non-retryable API error.\nError: {e}")
                return
        except Exception as e:
            print(f"An unexpected error occurred while performing sheet operations for ({config.AGGREGATE_RESULTS_G_SHEET}) on attempt {attempt + 1}.\nError: {e}")
            return

    print(f"Failed to write results to aggregate Google Sheet ({config.AGGREGATE_RESULTS_G_SHEET}) after {max_retries} retries for quota errors.")

    
def write_ai_report_google_sheet(data, config:Config):
    """
    This function updates a Google Sheet with AI analysis results (AI prediction and Hint only).
    Includes a retry mechanism for API quota errors (status_code 429).
    
    Note:
        The function writes all provided 'data' in order. It is not designed
        for partial data updates to an existing dataset in the sheet,
        as it may not align the rows of issues correctly with their investigation results.

    Args:
        data: A list of tuples, where each tuple contains summary information
              (e.g., (issue_object, summary_info)) from which LLM response details are extracted.
        config: A Config object containing settings, including the input report Google Sheet URL
                and service account credentials path.
    """
    header_data = ['AI prediction', 'Hint']
    max_retries = 5
    delay = 30
    
    sheet = get_google_sheet(config.INPUT_REPORT_FILE_PATH, config.SERVICE_ACCOUNT_JSON_PATH)
    if not sheet:
        return
    
    for attempt in range(max_retries):
        try:
            sheet_data = sheet.get_all_values()
            num_rows = len(sheet_data)
            num_cols = len(sheet_data[0]) if num_rows > 0 else 0
            current_headers = sheet_data[0] if num_rows > 0 else []

            # Try to find the first header of our header_data
            if header_data[0] in current_headers:
                start_col_for_data = current_headers.index(header_data[0]) + 1
                # Headers found, use existing column
                print(f"Found existing new headers ({header_data}) starting at column {start_col_for_data}.")
            else:
                # Insert the headers in the next empty columns
                cell_range = gspread.utils.rowcol_to_a1(1, num_cols + 1) + ":" + gspread.utils.rowcol_to_a1(1, num_cols + len(header_data))
                sheet.update([header_data], cell_range)
                start_col_for_data = num_cols + 1
                sheet.format(cell_range, {'textFormat': {'bold': True}})
                print(f"New headers ({header_data}) written successfully.")

            start_row_for_data = 2 # Assuming data starts from the second row (after headers)
            batch_update_data = []
            
            for (_, summary_info) in data:
                row_values = [
                    summary_info.llm_response.investigation_result.title(),
                    summary_info.llm_response.short_justifications
                ]
                batch_update_data.append(row_values)

            if batch_update_data:           
                # The 'update' method with a starting cell and a 2D array of values
                # will fill out from that starting cell.
                sheet.update(batch_update_data, f'{gspread.utils.rowcol_to_a1(start_row_for_data, start_col_for_data)}')

            print("Results added successfully to Google Sheet.")
            return

        except gspread.exceptions.APIError as e:
            if e.response.status_code == 429 and attempt < max_retries:
                print(f"Quota exceeded for Google Sheet ({config.INPUT_REPORT_FILE_PATH}). Retrying in {delay:.2f} seconds... (Attempt {attempt}/{max_retries})")
                time.sleep(delay)
            else:
                print(f"Failed to write results to Google Sheet ({config.INPUT_REPORT_FILE_PATH}) after {attempt + 1} attempts or due to non-retryable API error.\nError: {e}")
                return
        except Exception as e:
            print(f"An unexpected error occurred while performing sheet operations for ({config.INPUT_REPORT_FILE_PATH}) on attempt {attempt + 1}.\nError: {e}")
            return

    print(f"Failed to write results to Google Sheet ({config.INPUT_REPORT_FILE_PATH}) after {max_retries} retries for quota errors.")

def write_ai_report_worksheet(data, workbook, config:Config):
    """
    This function populates the sheet (loacl Excel file) with headers and rows detailing each analyzed
    issue
    Optionally, it includes "Critique Response" and "Context" columns based on the
    `config` settings.

    Args:
        data: A list of tuples, where each tuple is (issue_objuct, summary_info).
              `issue` contains issue details (id, issue_type, trace).
              `summary_info` contains LLM response, metrics, critique, and context.
        workbook: An XlsxWriter workbook object to which the new worksheet will be added.
        config: A Config object containing settings that may affect report columns
                (e.g., RUN_WITH_CRITIQUE, SHOW_FINAL_JUDGE_CONTEXT).
    """
    worksheet = workbook.add_worksheet("AI report")
    worksheet.set_column(1, 1, 25)
    worksheet.set_column(2, 4, 40)
    worksheet.set_column(5, 6, 25)
    cell_format = workbook.add_format({
        'valign': 'top',
        'text_wrap': True 
    })

    header_data = ['Issue ID', 'Issue Name', 'Error', 'Investigation Result', 'Hint', 'Justifications', 'Recommendations', 'Answer Relevancy']
    if config.RUN_WITH_CRITIQUE:
          header_data.append("Critique Response")
    if config.SHOW_FINAL_JUDGE_CONTEXT:
          header_data.append("Context")
    header_format = workbook.add_format({'bold': True,
                                         'bottom': 2,
                                         'bg_color': '#73cc82'})
    for col_num, h in enumerate(header_data):
        worksheet.write(0, col_num, h, header_format)

    for idx, (issue, summary_info) in enumerate(data):
        worksheet.write(idx + 1, 0, issue.id, cell_format)
        worksheet.write(idx + 1, 1, issue.issue_type, cell_format)
        worksheet.write(idx + 1, 2, issue.trace, cell_format)
        worksheet.write(idx + 1, 3, summary_info.llm_response.investigation_result, cell_format)
        worksheet.write(idx + 1, 4, summary_info.llm_response.short_justifications, cell_format)
        worksheet.write(idx + 1, 5, "\n\n".join(summary_info.llm_response.justifications), cell_format)
        worksheet.write(idx + 1, 6, "\n\n".join(summary_info.llm_response.recommendations), cell_format)

        ar = get_percentage_value(summary_info.metrics.get('answer_relevancy', 0))
        worksheet.write(idx + 1, 7, f"{ar}%",
                        workbook.add_format({'border': 2, 'bg_color': '#f1541e' if ar < 50 else '#00d224'}))
        dynumic_column = 7
        if config.RUN_WITH_CRITIQUE:
            dynumic_column += 1
            worksheet.write(idx + 1, dynumic_column, summary_info.critique_response, workbook.add_format({'text_wrap': True}))
        if config.SHOW_FINAL_JUDGE_CONTEXT:
            dynumic_column += 1
            worksheet.write(idx + 1, dynumic_column, str(summary_info.context).replace('\\n', '\n'), workbook.add_format({'text_wrap': True}))

def write_results_table(workbook, worksheet, evaluation_summary):
    worksheet.merge_range("A1:B1", "Human Results", cell_formatting(workbook, "#4f8df1"))
    worksheet.write(1, 0, 'Verified True Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(1, 1, len(evaluation_summary.actual_true_positives), cell_formatting(workbook, '#ffffff'))
    worksheet.write(2, 0, 'Verified False Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(2, 1, len(evaluation_summary.actual_false_positives), cell_formatting(workbook, '#ffffff'))

    worksheet.merge_range("C1:D1", "AI Results", cell_formatting(workbook, "#4f8df1"))
    worksheet.write(1, 2, 'Predicted True Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(1, 3, len(evaluation_summary.predicted_true_positives), cell_formatting(workbook, '#ffffff'))
    worksheet.write(2, 2, 'Predicted False Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(2, 3, len(evaluation_summary.predicted_false_positives), cell_formatting(workbook, '#ffffff'))

def write_confusion_matrix(workbook, worksheet, evaluation_summary):
    worksheet.merge_range("A8:A9", "Human Results", cell_formatting(workbook, "#00b903"))
    worksheet.write(7, 1, 'Verified False Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(7, 2, evaluation_summary.tp, cell_formatting(workbook, '#28A745'))
    worksheet.write(8, 1, 'Verified True Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(8, 2, evaluation_summary.fp, cell_formatting(workbook, '#FF0000'))

    worksheet.merge_range("C6:D6", "AI Results", cell_formatting(workbook, "#4f8df1"))
    worksheet.write(6, 2, 'Predicted False Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(7, 3, evaluation_summary.fn, cell_formatting(workbook, '#FF0000'))
    worksheet.write(6, 3, 'Predicted True Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(8, 3, evaluation_summary.tn, cell_formatting(workbook, '#28A745'))

def write_model_performance(workbook, worksheet, evaluation_summary, METRICS_START_ROW):
    accuracy, recall, precision, f1_score = get_metrics(        
        evaluation_summary.tp, 
        evaluation_summary.tn, 
        evaluation_summary.fp, 
        evaluation_summary.fn
        )

    worksheet.write(METRICS_START_ROW, 0, "Model's Performance:", workbook.add_format({"bold": 1}))
    worksheet.write(METRICS_START_ROW + 1, 0, "Accuracy =", workbook.add_format({"bold": 1}))
    worksheet.write(METRICS_START_ROW + 1, 1, accuracy, workbook.add_format({"italic": 1}))

    worksheet.write(METRICS_START_ROW + 2, 0, "Recall =", workbook.add_format({"bold": 1}))
    worksheet.write(METRICS_START_ROW + 2, 1, recall, workbook.add_format({"italic": 1}))

    worksheet.write(METRICS_START_ROW + 3, 0, "Precision =", workbook.add_format({"bold": 1}))
    worksheet.write(METRICS_START_ROW + 3, 1, precision, workbook.add_format({"italic": 1}))

    worksheet.write(METRICS_START_ROW + 4, 0, "F1 Score =", workbook.add_format({"bold": 1}))
    worksheet.write(METRICS_START_ROW + 4, 1, f1_score, workbook.add_format({"italic": 1}))

def write_table_key(workbook, worksheet, KEY_START_ROW):
    worksheet.write(KEY_START_ROW, 0, "Table Key:", workbook.add_format({"bold": 1}))
    worksheet.write(KEY_START_ROW + 1, 0, "Verified True Positives =", workbook.add_format({"bold": 1}))
    worksheet.write(KEY_START_ROW + 1, 1, "Human verified as a real issue (true positive)", workbook.add_format({"italic": 1}))

    worksheet.write(KEY_START_ROW + 2, 0, "Verified False Positives =", workbook.add_format({"bold": 1}))
    worksheet.write(KEY_START_ROW + 2, 1, "Human verified as not a real issue (false positive)", workbook.add_format({"italic": 1}))

    worksheet.write(KEY_START_ROW + 3, 0, "Predicted True Positives =", workbook.add_format({"bold": 1}))
    worksheet.write(KEY_START_ROW + 3, 1, "AI Predicted as a real issue (true positive)", workbook.add_format({"italic": 1}))

    worksheet.write(KEY_START_ROW + 4, 0, "Predicted False Positives =", workbook.add_format({"bold": 1}))
    worksheet.write(KEY_START_ROW + 4, 1, "AI Predicted as not a real issue (false positive)", workbook.add_format({"italic": 1}))

def write_confusion_matrix_worksheet(workbook, evaluation_summary):    
    METRICS_START_ROW = 11
    KEY_START_ROW = 18
      
    worksheet = workbook.add_worksheet("Confusion Matrix")
    worksheet.set_column("A:B", 30)
    worksheet.set_column("C:D", 40)

    for idx in range(5):
        worksheet.set_row(idx + 4, 30)
   
    write_results_table(workbook, worksheet, evaluation_summary)
    write_confusion_matrix(workbook, worksheet, evaluation_summary)
    write_model_performance(workbook, worksheet, evaluation_summary, METRICS_START_ROW)
    write_table_key(workbook, worksheet, KEY_START_ROW)

 