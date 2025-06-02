import sys

import xlsxwriter
from tqdm import tqdm
import gspread
from oauth2client.service_account import ServiceAccountCredentials
from tornado.gen import sleep
from datetime import datetime

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

def write_summary_results_to_aggregate_google_sheet(config:Config, evaluation_summary:EvaluationSummary):
    # Define the scope for Google Sheets API
    scope = ['https://spreadsheets.google.com/feeds', 'https://www.googleapis.com/auth/drive']
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

    try:
        # Authenticate using the service account JSON file
        credentials = ServiceAccountCredentials.from_json_keyfile_name(config.SERVICE_ACCOUNT_JSON_PATH, scope)
        client = gspread.authorize(credentials)

        sheet = client.open_by_url(config.AGGREGATE_RESULTS_G_SHEET).sheet1  # Assumes the data is in the first sheet

        # Append the row at the bottom of the sheet
        sheet.append_row(row_data, value_input_option='RAW')

        print("Results added successfully to aggregate Google Sheet.")
    except Exception as e:
        print(f"Failed to write results to aggregate Google Sheet ({config.AGGREGATE_RESULTS_G_SHEET}).\nError: {e}")
    
def write_ai_report_google_sheet(data, config:Config):
    header_data = ['AI prediction', 'Hint']
    # Define the scope for Google Sheets API
    scope = ['https://spreadsheets.google.com/feeds', 'https://www.googleapis.com/auth/drive']

    try:
        # Authenticate using the service account JSON file
        credentials = ServiceAccountCredentials.from_json_keyfile_name(config.SERVICE_ACCOUNT_JSON_PATH, scope)
        client = gspread.authorize(credentials)

        sheet = client.open_by_url(config.INPUT_REPORT_FILE_PATH).sheet1  # Assumes the data is in the first sheet

        sheet_data = sheet.get_all_values()
        num_rows = len(sheet_data)
        num_cols = len(sheet_data[0]) if num_rows > 0 else 0

        # Insert the headers in the next empty columns
        cell_range = gspread.utils.rowcol_to_a1(1, num_cols + 1) + ":" + gspread.utils.rowcol_to_a1(1, num_cols + len(header_data))
        sheet.update([header_data], cell_range)
        sheet.format(cell_range, {'textFormat': {'bold': True}})

        # Insert the LLM results to the new columns
        for row, (_, summary_info) in enumerate(data):
            sheet.update_cell(row + 2, num_cols + 1, summary_info.llm_response.investigation_result.title())  # row + 2 to skip header row
            sheet.update_cell(row + 2, num_cols + 2, summary_info.llm_response.short_justifications)

        print("Results added successfully to Google Sheet.")
    except Exception as e:
        print(f"Failed to write results to Google Sheet ({config.INPUT_REPORT_FILE_PATH}).\nError: {e}")

def write_ai_report_worksheet(data, workbook, config:Config):
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

 