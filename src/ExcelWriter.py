import os
import sys

import xlsxwriter
from tqdm import tqdm

from tornado.gen import sleep

from Utils.ExcelWriter_utils import (
    cell_formatting,
    get_percentage_value,
    count_predicted_values,
    count_actual_values,
    get_human_verified_results,
    calculate_confusion_matrix_metrics
)


def write_to_excel_file(data):
    filename = os.getenv("OUTPUT_FILE_PATH")
    print(f" Writing to {filename} ".center(80, '*'))
    with tqdm(total=len(data), file=sys.stdout, desc="Writing to " + filename + ": ") as pbar:
        workbook = xlsxwriter.Workbook(filename)

        summary = write_ai_report_worksheet(data, workbook)
        write_confusion_matrix_worksheet(summary, workbook)
        workbook.close()

        pbar.update(1)
        sleep(1)

def write_ai_report_worksheet(data, workbook):
    worksheet = workbook.add_worksheet("AI report")
    worksheet.set_column(1, 1, 25)
    worksheet.set_column(2, 4, 40)
    worksheet.set_column(5, 6, 25)
    header_data = ['Issue ID', 'Issue Name', 'Error', 'AI response', 'Answer Relevancy']
    header_format = workbook.add_format({'bold': True,
                                         'bottom': 2,
                                         'bg_color': '#73cc82'})
    for col_num, h in enumerate(header_data):
        worksheet.write(0, col_num, h, header_format)

    summary = []

    for idx, (issue, summary_info) in enumerate(data):
        worksheet.write(idx + 1, 0, issue.id)
        worksheet.write(idx + 1, 1, issue.issue_name)
        worksheet.write(idx + 1, 2, issue.trace)
        worksheet.write(idx + 1, 3, summary_info.llm_response, workbook.add_format({'text_wrap': True}))

        ar = get_percentage_value(summary_info.metrics['answer_relevancy'])
        summary.append((issue.id, summary_info.llm_response, ar))
        worksheet.write(idx + 1, 4, f"{ar}%",
                        workbook.add_format({'border': 2, 'bg_color': '#f1541e' if ar < 50 else '#00d224'}))
    return summary

def write_results_table(workbook, worksheet, actual_positives, actual_negatives, predicted_positives, predicted_negatives):
    worksheet.merge_range("A1:B1", "Human Results", cell_formatting(workbook, "#4f8df1"))
    worksheet.write(1, 0, 'Actual Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(1, 1, len(actual_positives), cell_formatting(workbook, '#ffffff'))
    worksheet.write(2, 0, 'Actual Negatives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(2, 1, len(actual_negatives), cell_formatting(workbook, '#ffffff'))

    worksheet.merge_range("C1:D1", "AI Results", cell_formatting(workbook, "#4f8df1"))
    worksheet.write(1, 2, 'Predicted Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(1, 3, len(predicted_positives), cell_formatting(workbook, '#ffffff'))
    worksheet.write(2, 2, 'Predicted Negatives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(2, 3, len(predicted_negatives), cell_formatting(workbook, '#ffffff'))

def write_confusion_matrix(workbook, worksheet, tp, tn, fp, fn):
    worksheet.merge_range("A8:A9", "Human Results", cell_formatting(workbook, "#00b903"))
    worksheet.write(7, 1, 'Actual Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(7, 2, tp, cell_formatting(workbook, '#28A745'))
    worksheet.write(8, 1, 'Actual Negatives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(8, 2, fp, cell_formatting(workbook, '#FF0000'))

    worksheet.merge_range("C6:D6", "AI Results", cell_formatting(workbook, "#4f8df1"))
    worksheet.write(6, 2, 'Predicted Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(7, 3, fn, cell_formatting(workbook, '#FF0000'))
    worksheet.write(6, 3, 'Predicted Negatives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(8, 3, tn, cell_formatting(workbook, '#28A745'))

def write_model_performance(workbook, worksheet, tp, tn, fp, fn, METRICS_START_ROW):
    EPSILON = 1e-11 
    accuracy = (tp + tn) / (tp + tn + fp + fn + EPSILON)
    recall = tp / (tp + fn + EPSILON)
    precision = tp / (tp + fp + EPSILON)
    f1_score = 2 * precision * recall / (precision + recall + EPSILON)

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
    worksheet.write(KEY_START_ROW + 1, 0, "Actual Positives =", workbook.add_format({"bold": 1}))
    worksheet.write(KEY_START_ROW + 1, 1, "Real Problem", workbook.add_format({"italic": 1}))

    worksheet.write(KEY_START_ROW + 2, 0, "Actual Negatives =", workbook.add_format({"bold": 1}))
    worksheet.write(KEY_START_ROW + 2, 1, "Not a Real Problem", workbook.add_format({"italic": 1}))

    worksheet.write(KEY_START_ROW + 3, 0, "Predicted Positives =", workbook.add_format({"bold": 1}))
    worksheet.write(KEY_START_ROW + 3, 1, "AI Predicted as a Problem", workbook.add_format({"italic": 1}))

    worksheet.write(KEY_START_ROW + 4, 0, "Predicted Negatives =", workbook.add_format({"bold": 1}))
    worksheet.write(KEY_START_ROW + 4, 1, "AI Predicted as Not a Problem", workbook.add_format({"italic": 1}))

def write_confusion_matrix_worksheet(data, workbook):    
    METRICS_START_ROW = 11
    KEY_START_ROW = 18
      
    worksheet = workbook.add_worksheet("Confusion Matrix")
    worksheet.set_column("A:B", 20)
    worksheet.set_column("C:D", 40)

    for idx in range(5):
        worksheet.set_row(idx + 4, 30)

    ground_truth = get_human_verified_results() 
    actual_positives, actual_negatives = count_actual_values(data, ground_truth)
    predicted_positives, predicted_negatives = count_predicted_values(data)
    tp, tn, fp, fn = calculate_confusion_matrix_metrics(actual_positives, actual_negatives, predicted_positives, predicted_negatives)

    write_results_table(workbook, worksheet, actual_positives, actual_negatives, predicted_positives, predicted_negatives)
    write_confusion_matrix(workbook, worksheet, tp, tn, fp, fn)
    write_model_performance(workbook, worksheet, tp, tn, fp, fn, METRICS_START_ROW)
    write_table_key(workbook, worksheet, KEY_START_ROW)

 