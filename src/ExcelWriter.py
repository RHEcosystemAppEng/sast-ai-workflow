import os
import sys
from decimal import Decimal

import xlsxwriter
import math
from tqdm import tqdm

from tornado.gen import sleep


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

def get_numeric_value(value):
    return 0 if math.isnan(value) or math.isinf(value) else value

def get_percentage_value(n):
    from decimal import Decimal
    n = get_numeric_value(n)
    n = n if isinstance(n, Decimal) else Decimal(str(n))
    return round(n, 2) * 100

def write_confusion_matrix_worksheet(data, workbook):

    def cell_formatting(color):
        return workbook.add_format({
            "bold": 1,
            "border": 1,
            "align": "center",
            "valign": "vcenter",
            "fg_color": color,
        })

    worksheet = workbook.add_worksheet("Confusion Matrix")
    worksheet.set_column("A:B", 20)
    worksheet.set_column("C:D", 40)
    for idx in range(5):
        worksheet.set_row(idx + 4, 30)

    worksheet.merge_range("A1:B1", "Human Results", cell_formatting("#4f8df1"))
    worksheet.write(1, 0, 'Actual Positives', cell_formatting('#bfbfbf'))
    worksheet.write(2, 0, 'Actual Negatives', cell_formatting('#bfbfbf'))

    worksheet.merge_range("A8:A9", "Human Results", cell_formatting("#00b903"))
    worksheet.write(7, 1, 'Actual Positives', cell_formatting('#bfbfbf'))
    worksheet.write(8, 1, 'Actual Negatives', cell_formatting('#bfbfbf'))

    ai_counts = count_predicted_values(data)
    worksheet.merge_range("C1:D1", "AI Results", cell_formatting("#4f8df1"))
    worksheet.write(1, 2, 'Predicted Positives', cell_formatting('#bfbfbf'))
    worksheet.write(1, 3, len(ai_counts[0]), cell_formatting('#ffffff'))
    worksheet.write(2, 2, 'Predicted Negatives', cell_formatting('#bfbfbf'))
    worksheet.write(2, 3, len(ai_counts[1]), cell_formatting('#ffffff'))

    worksheet.merge_range("C6:D6", "AI Results", cell_formatting("#4f8df1"))
    worksheet.write(6, 2, 'Predicted Positives', cell_formatting('#bfbfbf'))
    worksheet.write(6, 3, 'Predicted Negatives', cell_formatting('#bfbfbf'))



def count_predicted_values(data):
    positives = []
    negatives = []
    for (issue_id, llm_text, metric_ar) in data:
        if "not a false positive" in str(llm_text).lower():
            negatives.append(issue_id)
        else:
            positives.append(issue_id)
    return positives, negatives

def count_actual_values(data):
    positives = []
    negatives = []
    for (issue_id, llm_text, metric_ar) in data:
        if "not a false positive" in str(llm_text).lower():
            negatives.append(issue_id)
        else:
            positives.append(issue_id)
    return positives, negatives
