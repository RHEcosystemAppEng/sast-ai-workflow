import os
import sys
import xlsxwriter
from tqdm import tqdm

from tornado.gen import sleep


def write_to_excel_file(data):
    filename = os.getenv("OUTPUT_FILE_PATH")
    print(f" Writing to {filename} ".center(80, '*'))
    workbook = xlsxwriter.Workbook(filename)
    worksheet = workbook.add_worksheet()
    worksheet.set_column(1, 1, 25)
    worksheet.set_column(2, 4, 40)
    worksheet.set_column(5, 6, 25)
    header_data = ['Issue ID', 'Issue Name', 'Error', 'AI response', 'Answer Relevancy', 'Faithfulness']
    header_format = workbook.add_format({'bold': True,
                                         'bottom': 2,
                                         'bg_color': '#73cc82'})
    for col_num, h in enumerate(header_data):
        worksheet.write(0, col_num, h, header_format)

    with tqdm(total=len(data), file=sys.stdout, desc="Writing to " + filename + ": ") as pbar:

        for idx, (issue, summary_info)  in enumerate(data):
            worksheet.write(idx + 1, 0, issue.id)
            worksheet.write(idx + 1, 1, issue.issue_name)
            worksheet.write(idx + 1, 2, issue.trace)
            worksheet.write(idx + 1, 3, summary_info.llm_response, workbook.add_format({'text_wrap': True}))
            worksheet.write(idx + 1, 4, summary_info.metrics['answer_relevancy'])
            worksheet.write(idx + 1, 5, summary_info.metrics['faithfulness'])

            pbar.update(1)
            sleep(1)

    workbook.close()
