import sys
import xlsxwriter
from tqdm import tqdm

from tornado.gen import sleep


def write_to_excel_file(data):
    filename = 'sast-ai-generated-report.xlsx'
    print(f" Writing to {filename} ".center(80, '*'))
    workbook = xlsxwriter.Workbook(filename)
    worksheet = workbook.add_worksheet()
    worksheet.set_column(1, 1, 25)
    worksheet.set_column(2, 4, 40)
    header_data = ['Issue ID', 'Issue Name', 'Error', 'AI response']
    header_format = workbook.add_format({'bold': True,
                                         'bottom': 2,
                                         'bg_color': '#73cc82'})
    for col_num, h in enumerate(header_data):
        worksheet.write(0, col_num, h, header_format)

    # Start from the first cell.
    # Rows and columns are zero indexed.
    row = 1

    with tqdm(total=len(data), file=sys.stdout, desc="Writing to " + filename + ": ") as pbar:

        for item in data:
            worksheet.write(row, 0, item[0].id)
            worksheet.write(row, 1, item[0].issue_name)
            worksheet.write(row, 2, item[0].trace)
            worksheet.write(row, 3, item[1], workbook.add_format({'text_wrap': True}))

            row += 1
            pbar.update(1)
            sleep(1)

    workbook.close()
