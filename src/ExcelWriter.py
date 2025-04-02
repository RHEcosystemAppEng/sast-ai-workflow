import sys

import xlsxwriter
from tqdm import tqdm

from tornado.gen import sleep

from Utils.metrics_utils import get_metrics, get_percentage_value
from Utils.output_utils import cell_formatting
from common.config import Config
from model.EvaluationSummary import EvaluationSummary



def write_to_excel_file(data:list, evaluation_summary:EvaluationSummary, config:Config):
    print(f" Writing to {config.OUTPUT_FILE_PATH} ".center(80, '*'))
    
    try:
        with tqdm(total=len(data), file=sys.stdout, desc="Writing to " + config.OUTPUT_FILE_PATH + ": ") as pbar:
            workbook = xlsxwriter.Workbook(config.OUTPUT_FILE_PATH)

            write_ai_report_worksheet(data, workbook, config)
            write_confusion_matrix_worksheet(workbook, evaluation_summary)

            workbook.close()

            pbar.update(1)
            sleep(1)
    except Exception as e:
        print("Error occurred during Excel writing:", e)
    
def write_ai_report_worksheet(data, workbook, config:Config):
    worksheet = workbook.add_worksheet("AI report")
    worksheet.set_column(1, 1, 25)
    worksheet.set_column(2, 4, 40)
    worksheet.set_column(5, 6, 25)
    header_data = ['Issue ID', 'Issue Name', 'Error', 'AI response', 'Answer Relevancy']
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
        worksheet.write(idx + 1, 0, issue.id)
        worksheet.write(idx + 1, 1, issue.issue_type)
        worksheet.write(idx + 1, 2, issue.trace)
        worksheet.write(idx + 1, 3, summary_info.llm_response, workbook.add_format({'text_wrap': True}))

        ar = get_percentage_value(summary_info.metrics.get('answer_relevancy', 0))
        worksheet.write(idx + 1, 4, f"{ar}%",
                        workbook.add_format({'border': 2, 'bg_color': '#f1541e' if ar < 50 else '#00d224'}))
        if config.RUN_WITH_CRITIQUE:
            worksheet.write(idx + 1, 5, summary_info.critique_response, workbook.add_format({'text_wrap': True}))
        if config.SHOW_FINAL_JUDGE_CONTEXT:
            worksheet.write(idx + 1, 6, str(summary_info.context).replace('\\n', '\n'), workbook.add_format({'text_wrap': True}))

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
    worksheet.write(7, 1, 'Verified True Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(7, 2, evaluation_summary.tp, cell_formatting(workbook, '#28A745'))
    worksheet.write(8, 1, 'Verified False Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(8, 2, evaluation_summary.fp, cell_formatting(workbook, '#FF0000'))

    worksheet.merge_range("C6:D6", "AI Results", cell_formatting(workbook, "#4f8df1"))
    worksheet.write(6, 2, 'Predicted True Positives', cell_formatting(workbook, '#bfbfbf'))
    worksheet.write(7, 3, evaluation_summary.fn, cell_formatting(workbook, '#FF0000'))
    worksheet.write(6, 3, 'Predicted False Positives', cell_formatting(workbook, '#bfbfbf'))
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

 