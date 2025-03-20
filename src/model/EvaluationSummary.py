from Utils.metrics_utils import (
    count_actual_values,
    count_predicted_values,
    calculate_confusion_matrix_metrics, 
    get_predicted_summary,
    get_metrics
)


class EvaluationSummary:
    """
    Encapsulates the evaluation results, calculates confusion
    matrix metrics and performance metrics based on summary_data.

    Expects summary_data to be a list of (issue, summary_info) tuples.
    """
    def __init__(self, summary_data, ground_truth=None):
        self.summary_data = summary_data
        self.ground_truth = ground_truth
        self.predicted_summary = get_predicted_summary(summary_data)

        if not self.ground_truth:
            print("No human verified results provided. Skipping metric calculations.")
            self.actual_true_positives, self.actual_false_positives = set(), set()
            self.predicted_true_positives, self.predicted_false_positives = count_predicted_values(self.predicted_summary)
            self.tp = self.tn = self.fp = self.fn = 0
            self.accuracy = self.recall = self.precision = self.f1_score = None
        else:
            self.actual_true_positives, self.actual_false_positives = count_actual_values(self.predicted_summary, self.ground_truth)
            self.predicted_true_positives, self.predicted_false_positives = count_predicted_values(self.predicted_summary)
            self.tp, self.tn, self.fp, self.fn = calculate_confusion_matrix_metrics(
                self.actual_true_positives, self.actual_false_positives,
                self.predicted_true_positives, self.predicted_false_positives
            )
            self.accuracy, self.recall, self.precision, self.f1_score = get_metrics(self.tp, self.tn, self.fp, self.fn)

    def __repr__(self):
        return (
            f"EvaluationSummary(tp={self.tp}, tn={self.tn}, fp={self.fp}, fn={self.fn}, "
            f"accuracy={self.accuracy:.3f}, recall={self.recall:.3f}, "
            f"precision={self.precision:.3f}, f1_score={self.f1_score:.3f})"
        )