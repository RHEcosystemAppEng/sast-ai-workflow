from typing import Dict, List
from collections import defaultdict, Counter


class EvaluationMetrics:
    """Calculate and store evaluation metrics for classification results."""

    def __init__(self):
        self.total = 0
        self.correct = 0
        self.tp_as_tp = 0
        self.tp_as_fp = 0
        self.fp_as_fp = 0
        self.fp_as_tp = 0
        self.errors_skipped = 0  # Count of ERROR entries excluded from metrics 

        self.by_issue_type = defaultdict(lambda: {
            'total': 0,
            'correct': 0,
            'tp_as_tp': 0,
            'tp_as_fp': 0,
            'fp_as_fp': 0,
            'fp_as_tp': 0
        })

        self.pattern_citations = Counter()

        self.phase2_total_changes = 0
        self.phase2_changes_helped = 0 
        self.phase2_changes_hurt = 0   
        self.phase2_kept_correct = 0   
        self.phase2_kept_wrong = 0     

    def add_result(
        self,
        ground_truth: str,
        predicted: str,
        issue_type: str,
        cited_patterns: List[str],
        initial_classification: str = "",
        classification_changed: bool = False
    ) -> None:
        """
        Add a single classification result.

        Args:
            ground_truth: Ground truth classification (TRUE_POSITIVE or FALSE_POSITIVE)
            predicted: Predicted classification (final, after Phase 2 if applicable)
            issue_type: Type of issue (e.g., RESOURCE_LEAK)
            cited_patterns: List of pattern IDs cited in the prediction
            initial_classification: Phase 1 classification (before pattern verification)
            classification_changed: Whether Phase 2 changed the classification
        """
        # Skip ERROR entries - only include TRUE_POSITIVE or FALSE_POSITIVE predictions
        if predicted.upper() == "ERROR":
            self.errors_skipped += 1
            return

        self.total += 1

        gt_is_tp = "TRUE" in ground_truth.upper()
        pred_is_tp = "TRUE" in predicted.upper()

        is_correct = (gt_is_tp and pred_is_tp) or (not gt_is_tp and not pred_is_tp)

        if is_correct:
            self.correct += 1

        if gt_is_tp and pred_is_tp:
            self.tp_as_tp += 1
        elif gt_is_tp and not pred_is_tp:
            self.tp_as_fp += 1
        elif not gt_is_tp and not pred_is_tp:
            self.fp_as_fp += 1
        elif not gt_is_tp and pred_is_tp:
            self.fp_as_tp += 1

        stats = self.by_issue_type[issue_type]
        stats['total'] += 1
        if is_correct:
            stats['correct'] += 1

        if gt_is_tp and pred_is_tp:
            stats['tp_as_tp'] += 1
        elif gt_is_tp and not pred_is_tp:
            stats['tp_as_fp'] += 1
        elif not gt_is_tp and not pred_is_tp:
            stats['fp_as_fp'] += 1
        elif not gt_is_tp and pred_is_tp:
            stats['fp_as_tp'] += 1

        for pattern_id in cited_patterns:
            self.pattern_citations[pattern_id] += 1

        if initial_classification:  
            if classification_changed:
                self.phase2_total_changes += 1
                initial_is_tp = "TRUE" in initial_classification.upper()
                initial_was_correct = (gt_is_tp and initial_is_tp) or (not gt_is_tp and not initial_is_tp)

                if is_correct and not initial_was_correct:
                    self.phase2_changes_helped += 1
                elif not is_correct and initial_was_correct:
                    self.phase2_changes_hurt += 1
            else:
                if is_correct:
                    self.phase2_kept_correct += 1
                else:
                    self.phase2_kept_wrong += 1

    def calculate_metrics(self) -> Dict:
        """
        Calculate all evaluation metrics.

        Returns:
            Dictionary containing all metrics
        """
        if self.total == 0:
            return {
                'accuracy': 0.0,
                'precision': 0.0,
                'recall': 0.0,
                'f1_score': 0.0,
                'confusion_matrix': {},
                'by_issue_type': {},
                'pattern_citations': {}
            }

        accuracy = self.correct / self.total if self.total > 0 else 0.0

        precision = (
            self.fp_as_fp / (self.fp_as_fp + self.tp_as_fp)
            if (self.fp_as_fp + self.tp_as_fp) > 0
            else 0.0
        )

        recall = (
            self.fp_as_fp / (self.fp_as_fp + self.fp_as_tp)
            if (self.fp_as_fp + self.fp_as_tp) > 0
            else 0.0
        )

        f1_score = (
            2 * (precision * recall) / (precision + recall)
            if (precision + recall) > 0
            else 0.0
        )

        confusion_matrix = {
            'TP_as_TP': self.tp_as_tp,
            'TP_as_FP': self.tp_as_fp,
            'FP_as_FP': self.fp_as_fp,
            'FP_as_TP': self.fp_as_tp
        }

        by_issue_type = {}
        for issue_type, stats in self.by_issue_type.items():
            by_issue_type[issue_type] = {
                'count': stats['total'],
                'accuracy': stats['correct'] / stats['total'] if stats['total'] > 0 else 0.0,
                'tp_as_tp': stats['tp_as_tp'],
                'tp_as_fp': stats['tp_as_fp'],
                'fp_as_fp': stats['fp_as_fp'],
                'fp_as_tp': stats['fp_as_tp']
            }

        pattern_citations = dict(self.pattern_citations.most_common())

        phase2_metrics = {
            'total_changes': self.phase2_total_changes,
            'changes_helped': self.phase2_changes_helped,
            'changes_hurt': self.phase2_changes_hurt,
            'kept_correct': self.phase2_kept_correct,
            'kept_wrong': self.phase2_kept_wrong,
            'change_success_rate': (
                self.phase2_changes_helped / self.phase2_total_changes
                if self.phase2_total_changes > 0 else 0.0
            )
        }

        return {
            'accuracy': round(accuracy, 4),
            'precision': round(precision, 4),
            'recall': round(recall, 4),
            'f1_score': round(f1_score, 4),
            'confusion_matrix': confusion_matrix,
            'by_issue_type': by_issue_type,
            'pattern_citations': pattern_citations,
            'total_entries': self.total,
            'correct_predictions': self.correct,
            'errors_skipped': self.errors_skipped,
            'phase2_verification': phase2_metrics
        }

    def print_summary(self) -> None:
        """Print a formatted summary of metrics."""
        metrics = self.calculate_metrics()

        print("\n" + "="*60)
        print("EVALUATION METRICS SUMMARY")
        print("="*60)

        print(f"\nOverall Performance:")
        print(f"  Total Entries:     {metrics['total_entries']}")
        print(f"  Errors Skipped:    {metrics['errors_skipped']}")
        print(f"  Correct:           {metrics['correct_predictions']}")
        print(f"  Accuracy:          {metrics['accuracy']:.2%}")
        print(f"  Precision:         {metrics['precision']:.2%}")
        print(f"  Recall:            {metrics['recall']:.2%}")
        print(f"  F1-Score:          {metrics['f1_score']:.2%}")

        print(f"\nConfusion Matrix (FP detection is positive class):")
        cm = metrics['confusion_matrix']
        print(f"  FP → FP: {cm['FP_as_FP']:4d}  (correctly filtered false positives)")
        print(f"  FP → TP: {cm['FP_as_TP']:4d}  (missed false positives - sent to dev)")
        print(f"  TP → FP: {cm['TP_as_FP']:4d}  (real bugs incorrectly filtered out)")
        print(f"  TP → TP: {cm['TP_as_TP']:4d}  (real bugs correctly sent to dev)")

        print(f"\nBy Issue Type:")
        for issue_type in sorted(metrics['by_issue_type'].keys()):
            stats = metrics['by_issue_type'][issue_type]
            print(f"  {issue_type:25s}: {stats['accuracy']:.2%} ({stats['count']} entries)")

        print(f"\nMost Cited Patterns:")
        citations = metrics['pattern_citations']
        for i, (pattern_id, count) in enumerate(sorted(citations.items(), key=lambda x: x[1], reverse=True)[:10], 1):
            print(f"  {i:2d}. {pattern_id:15s}: {count:4d} citations")

        p2 = metrics.get('phase2_verification', {})
        if p2.get('total_changes', 0) > 0 or p2.get('kept_correct', 0) > 0:
            print(f"\nPhase 2 Verification Impact:")
            print(f"  Total decisions changed:     {p2.get('total_changes', 0)}")
            print(f"  Changes that HELPED:         {p2.get('changes_helped', 0)} (fixed mistakes)")
            print(f"  Changes that HURT:           {p2.get('changes_hurt', 0)} (introduced errors)")
            print(f"  Kept decision (correct):     {p2.get('kept_correct', 0)}")
            print(f"  Kept decision (wrong):       {p2.get('kept_wrong', 0)}")
            if p2.get('total_changes', 0) > 0:
                print(f"  Change success rate:         {p2.get('change_success_rate', 0):.2%}")
                net_impact = p2.get('changes_helped', 0) - p2.get('changes_hurt', 0)
                print(f"  Net impact of changes:       {net_impact:+d}")

        print("\n" + "="*60)


def calculate_summary_statistics(results: List[Dict]) -> Dict:
    """
    Calculate summary statistics from a list of evaluation results.

    Args:
        results: List of result dictionaries, each containing:
            - ground_truth_classification
            - predicted_classification
            - issue_type
            - cited_patterns

    Returns:
        Dictionary with comprehensive metrics
    """
    metrics = EvaluationMetrics()

    for result in results:
        metrics.add_result(
            ground_truth=result.get('ground_truth_classification', 'UNKNOWN'),
            predicted=result.get('predicted_classification', 'UNKNOWN'),
            issue_type=result.get('issue_type', 'UNKNOWN'),
            cited_patterns=result.get('cited_patterns', [])
        )

    return metrics.calculate_metrics()


def main():
    """Test metrics calculation with sample data."""
    sample_results = [
        {
            'ground_truth_classification': 'TRUE_POSITIVE',
            'predicted_classification': 'TRUE_POSITIVE',
            'issue_type': 'RESOURCE_LEAK',
            'cited_patterns': ['RHEL-C-002', 'RHEL-C-013']
        },
        {
            'ground_truth_classification': 'FALSE_POSITIVE',
            'predicted_classification': 'FALSE_POSITIVE',
            'issue_type': 'RESOURCE_LEAK',
            'cited_patterns': ['RHEL-C-002']
        },
        {
            'ground_truth_classification': 'TRUE_POSITIVE',
            'predicted_classification': 'FALSE_POSITIVE',
            'issue_type': 'INTEGER_OVERFLOW',
            'cited_patterns': []
        },
        {
            'ground_truth_classification': 'FALSE_POSITIVE',
            'predicted_classification': 'TRUE_POSITIVE',
            'issue_type': 'OVERRUN',
            'cited_patterns': ['RHEL-C-015']
        }
    ]

    metrics = EvaluationMetrics()
    for result in sample_results:
        metrics.add_result(
            result['ground_truth_classification'],
            result['predicted_classification'],
            result['issue_type'],
            result['cited_patterns']
        )

    metrics.print_summary()


if __name__ == "__main__":
    main()
