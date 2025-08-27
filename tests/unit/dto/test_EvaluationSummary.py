"""
Tests for EvaluationSummary class in dto/EvaluationSummary.py
Covers data integration and flow validation.
"""
import pytest
from unittest.mock import Mock
from src.dto.EvaluationSummary import EvaluationSummary
from src.dto.LLMResponse import AnalysisResponse
from src.dto.Issue import Issue
from src.dto.SummaryInfo import SummaryInfo


class TestEvaluationSummary:

    def test__init__with_ground_truth_calculates_metrics(self):
        # preparation
        mock_config = Mock()
        mock_config.USE_CRITIQUE_AS_FINAL_RESULTS = False
        
        issue1 = Issue(id="issue1")
        issue2 = Issue(id="issue2")
        
        response1 = AnalysisResponse("TRUE POSITIVE", "TRUE", "", [], "", [], [], [])
        response2 = AnalysisResponse(" FALSE POSITIVE", "TRUE", "", [], "", [], [], [])
        
        summary1 = SummaryInfo(response1, {}, "", "")
        summary2 = SummaryInfo(response2, {}, "", "")
        
        summary_data = [(issue1, summary1), (issue2, summary2)]
        ground_truth = {"issue1": "no", "issue2": "yes"}
        
        # testing
        eval_summary = EvaluationSummary(summary_data, mock_config, ground_truth)
        
        # assertion
        assert eval_summary.summary_data == summary_data
        assert eval_summary.ground_truth == ground_truth
        assert hasattr(eval_summary, 'accuracy')
        assert hasattr(eval_summary, 'precision')
        assert hasattr(eval_summary, 'recall')
        assert hasattr(eval_summary, 'f1_score')
        assert eval_summary.accuracy is not None
        assert len(eval_summary.predicted_summary) == 2

    def test__init__no_ground_truth_sets_metrics_none(self):
        # preparation
        mock_config = Mock()
        mock_config.USE_CRITIQUE_AS_FINAL_RESULTS = False
        
        issue1 = Issue(id="issue1")
        response1 = AnalysisResponse("TRUE POSITIVE", "TRUE", "", [], "", [], [], [])
        summary1 = SummaryInfo(response1, {}, "", "")
        
        summary_data = [(issue1, summary1)]
        
        # testing
        eval_summary = EvaluationSummary(summary_data, mock_config, ground_truth=None)
        
        # assertion
        assert eval_summary.ground_truth is None
        assert eval_summary.accuracy is None
        assert eval_summary.precision is None
        assert eval_summary.recall is None
        assert eval_summary.f1_score is None
        assert eval_summary.tp == 0
        assert eval_summary.tn == 0
        assert eval_summary.fp == 0
        assert eval_summary.fn == 0

    def test__init__empty_data_handles_gracefully(self):
        # preparation
        mock_config = Mock()
        mock_config.USE_CRITIQUE_AS_FINAL_RESULTS = False
        
        summary_data = []
        
        # testing with no ground truth
        eval_summary = EvaluationSummary(summary_data, mock_config, ground_truth=None)
        
        # assertion
        assert eval_summary.summary_data == []
        assert len(eval_summary.predicted_summary) == 0
        assert eval_summary.accuracy is None
        assert eval_summary.tp == 0
        assert eval_summary.tn == 0
        assert eval_summary.fp == 0
        assert eval_summary.fn == 0