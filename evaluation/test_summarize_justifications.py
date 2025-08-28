#!/usr/bin/env python3

import asyncio
import json
from pathlib import Path
from unittest.mock import Mock
from typing import Any

from base_node_evaluator import BaseNodeEvaluator
from dto.SASTWorkflowModels import SASTWorkflowTracker, PerIssueData
from dto.LLMResponse import AnalysisResponse
from dto.Issue import Issue
from sast_agent_workflow.tools.summarize_justifications import SummarizeJustificationsConfig, summarize_justifications
from evaluation.constants import *


class SummarizeJustificationsEvaluator(BaseNodeEvaluator):
    
    def load_test_data(self) -> SASTWorkflowTracker:
        if not Path(self.dataset_path).exists():
            print(f"Dataset not found: {self.dataset_path}")
            return SASTWorkflowTracker(issues={})
        
        with open(self.dataset_path, 'r') as f:
            golden_data = json.load(f)
        
        tracker = SASTWorkflowTracker(config=None, issues={})
        
        for i, item in enumerate(golden_data[:self.max_examples]):
            issue_id = item.get('issue_id', f"real_issue_{i}")
            
            issue = Issue(
                id=issue_id,
                issue_type=item.get('issue_type', 'SECURITY_ISSUE'),
                severity=item.get('severity', 'HIGH'),
                file_path=item.get('file_path', 'unknown.c'),
                line_number=item.get('line_number', 1),
                message=item.get('message', 'Security issue detected')
            )
            
            full_just = item.get('full_justification', '')
            justifications = [full_just] if full_just else item.get('justifications', [])
            
            analysis_response = AnalysisResponse(
                investigation_result=item.get('investigation_result', 'TRUE POSITIVE'),
                is_final='TRUE',
                justifications=justifications,
                prompt=item.get('prompt', 'Analyze this security issue'),
                short_justifications=""
            )
            
            per_issue_data = PerIssueData(
                issue=issue,
                analysis_response=analysis_response
            )
            
            tracker.issues[issue_id] = per_issue_data
        
        print(f"Loaded {len(tracker.issues)} test cases for summarize_justifications evaluation")
        return tracker
    
    def create_node_config(self) -> SummarizeJustificationsConfig:
        return SummarizeJustificationsConfig(
            llm_name="real_nvidia_llm",
            enable_evaluation=True,
            evaluation_output_path=self.evaluation_output_path,
            faithfulness_threshold=FAITHFULNESS_THRESHOLD,
            relevancy_threshold=RELEVANCY_THRESHOLD
        )
    
    def create_builder_mock(self) -> Any:
        real_llm = self.create_real_llm()
        builder = Mock()
        
        async def mock_get_llm(name, wrapper_type=None):
            return real_llm
        
        builder.get_llm = mock_get_llm
        return builder
    
    async def run_node_evaluation(self, tracker: SASTWorkflowTracker, config: SummarizeJustificationsConfig, builder: Any) -> SASTWorkflowTracker:
        async with summarize_justifications(config, builder) as func_info:
            return await func_info.single_fn(tracker)


if __name__ == "__main__":
    evaluator = SummarizeJustificationsEvaluator(
        dataset_path=GOLDEN_DATASET_PATH,
        evaluation_output_path=EVALUATION_REPORT_PATH,
        max_examples=MAX_EXAMPLES_TO_TEST
    )
    asyncio.run(evaluator.run_evaluation())