#!/usr/bin/env python3

import asyncio
import json
import logging
import os
import time
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Dict, Any, Optional
from dotenv import load_dotenv

load_dotenv(Path(__file__).parent.parent / '.env.evaluation')

logging.basicConfig(level=logging.ERROR, format='%(levelname)s: %(message)s')
logger = logging.getLogger(__name__)

from dto.SASTWorkflowModels import SASTWorkflowTracker
from common.config import Config


class BaseNodeEvaluator(ABC):
    
    def __init__(self, dataset_path: str, evaluation_output_path: str, max_examples: int = 3):
        self.dataset_path = dataset_path
        self.evaluation_output_path = evaluation_output_path
        self.max_examples = max_examples
        self.start_time = None
    
    @abstractmethod
    def load_test_data(self) -> SASTWorkflowTracker:
        pass
    
    @abstractmethod
    def create_node_config(self) -> Any:
        pass
    
    @abstractmethod
    def create_builder_mock(self) -> Any:
        pass
    
    @abstractmethod
    async def run_node_evaluation(self, tracker: SASTWorkflowTracker, config: Any, builder: Any) -> SASTWorkflowTracker:
        pass
    
    def create_llm_config(self) -> Dict[str, Any]:
        api_key = os.getenv('LLM_API_KEY')
        if not api_key:
            raise ValueError("LLM_API_KEY environment variable is required")
        
        model_name = os.getenv('LLM_MODEL_NAME', 'nvidia/llama-3.1-nemotron-70b-instruct')
        base_url = os.getenv('LLM_URL', 'https://integrate.api.nvidia.com/v1')
        
        return {
            'model': model_name,
            'api_key': api_key,
            'base_url': base_url,
            'temperature': 0.1
        }
    
    def create_real_llm(self):
        llm_config = self.create_llm_config()
        from langchain_openai import ChatOpenAI
        return ChatOpenAI(
            model=llm_config['model'],
            api_key=llm_config['api_key'],
            base_url=llm_config['base_url'],
            temperature=llm_config['temperature']
        )
    
    def setup_tracker_config(self, tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        real_config = Config()
        tracker.config = real_config
        return tracker
    
    def display_results(self, result_tracker: SASTWorkflowTracker):
        os.makedirs(os.path.dirname(self.evaluation_output_path), exist_ok=True)
        
        if Path(self.evaluation_output_path).exists():
            with open(self.evaluation_output_path, 'r') as f:
                report = json.load(f)
            
            metrics = report.get('metrics', {})
            total_time = time.time() - self.start_time
            
            print(f"Evaluation completed: {len(result_tracker.issues)} issues in {total_time:.1f}s")
            print(f"Average Faithfulness: {metrics.get('average_faithfulness', 0):.3f}")
            print(f"Average Relevancy: {metrics.get('average_relevancy', 0):.3f}")
            print(f"Report saved: {self.evaluation_output_path}")
    
    async def run_evaluation(self):
        self.start_time = time.time()
        
        try:
            test_tracker = self.load_test_data()
            
            if not test_tracker.issues:
                return
            
            test_tracker = self.setup_tracker_config(test_tracker)
            config = self.create_node_config()
            builder = self.create_builder_mock()
            
            result_tracker = await self.run_node_evaluation(test_tracker, config, builder)
            self.display_results(result_tracker)
            
        except Exception as e:
            elapsed = time.time() - self.start_time
            print(f"ERROR: {e} (after {elapsed:.1f}s)")


if __name__ == "__main__":
    print("BaseNodeEvaluator is an abstract class. Use a specific node evaluator.")