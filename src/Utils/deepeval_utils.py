"""
DeepEval utilities for SAST workflow evaluation
"""

import logging
import json
import time
from typing import Dict, Any, List, Optional
from pathlib import Path
from dataclasses import dataclass

from deepeval.metrics import FaithfulnessMetric, AnswerRelevancyMetric
from deepeval.test_case import LLMTestCase
from deepeval.models import DeepEvalBaseLLM

logger = logging.getLogger(__name__)


class LangChainLLMWrapper(DeepEvalBaseLLM):
    """Wrapper to make LangChain LLM compatible with DeepEval"""
    
    def __init__(self, langchain_llm):
        self.langchain_llm = langchain_llm
        
    def load_model(self):
        return self
        
    def generate(self, prompt: str) -> str:
        try:
            response = self.langchain_llm.invoke(prompt)
            return response.content if hasattr(response, 'content') else str(response)
        except Exception as e:
            logger.error(f"LLM generation failed: {e}")
            return ""
            
    async def a_generate(self, prompt: str) -> str:
        try:
            response = await self.langchain_llm.ainvoke(prompt)
            return response.content if hasattr(response, 'content') else str(response)
        except Exception as e:
            logger.error(f"Async LLM generation failed: {e}")
            return ""
        
    def get_model_name(self) -> str:
        return getattr(self.langchain_llm, 'model_name', 'langchain_llm')


@dataclass
class EvaluationResult:
    """Result of a single summary evaluation"""
    issue_id: str
    faithfulness_score: float
    relevancy_score: float
    faithfulness_passed: bool
    relevancy_passed: bool
    original_justification: str
    generated_summary: str
    expected_summary: Optional[str] = None
    error_message: Optional[str] = None


class SummaryEvaluator:
    """Evaluates summary quality using DeepEval metrics"""
    
    def __init__(self, llm_wrapper: LangChainLLMWrapper, 
                 faithfulness_threshold: float = 0.8,
                 relevancy_threshold: float = 0.75):
        self.llm_wrapper = llm_wrapper
        self.faithfulness_metric = FaithfulnessMetric(
            threshold=faithfulness_threshold, 
            model=llm_wrapper,
            include_reason=False
        )
        self.relevancy_metric = AnswerRelevancyMetric(
            threshold=relevancy_threshold,
            model=llm_wrapper,
            include_reason=False
        )
        self.results: List[EvaluationResult] = []
    
    async def evaluate_summary(self, issue_id: str, original_justification: str, 
                             generated_summary: str, expected_summary: Optional[str] = None) -> EvaluationResult:
        """Evaluate a single summary using DeepEval metrics"""
        
        try:
            import time
            start_time = time.time()
            
            # Create test case for DeepEval
            test_case = LLMTestCase(
                input=f"Summarize this security analysis: {original_justification}",
                actual_output=generated_summary,
                expected_output=expected_summary or generated_summary,
                context=[original_justification],
                retrieval_context=[original_justification]
            )
            
            logger.debug(f"Created test case in {time.time() - start_time:.2f}s")
            eval_start = time.time()
            
            # Evaluate metrics individually to get proper scores
            try:
                # Evaluate faithfulness
                self.faithfulness_metric.measure(test_case)
                faithfulness_score = self.faithfulness_metric.score or 0.0
                faithfulness_passed = self.faithfulness_metric.success or False
                
                # Evaluate relevancy  
                self.relevancy_metric.measure(test_case)
                relevancy_score = self.relevancy_metric.score or 0.0
                relevancy_passed = self.relevancy_metric.success or False
                
                logger.info(f"DeepEval evaluation took {time.time() - eval_start:.2f}s for issue {issue_id}")
                
            except Exception as eval_error:
                logger.error(f"Metric evaluation failed: {eval_error}")
                faithfulness_score = 0.0
                faithfulness_passed = False
                relevancy_score = 0.0
                relevancy_passed = False
            
            evaluation_result = EvaluationResult(
                issue_id=issue_id,
                faithfulness_score=faithfulness_score,
                relevancy_score=relevancy_score,
                faithfulness_passed=faithfulness_passed,
                relevancy_passed=relevancy_passed,
                original_justification=original_justification,
                generated_summary=generated_summary,
                expected_summary=expected_summary
            )
            
            self.results.append(evaluation_result)
            logger.debug(f"Evaluated summary for {issue_id}: F={faithfulness_score:.2f}, R={relevancy_score:.2f}")
            
            return evaluation_result
            
        except Exception as e:
            logger.error(f"Failed to evaluate summary for {issue_id}: {e}")
            error_result = EvaluationResult(
                issue_id=issue_id,
                faithfulness_score=0.0,
                relevancy_score=0.0,
                faithfulness_passed=False,
                relevancy_passed=False,
                original_justification=original_justification,
                generated_summary=generated_summary,
                expected_summary=expected_summary,
                error_message=str(e)
            )
            self.results.append(error_result)
            return error_result
    
    def generate_report(self) -> Dict[str, Any]:
        """Generate comprehensive evaluation report"""
        if not self.results:
            return {"error": "No evaluation results available"}
        
        total_evaluations = len(self.results)
        valid_results = [r for r in self.results if r.error_message is None]
        
        if valid_results:
            avg_faithfulness = sum((r.faithfulness_score or 0.0) for r in valid_results) / len(valid_results)
            avg_relevancy = sum((r.relevancy_score or 0.0) for r in valid_results) / len(valid_results)
            faithfulness_pass_rate = sum(1 for r in valid_results if (r.faithfulness_passed or False)) / len(valid_results)
            relevancy_pass_rate = sum(1 for r in valid_results if (r.relevancy_passed or False)) / len(valid_results)
        else:
            avg_faithfulness = avg_relevancy = faithfulness_pass_rate = relevancy_pass_rate = 0.0
        
        return {
            "summary": {
                "total_evaluations": total_evaluations,
                "valid_evaluations": len(valid_results),
                "failed_evaluations": total_evaluations - len(valid_results),
                "faithfulness_pass_rate": faithfulness_pass_rate,
                "relevancy_pass_rate": relevancy_pass_rate
            },
            "metrics": {
                "average_faithfulness": avg_faithfulness,
                "average_relevancy": avg_relevancy,
                "faithfulness_threshold": self.faithfulness_metric.threshold,
                "relevancy_threshold": self.relevancy_metric.threshold
            },
            "detailed_results": [
                {
                    "issue_id": r.issue_id,
                    "faithfulness_score": r.faithfulness_score,
                    "relevancy_score": r.relevancy_score,
                    "faithfulness_passed": r.faithfulness_passed,
                    "relevancy_passed": r.relevancy_passed,
                    "summary_length": len(r.generated_summary),
                    "original_length": len(r.original_justification),
                    "compression_ratio": (len(r.original_justification) - len(r.generated_summary)) / len(r.original_justification) if r.original_justification else 0,
                    "error_message": r.error_message
                }
                for r in self.results
            ],
            "metadata": {
                "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
                "model_name": self.llm_wrapper.get_model_name(),
                "evaluation_version": "1.0"
            }
        }
    
    def save_report(self, output_path: str) -> Path:
        """Save evaluation report to JSON file"""
        report = self.generate_report()
        output_file = Path(output_path)
        output_file.parent.mkdir(parents=True, exist_ok=True)
        
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(report, f, indent=2, ensure_ascii=False)
        
        logger.info(f"Evaluation report saved to: {output_file}")
        return output_file