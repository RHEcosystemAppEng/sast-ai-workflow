import json
import os
import datetime
import time
from typing import Any, Dict, List
from dataclasses import is_dataclass, asdict
from enum import Enum
from contextlib import contextmanager


class DebugDumper:
    """Utility class for dumping intermediate results during run.py execution"""
    
    def __init__(self, output_dir: str = "debug_dumps"):
        self.output_dir = output_dir
        self.session_id = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        self.dump_counter = 0
        self.timings = {}  # Store timing information
        
        # Create output directory if it doesn't exist
        os.makedirs(self.output_dir, exist_ok=True)
    
    def _serialize_object(self, obj: Any) -> Any:
        """Convert complex objects to JSON-serializable format"""
        if obj is None:
            return None
        elif isinstance(obj, (str, int, float, bool)):
            return obj
        elif isinstance(obj, dict):
            return {k: self._serialize_object(v) for k, v in obj.items()}
        elif isinstance(obj, (list, tuple)):
            return [self._serialize_object(item) for item in obj]
        elif isinstance(obj, Enum):
            return obj.value
        elif is_dataclass(obj):
            return self._serialize_object(asdict(obj))
        elif hasattr(obj, '__dict__'):
            # Handle custom objects with __dict__
            result = {}
            for attr, value in obj.__dict__.items():
                if not attr.startswith('_'):  # Skip private attributes
                    try:
                        result[attr] = self._serialize_object(value)
                    except Exception:
                        result[attr] = str(value)  # Fallback to string representation
            return result
        else:
            # Fallback to string representation
            return str(obj)
    
    @contextmanager
    def timer(self, operation_name: str, issue_id: str = None):
        """Context manager for timing operations"""
        start_time = time.time()
        try:
            yield
        finally:
            end_time = time.time()
            duration = end_time - start_time
            
            # Store timing information
            timing_key = f"{operation_name}_{issue_id}" if issue_id else operation_name
            if timing_key not in self.timings:
                self.timings[timing_key] = []
            self.timings[timing_key].append({
                "duration_seconds": duration,
                "start_time": start_time,
                "end_time": end_time,
                "timestamp": datetime.datetime.now().isoformat()
            })
    
    def dump_checkpoint(self, checkpoint_name: str, data: Any, issue_id: str = None, 
                      step: int = None, timing_duration: float = None) -> str:
        """
        Dump data at a specific checkpoint
        
        Args:
            checkpoint_name: Name of the checkpoint (e.g., 'read_sast_report')
            data: Data to dump
            issue_id: Optional issue ID for per-issue checkpoints
            step: Optional step number for iterative processes
            timing_duration: Optional timing duration for the operation
            
        Returns:
            Path to the created dump file
        """
        self.dump_counter += 1
        
        # Create filename
        filename_parts = [f"{self.dump_counter:03d}", checkpoint_name]
        if issue_id:
            filename_parts.append(issue_id)
        if step is not None:
            filename_parts.append(f"step{step}")
        filename_parts.append(self.session_id)
        filename = "_".join(filename_parts) + ".json"
        
        filepath = os.path.join(self.output_dir, filename)
        
        # Get relevant timing data
        timing_key = f"{checkpoint_name}_{issue_id}" if issue_id else checkpoint_name
        relevant_timings = self.timings.get(timing_key, [])
        
        # Prepare dump content
        dump_content = {
            "session_id": self.session_id,
            "dump_counter": self.dump_counter,
            "checkpoint_name": checkpoint_name,
            "issue_id": issue_id,
            "step": step,
            "timestamp": datetime.datetime.now().isoformat(),
            "timing": {
                "duration_seconds": timing_duration,
                "all_timings": relevant_timings
            },
            "data": self._serialize_object(data)
        }
        
        # Write to file
        try:
            with open(filepath, 'w', encoding='utf-8') as f:
                json.dump(dump_content, f, indent=2, ensure_ascii=False)
            return filepath
        except Exception as e:
            print(f"Error dumping checkpoint {checkpoint_name}: {e}")
            return None
    
    def dump_issue_list(self, issue_list: List) -> str:
        """Dump the initial issue list"""
        return self.dump_checkpoint("read_sast_report", {
            "total_issues": len(issue_list),
            "issue_ids": [getattr(issue, 'id', 'unknown') for issue in issue_list],
            "issues": issue_list
        })
    
    def dump_known_issues(self, already_seen_issues: Dict, similar_known_issues: Dict) -> str:
        """Dump known issues detection results"""
        return self.dump_checkpoint("capture_known_issues", {
            "already_seen_issues_count": len(already_seen_issues),
            "similar_known_issues_count": len(similar_known_issues),
            "already_seen_issues": already_seen_issues,
            "similar_known_issues": similar_known_issues
        })
    
    def dump_source_code_blocks(self, issue_id: str, source_code_blocks: Dict) -> str:
        """Dump source code blocks extracted from error trace"""
        return self.dump_checkpoint("get_source_code_blocks", {
            "source_code_blocks": source_code_blocks,
            "file_count": len(source_code_blocks),
            "files": list(source_code_blocks.keys())
        }, issue_id)
    
    def dump_llm_investigation(self, issue_id: str, llm_response: Any, critique_response: str, 
                             context: str, step: int = None, timing_duration: float = None) -> str:
        """Dump LLM investigation results"""
        return self.dump_checkpoint("llm_investigate", {
            "llm_response": llm_response,
            "critique_response": critique_response,
            "context_length": len(context) if context else 0,
            "context": context
        }, issue_id, step, timing_duration)
    
    def dump_missing_functions(self, issue_id: str, missing_source_code: str, 
                             instructions: List, step: int = None, timing_duration: float = None) -> str:
        """Dump extracted missing functions or macros"""
        return self.dump_checkpoint("extract_missing_functions", {
            "missing_source_code": missing_source_code,
            "instructions": instructions,
            "missing_code_length": len(missing_source_code) if missing_source_code else 0
        }, issue_id, step, timing_duration)
    
    def dump_metrics_evaluation(self, issue_id: str, score: Dict, metric_request: Any) -> str:
        """Dump metrics evaluation results"""
        return self.dump_checkpoint("evaluate_datasets", {
            "score": score,
            "metric_request": metric_request
        }, issue_id)
    
    def dump_evaluation_summary(self, evaluation_summary: Any, items_for_evaluation: List,
                              failed_item_ids: List) -> str:
        """Dump final evaluation summary"""
        return self.dump_checkpoint("evaluation_summary", {
            "evaluation_summary": evaluation_summary,
            "items_for_evaluation_count": len(items_for_evaluation),
            "failed_item_ids": failed_item_ids,
            "failed_items_count": len(failed_item_ids)
        })