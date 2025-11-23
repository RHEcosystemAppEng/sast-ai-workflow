import json
import logging
import atexit
import time
import functools
import os
from typing import Any, Dict, List, Optional
from langchain_core.callbacks import BaseCallbackHandler
from langchain_core.outputs import LLMResult

logger = logging.getLogger(__name__)

DURATION_DECIMAL_PLACES = 3
LANGGRAPH_NODE_METADATA_KEY = "langgraph_node"


class TokenUsageCallback(BaseCallbackHandler):
    def __init__(self, output_path: str):
        super().__init__()
        self.output_path = output_path
        self.metrics: List[Dict[str, Any]] = []
        self._current_contexts: Dict[str, Dict[str, Any]] = {}  # run_id -> {tool_name, model}
        self._written = False

        atexit.register(self._write_file)
        logger.info(f"TokenUsageCallback initialized, will write to: {output_path}")

    def on_llm_start(
        self, serialized: Dict[str, Any], prompts: List[str], **kwargs: Any
    ) -> None:
        run_id = kwargs.get("run_id")
        if run_id:
            metadata = kwargs.get("metadata", {})
            # Store context using run_id for thread-safe concurrent LLM calls
            self._current_contexts[run_id] = {
                "tool_name": metadata.get(LANGGRAPH_NODE_METADATA_KEY, "unknown"),
                "model": metadata.get("ls_model_name", "unknown")
            }
            logger.debug(f"Token tracking [run_id={run_id}]: tool={self._current_contexts[run_id]['tool_name']}, model={self._current_contexts[run_id]['model']}")

    def on_llm_end(self, response: LLMResult, **kwargs: Any) -> None:
        """Capture token usage and add to metrics (aggregates multiple LLM calls per node)"""
        if not response.llm_output:
            return

        run_id = kwargs.get("run_id")
        context = self._current_contexts.pop(run_id, {})
        tool_name = context.get("tool_name", "unknown")
        model = context.get("model", "unknown")

        token_usage = response.llm_output.get("token_usage", {})
        if token_usage:
            # Find any existing entry for this node
            existing_entry = next((m for m in self.metrics if m["tool_name"] == tool_name), None)

            if existing_entry:
                # Aggregate token usage (multiple LLM calls per node)
                existing_entry["input_tokens"] = (existing_entry.get("input_tokens") or 0) + token_usage.get("prompt_tokens", 0)
                existing_entry["output_tokens"] = (existing_entry.get("output_tokens") or 0) + token_usage.get("completion_tokens", 0)
                existing_entry["total_tokens"] = (existing_entry.get("total_tokens") or 0) + token_usage.get("total_tokens", 0)
                # Update model if not already set
                if existing_entry.get("model") is None:
                    existing_entry["model"] = model
            else:
                # Create new entry (timing will be added by decorator)
                token_entry = {
                    "tool_name": tool_name,
                    "model": model,
                    "input_tokens": token_usage.get("prompt_tokens", 0),
                    "output_tokens": token_usage.get("completion_tokens", 0),
                    "total_tokens": token_usage.get("total_tokens", 0),
                    "duration_seconds": None
                }
                self.metrics.append(token_entry)

            logger.info(f"Captured token usage for {tool_name}: {token_usage}")

    def track_node_timing(self, node_name: str):
        """Decorator to track timing for any node"""
        def decorator(func):
            @functools.wraps(func)
            async def wrapper(*args, **kwargs):
                start_time = time.time()
                result = await func(*args, **kwargs)
                duration = time.time() - start_time

                # Find or create entry
                existing_entry = next((m for m in self.metrics if m["tool_name"] == node_name), None)
                if existing_entry:
                    existing_entry["duration_seconds"] = round(duration, DURATION_DECIMAL_PLACES)
                else:
                    self.metrics.append({
                        "tool_name": node_name,
                        "duration_seconds": round(duration, DURATION_DECIMAL_PLACES),
                        "model": None,
                        "input_tokens": None,
                        "output_tokens": None,
                        "total_tokens": None
                    })

                logger.info(f"Node completed: {node_name}, duration={duration:.3f}s")
                return result
            return wrapper
        return decorator

    def _write_file(self) -> None:
        if self._written:
            return

        if not self.metrics:
            logger.warning("No metrics data to write")
            return

        try:
            output_dir = os.path.dirname(self.output_path)
            if output_dir:
                os.makedirs(output_dir, exist_ok=True)

            logger.info(f"Writing {len(self.metrics)} metric entries to: {self.output_path}")
            with open(self.output_path, 'w') as f:
                json.dump(self.metrics, f, indent=2)
            logger.info(f"Metrics file written successfully to: {self.output_path}")
            self._written = True
        except Exception as e:
            logger.error(f"Failed to write metrics file: {e}", exc_info=True)
