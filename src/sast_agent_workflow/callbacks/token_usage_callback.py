import json
import logging
import atexit
import time
from typing import Any, Dict, List, Optional
from langchain_core.callbacks import BaseCallbackHandler
from langchain_core.outputs import LLMResult

logger = logging.getLogger(__name__)


class TokenUsageCallback(BaseCallbackHandler):
    def __init__(self, output_path: str):
        super().__init__()
        self.output_path = output_path
        self.metrics: List[Dict[str, Any]] = []
        self.node_start_times: Dict[str, float] = {}
        self._current_tool_name = "unknown"
        self._current_model = "unknown"

        atexit.register(self._write_file)
        logger.info(f"TokenUsageCallback initialized, will write to: {output_path}")

    def on_chain_start(
        self, serialized: Dict[str, Any], inputs: Dict[str, Any], **kwargs: Any
    ) -> None:
        """Track when a tool/node starts (captures ALL nodes including non-LLM ones)"""
        metadata = kwargs.get("metadata", {})
        node_name = metadata.get("langgraph_node", "unknown")

        if node_name != "unknown":
            self.node_start_times[node_name] = time.time()
            logger.debug(f"Node started: {node_name}")

    def on_chain_end(self, outputs: Dict[str, Any], **kwargs: Any) -> None:
        """Track when a tool/node ends and calculate duration"""
        metadata = kwargs.get("metadata", {})
        node_name = metadata.get("langgraph_node", "unknown")

        if node_name in self.node_start_times:
            start_time = self.node_start_times[node_name]
            duration = time.time() - start_time

            # Create or update metrics entry for this node
            # Check if we already have an entry for this node from LLM callbacks
            existing_entry = next((m for m in self.metrics if m["tool_name"] == node_name), None)

            if existing_entry:
                # Update existing entry with timing
                existing_entry["duration_seconds"] = round(duration, 3)
            else:
                # Create new entry (for non-LLM nodes)
                self.metrics.append({
                    "tool_name": node_name,
                    "duration_seconds": round(duration, 3),
                    "model": None,
                    "input_tokens": None,
                    "output_tokens": None,
                    "total_tokens": None
                })

            logger.info(f"Node completed: {node_name}, duration={duration:.3f}s")
            del self.node_start_times[node_name]

    def on_llm_start(
        self, serialized: Dict[str, Any], prompts: List[str], **kwargs: Any
    ) -> None:
        metadata = kwargs.get("metadata", {})

        # Extract tool name from LangGraph metadata
        self._current_tool_name = metadata.get("langgraph_node", "unknown")

        # Extract model name from LangGraph metadata
        self._current_model = metadata.get("ls_model_name", "unknown")

        logger.debug(f"Token tracking: tool={self._current_tool_name}, model={self._current_model}")

    def on_llm_end(self, response: LLMResult, **kwargs: Any) -> None:
        """Capture token usage and add to metrics"""
        if not response.llm_output:
            return

        token_usage = response.llm_output.get("token_usage", {})
        if token_usage:
            # Check if we already have an entry for this node from chain callbacks
            existing_entry = next((m for m in self.metrics if m["tool_name"] == self._current_tool_name and m["model"] is None), None)

            if existing_entry:
                # Update existing entry with token data
                existing_entry["model"] = self._current_model
                existing_entry["input_tokens"] = token_usage.get("prompt_tokens", 0)
                existing_entry["output_tokens"] = token_usage.get("completion_tokens", 0)
                existing_entry["total_tokens"] = token_usage.get("total_tokens", 0)
            else:
                # Create new entry (timing will be added by chain_end)
                token_entry = {
                    "tool_name": self._current_tool_name,
                    "model": self._current_model,
                    "input_tokens": token_usage.get("prompt_tokens", 0),
                    "output_tokens": token_usage.get("completion_tokens", 0),
                    "total_tokens": token_usage.get("total_tokens", 0),
                    "duration_seconds": None
                }
                self.metrics.append(token_entry)

            logger.info(f"Captured token usage for {self._current_tool_name}: {token_usage}")

    def _write_file(self) -> None:
        if not self.metrics:
            logger.warning("No metrics data to write")
            return

        try:
            logger.info(f"Writing {len(self.metrics)} metric entries to: {self.output_path}")
            with open(self.output_path, 'w') as f:
                json.dump(self.metrics, f, indent=2)
            logger.info(f"Metrics file written successfully to: {self.output_path}")
        except Exception as e:
            logger.error(f"Failed to write metrics file: {e}", exc_info=True)
