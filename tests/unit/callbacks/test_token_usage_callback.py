"""
Tests for TokenUsageCallback in callbacks/token_usage_callback.py
Covers token usage tracking, timing tracking, and metrics file writing.
"""
import pytest
import json
import asyncio
import tempfile
import os
from unittest.mock import Mock, patch, ANY
from pathlib import Path

from src.sast_agent_workflow.callbacks.token_usage_callback import (
    TokenUsageCallback,
    DURATION_DECIMAL_PLACES,
    LANGGRAPH_NODE_METADATA_KEY
)
from langchain_core.outputs import LLMResult


class TestTokenUsageCallback:
    """Test suite for TokenUsageCallback class"""

    @pytest.fixture
    def temp_output_file(self):
        """Create a temporary file for testing output"""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.json') as f:
            temp_path = f.name
        yield temp_path
        if os.path.exists(temp_path):
            os.unlink(temp_path)

    @pytest.fixture
    def temp_output_dir(self):
        """Create a temporary directory for testing output with nested paths"""
        with tempfile.TemporaryDirectory() as temp_dir:
            yield temp_dir

    @pytest.fixture
    def callback(self, temp_output_file):
        """Create a TokenUsageCallback instance for testing"""
        return TokenUsageCallback(output_path=temp_output_file)

    def test__init__creates_callback_with_correct_attributes(self, temp_output_file):
        """Test that callback is initialized with correct attributes"""
        callback = TokenUsageCallback(output_path=temp_output_file)

        assert callback.output_path == temp_output_file
        assert callback.metrics == []
        assert callback._current_contexts == {}
        assert callback._written is False

    def test__on_llm_start__stores_context_correctly(self, callback):
        """Test that on_llm_start stores run context"""
        serialized = {}
        prompts = ["test prompt"]
        run_id = "test-run-123"
        metadata = {
            LANGGRAPH_NODE_METADATA_KEY: "filter",
            "ls_model_name": "gpt-4"
        }

        callback.on_llm_start(
            serialized=serialized,
            prompts=prompts,
            run_id=run_id,
            metadata=metadata
        )

        assert run_id in callback._current_contexts
        assert callback._current_contexts[run_id]["tool_name"] == "filter"
        assert callback._current_contexts[run_id]["model"] == "gpt-4"

    def test__on_llm_start__uses_defaults_for_missing_metadata(self, callback):
        """Test that on_llm_start uses 'unknown' for missing metadata"""
        serialized = {}
        prompts = ["test prompt"]
        run_id = "test-run-456"
        metadata = {}

        callback.on_llm_start(
            serialized=serialized,
            prompts=prompts,
            run_id=run_id,
            metadata=metadata
        )

        assert callback._current_contexts[run_id]["tool_name"] == "unknown"
        assert callback._current_contexts[run_id]["model"] == "unknown"

    def test__on_llm_end__creates_new_metric_entry(self, callback):
        """Test that on_llm_end creates a new metric entry"""
        run_id = "test-run-789"
        callback._current_contexts[run_id] = {
            "tool_name": "judge_llm_analysis",
            "model": "gpt-4"
        }

        llm_output = {
            "token_usage": {
                "prompt_tokens": 100,
                "completion_tokens": 50,
                "total_tokens": 150
            }
        }
        response = LLMResult(
            generations=[],
            llm_output=llm_output
        )

        callback.on_llm_end(response=response, run_id=run_id)

        assert len(callback.metrics) == 1
        assert callback.metrics[0]["tool_name"] == "judge_llm_analysis"
        assert callback.metrics[0]["model"] == "gpt-4"
        assert callback.metrics[0]["input_tokens"] == 100
        assert callback.metrics[0]["output_tokens"] == 50
        assert callback.metrics[0]["total_tokens"] == 150
        assert callback.metrics[0]["duration_seconds"] is None
        assert run_id not in callback._current_contexts

    def test__on_llm_end__aggregates_multiple_calls_for_same_node(self, callback):
        """Test that multiple LLM calls for same node aggregate token counts"""
        callback._current_contexts["run-1"] = {
            "tool_name": "filter",
            "model": "gpt-4"
        }

        llm_output_1 = {
            "token_usage": {
                "prompt_tokens": 100,
                "completion_tokens": 50,
                "total_tokens": 150
            }
        }
        response_1 = LLMResult(generations=[], llm_output=llm_output_1)
        callback.on_llm_end(response=response_1, run_id="run-1")

        callback._current_contexts["run-2"] = {
            "tool_name": "filter",
            "model": "gpt-4"
        }

        llm_output_2 = {
            "token_usage": {
                "prompt_tokens": 200,
                "completion_tokens": 75,
                "total_tokens": 275
            }
        }
        response_2 = LLMResult(generations=[], llm_output=llm_output_2)
        callback.on_llm_end(response=response_2, run_id="run-2")

        assert len(callback.metrics) == 1
        assert callback.metrics[0]["tool_name"] == "filter"
        assert callback.metrics[0]["input_tokens"] == 300
        assert callback.metrics[0]["output_tokens"] == 125
        assert callback.metrics[0]["total_tokens"] == 425

    def test__on_llm_end__handles_missing_llm_output(self, callback):
        """Test that on_llm_end handles missing llm_output gracefully"""
        response = LLMResult(generations=[], llm_output=None)

        callback.on_llm_end(response=response, run_id="test-run")

        assert len(callback.metrics) == 0

    def test__on_llm_end__handles_missing_token_usage(self, callback):
        """Test that on_llm_end handles missing token_usage in llm_output"""
        llm_output = {}
        response = LLMResult(generations=[], llm_output=llm_output)

        callback._current_contexts["test-run"] = {
            "tool_name": "filter",
            "model": "gpt-4"
        }

        callback.on_llm_end(response=response, run_id="test-run")

        assert len(callback.metrics) == 0

    @pytest.mark.asyncio
    async def test__track_node_timing__adds_duration_to_existing_entry(self, callback):
        """Test that timing decorator adds duration to existing metric entry"""
        callback.metrics.append({
            "tool_name": "pre_process",
            "model": "gpt-4",
            "input_tokens": 100,
            "output_tokens": 50,
            "total_tokens": 150,
            "duration_seconds": None
        })

        @callback.track_node_timing("pre_process")
        async def test_node():
            await asyncio.sleep(0.1)
            return {"result": "success"}

        result = await test_node()

        assert result == {"result": "success"}
        assert len(callback.metrics) == 1
        assert callback.metrics[0]["duration_seconds"] is not None
        assert callback.metrics[0]["duration_seconds"] >= 0.1
        assert isinstance(callback.metrics[0]["duration_seconds"], float)

    @pytest.mark.asyncio
    async def test__track_node_timing__creates_new_entry_if_not_exists(self, callback):
        """Test that timing decorator creates new entry if node has no tokens"""
        @callback.track_node_timing("data_fetcher")
        async def test_node():
            await asyncio.sleep(0.05)
            return {"result": "data"}

        result = await test_node()

        assert result == {"result": "data"}
        assert len(callback.metrics) == 1
        assert callback.metrics[0]["tool_name"] == "data_fetcher"
        assert callback.metrics[0]["duration_seconds"] is not None
        assert callback.metrics[0]["duration_seconds"] >= 0.05
        assert callback.metrics[0]["model"] is None
        assert callback.metrics[0]["input_tokens"] is None
        assert callback.metrics[0]["output_tokens"] is None
        assert callback.metrics[0]["total_tokens"] is None

    @pytest.mark.asyncio
    async def test__track_node_timing__rounds_to_correct_decimal_places(self, callback):
        """Test that duration is rounded to DURATION_DECIMAL_PLACES"""
        @callback.track_node_timing("calculate_metrics")
        async def test_node():
            await asyncio.sleep(0.123456)
            return True

        await test_node()

        duration = callback.metrics[0]["duration_seconds"]
        decimal_places = len(str(duration).split('.')[1]) if '.' in str(duration) else 0
        assert decimal_places <= DURATION_DECIMAL_PLACES

    @pytest.mark.asyncio
    async def test__track_node_timing__preserves_function_metadata(self, callback):
        """Test that @functools.wraps preserves function metadata"""
        @callback.track_node_timing("test_node")
        async def sample_function():
            """Sample docstring"""
            return "result"

        assert sample_function.__name__ == "sample_function"
        assert sample_function.__doc__ == "Sample docstring"

    def test__write_file__writes_metrics_to_json(self, callback, temp_output_file):
        """Test that _write_file writes metrics to JSON file"""
        callback.metrics = [
            {
                "tool_name": "filter",
                "model": "gpt-4",
                "input_tokens": 100,
                "output_tokens": 50,
                "total_tokens": 150,
                "duration_seconds": 1.234
            }
        ]

        callback._write_file()

        assert os.path.exists(temp_output_file)
        with open(temp_output_file, 'r') as f:
            data = json.load(f)

        assert len(data) == 1
        assert data[0]["tool_name"] == "filter"
        assert data[0]["input_tokens"] == 100
        assert callback._written is True

    def test__write_file__creates_parent_directory(self, temp_output_dir):
        """Test that _write_file creates parent directory if it doesn't exist"""
        nested_path = os.path.join(temp_output_dir, "nested", "dir", "metrics.json")
        callback = TokenUsageCallback(output_path=nested_path)

        callback.metrics = [{"tool_name": "test", "duration_seconds": 1.0}]
        callback._write_file()

        assert os.path.exists(nested_path)
        assert os.path.exists(os.path.dirname(nested_path))

    def test__write_file__handles_empty_metrics(self, callback, caplog):
        """Test that _write_file handles empty metrics gracefully"""
        callback.metrics = []

        with caplog.at_level('WARNING'):
            callback._write_file()

        assert "No metrics data to write" in caplog.text

    def test__write_file__prevents_duplicate_writes(self, callback, temp_output_file):
        """Test that _write_file prevents duplicate writes"""
        callback.metrics = [
            {
                "tool_name": "filter",
                "model": "gpt-4",
                "input_tokens": 100,
                "output_tokens": 50,
                "total_tokens": 150,
                "duration_seconds": 1.234
            }
        ]

        callback._write_file()
        first_write_time = os.path.getmtime(temp_output_file)

        callback.metrics.append({
            "tool_name": "judge",
            "model": "gpt-4",
            "input_tokens": 200,
            "output_tokens": 100,
            "total_tokens": 300,
            "duration_seconds": 2.345
        })

        callback._write_file()
        second_write_time = os.path.getmtime(temp_output_file)

        assert first_write_time == second_write_time

        with open(temp_output_file, 'r') as f:
            data = json.load(f)
        assert len(data) == 1

    def test__write_file__handles_write_errors(self, callback, caplog):
        """Test that _write_file handles write errors gracefully"""
        callback.output_path = "/invalid/path/that/does/not/exist/metrics.json"
        callback.metrics = [{"tool_name": "test", "duration_seconds": 1.0}]

        with caplog.at_level('ERROR'):
            callback._write_file()

        assert "Failed to write metrics file" in caplog.text

    @pytest.mark.asyncio
    async def test__integration__full_workflow_with_tokens_and_timing(self, callback):
        """Integration test: full workflow with both token tracking and timing"""
        run_id = "integration-run-1"
        callback._current_contexts[run_id] = {
            "tool_name": "judge_llm_analysis",
            "model": "gpt-4"
        }

        llm_output = {
            "token_usage": {
                "prompt_tokens": 500,
                "completion_tokens": 250,
                "total_tokens": 750
            }
        }
        response = LLMResult(generations=[], llm_output=llm_output)
        callback.on_llm_end(response=response, run_id=run_id)

        @callback.track_node_timing("judge_llm_analysis")
        async def judge_node():
            await asyncio.sleep(0.2)
            return {"verdict": "not_vulnerable"}

        result = await judge_node()

        assert result == {"verdict": "not_vulnerable"}
        assert len(callback.metrics) == 1
        assert callback.metrics[0]["tool_name"] == "judge_llm_analysis"
        assert callback.metrics[0]["model"] == "gpt-4"
        assert callback.metrics[0]["input_tokens"] == 500
        assert callback.metrics[0]["output_tokens"] == 250
        assert callback.metrics[0]["total_tokens"] == 750
        assert callback.metrics[0]["duration_seconds"] >= 0.2
        assert callback.metrics[0]["duration_seconds"] is not None