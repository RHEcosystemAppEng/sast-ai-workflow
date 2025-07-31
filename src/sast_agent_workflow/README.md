# SAST Agent Workflow

> **Note:** This README is temporary and is only used during the development phase of NeMo integration.

## Overview

This workflow provides a skeleton for implementing SAST analysis capabilities using an AI agent approach. The workflow is designed as a `sast_agent` type and implements various tools for SAST analysis and result processing.

## Structure

- **Workflow Type**: `sast_agent` - Custom workflow type registered in `register.py`
- **Tools Package**: Contains the following analysis and processing tools:
  - calculate_metrics
  - data_fetcher
  - evaluate_analysis
  - filter
  - judge_llm_analysis
  - pre_process
  - summarize_justifications
  - write_results

## Files

- `src/sast_agent_workflow/tools/` - Directory containing all analysis and processing tools
- `src/sast_agent_workflow/register.py` - Defines and registers the `sast_agent` workflow, including its LangGraph-based logic
- `configs/config.yml` - Workflow configuration
- `pyproject.toml` - Package configuration

## Installation

1. Create and activate a virtual environment:
   ```bash
   uv venv --seed -p python3.13 .venv
   source .venv/bin/activate
   ```

2. Install the SAST workflow package (first time):
   ```bash
   uv pip install -e .
   ```

   **Note:** For subsequent updates, use `aiq workflow reinstall sast_agent_workflow`

## Usage

Run the SAST agent workflow:
```bash
aiq run --config_file src/sast_agent_workflow/configs/config.yml --input <some str>
```

**Note:** The string input is only present because NeMo requires some input parameter, but this input is not actually used in the workflow. The real inputs come from:
1. The `default_config.yaml` file
2. Environment variables
