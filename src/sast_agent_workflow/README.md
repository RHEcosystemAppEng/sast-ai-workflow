# SAST Agent Workflow

## Overview

This workflow provides a skeleton for implementing SAST analysis capabilities using an AI agent approach. The workflow is designed as a `sast_agent` type and includes placeholder functionality that can be extended for actual SAST analysis tasks.

## Structure

- **Workflow Type**: `sast_agent` - Custom workflow type registered in `register.py`
- **Functions**: 
  - `sast_placeholder_function` - Placeholder for SAST analysis functionality
- **Agent Registration**: `sast_agent` workflow type defines how the agent works (placeholder for LangGraph flow)

## Files

- `src/sast_agent_workflow/tools/sast_placeholder_function.py` - Placeholder function implementation
- `src/sast_agent_workflow/register.py` - Defines and registers the `sast_agent` workflow, including its LangGraph-based logic
- `configs/config.yml` - Workflow configuration
- `pyproject.toml` - Package configuration

## Installation

1. Create and activate a virtual environment:
   ```bash
   uv venv --seed -p python3.13 .venv
   source .venv/bin/activate
   ```

2. Install dependencies:
   ```bash
   uv pip install agentiq
   uv pip install aiqtoolkit[langchain]
   ```

3. Install the SAST workflow package (first time):
   ```bash
   uv pip install -e .
   ```

   **Note:** For subsequent updates, use `aiq workflow reinstall sast_agent_workflow`

## Usage

Run the SAST agent workflow with test data:
```bash
aiq run --config_file src/sast_agent_workflow/configs/config.yml --input_file sast_test_data.json
```

**Note:** The `sast_test_data.json` file is temporary and will be replaced with the current input format of the original workflow.

## Development

This is a skeleton/template that should be customized with actual SAST analysis logic:

1. Replace the placeholder function with real SAST analysis capabilities
2. Add additional functions as needed for your specific SAST workflow
3. Integrate with existing SAST tools and analysis pipelines
