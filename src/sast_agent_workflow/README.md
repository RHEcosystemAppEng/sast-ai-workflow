# SAST Agent Workflow

This directory contains the AIQ framework implementation of the SAST AI Workflow using [Nvidia NeMo's agent architecture](https://docs.nvidia.com/aiqtoolkit/latest/index.html).

## Overview

The SAST Agent Workflow implements a LangGraph-based agent architecture for static application security testing analysis. It provides modular tools for processing SAST reports, analyzing vulnerabilities with LLMs, and generating comprehensive results.

## Architecture

- **Workflow Type**: `sast_agent` - Custom workflow registered via AIQ framework
- **LangGraph Integration**: Uses conditional edges for iterative analysis loops
- **Modular Tools**: Each analysis step implemented as a separate tool

## Core Components

### Tools (`tools/`)
- `pre_process` - Initialize workflow and load configuration
- `filter` - Filter known false positives using embeddings
- `data_fetcher` - Fetch source code for analysis
- `judge_llm_analysis` - Perform LLM-based vulnerability analysis
- `evaluate_analysis` - Evaluate results and determine next steps
- `summarize_justifications` - Generate analysis summaries
- `calculate_metrics` - Calculate performance metrics
- `write_results` - Export final results

### Configuration Files
- `configs/config.yml` - AIQ workflow configuration
- `graph_builder.py` - LangGraph workflow structure
- `register.py` - AIQ component registration

## Quick Reference

For complete setup and usage instructions, see the main project documentation:
- [Setup Guide](../../docs/setup.md) - Installation and configuration
- [Architecture Details](../../docs/architecture.md) - Technical architecture overview

### Quick Commands

**Install (from project root):**
```bash
uv pip install -e .
```

**Run:**
```bash
aiq run --config_file src/sast_agent_workflow/configs/config.yml --input "sast_analysis"
```

**Test:**
```bash
PYTHONPATH=. pytest tests/
```
