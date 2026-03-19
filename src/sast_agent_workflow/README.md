# SAST Agent Workflow

This directory contains the NAT framework implementation of the SAST AI Workflow using [Nvidia NeMo's agent architecture](https://docs.nvidia.com/nemo/agent-toolkit/latest/).

## Overview

The SAST Agent Workflow implements a LangGraph-based agent architecture for static application security testing analysis. It provides modular tools for processing SAST reports, analyzing vulnerabilities with LLMs, and generating comprehensive results.

## Architecture

- **Workflow Type**: `sast_agent` - Custom workflow registered via NAT framework
- **LangGraph Integration**: Uses a linear pipeline; iterative logic is encapsulated inside the `investigate` node
- **Modular Nodes**: Each analysis step implemented as a separate node

## Core Components

### Nodes (`nodes/`)
- `pre_process` - Initialize workflow and load configuration
- `filter` - Filter known false positives using embeddings
- `investigate` - Autonomously investigate each SAST issue via a Research → Analysis → Evaluation subgraph (see [investigation/README.md](nodes/sub_agents/investigation/README.md))
- `summarize_justifications` - Generate analysis summaries
- `calculate_metrics` - Calculate performance metrics
- `write_results` - Export final results

### Configuration Files
- `configs/config.yml` - NAT workflow configuration
- `graph_builder.py` - LangGraph workflow structure
- `register.py` - NAT component registration

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
nat run --config_file src/sast_agent_workflow/configs/config.yml --input "sast_analysis"
```

**Test:**
```bash
PYTHONPATH=. pytest tests/
```
