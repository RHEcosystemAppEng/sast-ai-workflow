# Testing Setup Guide

This guide explains how to set up the testing environment and pre-commit hooks for the SAST-AI workflow project.

## Quick Setup

### 1. Install Dependencies

```bash
# Install all dependencies including testing tools
pip install -r requirements.txt
pip install -r dev-requirements.txt
```

### 2. Set Up Pre-commit Hooks

```bash
# Install pre-commit hooks (run once)
pre-commit install

# Test the hooks manually (optional)
pre-commit run --all-files
```

### 3. Run Tests

```bash
# Run all tests from the project root
pytest

# Run with verbose output
pytest -v

# Run tests in a specific file
pytest tests/unit/report_readers/test_html_reader.py

# Run tests in a specific directory
pytest tests/unit/

# Run a specific test using keywords
pytest -k "some_keyword_in_test_name"
```

## Pre-commit Hooks

The pre-commit configuration (`.pre-commit-config.yaml`) includes:

### Code Quality Checks
- **Black**: Code formatting (100-character line length)
- **isort**: Import statement sorting
- **flake8**: Code linting and style checking

### Testing
- **Unit Tests**: Automatic execution of all tests.
  - Runs when Python files in `src/` or `tests/` are modified
  - Fails commit if any tests fail
`