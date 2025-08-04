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
# Run all tests
cd tests
python3 run_tests.py

# Run with verbose output
python3 run_tests.py --verbose

# Run specific test module
python3 run_tests.py --module test_sarif_reader
```

## Pre-commit Hooks

The pre-commit configuration (`.pre-commit-config.yaml`) includes:

### Code Quality Checks
- **Black**: Code formatting (100-character line length)
- **isort**: Import statement sorting
- **flake8**: Code linting and style checking

### Testing
- **Unit Tests**: Automatic execution of report reader tests
  - Runs when Python files in `src/` or `tests/` are modified
  - Uses the custom test runner with detailed output
  - Fails commit if any tests fail
`