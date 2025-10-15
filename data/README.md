# SAST-AI Workflow Data Directory

This directory contains all data assets managed by DVC for the SAST-AI workflow.

## Directory Structure

### Input Data
- **sast-reports/**: SAST scan reports from various tools (HTML/XML formats)
- **source-repos/**: Cloned source code repositories for analysis  
- **known-issues/**: Known false positives and verified known issue datasets
- **ground-truth/**: Human-verified evaluation data for performance assessment

### Processed Data  
- **embeddings/**: FAISS vector embeddings and indices
- **models/**: 
  - **embeddings/**: Embedding model caches
  - **prompts/**: Versioned LLM prompt templates

### Output Data
- **outputs/**:
  - **excel-reports/**: Generated analysis reports in Excel format
  - **metrics/**: Evaluation metrics and performance data

## DVC Management

All directories in this structure are managed by DVC for version control and reproducibility. Each major data category has its own `.dvc` tracking file to enable granular version management.

## Usage

Data files are automatically pulled by DVC during pipeline execution. Manual data access:

```bash
# Pull all data
dvc pull

# Pull specific data category  
dvc pull data/sast-reports
dvc pull data/embeddings
dvc pull data/ground-truth

# Check data status
dvc status
```