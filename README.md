# sast-ai-workflow

## Overview

SAST-AI-Workflow is a LLM-based tool designed to detect and flag suspected vulnerabilities. It inspects suspicious lines of code in a given repository and deeply review legitimacy of the error. Workflow is capable of  integrating SAST reports, source code analysis, CVE data and other known examples. 

SAST-AI-Workflow can be incorporated to help and provide insights in the vulnerability detection precess. As an instance, in this project we demonstrate SAST scanning of RHEL systemd (source: [systemd GitHub](https://github.com/systemd/systemd)) project.

## Architecture

Key components:

### Input Sources

- **SAST HTML Reports:**  
  Processes scan results from SAST HTML reports.

- **Source Code Repository:**  
  Source code is obtained from the systemd-rhel9 repository. It recursively scans the `src` directory for `.c` and `.h` files and converts all detected source files into embeddings.

- **CVE Information:**  
  Embeds additional CVE data extracted from HTML pages to enrich the context used for the vulnerability analysis.

- **Known False Positives:**  
  Incorporates known cases for better results.

### Embeddings & Vector Store

- Converts the source data into embeddings using a local HuggingFace model ([all-mpnet-base-v2](https://huggingface.co/sentence-transformers/all-mpnet-base-v2)) and stores them in a FAISS vector store.

### Language Model Integration

- Uses NVIDIA's API via the `ChatNVIDIA` integration to query the vector store and generate analysis responses.

### Evaluation

- Applies metrics (from Ragas library) to assess the quality of model outputs.

A detailed architecture diagram is provided in the `diagrams/` folder (e.g., [`diagrams/architecture.png`](diagrams/architecture.png)).

## Evaluation & Metrics

The evaluations of the model responses are being done using the following metrics:

- **Response Relevancy:**  
  Ensures that the generated answers are directly related to the query.  
  [Response Relevancy](https://docs.ragas.io/en/latest/concepts/metrics/available_metrics/answer_relevance/).
  

## Installation & Setup

### 1. Clone the Repository

```bash
git clone git@github.com:RHEcosystemAppEng/sast-ai-workflow.git
```

### 2. Download Secret Configuration Files

Retrieve the secret configuration files from the project’s Google Drive and place them in the appropriate location.

### 3. Optional - Use Existing FAISS Index

If you prefer not to generate embeddings for the source code files, download the index.faiss file from the drive and place it under the appropriate folder (e.g., the `src` folder).

### 4. Install Dependencies

Install the required dependencies:

```bash
pip install -r requirements.txt
```

### 5. Configure Environment Variables

Create a .env file (or use the existing one in the drive and place it) in the root directory and set the following:

```bash
NVIDIA_API_KEY=<your_nvidia_api_key>
```

### 6. Install the Embedding Model

Download the embedding model locally:

```bash
git clone https://huggingface.co/sentence-transformers/all-mpnet-base-v2
```

Alternatively, if you are using the OpenShift cluster, follow the provided cluster-specific instructions.

## Configuration Options

The project supports configuration via a YAML file located in the `config/` folder (e.g., [`config/default_config.yaml`](config/default_config.yaml)). These values provide default settings that can be overridden by environment variables. Below is a table that describes the available configuration options:

| **Config Key**                      | **Default Value**                      | **Description**                                                                                     |
|-------------------------------------|----------------------------------------|-----------------------------------------------------------------------------------------------------|
| `LLM_URL`                           | `http://<<please-set-llm-url>>`          | URL of the language model endpoint.                                                               |
| `LLM_MODEL_NAME`                    | `llm-model`                            | Identifier of the language model to use.                                                          |
| `EMBEDDINGS_LLM_MODEL_NAME`         | `embedding-llm-model`                    | Model used for generating embeddings.                                                             |
| `REPORT_FILE_PATH`                  | `/path/to/report.html`                   | Path to the SAST HTML report.                                                                       |
| `KNOWN_FALSE_POSITIVE_FILE_PATH`    | `/path/to/known_false_positives_file`    | Path to the file containing known false positives data.                                           |
| `OUTPUT_FILE_PATH`                  | `/path/to/output_excel.xlsx`             | Path where the generated Excel report will be saved.                                              |
| `HUMAN_VERIFIED_FILE_PATH`          | `<<unknown>>`                           | Path to the human verified results file (used for evaluation).                                    |
| `GIT_REPO_PATH`                     | `/path/to/git/repo`                      | Path or URL of the Git repository to analyze.                                                     |
| `USE_KNOWN_FALSE_POSITIVE_FILE`     | `true`                                 | Flag indicating whether to use the known false positives file in the pipeline as an input.          |
| `CALCULATE_METRICS`                 | `true`                                 | **Important:** When enabled, evaluation metrics are calculated using the LLM, which sends a request and may consume API credits. |
| `OUTPUT_EXCEL_GENERATION`           | `true`                                 | Flag indicating whether to generate an Excel report with the results.                             |
| `DOWNLOAD_GIT_REPO`                 | `false`                                | Flag indicating whether to automatically download the Git repository.                             |

> **Note:**  
> The values set in the [configuration file](config/default_config.yaml) serve as defaults. Environment variables override these defaults at runtime. Sensitive values, such as API keys, should not be included in this file if the repository is public.

## Usage

Run the main workflow by executing:

```bash
python run.py
```

This command will:

Process the SAST report ->
Generate embeddings from the input sources ->
Query the language model to analyze the vulnerabilities ->
Evaluate the response using the defined metrics ->
Export the final summary to an Excel file.
