# How to run 

### 1. Clone the Repository
```bash
git clone git@github.com:RHEcosystemAppEng/sast-ai-workflow.git
```
### 2. Download Secret Configuration Files
Retrieve the secret configuration files(LLM API keys if required) from the project’s Google Drive and place them in the 
appropriate location.

### 3. Optional - Use Existing FAISS Index
If you prefer not to generate embeddings for the source code files(systemd project), 
download the index.faiss file from the drive and place it under the appropriate folder (e.g., the project root folder).

### 4. Install Dependencies
Install the required dependencies:

```bash
pip install -r requirements.txt
```

To extract C functions using Clang's AST, the libclang shared library must be installed on your system.  
This library is not bundled with the Python clang bindings and must be installed separately.

#### Linux:
```bash
sudo apt update
sudo apt install clang libclang-dev
```
To find the libclang path, run:
```bash
find /usr -name "libclang*"
```

#### Fedora:
```bash
sudo dnf update -y
sudo dnf install -y clang llvm-devel
```

#### macOS:
```bash
brew install llvm 
```
To find the libclang path, run:
```bash
ls -l $(brew --prefix llvm)/lib/libclang.dylib
```

### 5. Configure Environment Variables
Create a .env file (or use the existing one in the drive and place it) in the root directory and set the following:

```bash
LLM_API_KEY=<your_api_key>
CRITIQUE_LLM_API_KEY=<your_api_key> # If critique phase is enabled
```

### 6. Install the Embedding Model
Download the embedding model locally:

```bash
git clone https://huggingface.co/sentence-transformers/all-mpnet-base-v2
```

Alternatively, if you are using the Red Hat OpenShift AI, follow the provided cluster-specific instructions. 
(E.g: LLM_URL)

## Configuration Options

The project supports configuration via a YAML file located in the `config/` 
folder (e.g., [`config/default_config.yaml`](config/default_config.yaml)). These values provide default settings that can be overridden by 
environment variables. 

| **Environment Variable**       | **Default Value**                         | **Description**                                                                                                                  |
|--------------------------------|-------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------|
| LLM_URL                        | http://<<please-set-llm-url>>             | URL of the language model endpoint.                                                                                              |
| LLM_MODEL_NAME                 | <<please-set-llm-model-name>>             | Identifier of the language model to use.                                                                                         |
| EMBEDDINGS_LLM_MODEL_NAME      | <<please-set-embeddings-llm-model-name>>  | Model used for generating embeddings.                                                                                            |
| CHUNK_SIZE                     | 500                                       | Maximum size for each text chunk.                                                                                                |
| CHUNK_OVERLAP                  | 0                                         | Number of overlapping characters between consecutive chunks.                                                                     |
| CHUNK_SEPARATORS               | ["\n\n", "\n", ".", ";", ",", " ", ""]    | Ordered list of separators to use when splitting text into chunks.                                                               |
| LIBCLANG_PATH                  | /path/to/libclang                         | Path of to your libclang location.                                                                                               |                                                                  |
| INPUT_REPORT_FILE_PATH         | /path/to/report.html                      | Path to the SAST HTML report.                                                                                                    |
| USE_KNOWN_FALSE_POSITIVE_FILE  | true                                      | Flag indicating whether to use the known false positives file in the pipeline as an input.                                       |
| KNOWN_FALSE_POSITIVE_FILE_PATH | /path/to/ignore.err                       | Path to the file containing known false positives data.                                                                          |
| SIMILARITY_ERROR_THRESHOLD     | 2                                         | Number of Documents to return from known issues DB.                                                                              |
| OUTPUT_EXCEL_GENERATION        | true                                      | Flag indicating whether to generate an Excel report with the results.                                                            |
| OUTPUT_FILE_PATH               | /path/to/output_excel.xlsx                | Path where the generated Excel report will be saved.                                                                             |
| DOWNLOAD_GIT_REPO              | false                                     | Flag indicating whether to automatically download the Git repository.                                                            |
| GIT_REPO_PATH                  | /path/to/git/repo                         | Path or URL of the Git repository to analyze.                                                                                    |
| HUMAN_VERIFIED_FILE_PATH       | /path/to/manual_verification_results.xlsx | Path to the human verified results file (used for evaluation).                                                                   |
| CALCULATE_METRICS              | true                                      | **Important:** When enabled, evaluation metrics are calculated using the LLM, which sends a request and may consume API credits. |
| SHOW_FINAL_JUDGE_CONTEXT       | true                                      | Flag indicating whether to include context (of final judge) in the final output.                                                 |
| RUN_WITH_CRITIQUE              | false                                     | Flag indicating whether to enable critique phase.                                                                                |
| CRITIQUE_LLM_URL               | LLM_URL                                   | URL of the critique language model endpoint (if applicable). Default to LLM_URL if not provided.                                 |
| CRITIQUE_LLM_MODEL_NAME        | LLM_MODEL_NAME                            | Identifier of the language model to use for critique phase (if applicable). Must be set if Critique is enabled.                  |
| USE_CRITIQUE_AS_FINAL_RESULTS  | false                                     | Flag indicating whether to use critique for metrics calculation.                                                                 |


> **Note:**  
> The values set in the [configuration file](../config/default_config.yaml) serve as defaults. Environment variables 
> override these defaults at runtime. Sensitive values, such as API keys, should not be included in this file if the 
> repository is public.

### Optional - Enable Critique Model

The critique model introduces an independent phase to review the main model's response based on its context, the issue, 
and the instructions provided to the main LLM.

- **Required Configuration**:
  - `RUN_WITH_CRITIQUE`: Set to `true` to enable the critique phase.
  - `CRITIQUE_LLM_MODEL_NAME`: Name of the critique model (recommended to be different from the main LLM).
  - If the critique model uses a different endpoint, set:
    - `CRITIQUE_LLM_URL`
    - `CRITIQUE_LLM_API_KEY`

- **Optional Configuration**:
  - `USE_CRITIQUE_AS_FINAL_RESULTS`: Set to `true` to use critique results for final metrics calculation.

## Usage

Run the main workflow by executing:

```bash
python run.py
```

This command will:

1. Process the given SAST report
2. Generate embeddings from the input sources
3. Query the language model to analyze the vulnerabilities
4. Evaluate the response using the defined metrics ->
5. Export the final summary to an Excel file.