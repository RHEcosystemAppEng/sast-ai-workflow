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
EMBEDDINGS_API_KEY=<your_api_key>
CRITIQUE_LLM_API_KEY=<your_api_key> # If critique phase is enabled
```

### 6. Deploying an Embedding Model on RHOAI

If you haven't deployed an embedding model yet, follow [these steps](deploy_models.md) to set it up on Red Hat OpenShift AI

Then set the following environment variables:

- `EMBEDDINGS_LLM_URL`: Your model's external endpoint, including the version path (e.g., `http://<embedding-llm-endpoint>/v1`)
- `EMBEDDINGS_API_KEY`: Your model's API token
- `EMBEDDINGS_LLM_MODEL_NAME`: The name of your embedding model (e.g., `sentence-transformers/all-mpnet-base-v2`)




## Running the Application in a Container (Locally)

To run the container in detached mode, providing the LLM API key via an environment variable, use:

```bash
podman run -d --name sast-ai-app \
-e PROJECT_NAME=systemd \
-e PROJECT_VERSION=257-9 \
-e LLM_URL=http://<<please-set-llm-url>> \
-e LLM_MODEL_NAME="<Model Name>" \
-e LLM_API_KEY=<your_key> \
-e EMBEDDINGS_LLM_URL=http://<<please-set-embedding-llm-url>> \
-e EMBEDDINGS_API_KEY=<your_key> \
-e EMBEDDINGS_LLM_MODEL_NAME=<<embeddings-llm-model-name>> \
-e INPUT_REPORT_FILE_PATH=https://docs.google.com/spreadsheets/d/<sheet-id> \
-e KNOWN_FALSE_POSITIVE_FILE_PATH=/path/to/ignore.err \
-e OUTPUT_FILE_PATH=https://docs.google.com/spreadsheets/d/<sheet-id> \
quay.io/ecosystem-appeng/sast-ai-workflow:latest
```
Replace <your_key> with the actual LLM API key.

> **Note:**  
> Make sure the file paths required by the application (e.g., the HTML report, known false positives, etc.) point to the correct locations inside the container. For instance, if these files are copied into `/app`, update your configuration to reference `/app/<filename>` rather than the host paths.
> 
> If you ever need to run an interactive shell in your container (overriding the default entrypoint), use:
> 
> ```bash
> podman run -it --entrypoint /bin/bash quay.io/ecosystem-appeng/sast-ai-workflow:latest
> ```


## Configuration Options

The project supports configuration via a YAML file located in the `config/` 
folder (e.g., [`config/default_config.yaml`](config/default_config.yaml)). These values provide default settings that can be overridden by 
environment variables. 

| **Environment Variable**       | **Default Value**                         | **Mandatory** | **Example Value**                          | **Description**                                                                                                                  |
|--------------------------------|-------------------------------------------|---------------|--------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------|
| PROJECT_NAME                   |  ""                                       | ✔             | systemd                                    | Name of the project being analyzed.                                                                                              |
| PROJECT_VERSION                |  ""                                       | ✔             | 257-9                                      | Version of the project being analyzed.                                                                                           |
| LLM_URL                        | http://\<<please-set-llm-url\>>           | ✔             | https://integrate.api.nvidia.com/v1        | URL of the language model endpoint.                                                                                              |
| LLM_MODEL_NAME                 | \<<please-set-llm-model-name\>>           | ✔             | nvidia/llama-3.1-nemotron-70b-instruct     | Identifier of the language model to use.                                                                                         |
| EMBEDDINGS_LLM_URL             | http://\<<please-set-embedding-llm-url\>> | ✔             | https://integrate.api.nvidia.com/v1        | URL of the embedding model endpoint.                                                                                              |
| EMBEDDINGS_LLM_MODEL_NAME      | \<<please-set-embeddings-llm-model-name\>>| ✔             | all-mpnet-base-v2                          | Model used for generating embeddings.                                                                                            |
| INPUT_REPORT_FILE_PATH         | /path/to/report.html                      | ✔             | /path/to/report.html or https://docs.google.com/spreadsheets/d/\<sheet-id\> | Path to the SAST HTML report or URL of a Google Sheet containing the report.                                                                                                        |
| KNOWN_FALSE_POSITIVE_FILE_PATH | /path/to/ignore.err                       | ✔             | /path/to/ignore.err                        | Path to the file containing known false positives data.                                                                          |
| OUTPUT_FILE_PATH               | /path/to/output_excel.xlsx                | ✔             | /path/to/output.xlsx                       | Path where the generated Excel report will be saved.                                                                             |
| LIBCLANG_PATH                  | /path/to/libclang                         | ✔             | /usr/lib/llvm-12/lib/libclang.so           | Path of to your libclang location.                                                                                               |
| COMPILE_COMMANDS_JSON_PATH     | /path/to/compile_commands.json            |               | /path/to/compile_commands.json             | Path to the generated `compile_commands.json` file for the analyzed project. Required only for C projects. |
| CHUNK_SIZE                     | 500                                       |               | 500                                        | Maximum size for each text chunk.                                                                                                |
| CHUNK_OVERLAP                  | 0                                         |               | 50                                         | Number of overlapping characters between consecutive chunks.                                                                     |
| CHUNK_SEPARATORS               | ["\n\n", "\n", ".", ";", ",", " ", ""]    |               | ["\n\n", "\n", ".", ";"]                   | Ordered list of separators to use when splitting text into chunks.                                                               |
| CONFIG_H_PATH                  | /path/to/config.h                         |               | /path/to/config.h                          | *(Optional)* Path to the generated `config.h` containing macro definitions. Used for accurate Clang parsing, but not strictly required. |
| SERVICE_ACCOUNT_JSON_PATH      | ""                                        |               | /path/to/sheet-access-bot-abc123.json      | Path to the JSON file for the Google service account used to access Google Sheets. Mandatory only if using a Google Sheet as input. |
| USE_KNOWN_FALSE_POSITIVE_FILE  | true                                      |               | true                                       | Flag indicating whether to use the known false positives file in the pipeline as an input.                                       |
| SIMILARITY_ERROR_THRESHOLD     | 2                                         |               | 3                                          | Number of Documents to return from known issues DB.                                                                              |
| MAX_ANALYSIS_ITERATIONS     | 2                                            |               | 3                                          | Maximum number of analysis loops allowed for any single issue.  |
| OUTPUT_EXCEL_GENERATION        | true                                      |               | true                                       | Flag indicating whether to generate an Excel report with the results.                                                            |
| AGGREGATE_RESULTS_G_SHEET      | ""                                        |               | https://docs.google.com/spreadsheets/d/\<sheet-id\>  | Path to the Google Sheet for result aggregation. If set, final results will be written there for cumulative calculations. |
| DOWNLOAD_REPO              | false                                     |               | true                                       | Flag indicating whether to automatically download the Git repository.                                                            |
| REPO_LOCAL_PATH                  | /path/to/git/repo                         |               | /home/user/git/repo                        | Path of the Git repository to analyze.                                                                                    |
| REPO_REMOTE_URL                  | ""                         |               | "https://github.com/systemd/systemd/tree/v257"                        | URL of the Git repository to analyze.                                                                                    |
| HUMAN_VERIFIED_FILE_PATH       | ""                                        |               | /path/to/manual_verification_results.xlsx  | Path to the human verified results file (used for evaluation). If empty, it will attempt to load from INPUT_REPORT_FILE_PATH if a Google Sheet is provided.       |
| CALCULATE_METRICS              | true                                      |               | true                                       | **Important:** When enabled, evaluation metrics are calculated using the LLM, which sends a request and may consume API credits. |
| SHOW_FINAL_JUDGE_CONTEXT       | true                                      |               | true                                       | Flag indicating whether to include context (of final judge) in the final output.                                                 |
| RUN_WITH_CRITIQUE              | false                                     |               | true                                       | Flag indicating whether to enable critique phase.                                                                                |
| CRITIQUE_LLM_URL               | LLM_URL                                   |               | https://integrate.api.nvidia.com/v1        | URL of the critique language model endpoint (if applicable). Default to LLM_URL if not provided.                                 |
| CRITIQUE_LLM_MODEL_NAME        | LLM_MODEL_NAME                            |               | deepseek-ai/deepseek-r1                    | Identifier of the language model to use for critique phase (if applicable). Must be set if Critique is enabled.                  |
| USE_CRITIQUE_AS_FINAL_RESULTS  | false                                     |               | true                                       | Flag indicating whether to use critique for metrics calculation.                                                                 |               |


> **Note:**  
> The values set in the [configuration file](../config/default_config.yaml) serve as defaults. Environment variables 
> override these defaults at runtime. Sensitive values, such as API keys, should not be included in this file if the 
> repository is public.

### Optional - Use Google Sheet as Input

You can use a Google Sheet as the input report instead of a HTML file.

- **Required Configuration**:
  - Set the Google Sheet URL in `INPUT_REPORT_FILE_PATH`.
  - Provide the path to the service account JSON file in `SERVICE_ACCOUNT_JSON_PATH`.

- **Note**: Ensure that the Google Sheet is shared with the service account email specified in the JSON file, granting it the necessary permissions to access the sheet.

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

### Logging Configuration

You can configure logging behavior using environment variables:

#### Log Level
Set the logging level with `LOG_LEVEL`. Possible values are:

- `DEBUG` - Shows all messages (most verbose)
- `INFO` - Shows informational, warning, error, and critical messages (default)
- `WARNING` - Shows warning, error, and critical messages only
- `ERROR` - Shows error and critical messages only  
- `CRITICAL` - Shows only critical messages (least verbose)

#### File Logging (Optional)
Enable file logging by setting `LOG_FILE` to your desired log file path.

#### Module-Specific Debug Logging
You can enable DEBUG level logging for specific modules while keeping others at INFO level using `DEBUG_MODULES`. Use comma-separated values for multiple modules.

#### Configuration Examples

```bash
# Basic usage - INFO level, console only
LOG_LEVEL=INFO

# Debug everything
LOG_LEVEL=DEBUG

# INFO level with file logging
LOG_LEVEL=INFO
LOG_FILE=app.log

# INFO level globally, but DEBUG for specific modules
LOG_LEVEL=INFO
DEBUG_MODULES=llm_utils

# Multiple modules with DEBUG, others at INFO, with file logging
LOG_LEVEL=INFO
DEBUG_MODULES=llm_utils,LLMService
LOG_FILE=debug.log
```

#### Output Format
- **Console**: Colored output for better readability during development
- **File**: Plain text without colors for log analysis tools and production use

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