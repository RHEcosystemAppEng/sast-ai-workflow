## Deploying the Project with CRC

This guide outlines the steps to deploy the project on a local OpenShift cluster using CodeReady Containers (CRC).

If you wish to run the pipeline using our OpenShift cluster, you just need to use the `Makefile` as described in step 6.5. (Example Usage). Make sure `NAMESPACE` and `CONTEXT` in the `Makefile` are set to the right variables.

### 1. Install CRC

   * **Download CRC:**
        * Go to the Red Hat OpenShift Local download page: https://developers.redhat.com/products/openshift-local/overview
        * Download the CRC bundle for your operating system.
   * **Install CRC:**
        * Extract the downloaded file.
        * Run the `crc setup`. This may require administrator privileges.
        * Follow the on-screen instructions, which may involve providing your Red Hat account pull secret.
   * **Adjust Disk Size (Recommended):**
        * Run the `crc config set disk-size 100`. You can specify a different number but it's better to use no less then 60GB because it get could get full after many runs.
   * **Start CRC:**
        * Start the CRC cluster using the `crc start` command. This may also require administrator privileges.
   * **Verify CRC:**
        * Once CRC has started, verify that the cluster is running correctly:

            ```bash
            crc status
            ```

        * Ensure that the CRC VM is running and OpenShift is healthy.

### 2. Install the OpenShift Pipelines Operator

Go to the Operator Hub in the console and install the operator.

You can use:

```bash
oc whoami --show-console
```
     

### 3. Create Secrets and Patch the Pipeline Service Account

   This project requires several secrets for GitLab token, LLM API keys, Google service account, and Quay registry access.

   #### 3.1. Automated Secret Creation (Recommended)

       **Prerequisites:**
    * Create a `.env` file in the project root with the following variables:
      ```env
      # Copy this template to .env and fill in your actual values
      
      # GitLab API token for accessing repositories
      GITLAB_TOKEN=your_gitlab_token_here
      
      # LLM API key for the main language model
      LLM_API_KEY=your_llm_api_key_here
      
      # Embeddings API key for the embedding model
      EMBEDDINGS_API_KEY=your_embeddings_api_key_here
      
      # Path to Google Service Account JSON file (relative to project root)
      GOOGLE_SERVICE_ACCOUNT_JSON_PATH=./service_account.json
      
      # Optional: Override default Docker config path if needed
      # DOCKER_CONFIG_PATH=/custom/path/to/docker/config.json
      ```
   * Place your Google service account JSON file in the project root as `service_account.json` (or specify custom path in `.env`)
   * Login to Quay.io registry: `podman login quay.io` (or `docker login quay.io`)

   **Create all secrets automatically:**
   ```bash
   make secrets
   ```

   This single command will:
   - Create GitLab token secret from your `.env` file
   - Create LLM and embeddings API key secrets from your `.env` file  
   - Create Google service account secret from the JSON file
   - Create Quay pull secret from your local Docker/Podman credentials
   - Patch the pipeline service account to use the image pull secret

   #### 3.2. Manual Secret Creation (Alternative)

   If you prefer to create secrets manually or need to troubleshoot:

   * **Create the Kubernetes Secrets:**

        ```bash
        oc -n $(NAMESPACE) create secret generic gitlab-token-secret --from-literal=gitlab_token="$(GITLAB_TOKEN)"
        oc -n $(NAMESPACE) create secret generic embeddings-api-key-secret --from-literal=api_key="$(EMBEDDINGS_API_KEY)"
        oc -n $(NAMESPACE) create secret generic llm-api-key-secret --from-literal=api_key="$(LLM_API_KEY)"
        oc -n $(NAMESPACE) create secret generic google-service-account-secret --from-file=service_account.json=/path/to/google/service/account/secret.json
        ```

   * **Create Quay Pull Secret:**

        ```bash
        oc --context $(CONTEXT) create secret generic quay-sast-puller --from-file=.dockerconfigjson=$XDG_RUNTIME_DIR/containers/auth.json --type=kubernetes.io/dockerconfigjson -n $(NAMESPACE)
        ```

   * **Patch the Pipeline Service Account:**

        ```bash
        oc --context $(CONTEXT) patch serviceaccount pipeline -n $(NAMESPACE) -p '{"imagePullSecrets": [{"name": "quay-sast-puller"}]}'
        ```

### 4. Create PVC

   First, make sure you have the correct configuration in your `Makefile`:

```bash
NAMESPACE ?= name/of/your/namespace # e.g. sast-ai-workflow
CONTEXT   ?= name/of/your/context   # e.g. sast-ai-workflow/api-crc-testing:6443/kubeadmin
```

   Then, run:

```bash
make pvc
```

### 5. Makefile Options

   The `Makefile` provides several options for managing and running the project. Here's a table summarizing the available `make` commands:

   | Command       | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
   | :------------ | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
   | `all`         | Executes `setup`, `tasks`, `pipeline` and `run` sequentially - does everything from infrastructure setup to pipeline execution.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
   | `setup`       | Executes `pvc` and `secrets` sequentially - sets up the basic infrastructure needed for the pipeline.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
   | `tasks`       | Applies the Tekton Task definitions (e.g., `validate_urls.yaml`, `prepare_source.yaml` etc.).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
   | `pvc`         | Applies the PersistentVolumeClaim (PVC) definition (`pvc.yaml`) to create the shared workspace.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
   | `secrets`     | Creates all required Kubernetes secrets automatically from `.env` file variables and patches the pipeline service account.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
   | `pipeline`    | Applies the Tekton Pipeline definition (`pipeline.yaml`).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
   | `run`         | Deletes any previous PipelineRun and starts a new one with the specified parameters. You can override the pipeline parameters using environment variables (e.g., `make run SOURCE_URL="..."`).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
   | `logs`        | Displays the logs of the running PipelineRun.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
   | `clean`       | Performs a complete cleanup by deleting all PipelineRuns, TaskRuns, Tekton resources, ALL PVCs in the namespace, orphaned PVs, secrets, and unpatching the pipeline service account. **Warning: This deletes ALL PVCs in the namespace, not just the ones created by this project.**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |

### 6. Example Usage

   Here's an example of how to deploy and run the project using the `Makefile`:

   #### 6.1.  **Create a new project (recommended):**

        ```bash
        oc --context sast-ai-workflow/api-crc-testing:6443/kubeadmin new-project sast-ai-workflow
        oc --context sast-ai-workflow/api-crc-testing:6443/kubeadmin project sast-ai-workflow
        ```

   #### 6.2.  **Set up your environment file:**

        Create a `.env` file in the project root with your credentials:
        ```env
        # GitLab API token for accessing repositories
        GITLAB_TOKEN=your_gitlab_token_here
        
        # LLM API key for the main language model
        LLM_API_KEY=your_llm_api_key_here
        
        # Embeddings API key for the embedding model
        EMBEDDINGS_API_KEY=your_embeddings_api_key_here
        
        # Path to Google Service Account JSON file
        GOOGLE_SERVICE_ACCOUNT_JSON_PATH=./service_account.json
        ```

        Place your Google service account JSON file as `service_account.json` in the project root.
        Login to Quay.io: `podman login quay.io`

   #### 6.3.  **Run everything with a single command:**

        ```bash
        make all
        ```

        This single command does everything: creates PVCs, secrets, applies Tekton resources, and runs the pipeline.

   #### Alternative: Step-by-step approach

   If you prefer to run steps individually:

   * **Set up infrastructure:** `make setup`
   * **Apply Tekton resources and run:** `make tasks pipeline run`

   #### 6.4.  **Full example with parameters:**

```bash
make all SOURCE_URL="https://download.devel.redhat.com/brewroot/vol/rhel-10/packages/systemd/257/9.el10/src/systemd-257-9.el10.src.rpm" \
 PROJECT_NAME="systemd" \
 PROJECT_VERSION="257-9" \
 SPREADSHEET_URL="https://docs.google.com/spreadsheets/d/1NPGmERBsSTdHjQK2vEocQ-PvQlRGGLMds02E_RGF8vY" \
 FALSE_POSITIVES_URL="https://gitlab.cee.redhat.com/osh/known-false-positives/-/raw/master/systemd/ignore.err" \
 LLM_URL="https://llama-31-test-yossi-test.apps.ai-dev03.kni.syseng.devcluster.openshift.com/v1" \
 LLM_MODEL_NAME="llama-31-test" \
 EMBEDDINGS_LLM_URL="https://all-mpnet-base-v2-sast-ai-embedding.apps.ai-dev03.kni.syseng.devcluster.openshift.com/v1" \
 EMBEDDINGS_LLM_MODEL_NAME="sentence-transformers/all-mpnet-base-v2" 
```

Replace the placeholders with your actual values.

** Note you may have to replace the suffix of the spreadsheet URL with `/export?format=csv`.
