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

   This project requires a GitLab token secret and a Quay pull secret.

   #### 3.1. Create Secrets
   * **Create the Kubernetes Secret:**

        ```bash
        oc -n $(NAMESPACE) create secret generic gitlab-token-secret --from-literal=gitlab_token="$(GITLAB_TOKEN)"
        ```

        Replace `$(GITLAB_TOKEN)` with the actual GitLab access token and `$(NAMESPACE)` with the OpenShift namespace where you'll deploy the project (e.g., `sast-ai-workflow`).

        Similarly:

        ```bash
        oc -n $(NAMESPACE) create secret generic embeddings-api-key-secret --from-literal=api_key="$(EMBEDDINGS_API_KEY)"
        ```

        ```bash
        oc -n $(NAMESPACE) create secret generic llm-api-key-secret --from-literal=api_key="$(LLM_API_KEY)"
        ```

        ```bash
        oc -n $(NAMESPACE) create secret generic google-service-account-secret --from-file=service_account.json=/path/to/google/service/account/secret.json
        ```

   #### 3.2. Create Quay Pull Secret

   * To pull images from our private Quay.io registry, you'll need to create an image pull secret.
        * If you already have a `dockerconfig.json` file (e.g., from `docker login` or `podman login`), you can create the secret from that:

            ```bash
            oc --context $(CONTEXT) create secret generic quay-sast-puller --from-file=.dockerconfigjson=$XDG_RUNTIME_DIR/containers/auth.json --type=kubernetes.io/dockerconfigjson -n $(NAMESPACE)
            ```

        * If you don't have a `dockerconfig.json` file, you will need to create one, or use the oc create secret docker-registry command.
   #### 3.3. Patch the Pipeline Service Account

   * Tekton Pipelines uses a service account to pull images. You need to patch this service account to use the Quay pull secret. The default service account is `pipeline`.

        ```bash
        oc --context $(CONTEXT) patch serviceaccount pipeline -n $(NAMESPACE) -p '{"imagePullSecrets": [{"name": "quay-sast-puller"}]}'
        ```

        Replace `pipeline` with the name of the service account used by your Tekton tasks if it's different.

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
   | `all`         | Executes `tasks`, `pipeline` and `run` sequentially.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
   | `tasks`       | Applies the Tekton Task definitions (e.g., `validate_urls.yaml`, `prepare_source.yaml` etc.).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
   | `pvc`         | Applies the PersistentVolumeClaim (PVC) definition (`pvc.yaml`) to create the shared workspace.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
   | `pipeline`    | Applies the Tekton Pipeline definition (`pipeline.yaml`).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
   | `run`         | Deletes any previous PipelineRun and starts a new one with the specified parameters. You can override the pipeline parameters using environment variables (e.g., `make run SOURCE_URL="..."`).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
   | `logs`        | Displays the logs of the running PipelineRun.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
   | `clean`       | Deletes all the Tekton Task, Pipeline, and PipelineRun resources.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |

### 6. Example Usage

   Here's an example of how to deploy and run the project using the `Makefile`:

   #### 6.1.  **Create a new project (recommended):**

        ```bash
        oc --context sast-ai-workflow/api-crc-testing:6443/kubeadmin new-project sast-ai-workflow
        oc --context sast-ai-workflow/api-crc-testing:6443/kubeadmin project sast-ai-workflow
        ```

   #### 6.2.  **Create the necessary secrets:**

        See step 3.1

   #### 6.3.  **Patch the pipeline service account:**

        See step 3.2

   #### 6.4.  **Create the PVC:**

        See step 4

   #### 6.5.  **Apply the Tekton resources and run the pipeline:**

```bash
make all SOURCE_URL="<your_source_code_url>" SPREADSHEET_URL="<your_spreadsheet_url>"** FALSE_POSITIVES_URL="<your_false_positives_url>" LLM_URL="<your_llm_url>" LLM_MODEL_NAME="<your_llm_model_name>" EMBEDDINGS_LLM_URL="<your_embeddings_llm_url>" EMBEDDINGS_LLM_MODEL_NAME="<your_embeddings_llm_model_name>" PROJECT_NAME="<your_project_name>" PROJECT_VERSION="<your_project_version>"
```

Replace the placeholders with your actual values.

** Note you may have to replace the suffix of the spreadsheet URL with `/export?format=csv`.
