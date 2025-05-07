# 🚀 SAST‑AI‑Workflow Tekton Pipeline on OpenShift

This guide shows how to run the SAST‑AI‑Workflow Tekton pipeline on an OpenShift cluster and describes some details on the OCP project.

---

## 🔧 Existing Project Resources

The `sast-ai-workflow` project includes:

- **PersistentVolumeClaim** `sast-ai-workflow-pvc`  
  Used as the shared workspace across pipeline tasks (e.g., source tree extraction, false‑positives mount).

- **Secret** `gitlab-token-secret`  
  Holds the GitLab personal access token under the key `gitlab_token`, for authenticated fetch of known false‑positive files.

---

## 🛠️ Deploy the Pipeline

A `Makefile` at the repository root simplifies all operations. By default it targets the `sast-ai-workflow` namespace.

| Make Target    | Description                                       |
| -------------- | ------------------------------------------------- |
| `make tasks`   | Apply all Tekton **Task** definitions             |
| `make pipeline`| Apply the **Pipeline** definition                 |
| `make run`     | Re-create and start the **PipelineRun**           |
| `make logs`    | Stream **PipelineRun** logs                       |
| `make all`     | Run **tasks**, **pipeline**, **run**, and **logs**|
| `make clean`   | Delete all Tekton resources                       |

### 🔄 Overriding the Namespace

By default the Makefile targets the `sast-ai-workflow` namespace. To run against a different namespace, pass `NAMESPACE` on the `make` command line:

```bash
# e.g. deploy everything into "my-custom-ns" instead of the default
make NAMESPACE=my-custom-ns all

# or run individual steps against that namespace
make NAMESPACE=my-custom-ns tasks
make NAMESPACE=my-custom-ns pipeline
make NAMESPACE=my-custom-ns run
make NAMESPACE=my-custom-ns logs
```

### 🔧 Overriding Configuration Parameters

You can also override any of the LLM or embedding settings by adding them to the `make run` invocation:

```bash
make run \
  SOURCE_URL="https://my.rpm/url.rpm" \
  SPREADSHEET_URL="https://docs.google.com/…/export?format=csv" \
  FALSE_POSITIVES_URL="https://…/ignore.err" \
  LLM_URL="llm/model/url" \
  LLM_MODEL_NAME="llm-model-name" \
  EMBEDDINGS_LLM_URL="embedding/model/url" \
  EMBEDDINGS_LLM_MODEL_NAME="embedding-model-name"
```

That will start the pipeline with your custom endpoints and model names, injecting them as environment variables into the SAST-AI task.
