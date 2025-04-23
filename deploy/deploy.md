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

Use it like:

```bash
# Run the full deploy & execute flow
make all

# Or individual steps, e.g.
make tasks
make pipeline
make run
make logs
