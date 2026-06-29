# Konflux Integration

Tekton Task definition for running SAST-AI as a step in a [Konflux](https://konflux-ci.dev/) pipeline.

## `sast-ai-integration.yaml`

This Task should be added as a custom tekton task in Konflux CI. Then should configue the push/merge CI triggers to run. The purpose of this custom tekton task is to trigger SAST-AI from a Konflux CI pipeline. It does the following actions:

1. Downloads application source code from Trusted Artifacts.
2. Discovers and pulls SARIF findings attached to the built container image via ORAS.
3. Triggers a SAST-AI Orchestrator scan against the source.

### Parameters

| Parameter | Description | Required |
|---|---|---|
| `SOURCE_ARTIFACT` | Trusted Artifact URI for the application source code | Yes |
| `CACHI2_ARTIFACT` | Trusted Artifact URI for prefetched dependencies | No |
| `IMAGE_URL` | Container image URL to check for attached SARIF files | Yes |
| `IMAGE_DIGEST` | Container image digest | Yes |
| `GIT_URL` | Public git repository URL | Yes |
| `GIT_REVISION` | Git commit SHA or tag | Yes |
| `SAST_RESULT` | SAST scan result status from a previous pipeline task | No |

### Prerequisites

- A `source-list-config` ConfigMap must exist in the namespace.
- The pipeline must have ORAS-compatible registry credentials for SARIF artifact discovery.
