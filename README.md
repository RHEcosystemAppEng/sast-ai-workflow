# SAST-AI-Workflow
[![Quay.io](https://img.shields.io/badge/Quay.io-sast--ai--workflow-blue)](https://quay.io/repository/ecosystem-appeng/sast-ai-workflow)


## 🎯 Project Overview 
SAST-AI-Workflow is a LLM-based tool designed to detect and flag suspected vulnerabilities through 
SAST(Static Application Security Testing). It inspects suspicious lines of code in a given repository and 
deeply review the legitimacy of errors. This workflow involves existing SAST reports, source code analysis, CWE data 
and other known examples. 

### Purpose
The SAST-AI-Workflow can be integrated into the vulnerability detection process as an AI-assisted tool. It offers 
enhanced insights that may be overlooked during manual verification, while also reducing the time required by engineers.

As an initial step, we applied the workflow to the SAST scanning of the RHEL **systemd** project 
(source: [systemd GitHub](https://github.com/redhat-plumbers/systemd-rhel10)). We intend to extend this approach to support additional 
C-based projects in the future.

## 📐 Architecture 
![SAST-AI-Architecture](./diagrams/sast-architecture.svg)

### Input Sources
- **SAST HTML Reports:**  
  Processes scan results from SAST HTML reports.

- **Source Code:**  
  Pipeline requires to access the exact source code that was used to generate the SAST HTML report.

- **Verified Data:**  
  Incorporates a known error cases for continuous learning & better results.

- **CWE Information:**  
  Embeds additional CWE(Common Weakness Enumeration) data extracted to enrich the context data 
  used for the vulnerability analysis.

### Embeddings & Vector Store
- Converts input data(verified Data, source code) into embeddings using a specialized sentence transformer 
HuggingFace model ([all-mpnet-base-v2](https://huggingface.co/sentence-transformers/all-mpnet-base-v2)) and stores them in a in-memory vector store(FAISS).

### LLM Integration
- Uses NVIDIA's API via the `ChatNVIDIA` integration / uses LLM model deployed on Red Hat Openshift AI platform
to query the vector store and review potential SAST errors.

### Evaluation
- Applies metrics (from Ragas library) to assess the quality of model outputs.

## 📊 Evaluation & Metrics
The evaluations of the model responses are being done using the following metrics:
- **Response Relevancy:**  
  Ensures that the generated answers are directly related to the query.  
  [Response Relevancy](https://docs.ragas.io/en/latest/concepts/metrics/available_metrics/answer_relevance/).
  

## 🔌 Installation & Setup 
Please refer to [how to run](./docs/setup.md) guideline.


## 🚀 Running the Application in a Container (Locally)

Follow these steps to build, push, and run your container image:

1. **Build the Container Image:**

Use the following command to build your image. Adjust the tag (`1.0.0-SNAPSHOT`) as needed based on project stage.

  ```bash
  podman build -t quay.io/ecosystem-appeng/sast-ai-workflow:1.0.0-SNAPSHOT -f deployment/containerfile .
  ```

2. **Log in to Quay:**

Authenticate with your Quay credentials using:

```bash
podman login quay.io
```

3. **Push the Image:**

Once authenticated, push your image to the Quay repository:

```bash
podman push quay.io/ecosystem-appeng/sast-ai-workflow:1.0.0-SNAPSHOT
```

4. **Run the Container:**

To run the container in detached mode, providing the LLM API key via an environment variable, use:

```bash
podman run -d --name sast-ai-app -e LLM_API_KEY=<your_key> quay.io/ecosystem-appeng/sast-ai-workflow:1.0.0-SNAPSHOT
```
Replace <your_key> with the actual LLM API key.

> **Note:**  
> Make sure the file paths required by the application (e.g., the HTML report, known false positives, etc.) point to the correct locations inside the container. For instance, if these files are copied into `/app`, update your configuration to reference `/app/<filename>` rather than the host paths.
> 
> If you ever need to run an interactive shell in your container (overriding the default entrypoint), use:
> 
> ```bash
> podman run -it --entrypoint /bin/bash quay.io/ecosystem-appeng/sast-ai-workflow:1.0.0-SNAPSHOT
> ```
