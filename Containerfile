FROM quay.io/ecosystem-appeng/sast-ai-base-image:latest
USER 0
RUN yum install -y clang llvm-devel && yum clean all

WORKDIR /app

COPY requirements.txt .
# Install nvidia-nat packages first with --pre flag to ensure beta versions are available
RUN pip install --upgrade pip && \
    pip install --pre nvidia-nat==1.4.0b5 nvidia-nat-langchain==1.4.0b5 && \
    pip install -r requirements.txt

COPY config ./config/
COPY src ./src/
COPY evaluation ./evaluation/
COPY deploy ./deploy/
COPY pyproject.toml .

# Set version for setuptools-scm since .git folder is not available in container
ENV SETUPTOOLS_SCM_PRETEND_VERSION=1.0.0

RUN pip install -e .

RUN chown -R 1001:1001 /app && \
    chmod +x /app/deploy/tekton/scripts/*.sh && \
    mkdir -p /app/.cache/huggingface && \
    chown -R 1001:1001 /app/.cache
USER 1001

# Set Hugging Face cache directories to writable location
ENV HF_HOME=/app/.cache/huggingface
ENV TRANSFORMERS_CACHE=/app/.cache/huggingface
ENV HF_DATASETS_CACHE=/app/.cache/huggingface/datasets

VOLUME ["/etc/secrets"]

ENTRYPOINT ["nat", "run", "--config_file", "src/sast_agent_workflow/configs/config.yml", "--input", "sast_agent"]

