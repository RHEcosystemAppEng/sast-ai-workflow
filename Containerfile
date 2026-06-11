# Note: Using 'latest' tag here is intentional as this image is built from
# Containerfile.base.dockerfile in the same repository and versioned together.
# The base image is published via GitHub Actions workflow (build-base-image.yml).
# hadolint ignore=DL3007
FROM quay.io/ecosystem-appeng/sast-ai-base-image:latest
USER 0
RUN yum install -y clang llvm-devel && yum clean all && \
    ln -sf /usr/lib64/libclang.so.21.1 /usr/lib64/libclang.so

WORKDIR /app

COPY requirements.txt requirements.lock ./
# Install from the fully pinned lock file without dependency resolution.
# for example:
# The base image contains s3fs==2026.4.0, whose metadata requires fsspec==2026.4.0,
# but the application intentionally pins fsspec==2025.10.0 because datasets==4.5.0
# requires fsspec<=2025.10.0. Runtime was validated with this combination.
# Using --no-deps avoids pip's dependency conflict check for this known mismatch.
RUN pip install --upgrade pip && pip install --no-deps -r requirements.lock

COPY config ./config/
COPY src ./src/
COPY evaluation ./evaluation/
COPY deploy ./deploy/
COPY pyproject.toml .

# Set version for setuptools-scm since .git folder is not available in container
ENV SETUPTOOLS_SCM_PRETEND_VERSION=1.0.0

RUN pip install --no-deps -e .

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

