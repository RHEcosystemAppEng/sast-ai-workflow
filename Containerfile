FROM registry.access.redhat.com/ubi9/python-312 AS builder
USER 0
RUN yum install -y git clang llvm-devel && yum clean all

FROM builder
USER 1001
WORKDIR /app

COPY requirements.txt .
RUN pip install --upgrade pip && pip install -r requirements.txt

COPY config ./config/
COPY src ./src/
COPY pyproject.toml .

USER 0
RUN chown -R 1001:1001 /app
USER 1001

# Set version for setuptools-scm since .git folder is not available in container
ENV SETUPTOOLS_SCM_PRETEND_VERSION=1.0.0

# Fix Hugging Face cache permissions by setting cache directory to writable location
ENV HF_HOME=/tmp/huggingface
ENV HUGGINGFACE_HUB_CACHE=/tmp/huggingface/hub
ENV TRANSFORMERS_CACHE=/tmp/huggingface/transformers

RUN pip install -e . 

VOLUME ["/etc/secrets"]

ENTRYPOINT ["aiq", "run", "--config_file", "src/sast_agent_workflow/configs/config.yml", "--input", "sast_agent"]

