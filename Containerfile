FROM registry.access.redhat.com/ubi9/python-312 AS builder
USER 0

# Install EPEL repository (needed for csdiff/csgrep)
RUN dnf install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-9.noarch.rpm

# Install system packages
RUN dnf install -y --allowerasing \
    git \
    clang \
    llvm-devel \
    rpm-build \
    curl \
    jq \
    file \
    csdiff \
    && dnf clean all

FROM builder
USER 1001
WORKDIR /app

COPY requirements.txt .
RUN pip install --upgrade pip && pip install -r requirements.txt

# Install infrastructure packages separately to avoid dependency conflicts
RUN pip install --no-cache-dir \
    google-api-python-client==2.187.0 \
    google-auth-httplib2==0.2.1 \
    google-cloud-storage==3.6.0 \
    dvc==3.64.0 \
    dvc-s3==3.2.2

COPY config ./config/
COPY src ./src/
COPY evaluation ./evaluation/
COPY deploy ./deploy/
COPY pyproject.toml .

USER 0
RUN chown -R 1001:1001 /app && \
    chmod +x /app/deploy/tekton/scripts/*.sh
USER 1001

# Set version for setuptools-scm since .git folder is not available in container
ENV SETUPTOOLS_SCM_PRETEND_VERSION=1.0.0


RUN pip install -e . 

VOLUME ["/etc/secrets"]

ENTRYPOINT ["nat", "run", "--config_file", "src/sast_agent_workflow/configs/config.yml", "--input", "sast_agent"]

