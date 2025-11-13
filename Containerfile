FROM registry.access.redhat.com/ubi9/python-312 AS builder
USER 0
COPY requirements.txt /tmp/requirements.txt
RUN yum install -y git clang llvm-devel && yum clean all && \
    REQUIRED_CLANG_VERSION=$(grep -E '^clang==' /tmp/requirements.txt | cut -d'=' -f3) && \
    INSTALLED_LIBCLANG=$(ls /usr/lib64/libclang.so.* | grep -E 'libclang\.so\.[0-9]+\.[0-9]+\.[0-9]+$' | head -1) && \
    ln -sf "$INSTALLED_LIBCLANG" "/usr/lib64/libclang.so.${REQUIRED_CLANG_VERSION}" && \
    rm /tmp/requirements.txt

FROM builder
USER 1001
WORKDIR /app

COPY requirements.txt .
RUN pip install --upgrade pip && pip install -r requirements.txt

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

