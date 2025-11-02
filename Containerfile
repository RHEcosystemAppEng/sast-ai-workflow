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
COPY evaluation ./evaluation/
COPY pyproject.toml .

USER 0
RUN chown -R 1001:1001 /app
USER 1001

# Set version for setuptools-scm since .git folder is not available in container
ENV SETUPTOOLS_SCM_PRETEND_VERSION=1.0.0


RUN pip install -e . 

VOLUME ["/etc/secrets"]

ENTRYPOINT ["nat", "run", "--config_file", "src/sast_agent_workflow/configs/config.yml", "--input", "sast_agent"]

