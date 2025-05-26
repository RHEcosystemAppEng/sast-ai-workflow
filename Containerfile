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

VOLUME ["/etc/secrets"]

ENTRYPOINT ["python", "src/run.py"]

