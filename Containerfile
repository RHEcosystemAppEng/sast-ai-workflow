FROM registry.access.redhat.com/ubi9/python-312 as builder
USER 0
RUN yum install -y git clang llvm-devel && yum clean all

FROM builder
USER 1001
WORKDIR /app
COPY requirements.txt .
RUN pip install --upgrade pip && pip install -r requirements.txt

COPY config src ./

VOLUME ["/etc/secrets"]

ENTRYPOINT ["python", "run.py"]

