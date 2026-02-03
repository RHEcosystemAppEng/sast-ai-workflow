# SAST AI Workflow Infrastructure Container
# This container includes all dependencies needed for the Tekton pipeline steps

# Using floating tag intentionally - Red Hat UBI images are security-maintained
# SonarQube rule docker:S6504 is suppressed in sonar-project.properties
FROM registry.access.redhat.com/ubi9/python-312:latest

USER 0

# ============================================================================
# Install all dependencies in a single RUN layer for efficiency
# ============================================================================
#
# System packages:
# - git: For cloning repositories (Step 3: prepare-source)
# - rpm-build: For SRPM processing (Step 3: prepare-source)
# - curl: For URL validation and downloads (Steps 1, 4, 5)
# - jq: For JSON processing (Step 7)
# - file: For file type detection (Step 4: transform-report)
# - csdiff: For SARIF conversion (Step 4: transform-report)
#
# Python packages:
# - google-api-python-client, google-auth*: For Google Sheets/Drive (Steps 2, 7)
# - google-cloud-storage: For GCS uploads (Step 8)
# - packaging: Python package utilities (Step 7)
# - boto3: For S3/MinIO operations (MLOps Step 7)
# - dvc, dvc-s3: For DVC data versioning (MLOps Step 4)
#
RUN dnf install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-9.noarch.rpm && \
    dnf install -y --allowerasing \
        git \
        rpm-build \
        curl \
        jq \
        file \
        csdiff && \
    dnf clean all && \
    pip install --no-cache-dir --upgrade pip && \
    pip install --no-cache-dir \
        google-api-python-client==2.187.0 \
        google-auth==2.41.1 \
        google-auth-httplib2==0.2.1 \
        google-auth-oauthlib==1.2.3 \
        google-cloud-storage==3.6.0 \
        packaging==25.0 \
        boto3==1.40.70 \
        dvc==3.64.0 \
        dvc-s3==3.2.2 && \
    mkdir -p /scripts

# ============================================================================
# Copy pipeline scripts
# ============================================================================

COPY --chmod=755 deploy/tekton/scripts/*.sh /scripts/
COPY --chmod=644 deploy/tekton/scripts/*.py /scripts/

# ============================================================================
# Container Configuration
# ============================================================================

# Set environment variable for environment selection (base/mlops/prod)
ENV ENVIRONMENT=base

# Set working directory
WORKDIR /workspace

# Default command
CMD ["/bin/bash"]
