# SAST AI Workflow Infrastructure Container
# This container includes all dependencies needed for the Tekton pipeline steps

FROM registry.access.redhat.com/ubi9/python-312

USER 0

# ============================================================================
# System Dependencies Installation
# ============================================================================

# Install EPEL repository (needed for csdiff/csgrep)
RUN dnf install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-9.noarch.rpm

# Install system packages
# - git: For cloning repositories (Step 3: prepare-source)
# - rpm-build: For SRPM processing (Step 3: prepare-source)
# - curl: For URL validation and downloads (Steps 1, 4, 5)
# - jq: For JSON processing (Step 7)
# - file: For file type detection (Step 4: transform-report)
# - csdiff: For SARIF conversion (Step 4: transform-report)
RUN dnf install -y --allowerasing \
    git-2.47.3-1.el9_6 \
    rpm-build-4.16.1.3-39.el9 \
    curl-7.76.1-34.el9 \
    jq-1.6-19.el9 \
    file-5.39-16.el9 \
    csdiff-3.5.5-1.el9 \
    && dnf clean all

# ============================================================================
# Python Dependencies Installation
# ============================================================================

# Upgrade pip to latest version
RUN pip install --no-cache-dir --upgrade pip

# Install Python packages for various pipeline steps:
# - google-api-python-client, google-auth*: For Google Sheets/Drive (Steps 2, 7)
# - google-cloud-storage: For GCS uploads (Step 8)
# - packaging: Python package utilities (Step 7)
# - boto3: For S3/MinIO operations (MLOps Step 7)
# - dvc, dvc-s3: For DVC data versioning (MLOps Step 4)
RUN pip install --no-cache-dir \
    google-api-python-client==2.187.0 \
    google-auth==2.41.1 \
    google-auth-httplib2==0.2.1 \
    google-auth-oauthlib==1.2.3 \
    google-cloud-storage==3.6.0 \
    packaging==25.0 \
    boto3==1.40.70 \
    dvc==3.64.0 \
    dvc-s3==3.2.2

# ============================================================================
# Container Setup
# ============================================================================

# Create scripts directory
RUN mkdir -p /scripts && chmod 755 /scripts

# Set environment variable for environment selection (base/mlops/prod)
ENV ENVIRONMENT=base

# Set working directory
WORKDIR /workspace

# Default command
CMD ["/bin/bash"]