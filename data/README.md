# SAST-AI Workflow Data Directory

This directory contains all data assets managed by DVC for the SAST-AI workflow, with remote storage on MinIO.

## Directory Structure

### Input Data
- **known-issues/**: Known false positives and verified known issue datasets (256 files, 4.6MB) *[DVC tracked]*
- **ground-truth/**: Human-verified evaluation data for performance assessment (260 files, 3.1MB) *[DVC tracked]*
- **sast-reports/**: SAST scan reports from various tools 
- **source-repos/**

### Processed Data
- **embeddings/**: FAISS vector embeddings and indices
- **models/**:
  - **embeddings/**: Embedding model caches
  - **prompts/**: Versioned LLM prompt templates

### Output Data
- **outputs/**:
  - **excel-reports/**: Generated analysis reports in Excel format
  - **metrics/**: Evaluation metrics and performance data

## DVC Remote Storage Configuration

### MinIO Connection Details
- **MinIO Console UI**: `https://minio-ui-minio.apps.appeng.clusters.se-apps.redhat.com/browser/test`
- **MinIO API Endpoint**: `http://minio-api-minio.apps.appeng.clusters.se-apps.redhat.com`
- **Bucket Name**: `test`
- **SSL**: Disabled (HTTP API endpoint)

### DVC Configuration
Current configuration in `.dvc/config`:
```ini
[core]
    remote = minio
['remote "minio"']
    url = s3://test
    endpointurl = http://minio-api-minio.apps.appeng.clusters.se-apps.redhat.com
    access_key_id = <ACCESS_KEY>
    secret_access_key = <SECRET_KEY>
    ssl_verify = false
    use_ssl = false
```

### 🔐 Credential Management and Setup

#### Setup
```bash
# 1. Configure DVC remote
dvc remote add -d minio s3://test
dvc remote modify minio endpointurl http://minio-api-minio.apps.appeng.clusters.se-apps.redhat.com
dvc remote modify minio ssl_verify false
dvc remote modify minio use_ssl false

# 2. Add credentials (obtain from team members)
dvc remote modify minio access_key_id <ACCESS_KEY>
dvc remote modify minio secret_access_key <SECRET_KEY>
```

## DVC Management

### Current Tracked Datasets
- **ground-truth.dvc**: Clean SAST analysis results with annotations (260 Excel files, 3.1MB)
- **known-issues.dvc**: Organized known non-issues (256 files in subdirectories, 4.6MB)

### Content-Addressable Storage
DVC stores files in MinIO using content hashes (not original names):
- Files stored as: `files/md5/[hash]/[content]`
- Directories stored as: `files/md5/[hash].dir` (containing file mappings)
- Original structure reconstructed on `dvc pull`
- Automatic deduplication of identical files

## Usage

### Basic Operations
```bash
# Pull all data from MinIO
dvc pull

# Pull specific datasets
dvc pull data/ground-truth.dvc
dvc pull data/known-issues.dvc

# Push local changes to MinIO
dvc push

# Check data status
dvc status

# Add new data to tracking
dvc add data/new-dataset/
```

### Network Requirements
- **VPN connection required** to access MinIO endpoints
- Ensure connectivity to `minio-api-minio.apps.appeng.clusters.se-apps.redhat.com`

### Troubleshooting
```bash
# Test MinIO connectivity
curl -I http://minio-api-minio.apps.appeng.clusters.se-apps.redhat.com

# Check DVC remote configuration
dvc remote list -v

# Verify bucket access
dvc status
```

## Dataset Details

### Ground Truth Data (`ground-truth/`)
- **Source**: Clean Data folder
- **Content**: 260 Excel files with verified SAST analysis results
- **Size**: ~3.1MB
- **Format**: `.xlsx` files with structured analysis data

### Known Issues Data (`known-issues/`)
- **Source**: Known-non-issues-el10 folder
- **Content**: 256 files organized in package subdirectories
- **Size**: ~4.6MB
- **Structure**: `package-name/ignore.err` files with known false positives
