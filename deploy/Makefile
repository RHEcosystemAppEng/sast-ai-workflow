# Check if .env file exists and load it
ifneq (,$(wildcard ../.env))
    include ../.env
    export
endif

CONTEXT := $(shell oc config current-context)
NAMESPACE ?= $(shell oc config view --minify --output 'jsonpath={..namespace}')

CO := oc  --context $(CONTEXT)

# Pipeline parameters (overrideable on the CLI):
HUMAN_VERIFIED_FILE_PATH         ?= ""

LLM_URL                          ?= http://<<please-set-llm-url>>
LLM_MODEL_NAME                   ?= llm-model
EMBEDDINGS_LLM_URL               ?= http://<<please-set-embedding-llm-url>>
EMBEDDINGS_LLM_MODEL_NAME        ?= embedding-llm-model

PROJECT_NAME					 ?= project-name
PROJECT_VERSION					 ?= project-version

DOWNLOAD_REPO					 ?= false
REPO_REMOTE_URL					 ?= source/code/url
REPO_LOCAL_PATH					 ?= /path/to/repo

INPUT_REPORT_FILE_PATH			 ?= http://<<please-set-google-spreadsheet-url>>

FALSE_POSITIVES_URL              ?= false/positives/url
USE_KNOWN_FALSE_POSITIVE_FILE    ?= true

AGGREGATE_RESULTS_G_SHEET        ?= "aggregate/sheet/url"

# Optional Google Drive configuration
GDRIVE_FOLDER_ID				 ?= ""
GDRIVE_SA_FILE_NAME			     ?= service_account.json

# Container Image Configuration
IMAGE_REGISTRY                   ?= quay.io/ecosystem-appeng
IMAGE_NAME                       ?= sast-ai-workflow
IMAGE_VERSION                    ?= 
CONTAINER_IMAGE                  ?= $(IMAGE_REGISTRY)/$(IMAGE_NAME):latest

# GCS Bucket Configuration
GCS_BUCKET_NAME                  ?=
GCS_SA_FILE_NAME                 := gcs_service_account.json

# DVC and S3/MinIO Configuration
DVC_REPO_URL                     ?= https://github.com/RHEcosystemAppEng/sast-ai-dvc
DVC_DATA_VERSION                 ?= v1.0
S3_ENDPOINT_URL                  ?= http://minio-api-minio.apps.appeng.clusters.se-apps.redhat.com
S3_INPUT_BUCKET_NAME             ?= test

# GitOps Configuration
GITHUB_REPO_URL                  ?= https://github.com/RHEcosystemAppEng/sast-ai-workflow.git
ARGOCD_NAMESPACE                 ?= sast-ai

# EventListener Configuration
ORCHESTRATOR_API_URL             ?=

# Secret configuration (loaded from .env file)
GITLAB_TOKEN                     ?= ""
LLM_API_KEY                      ?= ""
EMBEDDINGS_LLM_API_KEY           ?= ""
GOOGLE_SERVICE_ACCOUNT_JSON_PATH ?= ./service_account.json
GCS_SERVICE_ACCOUNT_JSON_PATH    ?= ./gcs_service_account.json
DOCKER_CONFIG_PATH               ?= $(HOME)/.config/containers/auth.json


# S3/Minio Configuration
S3_OUTPUT_BUCKET_NAME            ?= bucket-name
AWS_ACCESS_KEY_ID                ?= ""
AWS_SECRET_ACCESS_KEY            ?= ""
S3_ENDPOINT_URL                  ?= ""
.PHONY: deploy deploy-dev deploy-prod deploy-mlops setup tasks-dev tasks-prod tasks-mlops secrets pipeline scripts configmaps run clean generate-prompts prompts argocd-deploy-dev argocd-deploy-prod argocd-clean eventlistener eventlistener-clean

# Unified deploy command
# Usage:
#   make deploy                     # Deploy base (Google Drive, :latest)
#   make deploy ENV=mlops           # Deploy MLOps (S3/Minio, :latest)
#   make deploy ENV=prod IMAGE_VERSION=1.2.3  # Deploy prod (Google Drive, versioned)
deploy: deploy-$(ENV)

deploy-dev: CONTAINER_IMAGE=$(IMAGE_REGISTRY)/$(IMAGE_NAME):latest
deploy-dev: setup tasks-dev argocd-deploy-dev
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "🚀 SAST AI Workflow - Development Deployment"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "   Environment: Development"
	@echo "   Container Image: $(CONTAINER_IMAGE)"
	@echo ""
	@echo "✅ Development deployment completed successfully!"

deploy-prod: CONTAINER_IMAGE=$(IMAGE_REGISTRY)/$(IMAGE_NAME):$(IMAGE_VERSION)
deploy-prod: setup tasks-prod argocd-deploy-prod
	@if [ -z "$(IMAGE_VERSION)" ]; then \
		echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"; \
		echo "❌ ERROR: IMAGE_VERSION is required for production deployment"; \
		echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"; \
		echo ""; \
		echo "Usage: make deploy ENV=prod IMAGE_VERSION=1.2.3"; \
		echo ""; \
		echo "Available versions can be found at:"; \
		echo "https://quay.io/repository/ecosystem-appeng/sast-ai-workflow?tab=tags"; \
		exit 1; \
	fi

deploy-mlops: CONTAINER_IMAGE=$(IMAGE_REGISTRY)/$(IMAGE_NAME):latest
deploy-mlops: setup tasks-mlops argocd-deploy-mlops
	@if [ -z "$(ORCHESTRATOR_API_URL)" ]; then \
		echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"; \
		echo "❌ ERROR: ORCHESTRATOR_API_URL is required for MLOps deployment"; \
		echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"; \
		echo ""; \
		echo "Usage: make deploy-mlops ORCHESTRATOR_API_URL=<url>"; \
		echo ""; \
		echo "Example:"; \
		echo "  make deploy-mlops ORCHESTRATOR_API_URL=http://orchestrator.sast-ai.svc.cluster.local:8080"; \
		exit 1; \
	fi
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "🤖 SAST AI Workflow - MLOps Benchmarking Deployment"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "   Environment: MLOps (Benchmarking)"
	@echo "   Container Image: $(CONTAINER_IMAGE)"
	@echo "   Orchestrator URL: $(ORCHESTRATOR_API_URL)"
	@echo ""
	@echo "🎯 Deploying EventListener..."
	@sed -e 's|ORCHESTRATOR_API_URL_PLACEHOLDER|$(ORCHESTRATOR_API_URL)|g' \
		tekton/eventlistener/benchmark-config.yaml.example > tekton/eventlistener/benchmark-config.yaml
	@$(CO) apply -k tekton/eventlistener/ -n $(NAMESPACE) || \
		{ echo "   ❌ Failed to deploy EventListener resources"; exit 1; }
	@echo "   ✓ EventListener deployed"
	@echo ""
	@echo "✅ MLOps deployment completed successfully!"

setup: secrets scripts prompts configmaps
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "🚀 Common Infrastructure Ready"
	@echo "   Context: $(CONTEXT)"
	@echo "   Namespace: $(NAMESPACE)"

tasks-dev:
	@echo "📋 Deploying Tekton resources (dev)..."
	@$(CO) apply -k tekton/base -n $(NAMESPACE)
	@echo "   ✓ Base Tekton resources (base - Google Drive storage)"

tasks-prod:
	@echo "📋 Deploying Tekton resources (prod)..."
	@$(CO) apply -k tekton/overlays/prod -n $(NAMESPACE)
	@echo "   ✓ Production Tekton resources (versioned)"

tasks-mlops:
	@echo "📋 Deploying Tekton resources (mlops)..."
	@$(CO) apply -k tekton/overlays/mlops -n $(NAMESPACE)
	@echo "   ✓ MLOps Tekton resources (MinIO/S3)"

secrets:
	@echo "🔐 Configuring Secrets..."
	# Create GitLab token secret
	@if [ -z "$(GITLAB_TOKEN)" ]; then \
		echo "   ❌ GitLab token not configured - required for pipeline execution"; \
		echo "   💡 Set GITLAB_TOKEN in .env file or environment"; \
		exit 1; \
	else \
		$(CO) create secret generic sast-ai-gitlab-token \
			--from-literal=gitlab_token="$(GITLAB_TOKEN)" \
			-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - || \
		{ echo "   ❌ Failed to create GitLab token secret"; exit 1; }; \
		echo "   ✓ GitLab token secret"; \
	fi
	# Create consolidated LLM credentials secret
	@if [ -z "$(LLM_API_KEY)" ]; then \
		echo "   ❌ LLM API key not configured - required for AI analysis"; \
		echo "   💡 Set LLM_API_KEY in .env file or environment"; \
		exit 1; \
	fi; \
	if [ -z "$(EMBEDDINGS_LLM_API_KEY)" ]; then \
		echo "   ❌ Embeddings API key not configured - required for vector analysis"; \
		echo "   💡 Set EMBEDDINGS_LLM_API_KEY in .env file or environment"; \
		exit 1; \
	fi; \
	$(CO) create secret generic sast-ai-default-llm-creds \
		--from-literal=llm_url="$(LLM_URL)" \
		--from-literal=llm_api_key="$(LLM_API_KEY)" \
		--from-literal=embeddings_llm_url="$(EMBEDDINGS_LLM_URL)" \
		--from-literal=embeddings_llm_api_key="$(EMBEDDINGS_LLM_API_KEY)" \
		--from-literal=llm_model_name="$(LLM_MODEL_NAME)" \
		--from-literal=embedding_llm_model_name="$(EMBEDDINGS_LLM_MODEL_NAME)" \
		-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - || \
		{ echo "   ❌ Failed to create LLM credentials secret"; exit 1; }; \
	echo "   ✓ LLM credentials secret"
	# Create Google Service Account secrets
	@echo "   🔍 Checking Google service account (REQUIRED)..."
	@if [ ! -f "$(GOOGLE_SERVICE_ACCOUNT_JSON_PATH)" ]; then \
		echo "   ❌ Google service account not found - REQUIRED for pipeline execution"; \
		echo "   💡 Place service account JSON file at: $(GOOGLE_SERVICE_ACCOUNT_JSON_PATH)"; \
		exit 1; \
	else \
		$(CO) create secret generic sast-ai-google-service-account \
            --from-file=service_account.json="$(GOOGLE_SERVICE_ACCOUNT_JSON_PATH)" \
            -n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - || \
		{ echo "   ❌ Failed to create Google service account secret"; exit 1; }; \
        echo "   ✓ Google service account secret"; \
    fi
	@echo "   🔍 Checking GCS service account (OPTIONAL)..."
	@if [ ! -f "$(GCS_SERVICE_ACCOUNT_JSON_PATH)" ]; then \
		echo "   ⚠️  GCS service account not found at: $(GCS_SERVICE_ACCOUNT_JSON_PATH)"; \
		echo "   💡 GCS uploads will be skipped - this is optional"; \
	else \
		echo "   ✓ GCS service account found"; \
		$(CO) create secret generic sast-ai-gcs-service-account \
            --from-file=$(GCS_SA_FILE_NAME)="$(GCS_SERVICE_ACCOUNT_JSON_PATH)" \
            -n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - > /dev/null 2>&1; \
        echo "   ✓ GCS service account secret"; \
    fi
	# Create S3/Minio output credentials secret (OPTIONAL)
	@echo "   🔍 Checking S3/Minio output credentials (OPTIONAL)..."
	@if [ -z "$(AWS_ACCESS_KEY_ID)" ] || [ -z "$(AWS_SECRET_ACCESS_KEY)" ]; then \
		echo "   ⚠️  S3/Minio output credentials not configured"; \
		echo "   💡 S3 output uploads will be skipped - this is optional"; \
	else \
		echo "   ✓ S3/Minio output credentials found"; \
		if [ -n "$(S3_ENDPOINT_URL)" ]; then \
			$(CO) create secret generic sast-ai-s3-output-credentials \
				--from-literal=access_key_id="$(AWS_ACCESS_KEY_ID)" \
				--from-literal=secret_access_key="$(AWS_SECRET_ACCESS_KEY)" \
				--from-literal=endpoint_url="$(S3_ENDPOINT_URL)" \
				-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - > /dev/null 2>&1; \
		else \
			$(CO) create secret generic sast-ai-s3-output-credentials \
				--from-literal=access_key_id="$(AWS_ACCESS_KEY_ID)" \
				--from-literal=secret_access_key="$(AWS_SECRET_ACCESS_KEY)" \
				-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - > /dev/null 2>&1; \
		fi; \
		echo "   ✓ S3/Minio output credentials secret"; \
	fi
	# Create Quay pull secret
	@DOCKER_AUTH_FILE=""; \
	if [ -f "$(DOCKER_CONFIG_PATH)" ]; then \
		DOCKER_AUTH_FILE="$(DOCKER_CONFIG_PATH)"; \
	elif [ -f "$(XDG_RUNTIME_DIR)/containers/auth.json" ]; then \
		DOCKER_AUTH_FILE="$(XDG_RUNTIME_DIR)/containers/auth.json"; \
	elif [ -f "$(HOME)/.docker/config.json" ]; then \
		DOCKER_AUTH_FILE="$(HOME)/.docker/config.json"; \
	elif [ -f "$(HOME)/.config/containers/auth.json" ]; then \
		DOCKER_AUTH_FILE="$(HOME)/.config/containers/auth.json"; \
	fi; \
	if [ -z "$$DOCKER_AUTH_FILE" ]; then \
		echo "   ❌ Container registry auth not found - required for image pulling"; \
		echo "   💡 Login to container registry: podman login quay.io (or docker login quay.io)"; \
		exit 1; \
	else \
		$(CO) create secret generic sast-ai-quay-registry-config \
			--from-file=.dockerconfigjson="$$DOCKER_AUTH_FILE" \
			--type=kubernetes.io/dockerconfigjson \
			-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - || \
		{ echo "   ❌ Failed to create container registry pull secret"; exit 1; }; \
		echo "   ✓ Container registry pull secret"; \
	fi
	# Patch pipeline service account to use Quay pull secret
	@$(CO) patch serviceaccount pipeline \
		-n $(NAMESPACE) \
		-p '{"imagePullSecrets": [{"name": "sast-ai-quay-registry-config"}]}' \
		--type=merge || \
		{ echo "   ❌ Failed to patch pipeline service account"; exit 1; }
	@echo "   ✓ Service account configured"

scripts:
	@echo "📜 Setting up Scripts..."
	@$(CO) apply -n $(NAMESPACE) -f tekton/scripts/upload_to_drive_cm.yaml || \
		{ echo "   ❌ Failed to apply Google Drive upload script"; exit 1; }
	@$(CO) apply -n $(NAMESPACE) -f tekton/scripts/upload_to_gcs_cm.yaml || \
		{ echo "   ❌ Failed to apply GCS upload script"; exit 1; }
	@$(CO) apply -n $(NAMESPACE) -f tekton/scripts/upload_to_s3_output_cm.yaml || \
		{ echo "   ❌ Failed to apply S3 output upload script"; exit 1; }
	@echo "   ✓ Upload scripts configured"

generate-prompts:
	@echo "   🔄 Generating prompts from templates..."
	@python3 scripts/generate_prompts.py > /dev/null 2>&1 || { echo "   ❌ Failed to generate prompts"; exit 1; }
	@echo "   ✓ Prompts generated successfully"

prompts:
	@echo "💬 Configuring Prompts..."
	@$(MAKE) --no-print-directory generate-prompts
	@echo "   🔄 Applying prompts ConfigMap to cluster..."
	@$(CO) apply -n $(NAMESPACE) -f tekton/sast-ai-prompt-templates.yaml > /dev/null 2>&1 || { echo "   ❌ Failed to apply prompts ConfigMap"; exit 1; }
	@echo "   ✓ Prompt templates deployed"

configmaps:
	@echo "🗂️  Configuring Optional ConfigMaps..."
	# Create Google Drive ConfigMap if GDRIVE_FOLDER_ID is set
	@if [ -n "$(GDRIVE_FOLDER_ID)" ] && [ "$(GDRIVE_FOLDER_ID)" != "" ]; then \
		echo "   🔄 Creating Google Drive ConfigMap..."; \
		$(CO) create configmap sast-ai-gdrive-config \
			--from-literal=folder-id="$(GDRIVE_FOLDER_ID)" \
			-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - > /dev/null 2>&1; \
		echo "   ✓ Google Drive ConfigMap created (folder_id: $(GDRIVE_FOLDER_ID))"; \
	else \
		echo "   ⚠️  Google Drive ConfigMap skipped (GDRIVE_FOLDER_ID not set)"; \
		echo "   💡 Set GDRIVE_FOLDER_ID in .env file to enable Google Drive uploads"; \
	fi

run:
	@echo ""
	@echo "🏃 Starting Pipeline Execution..."
	@echo "   Container Image: $(CONTAINER_IMAGE)"
	@echo "   🔄 Removing old pipeline runs..."
	@$(CO) delete pipelinerun sast-ai-workflow-pipelinerun \
				-n $(NAMESPACE) --ignore-not-found
	# Create PipelineRun with current parameters
	@sed \
		-e 's|PROJECT_NAME_PLACEHOLDER|$(PROJECT_NAME)|g' \
		-e 's|PROJECT_VERSION_PLACEHOLDER|$(PROJECT_VERSION)|g' \
		-e 's|REPO_REMOTE_URL_PLACEHOLDER|$(REPO_REMOTE_URL)|g' \
		-e 's|FALSE_POSITIVES_URL_PLACEHOLDER|$(FALSE_POSITIVES_URL)|g' \
		-e 's|EMBEDDINGS_LLM_URL_PLACEHOLDER|$(EMBEDDINGS_LLM_URL)|g' \
		-e 's|EMBEDDINGS_LLM_MODEL_NAME_PLACEHOLDER|$(EMBEDDINGS_LLM_MODEL_NAME)|g' \
		-e 's|LLM_URL_PLACEHOLDER|$(LLM_URL)|g' \
		-e 's|LLM_MODEL_NAME_PLACEHOLDER|$(LLM_MODEL_NAME)|g' \
		-e 's|INPUT_REPORT_FILE_PATH_PLACEHOLDER|$(INPUT_REPORT_FILE_PATH)|g' \
		-e 's|HUMAN_VERIFIED_FILE_PATH_PLACEHOLDER|$(HUMAN_VERIFIED_FILE_PATH)|g' \
		-e 's|USE_KNOWN_FALSE_POSITIVE_FILE_PLACEHOLDER|$(USE_KNOWN_FALSE_POSITIVE_FILE)|g' \
		-e 's|AGGREGATE_RESULTS_G_SHEET_PLACEHOLDER|$(AGGREGATE_RESULTS_G_SHEET)|g' \
		-e 's|GDRIVE_FOLDER_ID_PLACEHOLDER|$(GDRIVE_FOLDER_ID)|g' \
		-e 's|GDRIVE_SA_FILE_NAME_PLACEHOLDER|$(GDRIVE_SA_FILE_NAME)|g' \
		-e 's|CONTAINER_IMAGE_PLACEHOLDER|$(CONTAINER_IMAGE)|g' \
		-e 's|GCS_BUCKET_NAME_PLACEHOLDER|$(GCS_BUCKET_NAME)|g' \
		-e 's|GCS_SA_FILE_NAME_PLACEHOLDER|$(GCS_SA_FILE_NAME)|g' \
		-e 's|DVC_REPO_URL_PLACEHOLDER|$(DVC_REPO_URL)|g' \
		-e 's|DVC_DATA_VERSION_PLACEHOLDER|$(DVC_DATA_VERSION)|g' \
		-e 's|S3_ENDPOINT_URL_PLACEHOLDER|$(S3_ENDPOINT_URL)|g' \
		-e 's|S3_INPUT_BUCKET_NAME_PLACEHOLDER|$(S3_INPUT_BUCKET_NAME)|g' \
		-e 's|S3_OUTPUT_BUCKET_NAME_PLACEHOLDER|$(S3_OUTPUT_BUCKET_NAME)|g' \
		tekton/pipelinerun.yaml > tekton/pipelinerun-temp.yaml
	@$(CO) apply -n $(NAMESPACE) -f tekton/pipelinerun-temp.yaml
	@rm -f tekton/pipelinerun-temp.yaml
	@echo "   ✓ Pipeline execution started"
	@echo "   ✓ View status: oc get pipelineruns -n $(NAMESPACE)"
	@echo "   ✓ Follow logs: oc logs -l tekton.dev/pipelineRun=sast-ai-workflow-pipelinerun -n $(NAMESPACE) -f"

argocd-deploy-mlops:
	@echo "🔄 Deploying ArgoCD Application (MLOps)..."
	@# Create a temporary file with placeholders replaced
	@sed \
		-e 's|ARGOCD_NAMESPACE_PLACEHOLDER|$(NAMESPACE)|g' \
		-e 's|GITHUB_REPO_URL_PLACEHOLDER|$(GITHUB_REPO_URL)|g' \
		-e 's|TARGET_NAMESPACE_PLACEHOLDER|$(NAMESPACE)|g' \
		argocd/argocd-application-dev.yaml > argocd/argocd-application-dev-temp.yaml
	@$(CO) apply -n $(NAMESPACE) -f argocd/argocd-application-dev-temp.yaml
	@rm -f argocd/argocd-application-dev-temp.yaml
	@echo "   ✓ ArgoCD Application (mlops) deployed to $(NAMESPACE) namespace"
	@echo "   ✓ Watching: deploy/tekton/overlays/mlops (S3 output storage)"
	@echo "   ✓ Auto-sync: enabled"

argocd-deploy-prod:
	@echo "🔄 Deploying ArgoCD Application (Production)..."
	@# Create a temporary file with placeholders replaced
	@sed \
		-e 's|ARGOCD_NAMESPACE_PLACEHOLDER|$(NAMESPACE)|g' \
		-e 's|GITHUB_REPO_URL_PLACEHOLDER|$(GITHUB_REPO_URL)|g' \
		-e 's|TARGET_NAMESPACE_PLACEHOLDER|$(NAMESPACE)|g' \
		argocd/argocd-application-prod.yaml > argocd/argocd-application-prod-temp.yaml
	@$(CO) apply -n $(NAMESPACE) -f argocd/argocd-application-prod-temp.yaml
	@rm -f argocd/argocd-application-prod-temp.yaml
	@echo "   ✓ ArgoCD Application (prod) deployed to $(NAMESPACE) namespace"
	@echo "   ✓ Watching: deploy/tekton/overlays/prod (uses release version)"
	@echo "   ✓ Auto-sync: enabled (self-heal disabled for production safety)"

argocd-clean:
	@echo "🧹 Removing ArgoCD Applications..."
	@$(CO) delete application sast-ai-tekton-pipeline-syncer-dev -n $(NAMESPACE) --ignore-not-found --timeout=10s > /dev/null 2>&1 || \
	$(CO) patch application sast-ai-tekton-pipeline-syncer-dev -n $(NAMESPACE) -p '{"metadata":{"finalizers":null}}' --type=merge > /dev/null 2>&1 || true
	@$(CO) delete application sast-ai-tekton-pipeline-syncer-prod -n $(NAMESPACE) --ignore-not-found --timeout=10s > /dev/null 2>&1 || \
	$(CO) patch application sast-ai-tekton-pipeline-syncer-prod -n $(NAMESPACE) -p '{"metadata":{"finalizers":null}}' --type=merge > /dev/null 2>&1 || true
	@echo "   ✓ ArgoCD Applications removed"

eventlistener:
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "🎯 EventListener Standalone Update"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "   ⚠️  Use 'make deploy-mlops' for full deployment"
	@echo ""
	@if [ -z "$(ORCHESTRATOR_API_URL)" ]; then \
		echo "❌ ERROR: ORCHESTRATOR_API_URL is required"; \
		echo ""; \
		echo "Usage:"; \
		echo "  make eventlistener ORCHESTRATOR_API_URL=<url>"; \
		echo ""; \
		echo "Example:"; \
		echo "  make eventlistener ORCHESTRATOR_API_URL=http://orchestrator.sast-ai.svc.cluster.local:8080"; \
		exit 1; \
	fi
	@echo "🎯 Deploying EventListener..."
	@sed -e 's|ORCHESTRATOR_API_URL_PLACEHOLDER|$(ORCHESTRATOR_API_URL)|g' \
		tekton/eventlistener/benchmark-config.yaml.example > tekton/eventlistener/benchmark-config.yaml
	@$(CO) apply -k tekton/eventlistener/ -n $(NAMESPACE) || \
		{ echo "   ❌ Failed to deploy EventListener resources"; exit 1; }
	@echo ""
	@echo "✅ EventListener updated"
	@echo ""
	@echo "📊 Verify: oc get eventlistener,task,pipeline -l app.kubernetes.io/component=benchmark-mlop -n $(NAMESPACE)"
	@echo "🧪 Test: cd tekton/eventlistener && ./test-eventlistener.sh"
	@echo ""

eventlistener-clean:
	@echo "🧹 Removing EventListener resources..."
	@echo "   🏃 Cleaning benchmark PipelineRuns..."
	@$(CO) delete pipelinerun -l app.kubernetes.io/component=benchmark-mlop -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ Benchmark PipelineRuns removed"
	@echo "   📋 Cleaning benchmark TaskRuns..."
	@$(CO) delete taskrun -l app.kubernetes.io/component=benchmark-mlop -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ Benchmark TaskRuns removed"
	@echo "   🗑️  Removing EventListener infrastructure..."
	@$(CO) delete -k tekton/eventlistener/ -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ EventListener resources removed"

clean:
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "🧹 SAST AI Workflow - Cleanup"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "   Context: $(CONTEXT)"
	@echo "   Namespace: $(NAMESPACE)"
	@echo ""
	# Delete all PipelineRuns first (this should release PVCs they're using)
	@echo "🏃 Cleaning Pipeline Runs..."
	@$(CO) delete pipelinerun --all -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ Pipeline runs removed"
	# Delete all TaskRuns that might be left behind
	@echo "📋 Cleaning Task Runs..."
	@$(CO) delete taskrun --all -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ Task runs removed"
	# Remove ArgoCD Application early to prevent GitOps interference
	@$(MAKE) --no-print-directory argocd-clean
	# Delete Tekton resources using kustomize (matching deployment method)
	@echo "🔧 Removing Pipeline Resources..."
	@if [ "$(ENV)" = "prod" ]; then \
		$(CO) delete -k tekton/overlays/prod -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true; \
		echo "   ✓ Production Tekton resources removed (kustomize overlay)"; \
	elif [ "$(ENV)" = "mlop" ]; then \
		$(CO) delete -k tekton/overlays/mlop -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true; \
		echo "   ✓ MLOp Tekton resources removed (kustomize overlay)"; \
	else \
		$(CO) delete -k tekton/base -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true; \
		echo "   ✓ Base Tekton resources removed (kustomize base)"; \
	fi
	@echo "💾 Cleaning Storage..."
	@DYNAMIC_PVCS=$$($(CO) get pvc -n $(NAMESPACE) --no-headers -o custom-columns=":metadata.name" 2>/dev/null | grep "sast-ai-workflow-pipeline" || true); \
	if [ -n "$$DYNAMIC_PVCS" ]; then \
		echo "   🔍 Found dynamic PVCs: $$DYNAMIC_PVCS"; \
		PV_NAMES=""; \
		for pvc in $$DYNAMIC_PVCS; do \
			pv_name=$$($(CO) get pvc $$pvc -n $(NAMESPACE) -o jsonpath='{.spec.volumeName}' 2>/dev/null || echo ""); \
			if [ -n "$$pv_name" ]; then \
				PV_NAMES="$$PV_NAMES $$pv_name"; \
			fi; \
		done; \
		for pvc in $$DYNAMIC_PVCS; do \
			$(CO) delete pvc $$pvc -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true; \
		done; \
		echo "   ✓ Dynamic persistent volume claims removed"; \
		echo "   ⏳ Waiting for PVCs to be fully deleted..."; \
		timeout=30; \
		while [ $$timeout -gt 0 ]; do \
			remaining_pvcs=$$($(CO) get pvc -n $(NAMESPACE) --no-headers -o custom-columns=":metadata.name" 2>/dev/null | grep "sast-ai-workflow-pipeline" | wc -l || echo "0"); \
			if [ "$$remaining_pvcs" -eq 0 ]; then \
				break; \
			fi; \
			sleep 1; \
			timeout=$$((timeout - 1)); \
		done; \
		if [ $$timeout -eq 0 ]; then \
			echo "   ⚠️  Warning: Some PVCs may still be terminating"; \
		else \
			echo "   ✓ All PVCs confirmed deleted"; \
		fi; \
		for pv in $$PV_NAMES; do \
			$(CO) delete pv $$pv --ignore-not-found > /dev/null 2>&1 || true; \
		done; \
		echo "   ✓ Associated persistent volumes cleaned"; \
	else \
		echo "   ✓ No dynamic PVCs found to clean"; \
	fi
	# Note: We don't unpatch the 'pipeline' service account to avoid breaking other projects
	# that may have also added image pull secrets to the shared SA in this namespace
	@echo "💬 Removing Prompts..."
	@$(CO) delete -f tekton/sast-ai-prompt-templates.yaml -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ Prompt templates removed"
	@echo "📜 Cleaning Scripts..."
	@$(CO) delete configmap sast-ai-gdrive-upload-scripts \
		sast-ai-gcs-upload-scripts \
		s3-output-upload-scripts \
		-n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ Upload scripts removed"
	@echo "🗂️  Cleaning Optional ConfigMaps..."
	@$(CO) delete configmap sast-ai-gdrive-config \
		-n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ Google Drive ConfigMap removed"
	@echo "🔐 Removing Secrets..."
	@$(CO) delete secret sast-ai-gitlab-token \
		sast-ai-default-llm-creds \
		sast-ai-google-service-account \
		sast-ai-gcs-service-account \
		sast-ai-s3-output-credentials \
		sast-ai-quay-registry-config \
		-n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ All secrets removed"
	@echo ""
	@echo "✅ Cleanup completed successfully!"
	@echo ""
