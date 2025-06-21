# Check if .env file exists and load it
ifneq (,$(wildcard ./.env))
    include .env
    export
endif

CONTEXT := $(shell oc config current-context)
NAMESPACE ?= $(shell oc config view --minify --output 'jsonpath={..namespace}')

CO := oc  --context $(CONTEXT)
TK := tkn --context $(CONTEXT)

# Pipeline parameters (overrideable on the CLI):
REPO_REMOTE_URL                       ?= source/code/url
FALSE_POSITIVES_URL              ?= false/positives/url

LLM_URL                          ?= http://<<please-set-llm-url>>
LLM_MODEL_NAME                   ?= llm-model
EMBEDDINGS_LLM_URL               ?= http://<<please-set-embedding-llm-url>>
EMBEDDINGS_LLM_MODEL_NAME        ?= embedding-llm-model

PROJECT_NAME					 ?= project-name
PROJECT_VERSION					 ?= project-version

DOWNLOAD_REPO					 ?= false
REPO_REMOTE_URL					 ?= ""
REPO_LOCAL_PATH					 ?= /path/to/repo

INPUT_REPORT_FILE_PATH			 ?= http://<<please-set-google-spreadsheet-url>>

AGGREGATE_RESULTS_G_SHEET        ?= "aggregate/sheet/url"

# Secret configuration (loaded from .env file)
GITLAB_TOKEN                     ?= ""
LLM_API_KEY                      ?= ""
EMBEDDINGS_LLM_API_KEY           ?= ""
GOOGLE_SERVICE_ACCOUNT_JSON_PATH ?= ./service_account.json
DOCKER_CONFIG_PATH               ?= $(HOME)/.config/containers/auth.json

.PHONY: all setup tasks pvc secrets pipeline run logs clean

all: setup tasks pipeline run

setup: pvc secrets

tasks:
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/tasks/validate_urls.yaml
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/tasks/prepare_source.yaml
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/tasks/fetch_false_positives.yaml
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/tasks/execute_sast_ai_workflow.yaml

pvc:
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/pvc.yaml
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/cache_pvc.yaml

secrets:
	@echo "Creating secrets in namespace $(NAMESPACE)..."
	# Create GitLab token secret
	@if [ -z "$(GITLAB_TOKEN)" ]; then \
		echo "Warning: GITLAB_TOKEN is not set. Please set it in .env file"; \
	else \
		$(CO) create secret generic gitlab-token-secret \
			--from-literal=gitlab_token="$(GITLAB_TOKEN)" \
			-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f -; \
		echo "Created gitlab-token-secret"; \
	fi
	# Create consolidated LLM credentials secret
	@LLM_MISSING=""; EMBEDDINGS_MISSING=""; \
	if [ -z "$(LLM_API_KEY)" ]; then \
		echo "Warning: LLM_API_KEY is not set. Please set it in .env file"; \
		LLM_MISSING="true"; \
	fi; \
	if [ -z "$(EMBEDDINGS_LLM_API_KEY)" ]; then \
		echo "Warning: EMBEDDINGS_LLM_API_KEY is not set. Please set it in .env file"; \
		EMBEDDINGS_MISSING="true"; \
	fi; \
	if [ -z "$$LLM_MISSING" ] && [ -z "$$EMBEDDINGS_MISSING" ]; then \
		$(CO) create secret generic sast-ai-default-llm-creds \
			--from-literal=llm_url="$(LLM_URL)" \
			--from-literal=llm_api_key="$(LLM_API_KEY)" \
			--from-literal=embeddings_llm_url="$(EMBEDDINGS_LLM_URL)" \
			--from-literal=embeddings_llm_api_key="$(EMBEDDINGS_LLM_API_KEY)" \
			-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f -; \
		echo "Created sast-ai-default-llm-creds with both LLM and embeddings API keys"; \
	else \
		echo "Skipping LLM credentials secret creation due to missing API keys"; \
	fi
	# Create Google Service Account secret
	@if [ ! -f "$(GOOGLE_SERVICE_ACCOUNT_JSON_PATH)" ]; then \
		echo "Warning: Google service account JSON file not found at $(GOOGLE_SERVICE_ACCOUNT_JSON_PATH)"; \
		echo "Please set GOOGLE_SERVICE_ACCOUNT_JSON_PATH in .env file or place service_account.json in project root"; \
	else \
		$(CO) create secret generic google-service-account-secret \
			--from-file=service_account.json="$(GOOGLE_SERVICE_ACCOUNT_JSON_PATH)" \
			-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f -; \
		echo "Created google-service-account-secret"; \
	fi
	# Create Quay pull secret
	@echo "Looking for Docker/Podman auth config..."
	@DOCKER_AUTH_FILE=""; \
	if [ -f "$(DOCKER_CONFIG_PATH)" ]; then \
		DOCKER_AUTH_FILE="$(DOCKER_CONFIG_PATH)"; \
		echo "Found auth config at: $(DOCKER_CONFIG_PATH)"; \
	elif [ -f "$(XDG_RUNTIME_DIR)/containers/auth.json" ]; then \
		DOCKER_AUTH_FILE="$(XDG_RUNTIME_DIR)/containers/auth.json"; \
		echo "Found auth config at: $(XDG_RUNTIME_DIR)/containers/auth.json"; \
	elif [ -f "$(HOME)/.docker/config.json" ]; then \
		DOCKER_AUTH_FILE="$(HOME)/.docker/config.json"; \
		echo "Found auth config at: $(HOME)/.docker/config.json"; \
	elif [ -f "$(HOME)/.config/containers/auth.json" ]; then \
		DOCKER_AUTH_FILE="$(HOME)/.config/containers/auth.json"; \
		echo "Found auth config at: $(HOME)/.config/containers/auth.json"; \
	fi; \
	if [ -z "$$DOCKER_AUTH_FILE" ]; then \
		echo "Warning: Docker config file not found in common locations:"; \
		echo "  - $(DOCKER_CONFIG_PATH)"; \
		echo "  - $(XDG_RUNTIME_DIR)/containers/auth.json"; \
		echo "  - $(HOME)/.docker/config.json"; \
		echo "  - $(HOME)/.config/containers/auth.json"; \
		echo "Please run 'podman login quay.io' or 'docker login quay.io' first"; \
		echo "Or set DOCKER_CONFIG_PATH in your .env file to the correct path"; \
	else \
		$(CO) create secret generic quay-sast-puller \
			--from-file=.dockerconfigjson="$$DOCKER_AUTH_FILE" \
			--type=kubernetes.io/dockerconfigjson \
			-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f -; \
		echo "Created quay-sast-puller secret from $$DOCKER_AUTH_FILE"; \
	fi
	# Patch pipeline service account to use Quay pull secret
	@$(CO) patch serviceaccount pipeline \
		-n $(NAMESPACE) \
		-p '{"imagePullSecrets": [{"name": "quay-sast-puller"}]}' \
		--type=merge
	@echo "Patched pipeline service account with image pull secret"
	@echo "All secrets created successfully!"

pipeline:
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/pipeline.yaml

run:
	# remove any old run
	$(CO) delete pipelinerun sast-ai-workflow-pipelinerun \
		-n $(NAMESPACE) --ignore-not-found

	# Check if tkn CLI is available
	@if command -v tkn >/dev/null 2>&1; then \
		echo "Tekton CLI found. Starting pipeline run..."; \
		$(TK) pipeline start sast-ai-workflow-pipeline \
		  -n $(NAMESPACE) \
		  -p REPO_REMOTE_URL="$(REPO_REMOTE_URL)" \
		  -p FALSE_POSITIVES_URL="$(FALSE_POSITIVES_URL)" \
		  -p LLM_URL="$(LLM_URL)" \
		  -p LLM_MODEL_NAME="$(LLM_MODEL_NAME)" \
		  -p EMBEDDINGS_LLM_URL="$(EMBEDDINGS_LLM_URL)" \
		  -p EMBEDDINGS_LLM_MODEL_NAME="$(EMBEDDINGS_LLM_MODEL_NAME)" \
		  -p PROJECT_NAME="$(PROJECT_NAME)" \
		  -p PROJECT_VERSION="$(PROJECT_VERSION)" \
		  -p INPUT_REPORT_FILE_PATH="$(INPUT_REPORT_FILE_PATH)" \
		  -p AGGREGATE_RESULTS_G_SHEET="$(AGGREGATE_RESULTS_G_SHEET)" \
		  --workspace name=shared-workspace,claimName=sast-ai-workflow-pvc \
		  --workspace name=gitlab-token-ws,secret=gitlab-token-secret \
		  --workspace name=llm-credentials-ws,secret=sast-ai-default-llm-creds \
		  --workspace name=google-sa-json-ws,secret=google-service-account-secret \
		  --workspace name=cache-workspace,claimName=sast-ai-cache-pvc \
		  --showlog; \
	else \
		echo ""; \
		echo "==============================================================================="; \
		echo "Tekton CLI (tkn) is not installed. Please install it to run the pipeline."; \
		echo "Or manually run the pipeline using the following command:"; \
		echo ""; \
		echo "tkn --context $(CONTEXT) pipeline start sast-ai-workflow-pipeline \\"; \
		echo "  -n $(NAMESPACE) \\"; \
		echo "  -p REPO_REMOTE_URL=\"$(REPO_REMOTE_URL)\" \\"; \
		echo "  -p FALSE_POSITIVES_URL=\"$(FALSE_POSITIVES_URL)\" \\"; \
		echo "  -p LLM_URL=\"$(LLM_URL)\" \\"; \
		echo "  -p LLM_MODEL_NAME=\"$(LLM_MODEL_NAME)\" \\"; \
		echo "  -p EMBEDDINGS_LLM_URL=\"$(EMBEDDINGS_LLM_URL)\" \\"; \
		echo "  -p EMBEDDINGS_LLM_MODEL_NAME=\"$(EMBEDDINGS_LLM_MODEL_NAME)\" \\"; \
		echo "  -p PROJECT_NAME=\"$(PROJECT_NAME)\" \\"; \
		echo "  -p PROJECT_VERSION=\"$(PROJECT_VERSION)\" \\"; \
		echo "  -p INPUT_REPORT_FILE_PATH=\"$(INPUT_REPORT_FILE_PATH)\" \\"; \
		echo "  -p AGGREGATE_RESULTS_G_SHEET=\"$(AGGREGATE_RESULTS_G_SHEET)\" \\"; \
		echo "  --workspace name=shared-workspace,claimName=sast-ai-workflow-pvc \\"; \
		echo "  --workspace name=gitlab-token-ws,secret=gitlab-token-secret \\"; \
		echo "  --workspace name=llm-credentials-ws,secret=sast-ai-default-llm-creds \\"; \
		echo "  --workspace name=google-sa-json-ws,secret=google-service-account-secret \\"; \
		echo "  --workspace name=cache-workspace,claimName=sast-ai-cache-pvc \\"; \
		echo "  --showlog"; \
		echo ""; \
		echo "To install Tekton CLI, visit: https://tekton.dev/docs/cli/"; \
		echo "==============================================================================="; \
		echo ""; \
		echo "Infrastructure setup completed successfully!"; \
		echo "Pipeline resources are ready. Install tkn CLI and run the above command to execute."; \
	fi

logs:
	@if command -v tkn >/dev/null 2>&1; then \
		tkn pipelinerun logs sast-ai-workflow-pipelinerun -n $(NAMESPACE) -f; \
	else \
		echo "Tekton CLI (tkn) is not installed."; \
		echo "To view logs manually, run:"; \
		echo "tkn --context $(CONTEXT) pipelinerun logs sast-ai-workflow-pipelinerun -n $(NAMESPACE) -f"; \
		echo ""; \
		echo "Or use oc/kubectl to view pod logs directly:"; \
		echo "$(CO) logs -n $(NAMESPACE) -l tekton.dev/pipelineRun=sast-ai-workflow-pipelinerun -f"; \
	fi

clean:
	@echo "Cleaning up all resources in namespace $(NAMESPACE)..."
	# Delete all PipelineRuns first (this should release PVCs they're using)
	@echo "Deleting all PipelineRuns..."
	@$(CO) delete pipelinerun --all -n $(NAMESPACE) --ignore-not-found || true
	# Delete all TaskRuns that might be left behind
	@echo "Deleting all TaskRuns..."
	@$(CO) delete taskrun --all -n $(NAMESPACE) --ignore-not-found || true
	# Delete Tekton resources
	@echo "Deleting Tekton resource definitions..."
	$(CO) delete -n $(NAMESPACE) \
		-f deploy/tekton/tasks/validate_urls.yaml \
		-f deploy/tekton/tasks/prepare_source.yaml \
		-f deploy/tekton/tasks/fetch_false_positives.yaml \
		-f deploy/tekton/tasks/execute_sast_ai_workflow.yaml \
		-f deploy/tekton/pipeline.yaml \
		--ignore-not-found
	# Delete all PVCs in the namespace (including any left behind from previous runs)
	@echo "Deleting all PVCs..."
	@$(CO) delete pvc --all -n $(NAMESPACE) --ignore-not-found || true
	# Wait a moment for PVCs to be deleted before checking PVs
	@echo "Waiting for PVCs to be deleted..."
	@sleep 5
	# Delete any orphaned PVs that might be left behind
	@echo "Checking for orphaned PVs..."
	@$(CO) get pv --no-headers 2>/dev/null | grep "Released\|Available" | awk '{print $$1}' | xargs -r $(CO) delete pv --ignore-not-found || true
	# Remove image pull secrets from pipeline service account
	@echo "Cleaning up service account..."
	@$(CO) patch serviceaccount pipeline \
		-n $(NAMESPACE) \
		-p '{"imagePullSecrets": null}' \
		--type=merge \
		--ignore-not-found=true || true
	# Delete secrets
	@echo "Deleting secrets..."
	@$(CO) delete secret gitlab-token-secret \
		sast-ai-default-llm-creds \
		google-service-account-secret \
		quay-sast-puller \
		-n $(NAMESPACE) --ignore-not-found || true
	@echo "Cleanup completed! All PVs, PVCs, and pipeline resources have been removed."
