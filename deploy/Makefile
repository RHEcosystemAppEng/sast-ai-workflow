# Check if .env file exists and load it
ifneq (,$(wildcard ../.env))
    include ../.env
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

.PHONY: all setup tasks secrets pipeline scripts run logs clean generate-prompts prompts

all: setup scripts tasks pipeline prompts run

setup: 
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "🚀 SAST AI Workflow - Infrastructure Setup"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "   Context: $(CONTEXT)"
	@echo "   Namespace: $(NAMESPACE)"
	@echo ""
	@$(MAKE) --no-print-directory secrets

tasks:
	@echo "📋 Setting up Tekton Task..."
	@$(CO) apply -n $(NAMESPACE) -f tekton/tasks/execute_sast_ai_workflow.yaml > /dev/null 2>&1
	@echo "   ✓ Consolidated task deployed successfully"

secrets:
	@echo "🔐 Configuring Secrets..."
	# Create GitLab token secret
	@if [ -z "$(GITLAB_TOKEN)" ]; then \
		echo "   ⚠️  GitLab token not configured"; \
	else \
		$(CO) create secret generic gitlab-token-secret \
			--from-literal=gitlab_token="$(GITLAB_TOKEN)" \
			-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - > /dev/null 2>&1; \
		echo "   ✓ GitLab token secret"; \
	fi
	# Create consolidated LLM credentials secret
	@LLM_MISSING=""; EMBEDDINGS_MISSING=""; \
	if [ -z "$(LLM_API_KEY)" ]; then \
		echo "   ⚠️  LLM API key not configured"; \
		LLM_MISSING="true"; \
	fi; \
	if [ -z "$(EMBEDDINGS_LLM_API_KEY)" ]; then \
		echo "   ⚠️  Embeddings API key not configured"; \
		EMBEDDINGS_MISSING="true"; \
	fi; \
	if [ -z "$$LLM_MISSING" ] && [ -z "$$EMBEDDINGS_MISSING" ]; then \
		$(CO) create secret generic sast-ai-default-llm-creds \
			--from-literal=llm_url="$(LLM_URL)" \
			--from-literal=llm_api_key="$(LLM_API_KEY)" \
			--from-literal=embeddings_llm_url="$(EMBEDDINGS_LLM_URL)" \
			--from-literal=embeddings_llm_api_key="$(EMBEDDINGS_LLM_API_KEY)" \
			--from-literal=llm_model_name="$(LLM_MODEL_NAME)" \
			--from-literal=embedding_llm_model_name="$(EMBEDDINGS_LLM_MODEL_NAME)" \
			-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - > /dev/null 2>&1; \
		echo "   ✓ LLM credentials secret"; \
	fi
	# Create Google Service Account secret
	@if [ ! -f "$(GOOGLE_SERVICE_ACCOUNT_JSON_PATH)" ]; then \
		echo "   ⚠️  Google service account not found"; \
	else \
		$(CO) create secret generic google-service-account-secret \
			--from-file=service_account.json="$(GOOGLE_SERVICE_ACCOUNT_JSON_PATH)" \
			-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - > /dev/null 2>&1; \
		echo "   ✓ Google service account secret"; \
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
		echo "   ⚠️  Container registry auth not found"; \
	else \
		$(CO) create secret generic quay-sast-puller \
			--from-file=.dockerconfigjson="$$DOCKER_AUTH_FILE" \
			--type=kubernetes.io/dockerconfigjson \
			-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - > /dev/null 2>&1; \
		echo "   ✓ Container registry pull secret"; \
	fi
	# Patch pipeline service account to use Quay pull secret
	@$(CO) patch serviceaccount pipeline \
		-n $(NAMESPACE) \
		-p '{"imagePullSecrets": [{"name": "quay-sast-puller"}]}' \
		--type=merge > /dev/null 2>&1
	@echo "   ✓ Service account configured"

pipeline:
	@echo "🔧 Deploying Pipeline..."
	@$(CO) apply -n $(NAMESPACE) -f tekton/pipeline.yaml > /dev/null 2>&1
	@echo "   ✓ Pipeline definition deployed"

scripts:
	@echo "📜 Setting up Scripts..."
	@$(CO) apply -n $(NAMESPACE) -f tekton/scripts/upload_to_drive_cm.yaml > /dev/null 2>&1
	@echo "   ✓ Upload scripts configured"

generate-prompts:
	@python3 scripts/generate_prompts.py > /dev/null 2>&1 || { echo "   ❌ Failed to generate prompts"; exit 1; }

prompts: 
	@echo "💬 Configuring Prompts..."
	@$(MAKE) --no-print-directory generate-prompts
	@$(CO) apply -n $(NAMESPACE) -f tekton/prompts-config-map.yaml > /dev/null 2>&1
	@echo "   ✓ Prompt templates deployed"

run:
	@echo ""
	@echo "🏃 Starting Pipeline Execution..."
	# remove any old run
	@$(CO) delete pipelinerun sast-ai-workflow-pipelinerun \
		-n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1

	# Check if tkn CLI is available
	@if command -v tkn >/dev/null 2>&1; then \
		echo "   ✓ Tekton CLI found, launching pipeline..."; \
		echo ""; \
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
		  --workspace name=gitlab-token-ws,secret=gitlab-token-secret \
		  --workspace name=google-sa-json-ws,secret=google-service-account-secret \
		  --showlog; \
	else \
		echo "   ❌ Tekton CLI (tkn) not installed"; \
		echo ""; \
		echo "   Install tkn CLI: https://tekton.dev/docs/cli/"; \
		echo "   Or run manually:"; \
		echo ""; \
		echo "   tkn --context $(CONTEXT) pipeline start sast-ai-workflow-pipeline \\"; \
		echo "     -n $(NAMESPACE) \\"; \
		echo "     -p REPO_REMOTE_URL=\"$(REPO_REMOTE_URL)\" \\"; \
		echo "     -p FALSE_POSITIVES_URL=\"$(FALSE_POSITIVES_URL)\" \\"; \
		echo "     -p LLM_URL=\"$(LLM_URL)\" \\"; \
		echo "     -p LLM_MODEL_NAME=\"$(LLM_MODEL_NAME)\" \\"; \
		echo "     -p EMBEDDINGS_LLM_URL=\"$(EMBEDDINGS_LLM_URL)\" \\"; \
		echo "     -p EMBEDDINGS_LLM_MODEL_NAME=\"$(EMBEDDINGS_LLM_MODEL_NAME)\" \\"; \
		echo "     -p PROJECT_NAME=\"$(PROJECT_NAME)\" \\"; \
		echo "     -p PROJECT_VERSION=\"$(PROJECT_VERSION)\" \\"; \
		echo "     -p INPUT_REPORT_FILE_PATH=\"$(INPUT_REPORT_FILE_PATH)\" \\"; \
		echo "     -p AGGREGATE_RESULTS_G_SHEET=\"$(AGGREGATE_RESULTS_G_SHEET)\" \\"; \
		echo "     --workspace name=gitlab-token-ws,secret=gitlab-token-secret \\"; \
		echo "     --workspace name=google-sa-json-ws,secret=google-service-account-secret \\"; \
		echo "     --showlog"; \
		echo ""; \
		echo "   ✅ Infrastructure setup completed successfully!"; \
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
	# Delete Tekton resources
	@echo "🔧 Removing Pipeline Resources..."
	@$(CO) delete -n $(NAMESPACE) \
		-f tekton/tasks/execute_sast_ai_workflow.yaml \
		-f tekton/pipeline.yaml \
		--ignore-not-found > /dev/null 2>&1
	@echo "   ✓ Pipeline definition removed"
	@echo "   ✓ Consolidated task removed"
	# Remove image pull secrets from pipeline service account
	@echo "👤 Cleaning Service Accounts..."
	@$(CO) patch serviceaccount pipeline \
		-n $(NAMESPACE) \
		-p '{"imagePullSecrets": null}' \
		--type=merge \
		--ignore-not-found=true > /dev/null 2>&1 || true
	@echo "   ✓ Service account reset"
	# Delete prompts
	@echo "💬 Removing Prompts..."
	@$(CO) delete -f  tekton/prompts-config-map.yaml \
		-n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ Prompt templates removed"
	# Delete ConfigMaps created by scripts
	@echo "📜 Cleaning Scripts..."
	@$(CO) delete -f tekton/scripts/upload_to_drive_cm.yaml \
		-n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ Script configurations removed"
	# Delete secrets
	@echo "🔐 Removing Secrets..."
	@$(CO) delete secret gitlab-token-secret \
		sast-ai-default-llm-creds \
		google-service-account-secret \
		quay-sast-puller \
		-n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ All secrets removed"
	@echo ""
	@echo "✅ Cleanup completed successfully!"
	@echo ""
