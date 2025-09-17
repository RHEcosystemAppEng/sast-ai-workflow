# Check if .env file exists and load it
ifneq (,$(wildcard ../.env))
    include ../.env
    export
endif

CONTEXT := $(shell oc config current-context)
NAMESPACE ?= $(shell oc config view --minify --output 'jsonpath={..namespace}')

CO := oc  --context $(CONTEXT)

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

.PHONY: all setup tasks pvc secrets pipeline scripts run clean generate-prompts prompts argocd-deploy argocd-clean

all: setup scripts tasks pipeline prompts argocd-deploy

setup: 
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "🚀 SAST AI Workflow - Infrastructure Setup"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "   Context: $(CONTEXT)"
	@echo "   Namespace: $(NAMESPACE)"
	@echo ""
	@$(MAKE) --no-print-directory pvc secrets

tasks:
	@echo "📋 Setting up Tekton Tasks..."
	@$(CO) apply -n $(NAMESPACE) -f tekton/tasks/ > /dev/null 2>&1
	@echo "   ✓ Tasks deployed successfully"

pvc:
	@echo "💾 Setting up Persistent Volume Claims..."
	@$(CO) apply -n $(NAMESPACE) -f tekton/pvc.yaml > /dev/null 2>&1
	@$(CO) apply -n $(NAMESPACE) -f tekton/cache_pvc.yaml > /dev/null 2>&1
	@echo "   ✓ Workspace PVC created"
	@echo "   ✓ Cache PVC created"

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
	@echo "   🔄 Generating prompts from templates..."
	@python3 scripts/generate_prompts.py > /dev/null 2>&1 || { echo "   ❌ Failed to generate prompts"; exit 1; }
	@echo "   ✓ Prompts generated successfully"

prompts: 
	@echo "💬 Configuring Prompts..."
	@$(MAKE) --no-print-directory generate-prompts
	@echo "   🔄 Applying prompts ConfigMap to cluster..."
	@$(CO) apply -n $(NAMESPACE) -f tekton/prompts-config-map.yaml > /dev/null 2>&1 || { echo "   ❌ Failed to apply prompts ConfigMap"; exit 1; }
	@echo "   ✓ Prompt templates deployed"

run:
	@echo ""
	@echo "🏃 Starting Pipeline Execution..."
	# remove any old run
	@$(CO) delete pipelinerun sast-ai-workflow-pipelinerun \
		-n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1
	# Create PipelineRun with current parameters
	@sed \
		-e 's|PROJECT_NAME_PLACEHOLDER|$(PROJECT_NAME)|g' \
		-e 's|PROJECT_VERSION_PLACEHOLDER|$(PROJECT_VERSION)|g' \
		-e 's|REPO_REMOTE_URL_PLACEHOLDER|$(REPO_REMOTE_URL)|g' \
		-e 's|FALSE_POSITIVES_URL_PLACEHOLDER|$(FALSE_POSITIVES_URL)|g' \
		-e 's|LLM_URL_PLACEHOLDER|$(LLM_URL)|g' \
		-e 's|LLM_MODEL_NAME_PLACEHOLDER|$(LLM_MODEL_NAME)|g' \
		-e 's|EMBEDDINGS_LLM_URL_PLACEHOLDER|$(EMBEDDINGS_LLM_URL)|g' \
		-e 's|EMBEDDINGS_LLM_MODEL_NAME_PLACEHOLDER|$(EMBEDDINGS_LLM_MODEL_NAME)|g' \
		-e 's|INPUT_REPORT_FILE_PATH_PLACEHOLDER|$(INPUT_REPORT_FILE_PATH)|g' \
		-e 's|AGGREGATE_RESULTS_G_SHEET_PLACEHOLDER|$(AGGREGATE_RESULTS_G_SHEET)|g' \
		tekton/pipelinerun.yaml > tekton/pipelinerun-temp.yaml
	@$(CO) apply -n $(NAMESPACE) -f tekton/pipelinerun-temp.yaml > /dev/null 2>&1
	@rm -f tekton/pipelinerun-temp.yaml
	@echo "   ✓ Pipeline execution started"
	@echo "   ✓ View status: oc get pipelineruns -n $(NAMESPACE)"
	@echo "   ✓ Follow logs: oc logs -l tekton.dev/pipelineRun=sast-ai-workflow-pipelinerun -n $(NAMESPACE) -f"

argocd-deploy:
	@echo "🔄 Deploying ArgoCD Application..."
	@# Create a temporary file with placeholders replaced
	@sed \
		-e 's|ARGOCD_NAMESPACE_PLACEHOLDER|$(NAMESPACE)|g' \
		-e 's|GITHUB_REPO_URL_PLACEHOLDER|https://github.com/RHEcosystemAppEng/sast-ai-workflow.git|g' \
		-e 's|TARGET_NAMESPACE_PLACEHOLDER|$(NAMESPACE)|g' \
		argocd/argocd-application.yaml > argocd/argocd-application-temp.yaml
	@$(CO) apply -n $(NAMESPACE) -f argocd/argocd-application-temp.yaml > /dev/null 2>&1
	@rm -f argocd/argocd-application-temp.yaml
	@echo "   ✓ ArgoCD Application deployed to $(NAMESPACE) namespace"
	@echo "   ✓ Syncing from: https://github.com/RHEcosystemAppEng/sast-ai-workflow.git"
	@echo "   ✓ Target namespace: $(NAMESPACE)"

argocd-clean:
	@echo "🧹 Removing ArgoCD Application..."
	@$(CO) delete application sast-ai-workflow -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1
	@echo "   ✓ ArgoCD Application removed"

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
	@echo "🔄 Cleaning ArgoCD Application..."
	@$(CO) delete application sast-ai-tekton-pipeline-syncer -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ ArgoCD Application removed"
	# Delete Tekton resources
	@echo "🔧 Removing Pipeline Resources..."
	@$(CO) delete -n $(NAMESPACE) \
		-f tekton/tasks/ \
		-f tekton/pipeline.yaml \
		--ignore-not-found > /dev/null 2>&1
	@echo "   ✓ Pipeline definitions removed"
	@echo "   ✓ Task definitions removed"
	# Delete only SAST AI specific PVCs
	@echo "💾 Cleaning Storage..."
	@$(CO) delete pvc sast-ai-workflow-pvc sast-ai-cache-pvc -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ Persistent volume claims removed"
	# Wait a moment for PVCs to be deleted before checking PVs
	@sleep 5
	# Delete only PVs that were bound to our specific PVCs
	@for pvc in sast-ai-workflow-pvc sast-ai-cache-pvc; do \
		pv_name=$$($(CO) get pvc $$pvc -n $(NAMESPACE) -o jsonpath='{.spec.volumeName}' 2>/dev/null || echo ""); \
		if [ -n "$$pv_name" ]; then \
			$(CO) delete pv $$pv_name --ignore-not-found > /dev/null 2>&1 || true; \
		fi; \
	done
	@echo "   ✓ Persistent volumes cleaned"
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
