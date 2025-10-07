# Check if .env file exists and load it
ifneq (,$(wildcard ../.env))
    include ../.env
    export
endif

CONTEXT := $(shell oc config current-context)
NAMESPACE ?= $(shell oc config view --minify --output 'jsonpath={..namespace}')

CO := oc  --context $(CONTEXT)

# Pipeline parameters (overrideable on the CLI):
REPO_REMOTE_URL                  ?= source/code/url
HUMAN_VERIFIED_FILE_PATH         ?= ""

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

FALSE_POSITIVES_URL              ?= false/positives/url
USE_KNOWN_FALSE_POSITIVE_FILE    ?= true

AGGREGATE_RESULTS_G_SHEET        ?= "aggregate/sheet/url"

GDRIVE_FOLDER_ID				 ?= ""
GDRIVE_SA_FILE_NAME			     ?= service_account.json

# GitOps Configuration
GITHUB_REPO_URL                  ?= https://github.com/RHEcosystemAppEng/sast-ai-workflow.git
ARGOCD_NAMESPACE                 ?= sast-ai
# Optional Google Drive configuration
GDRIVE_FOLDER_ID                 ?= ""

# Secret configuration (loaded from .env file)
GITLAB_TOKEN                     ?= ""
LLM_API_KEY                      ?= ""
EMBEDDINGS_LLM_API_KEY           ?= ""
GOOGLE_SERVICE_ACCOUNT_JSON_PATH ?= ./service_account.json
DOCKER_CONFIG_PATH               ?= $(HOME)/.config/containers/auth.json

.PHONY: deploy setup tasks secrets pipeline scripts configmaps run clean generate-prompts prompts argocd-deploy argocd-clean

deploy: setup scripts tasks pipeline prompts configmaps argocd-deploy

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
		echo "   ❌ GitLab token not configured - required for pipeline execution"; \
		echo "   💡 Set GITLAB_TOKEN in .env file or environment"; \
		exit 1; \
	else \
		$(CO) create secret generic sast-ai-gitlab-token \
			--from-literal=gitlab_token="$(GITLAB_TOKEN)" \
			-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - > /dev/null 2>&1; \
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
		-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - > /dev/null 2>&1; \
	echo "   ✓ LLM credentials secret"
	# Create Google Service Account secret
	@if [ ! -f "$(GOOGLE_SERVICE_ACCOUNT_JSON_PATH)" ]; then \
		echo "   ❌ Google service account not found - required for spreadsheet access"; \
		echo "   💡 Place service account JSON file at: $(GOOGLE_SERVICE_ACCOUNT_JSON_PATH)"; \
		exit 1; \
	else \
		$(CO) create secret generic sast-ai-google-service-account \
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
		echo "   ❌ Container registry auth not found - required for image pulling"; \
		echo "   💡 Login to container registry: podman login quay.io (or docker login quay.io)"; \
		exit 1; \
	else \
		$(CO) create secret generic sast-ai-quay-registry-config \
			--from-file=.dockerconfigjson="$$DOCKER_AUTH_FILE" \
			--type=kubernetes.io/dockerconfigjson \
			-n $(NAMESPACE) --dry-run=client -o yaml | $(CO) apply -f - > /dev/null 2>&1; \
		echo "   ✓ Container registry pull secret"; \
	fi
	# Patch pipeline service account to use Quay pull secret
	@$(CO) patch serviceaccount pipeline \
		-n $(NAMESPACE) \
		-p '{"imagePullSecrets": [{"name": "sast-ai-quay-registry-config"}]}' \
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
		-e 's|HUMAN_VERIFIED_FILE_PATH_PLACEHOLDER|$(HUMAN_VERIFIED_FILE_PATH)|g' \
		-e 's|USE_KNOWN_FALSE_POSITIVE_FILE_PLACEHOLDER|$(USE_KNOWN_FALSE_POSITIVE_FILE)|g' \
		-e 's|AGGREGATE_RESULTS_G_SHEET_PLACEHOLDER|$(AGGREGATE_RESULTS_G_SHEET)|g' \
		-e 's|GDRIVE_FOLDER_ID_PLACEHOLDER|$(GDRIVE_FOLDER_ID)|g' \
		-e 's|GDRIVE_SA_FILE_NAME_PLACEHOLDER|$(GDRIVE_SA_FILE_NAME)|g' \
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
	@$(CO) delete application sast-ai-tekton-pipeline-syncer -n $(NAMESPACE) --ignore-not-found --timeout=10s > /dev/null 2>&1 || \
	$(CO) patch application sast-ai-tekton-pipeline-syncer -n $(NAMESPACE) -p '{"metadata":{"finalizers":null}}' --type=merge > /dev/null 2>&1 || true
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
	@$(MAKE) --no-print-directory argocd-clean
	# Delete Tekton resources
	@echo "🔧 Removing Pipeline Resources..."
	@if [ -f "tekton/pipeline.yaml" ]; then \
		PIPELINE_NAME=$$(grep "^  name:" tekton/pipeline.yaml | head -1 | awk '{print $$2}' || echo ""); \
		if [ -n "$$PIPELINE_NAME" ]; then \
			$(CO) delete pipeline $$PIPELINE_NAME -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true; \
		fi; \
	fi
	@if ls tekton/tasks/*.yaml >/dev/null 2>&1; then \
		TASK_NAMES=$$(grep "^  name:" tekton/tasks/*.yaml | awk -F: '{print $$3}' | awk '{print $$1}' | tr '\n' ' ' || echo ""); \
		if [ -n "$$TASK_NAMES" ]; then \
			$(CO) delete task $$TASK_NAMES -n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true; \
		fi; \
	fi
	@echo "   ✓ Pipeline definitions removed"
	@echo "   ✓ Task definitions removed"
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
		-n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ Google Drive upload scripts removed"
	@echo "🗂️  Cleaning Optional ConfigMaps..."
	@$(CO) delete configmap sast-ai-gdrive-config \
		-n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ Google Drive ConfigMap removed"
	@echo "🔐 Removing Secrets..."
	@$(CO) delete secret sast-ai-gitlab-token \
		sast-ai-default-llm-creds \
		sast-ai-google-service-account \
		sast-ai-quay-registry-config \
		-n $(NAMESPACE) --ignore-not-found > /dev/null 2>&1 || true
	@echo "   ✓ All secrets removed"
	@echo ""
	@echo "✅ Cleanup completed successfully!"
	@echo ""
