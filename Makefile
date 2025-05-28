NAMESPACE ?= sast-ai-workflow-test
CONTEXT   ?= sast-ai-workflow-test/10-6-73-122:6443/kube:admin

# NAMESPACE ?= sast-ai-workflow
# CONTEXT   ?= sast-ai-workflow/api-crc-testing:6443/kubeadmin # CRC

CO := oc  --context $(CONTEXT)
TK := tkn --context $(CONTEXT)

# Pipeline parameters (overrideable on the CLI):
SOURCE_URL                       ?= source/code/url
SPREADSHEET_URL                  ?= google/spreadsheet/url
FALSE_POSITIVES_URL              ?= false/positives/url

LLM_URL                          ?= http://<<please-set-llm-url>>
LLM_MODEL_NAME                   ?= llm-model
EMBEDDINGS_LLM_URL               ?= http://<<please-set-embedding-llm-url>>
EMBEDDINGS_LLM_MODEL_NAME        ?= embedding-llm-model

PROJECT_NAME					 ?= project-name
PROJECT_VERSION					 ?= project-version

INPUT_REPORT_FILE_PATH			 ?= input-report

.PHONY: all tasks pvc pipeline run logs clean

all: tasks pipeline run

tasks:
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/tasks/validate_urls.yaml
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/tasks/prepare_source.yaml
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/tasks/fetch_false_positives.yaml
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/tasks/execute_sast_ai_workflow.yaml

pvc:
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/pvc.yaml
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/cache_pvc.yaml

pipeline:
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/pipeline.yaml

run:
	# remove any old run
	$(CO) delete pipelinerun sast-ai-workflow-pipelinerun \
		-n $(NAMESPACE) --ignore-not-found

	# start a new run, passing all params as env overrides
	$(TK) pipeline start sast-ai-workflow-pipeline \
	  -n $(NAMESPACE) \
	  -p sourceCodeUrl="$(SOURCE_URL)" \
	  -p googleSpreadsheetUrl="$(SPREADSHEET_URL)" \
	  -p falsePositivesUrl="$(FALSE_POSITIVES_URL)" \
	  -p LLM_URL="$(LLM_URL)" \
	  -p LLM_MODEL_NAME="$(LLM_MODEL_NAME)" \
	  -p EMBEDDINGS_LLM_URL="$(EMBEDDINGS_LLM_URL)" \
	  -p EMBEDDINGS_LLM_MODEL_NAME="$(EMBEDDINGS_LLM_MODEL_NAME)" \
	  -p PROJECT_NAME="$(PROJECT_NAME)" \
	  -p PROJECT_VERSION="$(PROJECT_VERSION)" \
	  -p INPUT_REPORT_FILE_PATH="$(INPUT_REPORT_FILE_PATH)" \
	  --workspace name=shared-workspace,claimName=sast-ai-workflow-pvc \
	  --workspace name=gitlab-token-ws,secret=gitlab-token-secret \
      --workspace name=llm-api-key-ws,secret=llm-api-key-secret \
      --workspace name=embeddings-api-key-ws,secret=embeddings-api-key-secret \
      --workspace name=google-sa-json-ws,secret=google-service-account-secret \
      --workspace name=cache-workspace,claimName=sast-ai-cache-pvc \
	  --showlog

logs:
	tkn pipelinerun logs sast-ai-workflow-pipelinerun -n $(NAMESPACE) -f

clean:
	$(CO) delete -n $(NAMESPACE) \
		-f deploy/tekton/tasks/validate_urls.yaml \
		-f deploy/tekton/tasks/prepare_source.yaml \
		-f deploy/tekton/tasks/fetch_false_positives.yaml \
		-f deploy/tekton/tasks/execute_sast_ai_workflow.yaml \
		-f deploy/tekton/pipeline.yaml \
		-f deploy/tekton/pipelinerun.yaml \
		--ignore-not-found
