NAMESPACE ?= sast-ai-workflow

CO := oc
TK := tkn

#— Pipeline parameters (overrideable on the CLI):
SOURCE_URL                       ?= source/code/url
SPREADSHEET_URL                  ?= google/spreadsheet/url
FALSE_POSITIVES_URL              ?= false/positives/url

LLM_URL                          ?= http://<<please-set-llm-url>>
LLM_MODEL_NAME                   ?= llm-model
EMBEDDINGS_LLM_URL               ?= http://<<please-set-embedding-llm-url>>
EMBEDDINGS_LLM_MODEL_NAME        ?= embedding-llm-model

.PHONY: all tasks pipeline run logs clean

all: tasks pipeline run logs

tasks:
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/tasks/validate_urls.yaml
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/tasks/prepare_source.yaml
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/tasks/fetch_false_positives.yaml
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/tasks/execute_sast_ai_workflow.yaml

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
	  --workspace name=shared-workspace,claimName=sast-ai-workflow-pvc \
	  --workspace name=gitlab-token-ws,secret=gitlab-token-secret \
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
		--ignore-not-f	