NAMESPACE ?= sast-ai-workflow

CO := oc

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
	$(CO) delete pipelinerun sast-ai-workflow-pipelinerun -n $(NAMESPACE) --ignore-not-found
	$(CO) apply -n $(NAMESPACE) -f deploy/tekton/pipelinerun.yaml

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