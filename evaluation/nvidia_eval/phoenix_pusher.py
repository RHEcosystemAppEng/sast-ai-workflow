"""
Phoenix metrics pusher for NVIDIA evaluation results.
Creates proper parent-child OTLP spans for evaluation grouping.
"""
import json
import logging
import os
import time
import requests
import statistics
from opentelemetry import trace
from phoenix.otel import register

logger = logging.getLogger(__name__)

def add_feedback_to_spans(evaluation_data, run_id):
    """Add feedback to spans using Phoenix GraphQL - get span IDs first, then annotate"""
    try:
        # First query Phoenix to get spans from our evaluation run using projects API
        spans_query = """
        query GetSpans {
          projects {
            edges {
              node {
                id
                name
                spans(first: 50) {
                  edges {
                    node {
                      id
                      name
                      attributes
                    }
                  }
                }
              }
            }
          }
        }
        """
        
        # Query for spans
        response = requests.post(
            "http://localhost:6007/graphql",
            json={"query": spans_query},
            headers={"Content-Type": "application/json"}
        )
        
        if response.status_code != 200:
            print(f"Failed to query spans: {response.status_code}")
            return False
            
        data = response.json()
        if "errors" in data:
            print(f"GraphQL errors querying spans: {data['errors']}")
            return False
            
        # Find our spans
        target_spans = []
        projects = data.get("data", {}).get("projects", {}).get("edges", [])
        
        for project_edge in projects:
            project = project_edge["node"]
            spans = project.get("spans", {}).get("edges", [])
            
            for span_edge in spans:
                span = span_edge["node"]
                span_name = span.get("name", "")
                span_id = span.get("id")
                
                # Look for our evaluation run spans (both parent and children)
                if "Security Analysis" in span_name and any(item["id"] in span_name for item in evaluation_data["eval_output_items"]):
                    target_spans.append({
                        "id": span_id,
                        "name": span_name,
                        "test_case_id": span_name.split(" - ")[0]
                    })
                elif "SAST Evaluation Run" in span_name and run_id in span_name:
                    target_spans.append({
                        "id": span_id,
                        "name": span_name,
                        "test_case_id": "parent"
                    })
        
        print(f"Found {len(target_spans)} spans to annotate")
        
        # Calculate median score for parent feedback
        all_scores = [item["score"] for item in evaluation_data["eval_output_items"]]
        median_score = statistics.median(all_scores)
        
        # Now add annotations to each span
        for span_info in target_spans:
            span_id = span_info["id"]
            test_case_id = span_info["test_case_id"]
            
            if test_case_id == "parent":
                # Parent span gets median score
                overall_score = median_score
                explanation = f"Median evaluation score: {median_score:.3f} across {len(all_scores)} test cases"
            else:
                # Find evaluation data for this test case
                eval_item = next((item for item in evaluation_data["eval_output_items"] 
                                if item["id"] == test_case_id), None)
                
                if eval_item:
                    overall_score = eval_item["score"]
                    explanation = f"Evaluation score: {overall_score:.3f}"
                else:
                    continue
            
            # Create annotation mutation with required fields
            mutation = """
            mutation CreateSpanAnnotations($input: [CreateSpanAnnotationInput!]!) {
              createSpanAnnotations(input: $input) {
                spanAnnotations {
                  id
                  name
                  label
                  score
                }
              }
            }
            """
            
            variables = {
                "input": [{
                    "spanId": span_id,
                    "name": "evaluation",
                    "label": "excellent" if overall_score >= 0.9 else "good" if overall_score >= 0.7 else "fair" if overall_score >= 0.5 else "poor" if overall_score >= 0.3 else "very_poor",
                    "score": overall_score,
                    "explanation": explanation,
                    "annotatorKind": "HUMAN"  # Required field
                }]
            }
            
            try:
                response = requests.post(
                    "http://localhost:6007/graphql",
                    json={"query": mutation, "variables": variables},
                    headers={"Content-Type": "application/json"}
                )
                
                if response.status_code == 200:
                    data = response.json()
                    if "errors" not in data:
                        if test_case_id == "parent":
                            print(f"    ✓ Added parent feedback: {overall_score:.3f}")
                        else:
                            print(f"    ✓ Added feedback for {test_case_id}: {overall_score:.3f}")
                    else:
                        print(f"    ✗ GraphQL errors for {test_case_id}: {data['errors']}")
                else:
                    print(f"    ✗ Failed to add feedback for {test_case_id}: {response.status_code}")
                    
            except Exception as e:
                print(f"    ✗ Feedback API error for {test_case_id}: {e}")
        
        return len(target_spans) > 0
        
    except Exception as e:
        print(f"Error adding feedback: {e}")
        return False

def push_current_results_to_phoenix():
    """Create proper parent-child span hierarchy using OTLP"""
    try:
        # Load results from latest evaluation (check latest symlink first, then current dir)
        results_base = "evaluation/nvidia_eval/results"
        latest_link = f"{results_base}/latest"
        
        if os.path.islink(latest_link):
            results_dir = latest_link
            print(f"Using latest results from: {os.readlink(latest_link)}")
        else:
            results_dir = results_base
            print("Using results from current directory")
            
        with open(f"{results_dir}/security_analysis_eval_output.json", 'r') as f:
            evaluation_data = json.load(f)
        with open(f"{results_dir}/workflow_output.json", 'r') as f:
            workflow_data = json.load(f)
        
        print("Creating parent-child span hierarchy using OTLP:")
        print(f"  Security analysis average: {evaluation_data['average_score']}")
        print(f"  Workflow items: {len(workflow_data)}")
        print(f"  Evaluation items: {len(evaluation_data['eval_output_items'])}")
        
        # Register with Phoenix to create the sast_eval_agg project
        tracer_provider = register(
            project_name="sast_eval_agg",
            endpoint="http://localhost:6007/v1/traces"
        )
        
        # Get tracer from the provider
        tracer = tracer_provider.get_tracer("sast_eval_agg", "1.0.0")
        
        # Create parent span for the evaluation run
        run_id = f"eval_run_{int(time.time())}"
        
        # Calculate median score for parent feedback
        all_scores = [item["score"] for item in evaluation_data["eval_output_items"]]
        median_score = statistics.median(all_scores)
        
        with tracer.start_as_current_span(f"SAST Evaluation Run - {run_id}") as parent_span:
            # Add parent span attributes with explicit project tagging
            parent_span.set_attribute("service.name", "sast_eval_agg")
            parent_span.set_attribute("project.name", "sast_eval_agg")
            parent_span.set_attribute("evaluation.run_id", run_id)
            parent_span.set_attribute("evaluation.average_score", evaluation_data['average_score'])
            parent_span.set_attribute("evaluation.median_score", median_score)
            parent_span.set_attribute("evaluation.total_items", len(workflow_data))
            parent_span.set_attribute("evaluation.type", "security_analysis")
            parent_span.set_attribute("llm.model_name", "nvidia/llama-3.1-nemotron-70b-instruct")
            
            # Add parent feedback attributes
            parent_span.set_attribute("user_feedback.score", median_score)
            parent_span.set_attribute("user_feedback.label", "excellent" if median_score >= 0.9 else "good" if median_score >= 0.7 else "fair" if median_score >= 0.5 else "poor" if median_score >= 0.3 else "very_poor")
            parent_span.set_attribute("user_feedback.explanation", f"Median evaluation score: {median_score:.3f} across {len(all_scores)} test cases")
            
            print(f"Created parent span: SAST Evaluation Run - {run_id}")
            print(f"  Median score: {median_score:.3f}")
            
            # Create child spans for each test case
            for workflow_item in workflow_data:
                item_id = workflow_item["id"]
                
                # Find scores from security analysis evaluation
                eval_item = next((item for item in evaluation_data["eval_output_items"] if item["id"] == item_id), None)
                
                if eval_item:
                    overall_score = eval_item["score"]
                    coverage_score = eval_item["reasoning"]["score_breakdown"]["coverage_score"]
                    correctness_score = eval_item["reasoning"]["score_breakdown"]["correctness_score"]
                    relevance_score = eval_item["reasoning"]["score_breakdown"]["relevance_score"]
                    
                    # Create child span for this test case
                    with tracer.start_as_current_span(f"{item_id} - Security Analysis") as child_span:
                        # Add child span attributes with explicit project tagging
                        child_span.set_attribute("service.name", "sast_eval_agg")
                        child_span.set_attribute("project.name", "sast_eval_agg")
                        child_span.set_attribute("test_case.id", item_id)
                        # Use OpenTelemetry LLM semantic conventions for proper column separation
                        child_span.set_attribute("input.value", workflow_item['question'])
                        child_span.set_attribute("output.value", workflow_item['generated_answer'])
                        child_span.set_attribute("reference.value", workflow_item['expected_output_obj'])
                        
                        # Add token usage and performance data using Phoenix-expected structure
                        if workflow_item['intermediate_steps']:
                            for step in workflow_item['intermediate_steps']:
                                if step['payload']['event_type'] == 'LLM_END':
                                    usage = step['payload']['usage_info']['token_usage']
                                    child_span.set_attribute("llm.token_count.prompt", usage['prompt_tokens'])
                                    child_span.set_attribute("llm.token_count.completion", usage['completion_tokens'])
                                    child_span.set_attribute("llm.token_count.total", usage['total_tokens'])
                                    child_span.set_attribute("llm.model_name", step['payload']['name'])
                                    
                                    # Add timing information
                                    latency = step['payload']['event_timestamp'] - step['payload']['span_event_timestamp']
                                    child_span.set_attribute("llm.latency", latency)
                                    break
                        
                        # Add evaluation scores as span attributes for metadata/info display
                        child_span.set_attribute("evaluation.overall_score", overall_score)
                        child_span.set_attribute("evaluation.coverage_score", coverage_score)
                        child_span.set_attribute("evaluation.correctness_score", correctness_score)
                        child_span.set_attribute("evaluation.relevance_score", relevance_score)
                        child_span.set_attribute("evaluation.reasoning", eval_item["reasoning"]["reasoning"])
                        
                        # Add feedback using the exact format from original working version
                        child_span.set_attribute("user_feedback.score", overall_score)
                        child_span.set_attribute("user_feedback.label", "excellent" if overall_score >= 0.9 else "good" if overall_score >= 0.7 else "fair" if overall_score >= 0.5 else "poor" if overall_score >= 0.3 else "very_poor")
                        child_span.set_attribute("user_feedback.explanation", f"Evaluation score: {overall_score:.3f}")
                        
                        print(f"Created child span for {item_id}: score={overall_score:.3f}")
                        
                        # Simulate some processing time for visualization
                        time.sleep(0.1)
                else:
                    print(f"No evaluation data found for {item_id}")
        
        # Force export all spans
        tracer_provider.force_flush(timeout_millis=5000)
        
        print("✓ Successfully created parent-child span hierarchy!")
        print(f"  - 1 parent span: SAST Evaluation Run - {run_id}")
        print(f"  - {len(workflow_data)} child spans for individual test cases")
        
        # Wait for spans to be indexed, then add feedback via direct API calls
        time.sleep(2)
        feedback_success = add_feedback_to_spans(evaluation_data, run_id)
        
        if feedback_success:
            print("  - Feedback successfully added to spans")
        else:
            print("  - Feedback attributes added to spans (feedback column TBD)")
        
        print("  - Project: sast_eval_agg")
        print("  - Token usage and performance data included")
        
        return True
                
    except Exception as e:
        print(f"Error creating span hierarchy: {e}")
        return False

if __name__ == "__main__":
    success = push_current_results_to_phoenix()
    if success:
        print("Phoenix metrics push completed successfully!")
    else:
        print("Phoenix metrics push failed!")