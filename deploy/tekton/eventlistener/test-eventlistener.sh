#!/bin/bash

# Test script for Tekton EventListener MLOps Benchmarking
# This script helps validate the EventListener setup and trigger test PipelineRuns

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
NAMESPACE="${NAMESPACE:-$(oc project -q 2>/dev/null || echo "default")}"
SERVICE_NAME="el-benchmark-mlop-listener"
LOCAL_PORT="${LOCAL_PORT:-8080}"

echo -e "${BLUE}=========================================${NC}"
echo -e "${BLUE}Tekton EventListener MLOps Benchmark Test${NC}"
echo -e "${BLUE}=========================================${NC}"
echo ""
echo "Namespace: $NAMESPACE"
echo "Service: $SERVICE_NAME"
echo "Local Port: $LOCAL_PORT"
echo ""
echo "Environment variables (optional):"
echo "  TRIGGER_SOURCE: ${TRIGGER_SOURCE:-manual-test} (argocd, webhook, jenkins, etc.)"
echo "  IMAGE_VERSION: ${IMAGE_VERSION:-latest}"
echo "  DVC_DATA_VERSION: ${DVC_DATA_VERSION:-(empty)}"
echo "  PROMPTS_VERSION: ${PROMPTS_VERSION:-(empty)}"
echo "  KNOWN_NON_ISSUES_VERSION: ${KNOWN_NON_ISSUES_VERSION:-(empty)}"
echo "  USE_KNOWN_FP: ${USE_KNOWN_FP:-true}"
echo ""

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to check prerequisites
check_prerequisites() {
    echo -e "${YELLOW}Checking prerequisites...${NC}"
    
    # Check for oc or kubectl
    if command_exists oc; then
        KUBECTL="oc"
        echo -e "${GREEN}✓${NC} Found oc CLI"
    elif command_exists kubectl; then
        KUBECTL="kubectl"
        echo -e "${GREEN}✓${NC} Found kubectl CLI"
    else
        echo -e "${RED}✗${NC} Neither oc nor kubectl found. Please install OpenShift or Kubernetes CLI."
        exit 1
    fi
    
    # Check for curl
    if ! command_exists curl; then
        echo -e "${RED}✗${NC} curl not found. Please install curl."
        exit 1
    fi
    echo -e "${GREEN}✓${NC} Found curl"
    
    # Check for jq (optional)
    if command_exists jq; then
        echo -e "${GREEN}✓${NC} Found jq (for JSON parsing)"
        HAS_JQ=true
    else
        echo -e "${YELLOW}⚠${NC} jq not found (optional, for pretty JSON output)"
        HAS_JQ=false
    fi
    
    # Check for tkn (optional)
    if command_exists tkn; then
        echo -e "${GREEN}✓${NC} Found tkn CLI (for watching PipelineRuns)"
        HAS_TKN=true
    else
        echo -e "${YELLOW}⚠${NC} tkn CLI not found (optional, for easier pipeline monitoring)"
        HAS_TKN=false
    fi
    
    echo ""
}

# Function to check if resources are deployed
check_deployment() {
    echo -e "${YELLOW}Checking if EventListener resources are deployed...${NC}"
    
    # Check ConfigMap
    if $KUBECTL get configmap benchmark-config -n "$NAMESPACE" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} ConfigMap 'benchmark-config' exists"
    else
        echo -e "${RED}✗${NC} ConfigMap 'benchmark-config' not found"
        echo ""
        echo "Please deploy the EventListener resources first:"
        echo "  cd deploy"
        echo "  make eventlistener  # Auto-detects namespace from current context"
        exit 1
    fi
    
    # Check Tasks
    if $KUBECTL get task call-orchestrator-api-mlop -n "$NAMESPACE" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Task 'call-orchestrator-api-mlop' exists"
    else
        echo -e "${RED}✗${NC} Task 'call-orchestrator-api-mlop' not found"
        exit 1
    fi
    
    if $KUBECTL get task poll-batch-status-mlop -n "$NAMESPACE" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Task 'poll-batch-status-mlop' exists"
    else
        echo -e "${RED}✗${NC} Task 'poll-batch-status-mlop' not found"
        exit 1
    fi
    
    # Check Pipeline
    if $KUBECTL get pipeline benchmark-mlop-pipeline -n "$NAMESPACE" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Pipeline 'benchmark-mlop-pipeline' exists"
    else
        echo -e "${RED}✗${NC} Pipeline 'benchmark-mlop-pipeline' not found"
        exit 1
    fi
    
    # Check EventListener
    if $KUBECTL get eventlistener benchmark-mlop-listener -n "$NAMESPACE" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} EventListener 'benchmark-mlop-listener' exists"
    else
        echo -e "${RED}✗${NC} EventListener 'benchmark-mlop-listener' not found"
        exit 1
    fi
    
    # Check Service
    if $KUBECTL get service "$SERVICE_NAME" -n "$NAMESPACE" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Service '$SERVICE_NAME' exists"
    else
        echo -e "${RED}✗${NC} Service '$SERVICE_NAME' not found"
        exit 1
    fi
    
    # Check if EventListener pod is running
    POD_NAME=$($KUBECTL get pods -n "$NAMESPACE" -l eventlistener=benchmark-mlop-listener -o jsonpath='{.items[0].metadata.name}' 2>/dev/null || echo "")
    if [ -n "$POD_NAME" ]; then
        POD_STATUS=$($KUBECTL get pod "$POD_NAME" -n "$NAMESPACE" -o jsonpath='{.status.phase}')
        if [ "$POD_STATUS" = "Running" ]; then
            echo -e "${GREEN}✓${NC} EventListener pod is running: $POD_NAME"
        else
            echo -e "${YELLOW}⚠${NC} EventListener pod status: $POD_STATUS"
        fi
    else
        echo -e "${YELLOW}⚠${NC} EventListener pod not found (may still be starting)"
    fi
    
    echo ""
}

# Function to display ConfigMap configuration
show_config() {
    echo -e "${YELLOW}Current Configuration:${NC}"
    $KUBECTL get configmap benchmark-config -n "$NAMESPACE" -o jsonpath='{.data}' | grep -o '"[^"]*"[[:space:]]*:[[:space:]]*"[^"]*"' | sed 's/^/  /' || echo "  (unable to read)"
    echo ""
}

# Function to start port-forward
start_port_forward() {
    echo -e "${YELLOW}Starting port-forward to EventListener...${NC}"
    echo "This will forward localhost:$LOCAL_PORT -> $SERVICE_NAME:8080"
    echo ""
    echo -e "${BLUE}Port-forward command:${NC}"
    echo "  $KUBECTL port-forward svc/$SERVICE_NAME $LOCAL_PORT:8080 -n $NAMESPACE"
    echo ""
    echo -e "${YELLOW}Note: Keep this terminal open. Use another terminal to send test requests.${NC}"
    echo ""
    
    # Start port-forward
    $KUBECTL port-forward "svc/$SERVICE_NAME" "$LOCAL_PORT:8080" -n "$NAMESPACE"
}

# Function to send test request
send_test_request() {
    echo -e "${YELLOW}Sending test request to EventListener...${NC}"
    
    # Generate timestamp for unique identification
    TIMESTAMP=$(date +%Y%m%d-%H%M%S)
    
    # Prepare payload with all MLOps parameters (using underscores to match TriggerBinding)
    PAYLOAD=$(cat <<EOF
{
  "submitted_by": "test-script-$TIMESTAMP",
  "trigger_source": "${TRIGGER_SOURCE:-manual-test}",
  "image_version": "${IMAGE_VERSION:-latest}",
  "dvc_data_version": "${DVC_DATA_VERSION:-v1}",
  "prompts_version": "${PROMPTS_VERSION:-v1}",
  "known_non_issues_version": "${KNOWN_NON_ISSUES_VERSION:-v1}",
  "use_known_false_positive_file": ${USE_KNOWN_FP:-true}
}
EOF
)
    
    echo "Payload:"
    if [ "$HAS_JQ" = true ]; then
        echo "$PAYLOAD" | jq '.'
    else
        echo "$PAYLOAD"
    fi
    echo ""
    
    # Send request
    echo "Sending POST request to http://localhost:$LOCAL_PORT ..."
    RESPONSE=$(curl -s -w "\nHTTP_STATUS:%{http_code}" -X POST "http://localhost:$LOCAL_PORT" \
        -H "Content-Type: application/json" \
        -d "$PAYLOAD" 2>&1)
    
    # Parse response
    HTTP_STATUS=$(echo "$RESPONSE" | grep "HTTP_STATUS:" | cut -d':' -f2)
    BODY=$(echo "$RESPONSE" | sed '/HTTP_STATUS:/d')
    
    echo "Response Status: $HTTP_STATUS"
    echo "Response Body:"
    if [ "$HAS_JQ" = true ] && echo "$BODY" | jq '.' >/dev/null 2>&1; then
        echo "$BODY" | jq '.'
    else
        echo "$BODY"
    fi
    echo ""
    
    if [ "$HTTP_STATUS" -ge 200 ] && [ "$HTTP_STATUS" -lt 300 ]; then
        echo -e "${GREEN}✓ EventListener accepted the request!${NC}"
        echo ""
        echo "A PipelineRun should have been created. Check with:"
        if [ "$HAS_TKN" = true ]; then
            echo "  tkn pipelinerun list -n $NAMESPACE"
            echo "  tkn pipelinerun logs -L -f -n $NAMESPACE"
        else
            echo "  $KUBECTL get pipelinerun -n $NAMESPACE"
            echo "  $KUBECTL get pipelinerun -n $NAMESPACE -l app.kubernetes.io/component=benchmark-mlop"
        fi
    else
        echo -e "${RED}✗ EventListener returned an error${NC}"
        echo "Check EventListener logs:"
        echo "  $KUBECTL logs -l eventlistener=benchmark-mlop-listener -n $NAMESPACE"
    fi
    echo ""
}

# Function to watch pipeline runs
watch_pipelineruns() {
    echo -e "${YELLOW}Watching recent benchmark PipelineRuns...${NC}"
    echo ""
    
    if [ "$HAS_TKN" = true ]; then
        echo "Using tkn to watch PipelineRuns:"
        tkn pipelinerun list -n "$NAMESPACE" -l app.kubernetes.io/component=benchmark-mlop
        echo ""
        echo "To follow logs of the latest run:"
        echo "  tkn pipelinerun logs -L -f -n $NAMESPACE"
    else
        echo "Recent benchmark PipelineRuns:"
        $KUBECTL get pipelinerun -n "$NAMESPACE" -l app.kubernetes.io/component=benchmark-mlop --sort-by=.metadata.creationTimestamp
        echo ""
        echo "To view logs:"
        echo "  $KUBECTL logs -l tekton.dev/pipelineTask=call-orchestrator-api -n $NAMESPACE --tail=100"
    fi
    echo ""
    
    echo -e "${YELLOW}Query PipelineRuns by trigger source:${NC}"
    echo "  # Manual tests:"
    echo "  $KUBECTL get pr -n $NAMESPACE -l sast-ai.redhat.com/trigger-source=manual-test"
    echo ""
    echo "  # ArgoCD triggers:"
    echo "  $KUBECTL get pr -n $NAMESPACE -l sast-ai.redhat.com/trigger-source=argocd"
    echo ""
    echo "  # All benchmark runs:"
    echo "  $KUBECTL get pr -n $NAMESPACE -l app.kubernetes.io/component=benchmark-mlop"
    echo ""
    
    echo -e "${YELLOW}To clean up test PipelineRuns:${NC}"
    echo "  cd ../.. && make eventlistener-clean NAMESPACE=$NAMESPACE"
    echo ""
}

# Main menu
show_menu() {
    echo -e "${BLUE}=========================================${NC}"
    echo -e "${BLUE}What would you like to do?${NC}"
    echo -e "${BLUE}=========================================${NC}"
    echo "1. Check deployment and configuration"
    echo "2. Start port-forward (keep terminal open)"
    echo "3. Send test request (requires port-forward in another terminal)"
    echo "4. Watch PipelineRuns"
    echo "5. Show current configuration"
    echo "6. Full test (port-forward in background, send request, watch)"
    echo "0. Exit"
    echo ""
    read -p "Enter choice [0-6]: " choice
    echo ""
    
    case $choice in
        1)
            check_deployment
            show_config
            read -p "Press Enter to continue..."
            show_menu
            ;;
        2)
            start_port_forward
            ;;
        3)
            send_test_request
            read -p "Press Enter to continue..."
            show_menu
            ;;
        4)
            watch_pipelineruns
            read -p "Press Enter to continue..."
            show_menu
            ;;
        5)
            show_config
            read -p "Press Enter to continue..."
            show_menu
            ;;
        6)
            check_deployment
            show_config
            echo -e "${YELLOW}Starting port-forward in background...${NC}"
            $KUBECTL port-forward "svc/$SERVICE_NAME" "$LOCAL_PORT:8080" -n "$NAMESPACE" &
            PF_PID=$!
            sleep 3
            send_test_request
            sleep 2
            watch_pipelineruns
            kill $PF_PID 2>/dev/null || true
            echo -e "${GREEN}Port-forward stopped${NC}"
            ;;
        0)
            echo "Exiting..."
            exit 0
            ;;
        *)
            echo -e "${RED}Invalid choice${NC}"
            show_menu
            ;;
    esac
}

# Main execution
check_prerequisites

# If script is run with argument, execute that action directly
if [ $# -gt 0 ]; then
    case "$1" in
        check|status)
            check_deployment
            show_config
            ;;
        port-forward|pf)
            check_deployment
            start_port_forward
            ;;
        test|trigger)
            send_test_request
            ;;
        watch|logs)
            watch_pipelineruns
            ;;
        *)
            echo "Usage: $0 [check|port-forward|test|watch]"
            exit 1
            ;;
    esac
else
    # Interactive mode
    check_deployment
    show_menu
fi

