#!/usr/bin/env python3
"""
Complete SAST evaluation workflow script.

This script runs the full evaluation pipeline:
1. NAT evaluation using NVIDIA API
2. Result organization into timestamped folders
3. Phoenix dashboard push with parent-child hierarchy

Usage:
    export LLM_API_KEY=your_nvidia_api_key
    python run_full_evaluation.py

Or run directly:
    LLM_API_KEY=your_key python run_full_evaluation.py
"""

import os
import subprocess
import sys
from pathlib import Path

def check_environment():
    """Check if required environment variables and dependencies are available"""
    # Check for API key
    api_key = os.getenv('LLM_API_KEY')
    if not api_key:
        print("Error: LLM_API_KEY environment variable not set")
        print("Please set it with: export LLM_API_KEY=your_nvidia_api_key")
        return False
    
    # Check if virtual environment exists
    venv_path = Path(".venv-test")
    if not venv_path.exists():
        print("Error: Virtual environment .venv-test not found")
        print("Please create it first or adjust the path in this script")
        return False
    
    # Check if config file exists
    config_path = Path("evaluation/nvidia_eval/eval_config.yml")
    if not config_path.exists():
        print(f"Error: Config file not found: {config_path}")
        return False
    
    print("Environment checks passed")
    return True

def run_nat_evaluation():
    """Run NAT evaluation with proper environment setup"""
    print("\nStep 1: Running NAT Evaluation...")
    
    # Construct the command
    cmd = [
        "bash", "-c", 
        "source .venv-test/bin/activate && "
        f"export LLM_API_KEY={os.getenv('LLM_API_KEY')} && "
        "nat eval --config_file evaluation/nvidia_eval/eval_config.yml"
    ]
    
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, cwd=".")
        
        if result.returncode == 0:
            print("NAT evaluation completed successfully")
            print("Output:", result.stdout[-500:])  # Show last 500 chars
            return True
        else:
            print("NAT evaluation failed")
            print("Error:", result.stderr)
            print("Output:", result.stdout)
            return False
            
    except Exception as e:
        print(f"Error running NAT evaluation: {e}")
        return False

def organize_results():
    """Run result organization script"""
    print("\nStep 2: Organizing results...")
    
    try:
        # Import and run the organize_results function
        sys.path.append('evaluation/nvidia_eval')
        from organize_results import organize_results as organize
        
        success = organize()
        if success:
            print("Results organized successfully")
            return True
        else:
            print("No results found to organize")
            return False
            
    except Exception as e:
        print(f"Error organizing results: {e}")
        return False

def push_to_phoenix():
    """Run Phoenix pusher script"""
    print("\nStep 3: Pushing to Phoenix dashboard...")
    
    try:
        # Import and run the phoenix pusher
        sys.path.append('evaluation/nvidia_eval')
        from phoenix_pusher import push_current_results_to_phoenix
        
        success = push_current_results_to_phoenix()
        if success:
            print("Phoenix push completed successfully")
            return True
        else:
            print("Phoenix push failed")
            return False
            
    except Exception as e:
        print(f"Error pushing to Phoenix: {e}")
        return False

def main():
    """Run the complete evaluation workflow"""
    print("Starting SAST Evaluation Workflow")
    print("=" * 50)
    
    # Check environment
    if not check_environment():
        sys.exit(1)
    
    # Step 1: Run NAT evaluation
    if not run_nat_evaluation():
        print("\nWorkflow failed at NAT evaluation step")
        sys.exit(1)
    
    # Step 2: Organize results
    if not organize_results():
        print("\nWorkflow failed at result organization step")
        sys.exit(1)
    
    # Step 3: Push to Phoenix
    if not push_to_phoenix():
        print("\nWorkflow failed at Phoenix push step")
        sys.exit(1)
    
    print("\nComplete SAST Evaluation Workflow Completed Successfully!")
    print("=" * 50)
    print("NAT evaluation completed")
    print("Results organized into timestamped folder")
    print("Metrics pushed to Phoenix dashboard")
    print("\nCheck your Phoenix dashboard at: http://localhost:6007")
    print("Project: sast_eval_agg")

if __name__ == "__main__":
    main()