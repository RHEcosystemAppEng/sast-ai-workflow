#!/usr/bin/env python3
"""
Script to organize evaluation results into timestamped folders
Usage: python organize_results.py
"""
import os
import shutil
from datetime import datetime
from pathlib import Path

def organize_results():
    """Move latest results to timestamped folder"""
    results_dir = Path("evaluation/nvidia_eval/results")
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    timestamped_dir = results_dir / f"run_{timestamp}"
    
    # Files to organize
    result_files = [
        "security_analysis_eval_output.json",
        "workflow_output.json", 
        "standardized_data_all.csv",
        "inference_optimization.json",
        "all_requests_profiler_traces.json"
    ]
    
    # Check if any result files exist
    existing_files = [f for f in result_files if (results_dir / f).exists()]
    if not existing_files:
        print("No result files found to organize")
        return False
        
    # Create timestamped directory
    timestamped_dir.mkdir(exist_ok=True)
    print(f"Organizing results into: {timestamped_dir}")
    
    # Move files to timestamped folder
    for file in existing_files:
        src = results_dir / file
        dst = timestamped_dir / file
        shutil.move(str(src), str(dst))
        print(f"  Moved {file} -> {timestamped_dir.name}/{file}")
    
    # Create a 'latest' symlink pointing to this run
    latest_link = results_dir / "latest"
    if latest_link.exists() or latest_link.is_symlink():
        latest_link.unlink()
    latest_link.symlink_to(timestamped_dir.name)
    print(f"  Created symlink: latest -> {timestamped_dir.name}")
    
    print(f"✓ Organized {len(existing_files)} files into timestamped folder")
    return True

if __name__ == "__main__":
    organize_results()