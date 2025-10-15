"""
DvcMetadataService - Responsible for DVC metadata operations.
Handles collection of DVC metadata from Tekton environment and provides DVC operations.
"""

import os
import logging
import json
import yaml
from datetime import datetime
from typing import Dict, Optional, Any, List
from pathlib import Path

logger = logging.getLogger(__name__)


class DvcMetadataService:
    """Service for managing DVC metadata operations"""
    
    def __init__(self):
        """Initialize DVC metadata service"""
        self.dvc_metadata = {}
        self._collect_metadata_from_environment()
    
    def _collect_metadata_from_environment(self) -> None:
        """Collect DVC metadata from Tekton environment variables"""
        logger.debug("Collecting DVC metadata from environment variables")
        
        self.dvc_metadata = {
            'git_commit_hash': os.getenv('DVC_GIT_COMMIT_HASH', ''),
            'repo_branch': os.getenv('DVC_REPO_BRANCH', 'main'),
            'repo_url': os.getenv('DVC_REPO_URL', ''),
            'data_version': self._generate_data_version(),
            'pipeline_stage': 'sast_ai_analysis', # default pipeline stage for now
            'execution_timestamp': datetime.now().isoformat()
        }
        
        logger.debug(f"Collected DVC metadata: {self.dvc_metadata}")
    
    def _generate_data_version(self) -> str:
        """Generate simple data version tag"""
        return "1.0.0"
    
    def get_metadata(self) -> Dict[str, Any]:
        """Get collected DVC metadata"""
        return self.dvc_metadata.copy()
    
    def get_data_version(self) -> str:
        """Get the generated data version tag"""
        return self.dvc_metadata.get('data_version', '')
    
    def get_git_commit_hash(self) -> str:
        """Get the git commit hash"""
        return self.dvc_metadata.get('git_commit_hash', '')
    
    def get_pipeline_stage(self) -> str:
        """Get the pipeline stage identifier"""
        return self.dvc_metadata.get('pipeline_stage', 'sast_ai_analysis')
    
    def _calculate_dvc_hash(self) -> str:
        """Get DVC hash from relevant .dvc files based on workflow execution"""
        try:
            dvc_files = self._get_relevant_dvc_files()
            if not dvc_files:
                logger.warning("No relevant DVC files found for current workflow")
                return 'no-dvc-files'
            
            # For now, return the first relevant DVC file hash
            first_dvc_file = dvc_files[0]
            dvc_hash = self._extract_hash_from_dvc_file(first_dvc_file)
            
            logger.debug(f"Using DVC hash from {first_dvc_file}: {dvc_hash}")
            return dvc_hash
            
        except (OSError, yaml.YAMLError) as e:
            logger.error(f"Failed to get DVC hash: {e}")
            return 'dvc-hash-failed'
    
    def _get_dvc_path(self) -> str:
        """Get the DVC-relative path from relevant .dvc files"""
        try:
            dvc_files = self._get_relevant_dvc_files()
            if not dvc_files:
                logger.warning("No relevant DVC files found for path determination")
                return 'data/unknown'
            
            # For now, return the path from the first relevant DVC file
            first_dvc_file = dvc_files[0]
            dvc_path = self._extract_path_from_dvc_file(first_dvc_file)
            
            logger.debug(f"Using DVC path from {first_dvc_file}: {dvc_path}")
            return f"data/{dvc_path}"
            
        except (OSError, yaml.YAMLError) as e:
            logger.error(f"Failed to determine DVC path: {e}")
            return 'data/unknown'

    def _get_analysis_summary(self) -> str:
        """Get a summary of the analysis execution for metadata"""
        try:
            summary_data = {
                'execution_time': self.dvc_metadata.get('execution_timestamp'),
                'pipeline_stage': self.get_pipeline_stage(),
                'data_version': self.get_data_version()
            }
            return json.dumps(summary_data, separators=(',', ':'))
        except (TypeError, ValueError) as e:
            logger.error(f"Failed to create analysis summary: {e}")
            return '{"status":"summary_generation_failed"}'
    
    def _get_relevant_dvc_files(self) -> List[str]:
        """Get list of relevant DVC files based on workflow context"""
        try:
            data_dir = Path(__file__).parent.parent.parent / 'data'
            
            available_dvc_files = [
                'sast-reports.dvc',
                'source-repos.dvc', 
                'known-issues.dvc',
                'models/embeddings.dvc',
                'models/prompts.dvc',
                'ground-truth.dvc'
            ]
            
            existing_dvc_files = []
            for dvc_file in available_dvc_files:
                dvc_path = data_dir / dvc_file
                if dvc_path.exists():
                    existing_dvc_files.append(str(dvc_path))
                    
            logger.debug(f"Found {len(existing_dvc_files)} DVC files: {existing_dvc_files}")
            return existing_dvc_files
            
        except OSError as e:
            logger.error(f"Failed to get relevant DVC files: {e}")
            return []
    
    def _extract_hash_from_dvc_file(self, dvc_file_path: str) -> str:
        """Extract MD5 hash from DVC file"""
        try:
            with open(dvc_file_path, 'r') as f:
                dvc_data = yaml.safe_load(f)
            
            if 'outs' in dvc_data and len(dvc_data['outs']) > 0:
                md5_hash = dvc_data['outs'][0].get('md5')
                if md5_hash and md5_hash != 'null':
                    return md5_hash
                else:
                    return 'no-hash-available'
            
            return 'invalid-dvc-format'
            
        except (OSError, yaml.YAMLError) as e:
            logger.error(f"Failed to extract hash from {dvc_file_path}: {e}")
            return 'hash-extraction-failed'
    
    def _extract_path_from_dvc_file(self, dvc_file_path: str) -> str:
        """Extract path from DVC file"""
        try:
            with open(dvc_file_path, 'r') as f:
                dvc_data = yaml.safe_load(f)
            
            if 'outs' in dvc_data and len(dvc_data['outs']) > 0:
                path = dvc_data['outs'][0].get('path')
                if path:
                    return path
            
            dvc_filename = Path(dvc_file_path).name
            return dvc_filename.replace('.dvc', '')
            
        except (OSError, yaml.YAMLError) as e:
            logger.error(f"Failed to extract path from {dvc_file_path}: {e}")
            return 'unknown'
    
    def track_workflow_execution(self, config, issue_list: list) -> None:
        """
        Workflow tracking that exports DVC metadata to Tekton
        
        Args:
            config: Configuration object with workflow settings
            issue_list: List of processed issues for metrics
        """
        logger.debug("Tracking basic DVC metadata for workflow execution")
        
        try:
            logger.debug(f"Workflow execution - Issues processed: {len(issue_list)}")
            logger.debug(f"Using known false positives: {getattr(config, 'USE_KNOWN_FALSE_POSITIVE_FILE', False)}")
            logger.debug(f"LLM model: {getattr(config, 'LLM_MODEL_NAME', 'unknown')}")
            
            self.export_to_tekton_results(config=config, issue_list=issue_list)
            
            logger.debug("DVC workflow tracking completed successfully")
            
        except (OSError, AttributeError) as e:
            logger.error(f"Failed to track workflow execution: {e}")
    
    def export_to_tekton_results(self, results_dir: str = "/tekton/results", config=None, issue_list=None) -> None:
        """
        Export comprehensive DVC metadata to Tekton task result files

        Args:
            results_dir: Directory where Tekton task results are written
            config: Configuration object with workflow settings (for report path)
            issue_list: List of processed issues (for issue count)
        """
        logger.debug("Exporting comprehensive DVC metadata to Tekton task results")
        
        try:
            Path(results_dir).mkdir(parents=True, exist_ok=True)
            
            metadata_exports = {
                # Core DVC data versioning
                "dvc-data-version": self.get_data_version(),
                "dvc-commit-hash": self.get_git_commit_hash(),
                "dvc-pipeline-stage": self.get_pipeline_stage(),
                
                # Artifact metadata
                "dvc-hash": self._calculate_dvc_hash(),
                "dvc-path": self._get_dvc_path(),

                # Execution context metadata
                "dvc-execution-timestamp": self.dvc_metadata.get('execution_timestamp', ''),
                "dvc-source-analysis-summary": self._get_analysis_summary(),
                
                # Repository context
                "dvc-repo-url": self.dvc_metadata.get('repo_url', ''),
                "dvc-repo-branch": self.dvc_metadata.get('repo_branch', ''),

                # Additional required metadata fields
                "dvc-split-type": "train",
                "dvc-sast-report-path": getattr(config, 'INPUT_REPORT_FILE_PATH', '') if config else '',
                "dvc-issues-count": str(len(issue_list)) if issue_list else "0"
            }
            
            try:
                for result_name, value in metadata_exports.items():
                    if value:
                        file_path = Path(results_dir) / result_name
                        with open(file_path, "w") as f:
                            f.write(str(value))
                        logger.debug(f"Exported {result_name}: {value}")
                    else:
                        logger.warning(f"Skipping empty value for {result_name}")

                logger.debug(f"Comprehensive DVC metadata exported to Tekton results: {results_dir}")

            except OSError as e:
                logger.error(f"Failed to export DVC metadata to Tekton results: {e}")

        except OSError as e:
            logger.error(f"Failed to create results directory {results_dir}: {e}")

    def log_execution_summary(self) -> None:
        """Log execution summary with DVC metadata"""
        logger.info("=== DVC Execution Summary ===")
        logger.info(f"Data Version: {self.get_data_version()}")
        logger.info(f"Pipeline Stage: {self.get_pipeline_stage()}")
        logger.info(f"Git Commit: {self.get_git_commit_hash()}")
        logger.info(f"Repository: {self.dvc_metadata.get('repo_url', 'unknown')}")
        logger.info(f"Branch: {self.dvc_metadata.get('repo_branch', 'unknown')}")
        logger.info("=============================")