import logging
from typing import Optional

from pydantic import Field

from aiq.builder.builder import Builder, LLMFrameworkEnum
from aiq.builder.function_info import FunctionInfo
from aiq.cli.register_workflow import register_function
from aiq.data_models.function import FunctionBaseConfig

from dto.SASTWorkflowModels import SASTWorkflowTracker
from dto.ResponseStructures import JudgeLLMResponse
from services.issue_analysis_service import IssueAnalysisService
from services.vector_store_service import VectorStoreService
try:
    from Utils.deepeval_utils import LangChainLLMWrapper, SummaryEvaluator
    DEEPEVAL_AVAILABLE = True
except ImportError:
    DEEPEVAL_AVAILABLE = False
    LangChainLLMWrapper = None
    SummaryEvaluator = None

logger = logging.getLogger(__name__)


class SummarizeJustificationsConfig(FunctionBaseConfig, name="summarize_justifications"):
    """
    Summarize justifications function for SAST workflow.
    """
    description: str = Field(
        default="Summarize justifications function that summarizes analysis justifications",
        description="Function description"
    )
    llm_name: str = Field(description="LLM name to use for summarization")
    enable_evaluation: bool = Field(default=False, description="Enable DeepEval evaluation of summaries")
    evaluation_output_path: Optional[str] = Field(default=None, description="Path to save evaluation results JSON")
    faithfulness_threshold: float = Field(default=0.8, description="Threshold for faithfulness evaluation")
    relevancy_threshold: float = Field(default=0.75, description="Threshold for relevancy evaluation")


@register_function(config_type=SummarizeJustificationsConfig, framework_wrappers=[LLMFrameworkEnum.LANGCHAIN])
async def summarize_justifications(
    config: SummarizeJustificationsConfig, builder: Builder
):
    """Register the Summarize_Justifications function."""
    
    logger.info("Initializing Summarize_Justifications function...")
    
    async def _summarize_justifications_fn(tracker: SASTWorkflowTracker) -> SASTWorkflowTracker:
        """
        Summarize justifications function for SAST workflow.
        """
        logger.info("Running Summarize_Justifications node - summarizing justifications")
        logger.info(f"Summarize_Justifications node processing tracker with {len(tracker.issues)} issues")
        
        if not tracker.config:
            logger.warning("No config found in tracker - creating config with real production prompts")
            # Create a config object with real production prompt templates
            class ProductionConfig:
                def __init__(self):
                    # Load real prompt templates from files
                    import os
                    import yaml
                    
                    prompts_dir = os.path.join(os.path.dirname(__file__), "../../templates/prompts")
                    
                    # Load justification summary system prompt
                    system_prompt_file = os.path.join(prompts_dir, "justification_summary_system_prompt.yaml")
                    with open(system_prompt_file, "r", encoding="utf-8") as f:
                        system_prompt_data = yaml.safe_load(f)
                        self.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT = system_prompt_data.get("template", "")
                    
                    # Load justification summary human prompt  
                    human_prompt_file = os.path.join(prompts_dir, "justification_summary_human_prompt.yaml")
                    with open(human_prompt_file, "r", encoding="utf-8") as f:
                        human_prompt_data = yaml.safe_load(f)
                        self.JUSTIFICATION_SUMMARY_HUMAN_PROMPT = human_prompt_data.get("template", "")
            
            try:
                tracker.config = ProductionConfig()
                logger.info("Created config with real production prompts for evaluation")
            except Exception as e:
                logger.warning(f"Failed to create production config: {e}")
                tracker.config = None


        llm = await builder.get_llm(config.llm_name, wrapper_type=LLMFrameworkEnum.LANGCHAIN)
        
        if tracker.config is None:
            logger.warning("Config is None, skipping summarization - will return original justifications")
            # Just return the tracker as-is with original justifications
            return tracker
            
        vector_service = VectorStoreService()
        issue_analysis_service = IssueAnalysisService(tracker.config, vector_service)
        
        # Initialize evaluator if evaluation is enabled
        evaluator = None
        if config.enable_evaluation and DEEPEVAL_AVAILABLE:
            llm_wrapper = LangChainLLMWrapper(llm)
            evaluator = SummaryEvaluator(
                llm_wrapper=llm_wrapper,
                faithfulness_threshold=config.faithfulness_threshold,
                relevancy_threshold=config.relevancy_threshold
            )
            logger.info("DeepEval evaluation enabled")
        elif config.enable_evaluation and not DEEPEVAL_AVAILABLE:
            logger.warning("DeepEval evaluation requested but DeepEval is not available")
        
        for issue_id, per_issue_data in tracker.issues.items():
            if (per_issue_data.analysis_response and 
                not per_issue_data.analysis_response.short_justifications):
                
                logger.debug(f"Summarizing justifications for issue {issue_id}")
                try:
                    response_to_summarize = JudgeLLMResponse(
                        investigation_result=per_issue_data.analysis_response.investigation_result,
                        justifications=per_issue_data.analysis_response.justifications
                    )
                    summary_response = issue_analysis_service.summarize_justification(
                        actual_prompt=per_issue_data.analysis_response.prompt,
                        response=response_to_summarize,
                        issue_id=issue_id,
                        main_llm=llm
                    )
                    per_issue_data.analysis_response.short_justifications = summary_response.short_justifications
                    logger.info(f"Successfully summarized justifications for issue {issue_id}: {summary_response.short_justifications[:100]}...")
                    
                    # Evaluate summary if evaluation is enabled
                    if evaluator:
                        try:
                            await evaluator.evaluate_summary(
                                issue_id=issue_id,
                                original_justification=" ".join(response_to_summarize.justifications),
                                generated_summary=summary_response.short_justifications
                            )
                            logger.debug(f"Evaluated summary for issue {issue_id}")
                        except Exception as eval_e:
                            logger.error(f"Failed to evaluate summary for issue {issue_id}: {eval_e}")
                            
                except Exception as e:
                    logger.error(f"Failed to summarize justifications for issue {issue_id}: {e}")
        
        # Save evaluation report if evaluation was enabled
        if evaluator and config.evaluation_output_path:
            try:
                evaluator.save_report(config.evaluation_output_path)
                logger.info(f"Evaluation report saved to {config.evaluation_output_path}")
            except Exception as e:
                logger.error(f"Failed to save evaluation report: {e}")
        
        logger.info("Summarize_Justifications node completed")
        return tracker

    def convert_str_to_sast_tracker(input_str: str) -> SASTWorkflowTracker:
        """Convert string input to SASTWorkflowTracker for real evaluation"""
        logger.info(f"Converting evaluation input to SASTWorkflowTracker: {input_str}")
        
        from dto.Issue import Issue
        from dto.LLMResponse import AnalysisResponse
        from dto.SASTWorkflowModels import PerIssueData
        from common.config import Config
        
        # Load the real golden dataset to get the full justification
        import json
        try:
            # Try multiple possible paths for the dataset
            dataset_paths = [
                'evaluation/nvidia_eval/summarize_eval_dataset.json',  # From root
                'summarize_eval_dataset.json',  # From evaluation/nvidia_eval/
                '../../evaluation/nvidia_eval/summarize_eval_dataset.json'  # From other locations
            ]
            
            dataset = None
            for path in dataset_paths:
                try:
                    with open(path, 'r') as f:
                        dataset = json.load(f)
                        logger.info(f"Successfully loaded dataset from: {path}")
                        break
                except FileNotFoundError:
                    continue
            
            if dataset is None:
                raise FileNotFoundError("Could not find dataset file in any of the expected locations")
            
            # Find the matching entry by id (input_str now contains the ID)
            entry = None
            for item in dataset:
                if item.get('id') == input_str or item.get('question') == input_str:
                    entry = item
                    break
            
            if not entry:
                logger.warning(f"No matching entry found for input: {input_str}")
                return SASTWorkflowTracker(issues={})
            
            # Create real issue from golden dataset
            issue = Issue(
                id=entry.get('id', 'eval_issue'),
                issue_type=entry.get('issue_type', 'SECURITY_ISSUE'),
                severity=entry.get('severity', 'HIGH'),
                file_path=entry.get('source_file', 'unknown.c'),
                line_number=1,
                message=f"SAST analysis for {entry.get('id')}"
            )
            
            # Create real analysis response with actual justifications from dataset
            full_justification = entry.get('full_justification', '')
            justifications = [full_justification] if full_justification else []
            
            analysis_response = AnalysisResponse(
                investigation_result=entry.get('investigation_result', 'TRUE POSITIVE'),
                is_final="TRUE",
                justifications=justifications,
                prompt=input_str,
                short_justifications=""
            )
            
            per_issue_data = PerIssueData(
                issue=issue,
                analysis_response=analysis_response
            )
            
            # Create a minimal config object for evaluation (skip validation)
            try:
                tracker_config = Config()
            except Exception:
                # If config validation fails, create a minimal config for evaluation
                tracker_config = None
            
            tracker = SASTWorkflowTracker(
                issues={entry.get('id', 'eval_issue'): per_issue_data},
                config=tracker_config
            )
            
            logger.info(f"Created tracker with real data for issue: {entry.get('id')}")
            return tracker
            
        except Exception as e:
            logger.error(f"Failed to load evaluation data: {e}")
            return SASTWorkflowTracker(issues={})
    
    def convert_sast_tracker_to_str(tracker: SASTWorkflowTracker) -> str:
        """Convert SASTWorkflowTracker to string output for evaluation"""
        logger.info("Converting SASTWorkflowTracker to string output")
        
        if not tracker.issues:
            return "No issues processed"
            
        results = []
        for issue_id, per_issue_data in tracker.issues.items():
            logger.info(f"Processing issue {issue_id} for output conversion")
            if per_issue_data.analysis_response:
                logger.info(f"Issue {issue_id} has analysis_response")
                logger.info(f"Issue {issue_id} short_justifications: {bool(per_issue_data.analysis_response.short_justifications)}")
                logger.info(f"Issue {issue_id} justifications: {bool(per_issue_data.analysis_response.justifications)}")
                if per_issue_data.analysis_response.short_justifications:
                    logger.info(f"Using short_justifications for issue {issue_id}")
                    results.append(per_issue_data.analysis_response.short_justifications)
                elif per_issue_data.analysis_response.justifications:
                    logger.info(f"Using original justifications for issue {issue_id}")
                    results.append(" ".join(per_issue_data.analysis_response.justifications))
            else:
                logger.info(f"Issue {issue_id} has no analysis_response")
        
        return results[0] if results else "No summary generated"

    try:
        yield FunctionInfo.create(
            single_fn=_summarize_justifications_fn,
            description=config.description,
            input_schema=SASTWorkflowTracker,
            converters=[
                convert_str_to_sast_tracker,
                convert_sast_tracker_to_str
            ]
        )
    except GeneratorExit:
        logger.info("Summarize_Justifications function exited early!")
    finally:
        logger.info("Cleaning up Summarize_Justifications function.")
