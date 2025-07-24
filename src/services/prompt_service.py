import os
from langchain_core.prompts import ChatPromptTemplate, SystemMessagePromptTemplate, HumanMessagePromptTemplate
from langchain_core.runnables import RunnablePassthrough, RunnableLambda

from Utils.file_utils import read_answer_template_file
from common.config import Config


class PromptService:
    """Service for managing prompt templates and formatting."""
    
    def __init__(self, config: Config):
        # Store prompt templates from config
        self.analysis_system_prompt = config.ANALYSIS_SYSTEM_PROMPT
        self.analysis_human_prompt = config.ANALYSIS_HUMAN_PROMPT
        self.filter_system_prompt = config.FILTER_SYSTEM_PROMPT
        self.filter_human_prompt = config.FILTER_HUMAN_PROMPT
        self.recommendations_prompt = config.RECOMMENDATIONS_PROMPT
        self.justification_summary_system_prompt = config.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT
        self.justification_summary_human_prompt = config.JUSTIFICATION_SUMMARY_HUMAN_PROMPT
        self.evaluation_prompt = config.EVALUATION_PROMPT
    
    def create_filter_prompt_chain(self, examples_context_str: str, answer_template: str):
        """Create filter prompt chain for known issue filtering."""
        prompt = ChatPromptTemplate.from_messages([
            ("system", self.filter_system_prompt),
            ("user", self.filter_human_prompt)
        ])
        
        return (
            {
                "context": RunnableLambda(lambda _: examples_context_str),
                "answer_template": RunnableLambda(lambda _: answer_template),
                "user_error_trace": RunnablePassthrough()
            }
            | prompt
        )
    
    def create_analysis_prompt_chain(self, context: str, issue_trace: str):
        """Create analysis prompt chain for issue investigation."""
        prompt = ChatPromptTemplate.from_messages([
            SystemMessagePromptTemplate.from_template(self.analysis_system_prompt),
            HumanMessagePromptTemplate.from_template(self.analysis_human_prompt)
        ])
        
        return (
            {
                "context": RunnableLambda(lambda _: context),
                "cve_error_trace": RunnableLambda(lambda _: issue_trace),
                "question": RunnablePassthrough()
            }
            | prompt
        )
    
    def create_recommendations_prompt_chain(self, issue_trace: str, analysis: str, context: str):
        """Create recommendations prompt chain."""
        prompt = ChatPromptTemplate.from_messages([
            HumanMessagePromptTemplate.from_template(self.recommendations_prompt)
        ])
        
        return (
            {
                "cve_error_trace": RunnableLambda(lambda _: issue_trace),
                "analysis": RunnableLambda(lambda _: analysis),
                "context": RunnableLambda(lambda _: context),
            }
            | prompt
        )
    
    def create_justification_summary_prompt_chain(self, actual_prompt: str, examples_str: str):
        """Create justification summary prompt chain."""
        prompt = ChatPromptTemplate.from_messages([
            ("system", self.justification_summary_system_prompt),
            ("user", self.justification_summary_human_prompt)
        ])
        
        return (
            {
                "actual_prompt": RunnableLambda(lambda _: actual_prompt),
                "examples_str": RunnableLambda(lambda _: examples_str),
                "response": RunnablePassthrough()
            }
            | prompt
        )
    
    def create_evaluation_prompt_chain(self, actual_prompt: str):
        """Create evaluation prompt chain for critique."""
        prompt = ChatPromptTemplate.from_messages([
            ("user", self.evaluation_prompt)
        ])
        
        return (
            {
                "actual_prompt": RunnableLambda(lambda _: actual_prompt),
                "response": RunnablePassthrough()
            }
            | prompt
        )
    
    def get_filter_answer_template(self) -> str:
        """Get the answer template for filter responses."""
        template_path = os.path.join(os.path.dirname(__file__), "..", "templates", "known_issue_filter_resp.json")
        return read_answer_template_file(template_path)
    
    def get_justification_examples(self) -> str:
        """Get examples for justification summary."""
        return ('[{"short_justifications": "t is reassigned so previously freed value is replaced by malloced string"}, '
                '{"short_justifications": "There is a check for k<0"}, '
                '{"short_justifications": "i is between 1 and BMAX, line 1623 checks that j < i, array C is of the size BMAX+1"}, '
                '{"short_justifications": "C is an array of size BMAX+1, i is between 1 and BMAX (inclusive)"}]') 