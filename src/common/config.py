import logging
import os

import yaml
from dotenv import load_dotenv

from common.constants import (
    CONFIG_H_PATH,
    CRITIQUE_LLM_API_KEY,
    CRITIQUE_LLM_MODEL_NAME,
    CRITIQUE_LLM_URL,
    EMBEDDINGS_LLM_API_KEY,
    EMBEDDINGS_LLM_MODEL_NAME,
    EMBEDDINGS_LLM_URL,
    HUMAN_VERIFIED_FILE_PATH,
    INPUT_REPORT_FILE_PATH,
    KNOWN_FALSE_POSITIVE_FILE_PATH,
    LLM_API_KEY,
    LLM_MODEL_NAME,
    LLM_URL,
    OUTPUT_FILE_PATH,
    PROJECT_NAME,
    PROJECT_VERSION,
    REPO_LOCAL_PATH,
    REPO_REMOTE_URL,
    RUN_WITH_CRITIQUE,
    SERVICE_ACCOUNT_JSON_PATH,
)

logger = logging.getLogger(__name__)


class Config:
    # Type hints for dynamically loaded attributes
    REPO_REMOTE_URL: str
    DOWNLOAD_REPO: bool
    REPO_LOCAL_PATH: str
    CONFIG_H_PATH: str
    COMPILE_COMMANDS_JSON_PATH: str
    LIBCLANG_PATH: str
    PROJECT_NAME: str
    PROJECT_VERSION: str
    LLM_URL: str
    LLM_MODEL_NAME: str
    EMBEDDINGS_LLM_URL: str
    EMBEDDINGS_LLM_MODEL_NAME: str
    INPUT_REPORT_FILE_PATH: str
    KNOWN_FALSE_POSITIVE_FILE_PATH: str
    OUTPUT_FILE_PATH: str
    AGGREGATE_RESULTS_G_SHEET: str
    HUMAN_VERIFIED_FILE_PATH: str
    USE_KNOWN_FALSE_POSITIVE_FILE: bool
    CALCULATE_METRICS: bool
    RUN_WITH_CRITIQUE: bool
    CRITIQUE_LLM_URL: str
    CRITIQUE_LLM_MODEL_NAME: str
    SERVICE_ACCOUNT_JSON_PATH: str
    
    # Prompt template type hints
    ANALYSIS_SYSTEM_PROMPT: str
    ANALYSIS_HUMAN_PROMPT: str
    FILTER_SYSTEM_PROMPT: str
    FILTER_HUMAN_PROMPT: str
    RECOMMENDATIONS_PROMPT: str
    JUSTIFICATION_SUMMARY_SYSTEM_PROMPT: str
    JUSTIFICATION_SUMMARY_HUMAN_PROMPT: str
    EVALUATION_PROMPT: str
    
    def __init__(self):
        self.load_config()
        self.print_config()
        self.validate_configurations()

    def load_config(self):
        load_dotenv()  # Take environment variables from .env
        config_path = os.path.join(
            os.path.dirname(__file__), "../..", "config", "default_config.yaml"
        )
        with open(config_path, "r") as f:
            config = yaml.safe_load(f)

        # Override default configuration with any environment variables if they exist.
        for key in config.keys():
            env_value = os.getenv(key)
            if env_value is not None:
                config[key] = env_value

        # Load Main LLM details in case critique details not provided
        if config.get(RUN_WITH_CRITIQUE):
            if not config.get(CRITIQUE_LLM_URL) or not os.getenv(CRITIQUE_LLM_API_KEY):
                logger.info("Critique model details not provided - using main LLM details instead")
                config[CRITIQUE_LLM_URL] = config.get(LLM_URL)
                self.CRITIQUE_LLM_API_KEY = os.getenv(LLM_API_KEY)

        self.__dict__.update(config)

        self.TOKENIZERS_PARALLELISM = False
        self.LLM_API_KEY = os.getenv(LLM_API_KEY)
        self.EMBEDDINGS_LLM_API_KEY = os.getenv(EMBEDDINGS_LLM_API_KEY)
        self.LLM_MODEL_NAME = os.getenv(LLM_MODEL_NAME)
        self.EMBEDDINGS_LLM_MODEL_NAME = os.getenv(EMBEDDINGS_LLM_MODEL_NAME)
        
        # Load prompt templates from environment variables with fallbacks
        self.ANALYSIS_SYSTEM_PROMPT = self._load_prompt_template('ANALYSIS_SYSTEM_PROMPT', self._get_default_analysis_system_prompt())
        self.ANALYSIS_HUMAN_PROMPT = self._load_prompt_template('ANALYSIS_HUMAN_PROMPT', "{question}")
        self.FILTER_SYSTEM_PROMPT = self._load_prompt_template('FILTER_SYSTEM_PROMPT', self._get_default_filter_system_prompt())
        self.FILTER_HUMAN_PROMPT = self._load_prompt_template('FILTER_HUMAN_PROMPT', self._get_default_filter_human_prompt())
        self.RECOMMENDATIONS_PROMPT = self._load_prompt_template('RECOMMENDATIONS_PROMPT', self._get_default_recommendations_prompt())
        self.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT = self._load_prompt_template('JUSTIFICATION_SUMMARY_SYSTEM_PROMPT', self._get_default_justification_summary_system_prompt())
        self.JUSTIFICATION_SUMMARY_HUMAN_PROMPT = self._load_prompt_template('JUSTIFICATION_SUMMARY_HUMAN_PROMPT', self._get_default_justification_summary_human_prompt())
        self.EVALUATION_PROMPT = self._load_prompt_template('EVALUATION_PROMPT', self._get_default_evaluation_prompt())
        
        self._convert_str_to_bool()

    def _load_prompt_template(self, env_var_name: str, default_value: str) -> str:
        """Load prompt template from environment variable or return default."""
        template = os.getenv(env_var_name)
        if template:
            logger.info(f"Loaded prompt template from environment variable: {env_var_name}")
            return template
        else:
            logger.info(f"Using default prompt template for: {env_var_name}")
            return default_value

    def _get_default_analysis_system_prompt(self) -> str:
        return ("You are an expert security analyst tasked with determining if a reported CVE (Common Vulnerabilities and Exposures) is a FALSE POSITIVE or a TRUE POSITIVE.\n"
                "You will be provided with a CVE report snippet, the source code of the function(s) mentioned in the CVE's error trace and examples of verified CVEs with the same CWE as the reported CVE.\n"
                "Your task is to analyze step-by-step the code of the reported CVE issue to identify if it is FALSE POSITIVE or TRUE POSITIVE.\n"
                "A finding of **TRUE POSITIVE** should be made if **any** execution path within the provided source code potentially leads to the vulnerability described in the CVE.\n\n"
                "**Crucially, you must base your analysis solely on the explicit behavior of the provided source code and the description in the CVE report.\n"
                "Do not make any assumptions about the code's behavior based on function names, variable names, or any implied functionality.**\n"
                "Respond only in the following JSON format:\n"
                "{{\"investigation_result\", type: string: (FALSE POSITIVE/TRUE POSITIVE), "
                "\"justifications\", type: [string]: (The reasoning that led to the investigation_result decision)}} "
                "**Here is the information for your analysis:**\n"
                "**CVE Report Snippet:**\n{cve_error_trace}\n\n"
                "{context}\n\n"
                "**Your analysis must adhere to the following strict guidelines:**\n"
                "* Provide evidence or context strictly based on the provided information.* You must explicitly reference lines of code. Do not provide justifications based on what you *infer* the code might do or how it is *typically* used.\n"
                "* If there are any uncertainties or lack of explicit proof within the provided code that *all* execution paths are safe with respect to the CVE description, you **must not** conclude FALSE POSITIVE. Clearly state the uncertainty\n"
                "* **No Implicit Behavior:** Analyze the code exactly as written. Do not assume what a function *might* do based on its name or common programming patterns. Focus only on the explicit operations performed within the provided code.\n"
                "* **No Clear False Positive Evidence Implies True Positive:** A conclusion of FALSE POSITIVE requires definitive proof within the provided CVE report and source code that the described vulnerability cannot occur under any circumstances within the analyzed code. Lack of such definitive proof should lean towards TRUE POSITIVE\n"
                "* **Single Vulnerable Path is Sufficient:** If you identify even one specific sequence of execution within the provided code that potentially triggers the vulnerability described in the CVE, the result should be **TRUE POSITIVE**\n"
                "* **Direct Correlation:** Ensure a direct and demonstrable link between the code's behavior and the vulnerability described in the CVE.\n"
                "* **Focus on Provided Information:** Your analysis and justifications must be solely based on the text of the CVE report snippet and the provided source code. Do not make assumptions about the broader system or environment.\n"
                "* If you identify syntax issue in the reported finding - mark it as TRUE POSITIVE.\n"
                "* Check that all of the justifications are based on code that its implementation is provided in the context.\n"
                "**Begin your analysis.**\n")

    def _get_default_filter_system_prompt(self) -> str:
        return ("You are an expert in identifying similar error stack traces.\n"
                "You are provided with:\n"
                "1. A list of known false positive issues (context_false_positives):\n"
                "Each issue in the list includes two key elements:\n"
                "false_positive_error_trace - the issue error trace.\n"
                "reason_marked_false_positive - A reason for its classification as a false positive.\n"
                "2. A new user error trace (user_error_trace).\n\n"
                "Your task is to determine whether the user error trace exactly matches any of the false positives.\n"
                "When comparing issues, you may ignore differences in line numbers and package version details. "
                "However, the error trace in the query must exactly match the error trace in the context, "
                "including the same method names and the same order of method calls. "
                "Answer the question using only the provided context.\n"
                "Your response must strictly follow the provided answer response template. "
                "Do not include any additional text outside the answer template.\n"
                "Answer response template:\n{answer_template}\n"
                "context_false_positives: {context}")

    def _get_default_filter_human_prompt(self) -> str:
        return ("Does the error trace of user_error_trace match any of the context_false_positives errors?\n"
                "user_error_trace: {user_error_trace}")

    def _get_default_recommendations_prompt(self) -> str:
        return ("You are an expert security analyst tasked with rigorously evaluating a provided analysis of a reported CVE (Common Vulnerabilities and Exposures) to determine if it's a FALSE POSITIVE or a TRUE POSITIVE.\n"
                "You will be given the reported CVE, an analysis of the CVE, and the data the analysis is based on (source code snippets, error traces, etc.), along with examples of validated CVEs for context.\n"
                "Your primary goal is to critically assess the provided analysis for completeness, accuracy, and relevance to the reported CVE. Determine if the analysis provides sufficient evidence for a conclusive TRUE or FALSE POSITIVE determination.\n"
                "If the initial analysis is insufficient, identify the specific gaps and recommend the necessary data or steps required for a thorough evaluation.\n"
                "Only provide recommendations that are directly crucial for validating the reported CVE and reaching a definitive conclusion.\n"
                "If the analysis fails to cover all relevant execution paths or potential conditions, explain the shortcomings and specify the additional data needed for a complete assessment.\n"
                "Any recommendation that necessitates inspecting the implementation of a referenced function or macro MUST be formatted as an entry in the 'instructions' list.\n"
                "Your output MUST be a valid JSON object and follow the exact structure defined below:\n"
                "{{\"is_final\", type: string: Indicate whether further investigation is needed. If clear and irrefutable evidence for a TRUE or FALSE POSITIVE is found within the evaluated analysis, set this value to the string 'TRUE'; otherwise, set it to the string 'FALSE'."
                "\"justifications\", type: [string]: Provide a detailed explanation of why the evaluated analysis is sound and complete, or clearly articulate its deficiencies and why it's insufficient for a final determination."
                "\"recommendations\"(optional), type: [string]: If further analysis is required, provide a concise list of the specific data or steps needed to reach a conclusive TRUE or FALSE POSITIVE determination. Only include essential recommendations."
                "\"instructions\" (optional):\n"
                "\t[{{\"expression_name\", type: string: The exact name of the missing function or macro (not the full declaration)."
                "\t\"referring_source_code_path\", type: string: The precise file path where the \"expression_name\" is called from (include ONLY the file path without any surrounding text)."
                "\t\"recommendation\", type: string: A clear and actionable recommendation related to this \"expression_name\" (e.g., \"Verify the implementation of `memcpy` to ensure no out-of-bounds write occurs.\").}}]\n" 
                "}}\n" 
                "Notes:\n"
                "- The entire output must be syntactically correct JSON.\n"
                "- All keys must be present. If a field is not applicable (e.g., recommendations or instructions), it must still be included with an empty list.\n"
                "- \"instructions\" is a list of dictionaries, where each dictionary represents a recommendation to examine the implementation of a function or macro referenced in the source code context. Include this list ONLY if such investigations are necessary.\n"
                "**The reported CVE:**\n{cve_error_trace}\n\n"
                "**The Analysis:**\n{analysis}\n\n"
                "**The Data used for the analysis:**\n{context}")

    def _get_default_justification_summary_system_prompt(self) -> str:
        return ("You are an experienced software engineer tasked with summarizing justifications for an investigation result. "
                "You are provided with the response of another model's analysis, which includes an investigation_result and justifications. " 
                "Your goal is to create a concise summary of the justifications provided in the response. "
                "Use the Query and the Response to ensure your summary is accurate and professional. "
                "Focus on the key technical reasons or evidence that support the investigation result. "
                "Write the summary in a clear, concise, and professional style, as if it were a comment in a code review or technical report. "
                "Limit the summary to a single sentence or two at most."
                "\n\nHere are examples of short justifications written by engineers:"
                "{examples_str}"
                "\n\nRespond only in the following JSON format:"
                "{{\"short_justifications\": string}} "   
                "short_justifications should be a clear, concise summary of the justification written in an engineer-style tone, highlighting the most impactful point.")

    def _get_default_justification_summary_human_prompt(self) -> str:
        return ("Summarize the justifications provided in the following response into a concise, professional comment:"
                "\n\nQuery: {actual_prompt}"
                "\n\nResponse: {response}")

    def _get_default_evaluation_prompt(self) -> str:
        return ("You are an experienced C developer tasked with analyzing code to identify potential flaws. "
                "You understand programming language control structures. Therefore, you are capable of verifying the "
                "call-hierarchy of a given source code. You can observe the runtime workflows. "
                "You understand the question has line numbers of the source code. "
                "Your goal is to critique the response of another model's analysis. "
                "First step is to see if the model justified its results by stating that Red Hat engineers have manually verified it as a false positive error. "
                "If so, check if the context really has the same error stack trace (you can ignore line numbers and code versions differences). If it does, it's a false positive. If not, this justification is incorrect. "
                "Your responses should be precise and no longer than two sentences. Provide justifications for your answers. "
                "Start you answer with '<think>\\n' and at the end add the json results"
                "Based on the context, the query, and the 'justifications' (from the response), your main goal is to check if the 'investigation_result' (from the response) is right. "
                "\nAssess it with the following parameters (give each one score 0,1,2 - 2 is the higher):"
                "\n1. Does the 'justifications' make sense given the data you have?"
                "\n2. Does the 'recommendations' make sense given the data you have?"
                "\n3. Factual accuracy (Does it match the context?)."
                "\n4. Completeness (Does it address all aspects of the query?)."
                "\nEventually decide whether the 'investigation_result' was right (is it really false positive or not false positive). "
                "Give it a overall confidence score 0,1,2 (2 is the higher)."
                "\nProvide detailed justifications for your answers and ensure your responses are clear and concise. "
                "Structure your output into JSON format with sections: 'critique_result' (which contain 'false positive' or 'not a false positive'), 'justifications'."
                "\nPerform an independent verification to determine the 'critique_result'. "
                "If the 'justifications' score is low, you can still use the same result as the 'investigation_result' for the 'critique_result', but only if you find another valid justification."
                "\n\nQuery and Context:{actual_prompt}"
                "\n\nResponse:{response}")

    def _convert_str_to_bool(self):
        for key, value in self.__dict__.items():
            if isinstance(value, str) and value.lower() in ("true", "false"):
                self.__dict__[key] = value.lower() == "true"

    def print_config(self):
        masked_vars = [LLM_API_KEY, EMBEDDINGS_LLM_API_KEY]
        logger.info(" Process started! ".center(80, "-"))
        logger.info("".center(80, "-"))
        for key, value in self.__dict__.items():
            if key in masked_vars:
                value = "******"
            # Don't print full prompt templates in logs (they're too long)
            elif key.endswith('_PROMPT'):
                value = f"<prompt template loaded: {len(str(value))} chars>"
            logger.info(f"{key}={value}")
        logger.info("".center(80, "-"))

    def validate_configurations(self):
        required_cfg_vars = {
            PROJECT_NAME,
            PROJECT_VERSION,
            LLM_URL,
            LLM_MODEL_NAME,
            EMBEDDINGS_LLM_URL,
            EMBEDDINGS_LLM_MODEL_NAME,
            INPUT_REPORT_FILE_PATH,
            OUTPUT_FILE_PATH,
        }
        required_cfg_files = {INPUT_REPORT_FILE_PATH}
            
        # Check if DOWNLOAD_REPO is True then validate a REPO URL was provided
        if self.DOWNLOAD_REPO is True:
            required_cfg_vars.add(REPO_REMOTE_URL)

        # make sure REPO_LOCAL_PATH exists, in the case DOWNLOAD_REPO is set to False
        else:
            required_cfg_files.add(REPO_LOCAL_PATH)

        # Validate that required configuration variables are set
        for var in required_cfg_vars:
            value = self.__dict__[var]
            if not value:
                raise ValueError(f"Configuration variable '{var}' is not set or is empty.")

        # Check if CONFIG_H_PATH is accessible if it was provided
        if self.CONFIG_H_PATH:
            required_cfg_files.add(CONFIG_H_PATH)

        # Check if HUMAN_VERIFIED_FILE_PATH is accessible if it was provided
        if self.HUMAN_VERIFIED_FILE_PATH:
            required_cfg_files.add(HUMAN_VERIFIED_FILE_PATH)

        # Ensure service account JSON exists if using Google Sheets as input
        if self.INPUT_REPORT_FILE_PATH.startswith("https"):
            required_cfg_files.add(SERVICE_ACCOUNT_JSON_PATH)
            required_cfg_files.remove(INPUT_REPORT_FILE_PATH)

        # Ensure service account JSON exists if write aggregate resutls to Google Sheet
        if self.AGGREGATE_RESULTS_G_SHEET:
            required_cfg_files.add(SERVICE_ACCOUNT_JSON_PATH)

        if self.USE_KNOWN_FALSE_POSITIVE_FILE:
            required_cfg_files.add(KNOWN_FALSE_POSITIVE_FILE_PATH)

        # Validate that input files exist and are accessible
        for var in required_cfg_files:
            value = self.__dict__[var]
            if not os.path.exists(value):
                raise FileNotFoundError(f"Configuration variable '{var}' not found.")

        # Validate that environment variable LLM API key exist
        if not self.LLM_API_KEY:
            raise ValueError(f"Environment variable {LLM_API_KEY} is not set or is empty.")

        # Validate that environment variable Embedding API key exist
        if not self.EMBEDDINGS_LLM_API_KEY:
            raise ValueError(
                f"Environment variable {EMBEDDINGS_LLM_API_KEY} is not set or is empty."
            )

        # Validate critique config if RUN_WITH_CRITIQUE is True
        if self.RUN_WITH_CRITIQUE and not self.CRITIQUE_LLM_MODEL_NAME:
            raise ValueError(
                f"'{CRITIQUE_LLM_MODEL_NAME}' must be set when '{RUN_WITH_CRITIQUE}' is True."
            )

        # Validate that prompt templates are loaded
        prompt_vars = ['ANALYSIS_SYSTEM_PROMPT', 'ANALYSIS_HUMAN_PROMPT', 'FILTER_SYSTEM_PROMPT', 
                      'FILTER_HUMAN_PROMPT', 'RECOMMENDATIONS_PROMPT', 'JUSTIFICATION_SUMMARY_SYSTEM_PROMPT',
                      'JUSTIFICATION_SUMMARY_HUMAN_PROMPT', 'EVALUATION_PROMPT']
        for var in prompt_vars:
            if not getattr(self, var, None):
                raise ValueError(f"Prompt template '{var}' is not loaded or is empty.")

        logger.info("All required configuration variables and files are valid and accessible.\n")