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
    LLM_API_TYPE,
    LLM_MODEL_NAME,
    LLM_URL,
    MAX_ANALYSIS_ITERATIONS,
    OUTPUT_FILE_PATH,
    PROJECT_NAME,
    PROJECT_VERSION,
    REPO_LOCAL_PATH,
    REPO_REMOTE_URL,
    RUN_WITH_CRITIQUE,
    SERVICE_ACCOUNT_JSON_PATH,
    SIMILARITY_ERROR_THRESHOLD,
    VALIDATION_LIMITS,
)
from Utils.validation_utils import is_valid_int_value

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
    LLM_API_TYPE: str
    EMBEDDINGS_LLM_URL: str
    EMBEDDINGS_LLM_MODEL_NAME: str | None
    EMBEDDINGS_LLM_API_KEY: str | None
    INPUT_REPORT_FILE_PATH: str
    KNOWN_FALSE_POSITIVE_FILE_PATH: str
    OUTPUT_FILE_PATH: str
    AGGREGATE_RESULTS_G_SHEET: str
    HUMAN_VERIFIED_FILE_PATH: str
    USE_KNOWN_FALSE_POSITIVE_FILE: bool
    CALCULATE_RAGAS_METRICS: bool
    RUN_WITH_CRITIQUE: bool
    WRITE_RESULTS: bool
    WRITE_RESULTS_INCLUDE_NON_FINAL: bool
    SHOW_FINAL_JUDGE_CONTEXT: bool
    CRITIQUE_LLM_URL: str
    CRITIQUE_LLM_MODEL_NAME: str
    CRITIQUE_LLM_API_KEY: str | None
    SERVICE_ACCOUNT_JSON_PATH: str
    SIMILARITY_ERROR_THRESHOLD: int
    MAX_ANALYSIS_ITERATIONS: int

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

    def _load_prompt_from_file(self, prompt_name: str) -> str:
        """Load prompt template from YAML file."""
        try:
            # Construct path to prompt file
            prompts_dir = os.path.join(os.path.dirname(__file__), "../templates/prompts")
            prompt_file = os.path.join(prompts_dir, f"{prompt_name}.yaml")

            # Check if file exists
            if not os.path.exists(prompt_file):
                logger.warning(f"Prompt file not found: {prompt_file}")
                return ""

            # Load and return template
            with open(prompt_file, "r", encoding="utf-8") as f:
                prompt_data = yaml.safe_load(f)
                template = prompt_data.get("template", "")
                logger.info(f"Loaded prompt template from file: {prompt_name}.yaml")
                return template

        except Exception as e:
            logger.error(f"Error loading prompt file {prompt_name}.yaml: {e}")
            return ""

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

        # Load prompt templates from files with environment variable overrides
        self.ANALYSIS_SYSTEM_PROMPT = self._load_prompt_template(
            "ANALYSIS_SYSTEM_PROMPT", "analysis_system_prompt"
        )
        self.ANALYSIS_HUMAN_PROMPT = self._load_prompt_template(
            "ANALYSIS_HUMAN_PROMPT", "analysis_human_prompt"
        )
        self.FILTER_SYSTEM_PROMPT = self._load_prompt_template(
            "FILTER_SYSTEM_PROMPT", "filter_system_prompt"
        )
        self.FILTER_HUMAN_PROMPT = self._load_prompt_template(
            "FILTER_HUMAN_PROMPT", "filter_human_prompt"
        )
        self.RECOMMENDATIONS_PROMPT = self._load_prompt_template(
            "RECOMMENDATIONS_PROMPT", "recommendations_prompt"
        )
        self.JUSTIFICATION_SUMMARY_SYSTEM_PROMPT = self._load_prompt_template(
            "JUSTIFICATION_SUMMARY_SYSTEM_PROMPT", "justification_summary_system_prompt"
        )
        self.JUSTIFICATION_SUMMARY_HUMAN_PROMPT = self._load_prompt_template(
            "JUSTIFICATION_SUMMARY_HUMAN_PROMPT", "justification_summary_human_prompt"
        )
        self.EVALUATION_PROMPT = self._load_prompt_template(
            "EVALUATION_PROMPT", "evaluation_prompt"
        )

        self._convert_str_to_bool()

    def _load_prompt_template(self, env_var_name: str, prompt_file_name: str) -> str:
        """Load prompt template from environment variable or from YAML file."""
        # First try to load from environment variable
        template = os.getenv(env_var_name)
        if template:
            logger.info(f"Loaded prompt template from environment variable: {env_var_name}")
            return template

        # If not found in environment, load from file
        template = self._load_prompt_from_file(prompt_file_name)
        if template:
            return template

        # If neither environment nor file works, log error and return empty string
        logger.error(
            f"Could not load prompt template for {env_var_name} from environment or "
            f"file {prompt_file_name}.yaml"
        )
        return ""

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
            elif key.endswith("_PROMPT"):
                value = f"<prompt template loaded: {len(str(value))} chars>"
            logger.info(f"{key}={value}")
        logger.info("".center(80, "-"))

    def validate_configurations(self):
        required_cfg_vars = {
            PROJECT_NAME,
            PROJECT_VERSION,
            LLM_URL,
            LLM_MODEL_NAME,
            LLM_API_TYPE,
            EMBEDDINGS_LLM_URL,
            EMBEDDINGS_LLM_MODEL_NAME,
            INPUT_REPORT_FILE_PATH,
            OUTPUT_FILE_PATH,
            SIMILARITY_ERROR_THRESHOLD,
        }
        required_cfg_files = {INPUT_REPORT_FILE_PATH}

        # Check if DOWNLOAD_REPO is True then validate a REPO URL was provided
        if self.DOWNLOAD_REPO is True:
            required_cfg_vars.add(REPO_REMOTE_URL)

        # make sure REPO_LOCAL_PATH exists, in the case DOWNLOAD_REPO is set to False
        else:
            required_cfg_files.add(REPO_LOCAL_PATH)

        if not self.WRITE_RESULTS:
            required_cfg_vars.remove(OUTPUT_FILE_PATH)
            logger.warning("WRITE_RESULTS is disabled in config - skipping results writing!")

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

        # Validate that LLM_API_TYPE is valid
        if self.LLM_API_TYPE not in ["openai", "nim"]:
            raise ValueError(
                f"Invalid LLM_API_TYPE: {self.LLM_API_TYPE}, must be 'openai' or 'nim'."
            )

        # Validate that similarity error threshold is a valid value
        if not is_valid_int_value(self.SIMILARITY_ERROR_THRESHOLD, 
                                  VALIDATION_LIMITS["MIN_SIMILARITY_THRESHOLD"], 
                                  VALIDATION_LIMITS["MAX_SIMILARITY_THRESHOLD"]):
            raise ValueError(
                f"Configuration variable '{SIMILARITY_ERROR_THRESHOLD}' is not a valid value."
            )

        # Validate that MAX_ANALYSIS_ITERATIONS is a positive integer
        if not is_valid_int_value(self.MAX_ANALYSIS_ITERATIONS, 
                                  VALIDATION_LIMITS["MIN_ANALYSIS_ITERATIONS"]):
            raise ValueError(
                f"Configuration variable '{MAX_ANALYSIS_ITERATIONS}' is not a valid value."
            )

        # Validate that prompt templates are loaded
        prompt_vars = [
            "ANALYSIS_SYSTEM_PROMPT",
            "ANALYSIS_HUMAN_PROMPT",
            "FILTER_SYSTEM_PROMPT",
            "FILTER_HUMAN_PROMPT",
            "RECOMMENDATIONS_PROMPT",
            "JUSTIFICATION_SUMMARY_SYSTEM_PROMPT",
            "JUSTIFICATION_SUMMARY_HUMAN_PROMPT",
            "EVALUATION_PROMPT",
        ]
        for var in prompt_vars:
            if not getattr(self, var, None):
                raise ValueError(f"Prompt template '{var}' is not loaded or is empty.")

        logger.info("All required configuration variables and files are valid and accessible.\n")
