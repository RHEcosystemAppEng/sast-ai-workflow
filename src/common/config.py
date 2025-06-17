import os
import yaml

from dotenv import load_dotenv

from common.constants import *

class Config:
    def __init__(self):
        self.load_config()
        self.print_config()
        self.validate_configurations()

    def load_config(self):
        load_dotenv() # Take environment variables from .env
        config_path = os.path.join(os.path.dirname(__file__), "../..", "config", "default_config.yaml")
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
                print("Critique model details not provided - using main LLM details instead")
                config[CRITIQUE_LLM_URL] = config.get(LLM_URL)
                self.CRITIQUE_LLM_API_KEY = os.getenv(LLM_API_KEY)

        self.__dict__.update(config)

        self.TOKENIZERS_PARALLELISM = False
        self.LLM_API_KEY = os.getenv(LLM_API_KEY)
        self.EMBEDDINGS_API_KEY = os.getenv(EMBEDDINGS_API_KEY)
        self._convert_str_to_bool()

    def _convert_str_to_bool(self):
        for key, value in self.__dict__.items():
            if isinstance(value, str) and value.lower() in ("true", "false"):
                self.__dict__[key] = value.lower() == "true"

    def print_config(self):
        masked_vars = [LLM_API_KEY, EMBEDDINGS_API_KEY]
        print(" Process started! ".center(80, '-'))
        print("".center(80, '-'))
        for key, value in self.__dict__.items():
            if key in masked_vars:
                value = "******"
            print(f"{key}={value}")
        print("".center(80, '-'))

    def validate_configurations(self):
        required_cfg_vars = {
            PROJECT_NAME,
            PROJECT_VERSION,
            LLM_URL,
            LLM_MODEL_NAME,
            EMBEDDINGS_LLM_URL,
            EMBEDDINGS_LLM_MODEL_NAME,
            INPUT_REPORT_FILE_PATH,
            KNOWN_FALSE_POSITIVE_FILE_PATH,
            OUTPUT_FILE_PATH,
        }
        required_cfg_files = {
            INPUT_REPORT_FILE_PATH,
            KNOWN_FALSE_POSITIVE_FILE_PATH
        }

        for var in required_cfg_vars:
            value = self.__dict__[var]
            if not value:
                raise ValueError(f"Configuration variable '{var}' is not set or is empty.")
            
        # Check if DOWNLOAD_REPO is True then validate a REPO URL was provided
        if self.DOWNLOAD_REPO == True:
            required_cfg_vars.add(REPO_REMOTE_URL)
        # make sure REPO_LOCAL_PATH exists, in the case DOWNLOAD_REPO is set to False
        else:
            required_cfg_files.add(REPO_LOCAL_PATH)

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

        # Validate that input files exist and are accessible
        for var in required_cfg_files:
            value = self.__dict__[var]
            if not os.path.exists(value):
                raise FileNotFoundError(f"Configuration variable '{var}' not found.")

        # Validate that environment variable LLM API key exist
        if not self.LLM_API_KEY:
            raise ValueError(f"Environment variable {LLM_API_KEY} is not set or is empty.")
        
        # Validate that environment variable Embedding API key exist
        if not self.EMBEDDINGS_API_KEY:
            raise ValueError(f"Environment variable {EMBEDDINGS_API_KEY} is not set or is empty.")

        # Validate critique config if RUN_WITH_CRITIQUE is True
        if self.RUN_WITH_CRITIQUE and not self.CRITIQUE_LLM_MODEL_NAME:
            raise ValueError(f"'{CRITIQUE_LLM_MODEL_NAME}' must be set when '{RUN_WITH_CRITIQUE}' is True.")


        print("All required configuration variables and files are valid and accessible.\n")