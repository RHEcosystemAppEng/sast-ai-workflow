import os
import yaml

from dotenv import load_dotenv
from .constants import *

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

        self.__dict__.update(config)

        self.TOKENIZERS_PARALLELISM = False
        self.LLM_API_KEY = os.getenv(LLM_API_KEY)
        
    def print_config(self):
        print(" Process started! ".center(80, '-'))
        print("".center(80, '-'))
        for key, value in self.__dict__.items():
            print(f"{key}={value}")
        print("".center(80, '-'))

    def validate_configurations(self):
        """Check for required environment/configuration variables"""

        required_cfg_vars = [
            LLM_URL,
            LLM_MODEL_NAME,
            EMBEDDINGS_LLM_MODEL_NAME,
            REPORT_FILE_PATH,
            KNOWN_FALSE_POSITIVE_FILE_PATH,
            OUTPUT_FILE_PATH,
            HUMAN_VERIFIED_FILE_PATH
        ]
        required_cfg_files = [
            REPORT_FILE_PATH,
            KNOWN_FALSE_POSITIVE_FILE_PATH,
            HUMAN_VERIFIED_FILE_PATH,
            OUTPUT_FILE_PATH
        ]

        for var in required_cfg_vars:
            value = self.__dict__[var]
            if not value:
                raise ValueError(f"Configuration variable '{var}' is not set or is empty.")

        # Validate that input files exist and are accessible
        for var in required_cfg_files:
            value = self.__dict__[var]
            if not os.path.exists(value):
                raise FileNotFoundError(f"Configuration variable '{var}' not found.")

        # Validate that environment variable LLM API key exist
        if not self.LLM_API_KEY:
            raise ValueError(f"Environment variable {LLM_API_KEY} is not set or is empty.")
        
        print("All required environment variables and files are valid and accessible.\n")