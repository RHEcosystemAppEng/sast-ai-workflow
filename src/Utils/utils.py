import os
import yaml

def print_config(config):
    print("".center(80, '-'))
    print("LLM_URL=", config["LLM_URL"])
    print("LLM_API_KEY= ********")
    print("LLM_MODEL_NAME=", config["LLM_MODEL_NAME"])
    print("OUTPUT_FILE_PATH=", config["OUTPUT_FILE_PATH"])
    print("GIT_REPO_PATH=", config["GIT_REPO_PATH"])
    print("EMBEDDINGS_LLM_MODEL_NAME=", config["EMBEDDINGS_LLM_MODEL_NAME"])
    print("REPORT_FILE_PATH=", config["REPORT_FILE_PATH"])
    print("KNOWN_FALSE_POSITIVE_FILE_PATH=", config["KNOWN_FALSE_POSITIVE_FILE_PATH"])
    print("HUMAN_VERIFIED_FILE_PATH=", config["HUMAN_VERIFIED_FILE_PATH"])
    print("CALCULATE_METRICS=", config["CALCULATE_METRICS"])
    print("DOWNLOAD_GIT_REPO=", config["DOWNLOAD_GIT_REPO"])
    print("".center(80, '-'))

def load_config():
    config_path = os.path.join(os.path.dirname(__file__), "../..", "config", "default_config.yaml")
    with open(config_path, "r") as f:
        config = yaml.safe_load(f)

    # Override default configuration with any environment variables if they exist.
    for key in config.keys():
        env_value = os.getenv(key)
        if env_value is not None:
            config[key] = env_value
    return config

def validate_configurations(config):
    # Check for required configuration variables
    required_cfg_vars = [
        "LLM_URL",
        "LLM_MODEL_NAME",
        "EMBEDDINGS_LLM_MODEL_NAME",
        "REPORT_FILE_PATH",
        "KNOWN_FALSE_POSITIVE_FILE_PATH",
        "OUTPUT_FILE_PATH"
    ]
    required_cfg_files = [
        "REPORT_FILE_PATH",
        "KNOWN_FALSE_POSITIVE_FILE_PATH"
    ]

    for var in required_cfg_vars:
        value = config[var]
        if not value:
            raise ValueError(f"Configuration variable '{var}' is not set or is empty.")

    # Validate that input files exist and are accessible
    for var in required_cfg_files:
        value = config[var]
        if not os.path.exists(value):
            raise FileNotFoundError(f"Configuration variable '{var}' not found.")

    # Validate that environment variable LLM API key exist
    llm_api_key = os.environ.get("LLM_API_KEY")
    if not llm_api_key:
        raise ValueError(f"Environment variable 'LLM_API_KEY' is not set or is empty.")
    
    print("All required environment variables and files are valid and accessible.\n")

  
