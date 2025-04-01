import os
import yaml

def print_config(config):
    print("".center(80, '-'))
    for key, value in config.items():
        if key not in ["LLM_API_KEY", "CRITIQUE_LLM_API_KEY"]:
            print(f"{key} = {value}")
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
    
    # Load Main LLM details in case critique details not provided
    if config.get("RUN_WITH_CRITIQUE"):
        if not config.get("CRITIQUE_LLM_URL") or not os.getenv("CRITIQUE_LLM_API_KEY"):
            print("Critique model details not provided - using main LLM details instead")
            config["CRITIQUE_LLM_URL"] = config.get("LLM_URL")
            os.environ["CRITIQUE_LLM_API_KEY"] = os.getenv("LLM_API_KEY")
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
    
    # Validate critique config if RUN_WITH_CRITIQUE is True
    if config.get("RUN_WITH_CRITIQUE") and not config.get("CRITIQUE_LLM_MODEL_NAME"):
        raise ValueError(
            "'CRITIQUE_LLM_MODEL_NAME' must be set when 'RUN_WITH_CRITIQUE' is True."
        )
    
    print("All required configuration variables and files are valid and accessible.\n")


  
