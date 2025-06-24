import logging
import os
import sys
from colorama import init, Fore, Style

# Initialize colorama
init(autoreset=True)

class ColoredFormatter(logging.Formatter):
    """Custom formatter with colors for different log levels"""
    
    COLORS = {
        'DEBUG': Fore.BLUE,
        'INFO': Fore.WHITE,
        'WARNING': Fore.YELLOW,
        'ERROR': Fore.RED,
        'CRITICAL': Fore.RED + Style.BRIGHT,
    }
    
    def __init__(self, use_colors=True):
        super().__init__()
        self.use_colors = use_colors
        
    def format(self, record):
        # Format timestamp
        if hasattr(record, 'created'):
            import time
            record.asctime = time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(record.created))
        
        # Apply colors if enabled
        if self.use_colors and record.levelname in self.COLORS:
            colored_levelname = f"{self.COLORS[record.levelname]}{record.levelname}{Style.RESET_ALL}"
        else:
            colored_levelname = record.levelname
            
        return f"{record.asctime} - {colored_levelname}: {record.getMessage()}"

def setup_logging():
    """Setup logging with console colors and optional file logging"""
    log_level = getattr(logging, os.getenv('LOG_LEVEL', 'INFO').upper())
    
    # Create logger
    logger = logging.getLogger(__name__)
    logger.setLevel(log_level)
    logger.handlers.clear()  # Clear existing handlers
    
    # Console handler with colors
    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setFormatter(ColoredFormatter(use_colors=True))
    console_handler.setLevel(log_level)
    logger.addHandler(console_handler)
    
    # File handler without colors (if LOG_FILE is set)
    log_file = os.getenv('LOG_FILE')
    if log_file:
        file_handler = logging.FileHandler(log_file)
        file_handler.setFormatter(ColoredFormatter(use_colors=False))
        file_handler.setLevel(log_level)
        logger.addHandler(file_handler)
        logger.info(f"File logging enabled: {log_file}")
    
    # Set specific modules to DEBUG level
    debug_modules = os.getenv('DEBUG_MODULES', '').split(',')
    for module in debug_modules:
        module = module.strip()
        if module:
            logging.getLogger(module).setLevel(logging.DEBUG)
            logger.info(f"Set module '{module}' to DEBUG level")
    
    return logger

# Get logger instance
logger = logging.getLogger(__name__)

def log_attempt_number(retry_state):
    """Log the attempt number and the exception that caused the retry."""
    logger.warning(
        f"Retrying: "
        f"Attempt number {retry_state.attempt_number} "
        f"failed with {retry_state.outcome.exception()}. "
        f"Waiting {retry_state.next_action.sleep} seconds before next attempt."
    )