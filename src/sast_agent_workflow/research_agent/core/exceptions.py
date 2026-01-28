"""Custom exceptions for the research agent workflow.

Centralizes error handling with typed exceptions for better
error classification and recovery strategies.
"""


class ResearchAgentError(Exception):
    """Base exception for research agent errors."""
    
    def __init__(self, message: str, recoverable: bool = True):
        """
        Initialize the exception.
        
        Args:
            message: Error description
            recoverable: Whether the error can be recovered from
        """
        self.recoverable = recoverable
        super().__init__(message)


class ToolExecutionError(ResearchAgentError):
    """Raised when a tool fails to execute."""
    
    def __init__(self, tool_name: str, message: str, recoverable: bool = True):
        """
        Initialize the exception.
        
        Args:
            tool_name: Name of the tool that failed
            message: Error description
            recoverable: Whether the error can be recovered from
        """
        self.tool_name = tool_name
        super().__init__(f"{tool_name}: {message}", recoverable)


class CodeExtractionError(ToolExecutionError):
    """Raised when code extraction fails."""
    
    def __init__(self, file_path: str, message: str):
        """
        Initialize the exception.
        
        Args:
            file_path: Path to the file that couldn't be extracted
            message: Error description
        """
        self.file_path = file_path
        super().__init__("fetch_code", f"Failed to extract from '{file_path}': {message}")


class SearchError(ToolExecutionError):
    """Raised when codebase search fails."""
    
    def __init__(self, pattern: str, message: str):
        """
        Initialize the exception.
        
        Args:
            pattern: Search pattern that failed
            message: Error description
        """
        self.pattern = pattern
        super().__init__("search_codebase", f"Search for '{pattern}' failed: {message}")


class InvestigationError(ResearchAgentError):
    """Raised when investigation encounters an error."""
    
    def __init__(self, issue_id: str, message: str, recoverable: bool = True):
        """
        Initialize the exception.
        
        Args:
            issue_id: ID of the issue being investigated
            message: Error description
            recoverable: Whether the error can be recovered from
        """
        self.issue_id = issue_id
        super().__init__(f"[{issue_id}] {message}", recoverable)


class CircuitBreakerTriggered(InvestigationError):
    """Raised when circuit breaker terminates investigation."""
    
    def __init__(self, issue_id: str, reason: str):
        """
        Initialize the exception.
        
        Args:
            issue_id: ID of the issue being investigated
            reason: Reason the circuit breaker was triggered
        """
        self.reason = reason
        super().__init__(
            issue_id,
            f"Circuit breaker triggered: {reason}",
            recoverable=False
        )


class AnalysisError(InvestigationError):
    """Raised when analysis phase fails."""
    
    def __init__(self, issue_id: str, message: str):
        """
        Initialize the exception.
        
        Args:
            issue_id: ID of the issue being investigated
            message: Error description
        """
        super().__init__(issue_id, f"Analysis failed: {message}")


class EvaluationError(InvestigationError):
    """Raised when evaluation phase fails."""
    
    def __init__(self, issue_id: str, message: str):
        """
        Initialize the exception.
        
        Args:
            issue_id: ID of the issue being investigated
            message: Error description
        """
        super().__init__(issue_id, f"Evaluation failed: {message}")


class LLMError(ResearchAgentError):
    """Raised when LLM operations fail."""
    
    def __init__(self, operation: str, message: str, recoverable: bool = True):
        """
        Initialize the exception.
        
        Args:
            operation: LLM operation that failed (e.g., 'create', 'invoke')
            message: Error description
            recoverable: Whether the error can be recovered from
        """
        self.operation = operation
        super().__init__(f"LLM {operation} failed: {message}", recoverable)


class ConfigurationError(ResearchAgentError):
    """Raised when configuration is invalid."""
    
    def __init__(self, config_key: str, message: str):
        """
        Initialize the exception.
        
        Args:
            config_key: Configuration key that has an issue
            message: Error description
        """
        self.config_key = config_key
        super().__init__(f"Configuration error for '{config_key}': {message}", recoverable=False)
