from abc import ABC, abstractmethod
from typing import List

from common.config import Config
from dto.Issue import Issue


class BaseReportReader(ABC):
    """
    Abstract base class for report readers.
    Each format (HTML, Google Sheets, SARIF, etc.) should implement this interface.
    """

    @abstractmethod
    def can_handle(self, file_path: str, config: Config) -> bool:
        """
        Determine if this reader can handle the given file/path.

        Args:
            file_path: The path or URL to the report file
            config: Configuration object containing additional context

        Returns:
            bool: True if this reader can handle the file, False otherwise
        """
        pass

    @abstractmethod
    def read_report(self, file_path: str, config: Config) -> List[Issue]:
        """
        Read and parse the report file into Issue objects.

        Args:
            file_path: The path or URL to the report file
            config: Configuration object containing additional context

        Returns:
            List[Issue]: List of Issue objects parsed from the report
        """
        pass
