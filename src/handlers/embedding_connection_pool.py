"""
Embedding Connection Pool Manager

Provides connection pooling for OpenShift AI embedding requests to eliminate
connection establishment overhead during parallel job execution.
"""

import logging
import threading
import time
from typing import Optional

import httpx

from common.config import Config


class EmbeddingConnectionPool:
    """
    Singleton connection pool manager for embedding HTTP connections.

    Manages a pool of persistent HTTP connections to the OpenShift AI embedding
    endpoint, reducing connection establishment overhead for high-frequency
    embedding operations during parallel job execution.
    """

    _instance: Optional["EmbeddingConnectionPool"] = None
    _lock = threading.Lock()

    def __new__(cls) -> "EmbeddingConnectionPool":
        """Ensure singleton pattern within each container."""
        with cls._lock:
            if cls._instance is None:
                cls._instance = super().__new__(cls)
        return cls._instance

    def __init__(self):
        """Initialize connection pool if not already initialized."""
        if hasattr(self, "_initialized"):
            return

        self._initialized = True
        self._client: Optional[httpx.Client] = None
        self._config: Optional[Config] = None
        self._created_at: Optional[float] = None
        self._pool_enabled = True

        # Pool configuration (will be loaded from config)
        self._pool_size = 5
        self._pool_ttl_seconds = 300  # 5 minutes

        logging.info("EmbeddingConnectionPool initialized")

    def configure(self, config: Config) -> None:
        """
        Configure the connection pool with settings from config.

        Args:
            config: Configuration object containing pool settings
        """
        self._config = config

        # Load pool configuration with defaults
        self._pool_enabled = getattr(config, "EMBEDDING_HTTP_POOL_ENABLED", True)
        self._pool_size = getattr(config, "EMBEDDING_HTTP_POOL_SIZE", 5)
        self._pool_ttl_seconds = getattr(config, "EMBEDDING_HTTP_POOL_TTL_SECONDS", 300)
        self._verify_ssl = getattr(config, "EMBEDDING_HTTP_VERIFY_SSL", False)

        logging.info(
            f"EmbeddingConnectionPool configured: enabled={self._pool_enabled}, "
            f"size={self._pool_size}, ttl={self._pool_ttl_seconds}s, verify_ssl={self._verify_ssl}"
        )

    def get_client(self) -> httpx.Client:
        """
        Get a pooled HTTP client for embedding requests.

        Returns:
            httpx.Client: Configured HTTP client with connection pooling

        Raises:
            RuntimeError: If pool is not configured or client creation fails
        """
        if not self._pool_enabled:
            logging.debug("Connection pool disabled, creating fresh client")
            return self._create_fresh_client()

        if self._config is None:
            raise RuntimeError("EmbeddingConnectionPool not configured. Call configure() first.")

        # Check if client needs refresh due to TTL
        if self._needs_refresh():
            self._refresh_client()

        # Create client if not exists
        if self._client is None:
            self._create_pooled_client()

        return self._client

    def _needs_refresh(self) -> bool:
        """Check if client needs refresh based on TTL."""
        if self._created_at is None or self._client is None:
            return True

        age = time.time() - self._created_at
        return age > self._pool_ttl_seconds

    def _refresh_client(self) -> None:
        """Refresh the pooled client by closing old and creating new."""
        if self._client is not None:
            try:
                self._client.close()
                logging.debug("Closed expired HTTP client")
            except Exception as e:
                logging.warning(f"Error closing HTTP client: {e}")

        self._client = None
        self._created_at = None

    def _create_pooled_client(self) -> None:
        """Create new pooled HTTP client."""
        # Close existing client before creating new one to prevent resource leak
        if self._client is not None:
            try:
                self._client.close()
                logging.debug("Closed existing HTTP client before creating new one")
            except Exception as e:
                logging.warning(f"Error closing existing HTTP client: {e}")

        try:
            limits = httpx.Limits(
                max_keepalive_connections=self._pool_size,
                max_connections=self._pool_size,
                keepalive_expiry=self._pool_ttl_seconds,
            )

            self._client = httpx.Client(
                verify=self._verify_ssl,  # Configurable SSL verification
                limits=limits,
                timeout=httpx.Timeout(60.0),  # 60 second timeout
            )

            self._created_at = time.time()

            logging.info(
                f"Created pooled HTTP client with {self._pool_size} connections, "
                f"TTL={self._pool_ttl_seconds}s"
            )

        except Exception as e:
            logging.error(f"Failed to create pooled HTTP client: {e}")
            # Fallback to fresh client with proper SSL configuration
            self._client = self._create_fresh_client()
            # Reset created_at since this is now a fresh client, not pooled
            self._created_at = None

    def _create_fresh_client(self) -> httpx.Client:
        """Create a fresh HTTP client without pooling (fallback behavior)."""
        verify_ssl = getattr(
            self, "_verify_ssl", False
        )  # Default to False for backward compatibility
        return httpx.Client(verify=verify_ssl)

    def close(self) -> None:
        """Close the connection pool and cleanup resources."""
        if self._client is not None:
            try:
                self._client.close()
                logging.info("EmbeddingConnectionPool closed")
            except Exception as e:
                logging.warning(f"Error closing connection pool: {e}")
            finally:
                self._client = None
                self._created_at = None

    def get_stats(self) -> dict:
        """
        Get connection pool statistics for monitoring.

        Returns:
            dict: Pool statistics including enabled status, age, and config
        """
        stats = {
            "enabled": self._pool_enabled,
            "pool_size": self._pool_size,
            "pool_ttl": self._pool_ttl_seconds,
            "client_exists": self._client is not None,
            "age_seconds": None,
        }

        if self._created_at is not None:
            stats["age_seconds"] = time.time() - self._created_at

        return stats


# Global instance for easy access
_pool_instance = EmbeddingConnectionPool()


def get_embedding_client(config: Config) -> httpx.Client:
    """
    Convenience function to get a configured embedding HTTP client.

    Args:
        config: Configuration object

    Returns:
        httpx.Client: Configured HTTP client for embedding requests
    """
    try:
        _pool_instance.configure(config)
        return _pool_instance.get_client()
    except Exception as e:
        logging.error(f"Failed to get pooled embedding client: {e}")
        logging.info("Falling back to fresh HTTP client")
        return httpx.Client(verify=False)


def close_embedding_pool() -> None:
    """Close the global embedding connection pool."""
    _pool_instance.close()


def get_embedding_pool_stats() -> dict:
    """Get statistics from the global embedding connection pool."""
    return _pool_instance.get_stats()
