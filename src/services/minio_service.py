"""
MinIO S3-compatible storage service for SAST workflow.
Provides basic file upload/download operations for SAST reports and outputs.
"""

import os
import logging
from typing import Optional, BinaryIO
from minio import Minio
from minio.error import S3Error
import boto3
from botocore.exceptions import ClientError

logger = logging.getLogger(__name__)


class MinIOService:
    """Service for interacting with MinIO S3-compatible storage."""

    def __init__(self, config: dict):
        """
        Initialize MinIO service with configuration.

        Args:
            config: Configuration dictionary containing S3 settings
        """
        self.storage_type = config.get('STORAGE_TYPE', 'filesystem')

        if self.storage_type.lower() == 's3':
            self.endpoint = config.get('S3_ENDPOINT', '')
            self.access_key = config.get('S3_ACCESS_KEY', '')
            self.secret_key = config.get('S3_SECRET_KEY', '')
            self.datasets_bucket = config.get('S3_BUCKET_DATASETS', 'sast-datasets')
            self.temp_bucket = config.get('S3_BUCKET_TEMP', 'sast-temp')
            self.region = config.get('S3_REGION', 'us-east-1')
            self.use_ssl = config.get('S3_USE_SSL', False)

            self._init_minio_client()
            self._init_boto3_client()

            logger.debug(f"MinIO service initialized - endpoint: {self.endpoint}")
        else:
            logger.debug("MinIO service initialized in filesystem mode")
            self.minio_client = None
            self.s3_client = None

    def _init_minio_client(self):
        """Initialize MinIO client."""
        try:
            endpoint_clean = self.endpoint.replace('http://', '').replace('https://', '')

            self.minio_client = Minio(
                endpoint_clean,
                access_key=self.access_key,
                secret_key=self.secret_key,
                secure=self.use_ssl
            )

            self._ensure_buckets_exist()

        except Exception as e:
            logger.error(f"Failed to initialize MinIO client: {e}")
            raise

    def _init_boto3_client(self):
        """Initialize boto3 S3 client for compatibility."""
        try:
            self.s3_client = boto3.client(
                's3',
                endpoint_url=self.endpoint,
                aws_access_key_id=self.access_key,
                aws_secret_access_key=self.secret_key,
                region_name=self.region,
                use_ssl=self.use_ssl
            )
        except Exception as e:
            logger.error(f"Failed to initialize boto3 client: {e}")
            raise

    def _ensure_buckets_exist(self):
        """Ensure required buckets exist."""
        try:
            for bucket in [self.datasets_bucket, self.temp_bucket]:
                if not self.minio_client.bucket_exists(bucket):
                    self.minio_client.make_bucket(bucket)
                    logger.debug(f"Created bucket: {bucket}")
                else:
                    logger.debug(f"Bucket already exists: {bucket}")
        except S3Error as e:
            logger.error(f"Failed to ensure buckets exist: {e}")
            raise

    def is_s3_enabled(self) -> bool:
        """Check if S3 storage is enabled."""
        return self.storage_type.lower() == 's3' and self.minio_client is not None

    def upload_file(self, local_file_path: str, object_name: str, bucket_name: Optional[str] = None) -> bool:
        """
        Upload a file to MinIO/S3.

        Args:
            local_file_path: Path to local file to upload
            object_name: Object name in bucket
            bucket_name: Bucket name (defaults to datasets bucket)

        Returns:
            True if successful, False otherwise
        """
        if not self.is_s3_enabled():
            logger.warning("S3 not enabled, skipping upload")
            return False

        bucket = bucket_name or self.datasets_bucket

        try:
            self.minio_client.fput_object(bucket, object_name, local_file_path)
            logger.debug(f"Uploaded {local_file_path} to {bucket}/{object_name}")
            return True
        except S3Error as e:
            logger.error(f"Failed to upload file: {e}")
            return False

    def download_file(self, object_name: str, local_file_path: str, bucket_name: Optional[str] = None) -> bool:
        """
        Download a file from MinIO/S3.

        Args:
            object_name: Object name in bucket
            local_file_path: Local path to save file
            bucket_name: Bucket name (defaults to datasets bucket)

        Returns:
            True if successful, False otherwise
        """
        if not self.is_s3_enabled():
            logger.warning("S3 not enabled, skipping download")
            return False

        bucket = bucket_name or self.datasets_bucket

        try:
            self.minio_client.fget_object(bucket, object_name, local_file_path)
            logger.debug(f"Downloaded {bucket}/{object_name} to {local_file_path}")
            return True
        except S3Error as e:
            logger.error(f"Failed to download file: {e}")
            return False

    def upload_data(self, data: bytes, object_name: str, bucket_name: Optional[str] = None) -> bool:
        """
        Upload data directly to MinIO/S3.

        Args:
            data: Data to upload as bytes
            object_name: Object name in bucket
            bucket_name: Bucket name (defaults to datasets bucket)

        Returns:
            True if successful, False otherwise
        """
        if not self.is_s3_enabled():
            logger.warning("S3 not enabled, skipping upload")
            return False

        bucket = bucket_name or self.datasets_bucket

        try:
            from io import BytesIO
            data_stream = BytesIO(data)
            self.minio_client.put_object(
                bucket,
                object_name,
                data_stream,
                length=len(data)
            )
            logger.debug(f"Uploaded data to {bucket}/{object_name}")
            return True
        except S3Error as e:
            logger.error(f"Failed to upload data: {e}")
            return False

    def download_data(self, object_name: str, bucket_name: Optional[str] = None) -> Optional[bytes]:
        """
        Download data from MinIO/S3.

        Args:
            object_name: Object name in bucket
            bucket_name: Bucket name (defaults to datasets bucket)

        Returns:
            File data as bytes, or None if failed
        """
        if not self.is_s3_enabled():
            logger.warning("S3 not enabled, skipping download")
            return None

        bucket = bucket_name or self.datasets_bucket

        try:
            response = self.minio_client.get_object(bucket, object_name)
            data = response.read()
            response.close()
            response.release_conn()
            logger.debug(f"Downloaded data from {bucket}/{object_name}")
            return data
        except S3Error as e:
            logger.error(f"Failed to download data: {e}")
            return None

    def list_objects(self, prefix: str = "", bucket_name: Optional[str] = None) -> list:
        """
        List objects in bucket with optional prefix.

        Args:
            prefix: Object name prefix to filter
            bucket_name: Bucket name (defaults to datasets bucket)

        Returns:
            List of object names
        """
        if not self.is_s3_enabled():
            logger.warning("S3 not enabled, returning empty list")
            return []

        bucket = bucket_name or self.datasets_bucket

        try:
            objects = self.minio_client.list_objects(bucket, prefix=prefix)
            return [obj.object_name for obj in objects]
        except S3Error as e:
            logger.error(f"Failed to list objects: {e}")
            return []

    def object_exists(self, object_name: str, bucket_name: Optional[str] = None) -> bool:
        """
        Check if object exists in bucket.

        Args:
            object_name: Object name to check
            bucket_name: Bucket name (defaults to datasets bucket)

        Returns:
            True if object exists, False otherwise
        """
        if not self.is_s3_enabled():
            return False

        bucket = bucket_name or self.datasets_bucket

        try:
            self.minio_client.stat_object(bucket, object_name)
            return True
        except S3Error:
            return False

    def get_storage_info(self) -> dict:
        """
        Get storage configuration information.

        Returns:
            Dictionary with storage information
        """
        if self.is_s3_enabled():
            return {
                'storage_type': 's3',
                'endpoint': self.endpoint,
                'datasets_bucket': self.datasets_bucket,
                'temp_bucket': self.temp_bucket,
                'region': self.region,
                'use_ssl': self.use_ssl
            }
        else:
            return {
                'storage_type': 'filesystem'
            }