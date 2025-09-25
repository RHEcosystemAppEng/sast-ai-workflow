"""
Tests for MinIO service functionality.
"""

import pytest
from unittest.mock import Mock, patch
from src.services.minio_service import MinIOService


class TestMinIOService:
    """Test cases for MinIO service."""

    def test_filesystem_mode_initialization(self):
        """Test MinIO service initialization in filesystem mode."""
        config = {
            'STORAGE_TYPE': 'filesystem'
        }

        service = MinIOService(config)

        assert not service.is_s3_enabled()
        assert service.minio_client is None
        assert service.s3_client is None

    def test_s3_mode_configuration(self):
        """Test MinIO service configuration for S3 mode."""
        config = {
            'STORAGE_TYPE': 's3',
            'S3_ENDPOINT': 'http://localhost:9000',
            'S3_ACCESS_KEY': 'test-access',
            'S3_SECRET_KEY': 'test-secret',
            'S3_BUCKET_DATASETS': 'test-datasets',
            'S3_BUCKET_TEMP': 'test-temp',
            'S3_REGION': 'us-east-1',
            'S3_USE_SSL': False
        }

        with patch('src.services.minio_service.Minio') as mock_minio, \
             patch('src.services.minio_service.boto3') as mock_boto3:

            # Mock MinIO client
            mock_minio_instance = Mock()
            mock_minio.return_value = mock_minio_instance
            mock_minio_instance.bucket_exists.return_value = True

            # Mock boto3 client
            mock_s3_client = Mock()
            mock_boto3.client.return_value = mock_s3_client

            service = MinIOService(config)

            assert service.is_s3_enabled()
            assert service.endpoint == 'http://localhost:9000'
            assert service.datasets_bucket == 'test-datasets'
            assert service.temp_bucket == 'test-temp'

    def test_upload_file_filesystem_mode(self):
        """Test file upload in filesystem mode."""
        config = {'STORAGE_TYPE': 'filesystem'}
        service = MinIOService(config)

        result = service.upload_file('/local/file.txt', 'test-object')

        assert result is False

    def test_upload_file_s3_mode(self):
        """Test file upload in S3 mode."""
        config = {
            'STORAGE_TYPE': 's3',
            'S3_ENDPOINT': 'http://localhost:9000',
            'S3_ACCESS_KEY': 'test-access',
            'S3_SECRET_KEY': 'test-secret',
            'S3_BUCKET_DATASETS': 'test-datasets',
            'S3_BUCKET_TEMP': 'test-temp',
            'S3_REGION': 'us-east-1',
            'S3_USE_SSL': False
        }

        with patch('src.services.minio_service.Minio') as mock_minio, \
             patch('src.services.minio_service.boto3') as mock_boto3:

            # Mock MinIO client
            mock_minio_instance = Mock()
            mock_minio.return_value = mock_minio_instance
            mock_minio_instance.bucket_exists.return_value = True
            mock_minio_instance.fput_object.return_value = None

            # Mock boto3 client
            mock_s3_client = Mock()
            mock_boto3.client.return_value = mock_s3_client

            service = MinIOService(config)

            result = service.upload_file('/local/file.txt', 'test-object')

            assert result is True
            mock_minio_instance.fput_object.assert_called_once_with(
                'test-datasets', 'test-object', '/local/file.txt'
            )

    def test_get_storage_info_filesystem(self):
        """Test storage info for filesystem mode."""
        config = {'STORAGE_TYPE': 'filesystem'}
        service = MinIOService(config)

        info = service.get_storage_info()

        assert info['storage_type'] == 'filesystem'

    def test_get_storage_info_s3(self):
        """Test storage info for S3 mode."""
        config = {
            'STORAGE_TYPE': 's3',
            'S3_ENDPOINT': 'http://localhost:9000',
            'S3_ACCESS_KEY': 'test-access',
            'S3_SECRET_KEY': 'test-secret',
            'S3_BUCKET_DATASETS': 'test-datasets',
            'S3_BUCKET_TEMP': 'test-temp',
            'S3_REGION': 'us-east-1',
            'S3_USE_SSL': False
        }

        with patch('src.services.minio_service.Minio') as mock_minio, \
             patch('src.services.minio_service.boto3') as mock_boto3:

            # Mock MinIO client
            mock_minio_instance = Mock()
            mock_minio.return_value = mock_minio_instance
            mock_minio_instance.bucket_exists.return_value = True

            # Mock boto3 client
            mock_s3_client = Mock()
            mock_boto3.client.return_value = mock_s3_client

            service = MinIOService(config)

            info = service.get_storage_info()

            assert info['storage_type'] == 's3'
            assert info['endpoint'] == 'http://localhost:9000'
            assert info['datasets_bucket'] == 'test-datasets'
            assert info['temp_bucket'] == 'test-temp'

    def test_object_exists_s3_mode(self):
        """Test object existence check in S3 mode."""
        config = {
            'STORAGE_TYPE': 's3',
            'S3_ENDPOINT': 'http://localhost:9000',
            'S3_ACCESS_KEY': 'test-access',
            'S3_SECRET_KEY': 'test-secret',
            'S3_BUCKET_DATASETS': 'test-datasets',
            'S3_BUCKET_TEMP': 'test-temp',
            'S3_REGION': 'us-east-1',
            'S3_USE_SSL': False
        }

        with patch('src.services.minio_service.Minio') as mock_minio, \
             patch('src.services.minio_service.boto3') as mock_boto3:

            # Mock MinIO client
            mock_minio_instance = Mock()
            mock_minio.return_value = mock_minio_instance
            mock_minio_instance.bucket_exists.return_value = True
            mock_minio_instance.stat_object.return_value = Mock()

            # Mock boto3 client
            mock_s3_client = Mock()
            mock_boto3.client.return_value = mock_s3_client

            service = MinIOService(config)

            result = service.object_exists('test-object')

            assert result is True
            mock_minio_instance.stat_object.assert_called_once_with(
                'test-datasets', 'test-object'
            )