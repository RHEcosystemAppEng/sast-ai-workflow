# Use Python 3.11 as base image
FROM python:3.11-slim

# Set working directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    git \
    curl \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements first for better Docker layer caching
COPY requirements.txt .

# Install Python dependencies
RUN pip install --no-cache-dir -r requirements.txt

# Copy the source code
COPY src/ ./src/
COPY .env* ./

# Create debug_dumps directory
RUN mkdir -p debug_dumps

# Set Python path to include src directory
ENV PYTHONPATH=/app/src

# Set default command
CMD ["python", "src/run.py"]