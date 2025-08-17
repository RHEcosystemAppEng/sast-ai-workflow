import logging
import os
import re
import shutil
from typing import Tuple

import git

logger = logging.getLogger(__name__)


def download_repo(repo_url: str) -> str:
    try:
        repo_url, branch_or_tag = get_repo_and_branch_from_url(repo_url)

        # Extract the project name (the last part before "/tree/")
        repo_name = repo_url.rstrip("/").split("/")[-1]
        if repo_name.endswith(".git"):
            repo_name = repo_name[:-4]

        # Set the destination path to the current directory
        destination_path = os.path.join(os.getcwd(), repo_name)

        # Check if the repository already exists at the destination path
        if os.path.exists(destination_path):
            try:
                # Check if it's a valid git repository
                repo = git.Repo(destination_path)
                logger.info(f"Repository already exists at {destination_path}, skipping download.")
                
            except git.exc.InvalidGitRepositoryError:
                # Path exists but is not a valid git repository
                logger.warning(f"Directory {destination_path} exists but is not a valid git repository. Removing and cloning fresh.")
                shutil.rmtree(destination_path)
                repo = None
        else:
            repo = None

        # Clone the repo if it doesn't exist or was invalid
        if repo is None:
            logger.info(f"Cloning {repo_url} into {destination_path}...")
            repo = git.Repo.clone_from(repo_url, destination_path)
            logger.info("Repository cloned successfully!")

        # Checkout the specified branch or tag if provided (single point of checkout logic)
        if branch_or_tag:
            logger.info(f"Checking out {branch_or_tag}...")
            repo.git.checkout(branch_or_tag)

    except Exception as e:
        logger.error(f"An error occurred: {e}")

    return destination_path


def get_repo_and_branch_from_url(repo_url: str) -> Tuple[str, str]:
    # Identify if the URL has a branch or tag with "/tree/"
    if "/tree/" in repo_url:
        # Split URL to separate repository URL and branch/tag
        repo_url, branch_or_tag = re.split(r"/tree/", repo_url, maxsplit=1)
    else:
        branch_or_tag = None

    return repo_url, branch_or_tag
