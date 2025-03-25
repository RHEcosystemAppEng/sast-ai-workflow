import git
import re
import os


def download_repo(repo_url: str) -> str:
    try:
        repo_url, branch_or_tag = get_repo_and_branch_from_url(repo_url)

        # Extract the project name (the last part before "/tree/")
        repo_name = repo_url.rstrip('/').split('/')[-1]
        if repo_name.endswith('.git'):
            repo_name = repo_name[:-4]

        # Set the destination path to the current directory
        destination_path = os.path.join(os.getcwd(), repo_name)

        # Clone the repo
        print(f"Cloning {repo_url} into {destination_path}...")
        repo = git.Repo.clone_from(repo_url, destination_path)

        # Checkout the specified branch or tag if provided
        if branch_or_tag:
            print(f"Checking out {branch_or_tag}...")
            repo.git.checkout(branch_or_tag)

        print("Repository cloned successfully!")

    except Exception as e:
        print(f"An error occurred: {e}")

    return destination_path


def get_repo_and_branch_from_url(repo_url: str) -> tuple[(str, str)]:
    # Identify if the URL has a branch or tag with "/tree/"
    if "/tree/" in repo_url:
        # Split URL to separate repository URL and branch/tag
        repo_url, branch_or_tag = re.split(r'/tree/', repo_url, maxsplit=1)
    else:
        branch_or_tag = None

    return repo_url, branch_or_tag
