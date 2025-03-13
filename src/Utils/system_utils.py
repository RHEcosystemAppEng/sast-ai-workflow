import sys
import re
import torch


def get_device():
    if sys.platform == "darwin":
        return "mps" if torch.backends.mps.is_available() else "cpu"

    return "cuda" if torch.cuda.is_available() else "cpu"

def extract_file_path(input_str):
    # Using regex to extract only the file path
    match = re.search(r"(src/[^:]+):(\d+):(\d+):", input_str)

    if match:
        file_path = match.group(1)
        return file_path
    else:
        print("No match found.")
