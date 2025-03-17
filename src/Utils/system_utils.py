import sys
import re
import torch


def get_device():
    if sys.platform == "darwin":
        return "mps" if torch.backends.mps.is_available() else "cpu"

    return "cuda" if torch.cuda.is_available() else "cpu"