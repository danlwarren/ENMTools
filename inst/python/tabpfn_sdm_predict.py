"""
Minimal TabPFN SDM inference helper for ENMTools.

Author: Russell Dinnage
"""

import torch


def get_device(device="cuda"):
    """Get device, falling back to CPU if CUDA unavailable."""
    if device == "cuda" and not torch.cuda.is_available():
        return "cpu"
    if device == "auto":
        return "cuda" if torch.cuda.is_available() else "cpu"
    return device
