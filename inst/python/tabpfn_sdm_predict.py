"""
Minimal TabPFN SDM inference helper for ENMTools.

Extracted from TabPFN-SDM project (tabpfn_sdm_common.py).
Contains only functions needed for inference with finetuned models.

Author: Russell Dinnage
"""

import torch
import numpy as np
from tabpfn import TabPFNClassifier

DEFAULT_SEED = 32639
DEFAULT_N_ESTIMATORS_INFER = 8


def get_device(device="cuda"):
    """Get device, falling back to CPU if CUDA unavailable."""
    if device == "cuda" and not torch.cuda.is_available():
        return "cpu"
    if device == "auto":
        return "cuda" if torch.cuda.is_available() else "cpu"
    return device


def load_finetuned_model(model_path, device="cuda", n_estimators=DEFAULT_N_ESTIMATORS_INFER):
    """
    Load a finetuned TabPFN model for inference.

    Parameters
    ----------
    model_path : str
        Path to the saved model checkpoint
    device : str
        Device to load model on (default "cuda")
    n_estimators : int
        Number of estimators for inference (default 8)

    Returns
    -------
    TabPFNClassifier
        Classifier with finetuned weights loaded
    """
    device = get_device(device)

    classifier = TabPFNClassifier(
        ignore_pretraining_limits=True,
        device=device,
        n_estimators=n_estimators,
        random_state=DEFAULT_SEED
    )
    classifier._initialize_model_variables()

    checkpoint = torch.load(model_path, map_location=device, weights_only=False)
    classifier.models_[0].load_state_dict(checkpoint['model_state_dict'])
    classifier.models_[0].eval()

    return classifier


def predict_with_finetuned(clf, X_train, y_train, X_test, cat_indices=None):
    """
    Make predictions with a finetuned TabPFN model.

    Parameters
    ----------
    clf : TabPFNClassifier
        Finetuned classifier
    X_train : array-like
        Training features
    y_train : array-like
        Training labels
    X_test : array-like
        Test features to predict
    cat_indices : list or None
        0-indexed column positions of categorical features

    Returns
    -------
    np.ndarray
        Predicted probabilities for class 1 (presence)
    """
    X_train = np.array(X_train, dtype=np.float32)
    y_train = np.array(y_train, dtype=np.int64)
    X_test = np.array(X_test, dtype=np.float32)

    if cat_indices is not None and len(cat_indices) > 0:
        clf.categorical_features_indices = list(int(i) for i in cat_indices)
    else:
        clf.categorical_features_indices = None

    clf.fit(X_train, y_train)
    proba = clf.predict_proba(X_test)
    return proba[:, 1]
