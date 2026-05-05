#---------------------------------------
# This script uses loads all the UEA/UCR
# univariate time-series classification
# problems and stores them as .csv files
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 16 April 2026
#---------------------------------------

import numpy as np
import os
import pandas as pd

from copy import deepcopy
from pathlib import Path

from aeon.datasets import load_classification
from aeon.datasets.tsc_datasets import univariate, univariate_bake_off_2024


def get_univariate_cfn_names():
    """Compile a list of all univariate classification problems

    Returns:
        list: List of unique univariate dataset names to be loaded
    """
    dataset_names = deepcopy(univariate_bake_off_2024)
    for name in univariate:
        if name not in dataset_names:
            dataset_names.append(name)
            
    return dataset_names


def load_ucruea_dataset(name, split=None, load_equal_length=False, load_no_missing=True):
    """Load a classification dataset from the UCR/UEA repository.

    Args:
        name (string): Name of classification dataset to load
        split (string, optional): Load the train or test data. Defaults to "train".
        load_equal_length (bool, optional): Whether to force equal-length time series. 
            Defaults to False.
        load_no_missing (bool, optional): Whether to automatically pad missing values. 
            Defaults to True.

    Returns:
        X, y for the given data split.
    """
    X, y = load_classification(
        name, 
        split=split,
        load_equal_length=load_equal_length,
        load_no_missing=load_no_missing,
    )

    return X, y


def save_dataset_to_csv(X, y, dataset_name, split, output_dir="data"):
    """Save Aeon dataset to CSV files.

    Args:
        X: Time series data (numpy array or nested format)
        y: Labels
        dataset_name (str): Name of dataset
        split (str): 'train' or 'test'
        output_dir (str): Directory to save CSV files
    """

    output_path = Path(output_dir) / dataset_name
    output_path.mkdir(parents=True, exist_ok=True)

    # Convert X to numpy if needed
    X_np = np.array(X)

    # Handle 3D case: (n_cases, n_channels, series_length)
    if X_np.ndim == 3:
        n_cases, n_channels, series_length = X_np.shape
        X_flat = X_np.reshape(n_cases, n_channels * series_length)

    # Handle 2D case: already flat
    elif X_np.ndim == 2:
        X_flat = X_np

    else:
        raise ValueError(f"Unsupported X shape: {X_np.shape}")

    # Convert to DataFrame
    X_df = pd.DataFrame(X_flat)
    y_df = pd.DataFrame(y, columns=["target"])

    # Save files
    X_file = output_path / f"{dataset_name}_{split}_X.csv"
    y_file = output_path / f"{dataset_name}_{split}_y.csv"

    X_df.to_csv(X_file, index=False)
    y_df.to_csv(y_file, index=False)

    print(f"Saved {dataset_name} ({split}) → {X_file}, {y_file}")

for name in get_univariate_cfn_names():
    try:
        for split in ["train", "test"]:
            X, y = load_ucruea_dataset(name, split=split)
            save_dataset_to_csv(X, y, name, split)
    except Exception as e:
        print(f"Skipping {name}: {e}")