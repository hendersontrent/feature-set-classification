# feature-set-classification
Compares various time-series feature sets on a set of classification tasks.

## Premise

[Previous work](https://ieeexplore.ieee.org/document/9679937) compared six time-series feature sets (`catch22`, `feasts`, `tsfeatures`, `tsfresh`, `TSFEL`, and `Kats`) on computation speed, within-set feature redundancy, and between-set feature correlations.
This work directly extends the findings to compare the performance of these six feature sets on a set of univariate time-series classification problems from the [UEA & UCR Time Series Classification repository](https://www.timeseriesclassification.com). This work aims to understand the types of problems each feature set performs the best on.

## Structure of the codebase

The repository is organised into sub folders, each of which contains a discrete part of this analysis:

* `data` -- contains the time-series data and class labels for the train and test splits for each problem (i.e., the outputs of `get-uea-ucr-datasets.py`). *NOTE: This folder is not pushed to git for size reasons.*
* `feature-calculations` -- contains R scripts to calculate features for each problem as well as the resulting feature objects stored as `.Rda` files
* `classification-models` -- contains R scripts to calculate classification performance of each feature set for each problem as well as the resulting classification objects stored as `.Rda` files
* `interpretation` -- contains R scripts which analyse the results computed in `feature-calculations` and `classification-models`

### Replication instructions

Scripts should be run in the following order:

1. `get-uea-ucr-datasets.py` (Python)
2. `feature-calculations/calculate-features.R` (R)
3. `feature-calculations/calculate-features-baseline.R` (R)
4. `classification-models/fit-models-defaults.R` (R)
5. `interpretation/mean-performance-line.R` (R)
6. `interpretation/normalised-performance-score.R` (R)
7. `interpretation/pairwise-comparisons.R` (R)
8. `interpretation/head-to-head-ternary.R` (R)
9. `interpretation/xgboost-vs-svm.R` (R)

Note that `feature-calculations/calculate-features.R` contains a call to [`theft::install_python_pkgs`](https://github.com/hendersontrent/theft/blob/main/R/installs.R) and [`theft::init_theft`](https://github.com/hendersontrent/theft/blob/main/R/init_theft.R) which automatically install the three Python-based time-series feature set libraries and load them into the R environment.
