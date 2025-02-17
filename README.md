# feature-set-classification
Compares various time-series feature sets on a set of classification tasks.

## Premise

[Previous work](https://ieeexplore.ieee.org/document/9679937) compared six time-series feature sets (`catch22`, `feasts`, `tsfeatures`, `tsfresh`, `TSFEL`, and `Kats`) on a range of criteria:

* Computation speed
* Within-set feature redundancy
* Between-set feature correlations

This work directly extends the findings to compare the performance of these six feature sets on a set of 128 univariate time-series classification problems from the [UEA & UCR Time Series Classification repository](https://www.timeseriesclassification.com). This work aims to understand the types of problems each feature set performs the best on.
