#------------------------------------------
# This script can be used to run the entire
# project in order from start to finish to
# fully replicate the analysis
#-----------------------------------------

#-------------------------------------
# Author: Trent Henderson, 20 May 2022
#-------------------------------------

source("setup.R")

#---------------- Time series preparation ------------

source("analysis/prepare-time-series-data.R")

#---------------- Problem summaries ------------------

source("analysis/summarise-problems.R")

#---------------- Problem scaling identification -----

source("analysis/check-z-score.R")
source("analysis/mean-and-sd-check.R")

#---------------- Feature calculation ----------------

source("analysis/compute-features.R")
source("analysis/compute-features-z-score.R")

#---------------- Classifier model fits --------------

source("analysis/fit-classifiers.R")
source("analysis/fit-classifiers-z-score.R")

#---------------- Comparative analysis ---------------

#-------------
# non-z-scored
#-------------

source("analysis/non-z-scored/comp-to-benchmarks.R")

#---------
# z-scored
#---------

source("analysis/z-scored/analyse-mean-performance-line-plot.R")
source("analysis/z-scored/analyse-best-vs-worst-set.R")
source("analysis/z-scored/analyse-best-vs-second-best-set.R")
source("analysis/z-scored/analyse_sets_vs_overall.R")
source("analysis/z-scored/analyse_sets_vs_overall_pie.R")
source("analysis/z-scored/analyse-head-to-head.R")
source("analysis/z-scored/normalised-performance-score.R")
source("analysis/z-scored/normalised-performance-score-distribution.R")
source("analysis/z-scored/case-studies.R")
source("analysis/z-scored/case-study-confusion-matrices.R")
