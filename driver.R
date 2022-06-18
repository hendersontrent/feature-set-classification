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

#---------------- Problem scaling identificaiton -----

source("analysis/check-z-score.R")
source("analysis/mean-and-sd-check.R")

#---------------- Feature calculation ----------------

source("analysis/compute-features.R")

#---------------- Classifier model fits --------------

source("analysis/fit-classifiers.R")

#---------------- Comparative analysis ---------------

source("analysis/analyse-head-to-head.R")
source("analysis/analyse-performance.R")
source("analysis/analyse_sets_vs_overall.R")
