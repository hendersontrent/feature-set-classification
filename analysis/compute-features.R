#------------------------------------------
# This script sets out to compute features
# for each set and time-series problem
#
# NOTE: This script requires setup.R and
# R/tidy_arff_files.R to have been run 
# first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 29 October 2021
#-----------------------------------------

# Load time series data

load("data/TimeSeriesData.Rda")

# Fix my Python environment

reticulate::use_python("~/opt/anaconda3/bin/python", required = TRUE)

#------------- Feature extraction --------------

# Need to do each set individually to avoid computer crashes

outs_22 <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "target", feature_set = "catch22")
outs_feasts <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "target", feature_set = "feasts")
outs_tsfeatures <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "target", feature_set = "tsfeatures")
outs_kats <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "target", feature_set = "kats")
outs_tsfresh <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "target", feature_set = "tsfresh", tsfresh_cleanup = FALSE)
outs_TSFEL <- calculate_features(empirical1000, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "target", feature_set = "tsfel")

#------------- Bind all and store --------------

# Bind features

FeatureMatrix <- bind_rows(outs_22, outs_feasts, outs_tsfeatures, outs_kats, outs_tsfresh, outs_TSFEL)

# Re-join problem labels

problems <- TimeSeriesData %>%
  dplyr::select(c(id, problem)) %>%
  distinct()

FeatureMatrix <- FeatureMatrix %>%
  left_join(problems, by = c("id" = "id"))

# Save

save(FeatureMatrix, file = "data/FeatureMatrix.Rda")
