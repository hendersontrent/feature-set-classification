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

outs_22 <- calculate_features(TimeSeriesData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "target", feature_set = "catch22", catch24 = TRUE)
save(outs_22, file = "data/outs_22.Rda")
rm(outs_22)

outs_feasts <- calculate_features(TimeSeriesData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "target", feature_set = "feasts")
save(outs_feasts, file = "data/outs_feasts.Rda")
rm(outs_feasts)

outs_tsfeatures <- calculate_features(TimeSeriesData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "target", feature_set = "tsfeatures")
save(outs_tsfeatures, file = "data/outs_tsfeatures.Rda")
rm(outs_tsfeatures)

outs_kats <- calculate_features(TimeSeriesData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "target", feature_set = "kats")
save(outs_kats, file = "data/outs_kats.Rda")
rm(outs_kats)

outs_tsfresh <- calculate_features(TimeSeriesData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "target", feature_set = "tsfresh", tsfresh_cleanup = FALSE)
save(outs_tsfresh, file = "data/outs_tsfresh.Rda")
rm(outs_tsfresh)

outs_TSFEL <- calculate_features(TimeSeriesData, id_var = "id", time_var = "timepoint", values_var = "values", group_var = "target", feature_set = "tsfel")
save(outs_TSFEL, file = "data/outs_TSFEL.Rda")
rm(outs_TSFEL)

#------------- Bind all and store --------------

features <- c("data/outs_22.Rda", "data/outs_feasts.Rda", "data/outs_tsfeatures.Rda",
              "data/outs_kats.Rda", "data/outs_tsfresh.Rda", "data/outs_TSFEL.Rda")

for(f in features){
  load(f)
}

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
