#------------------------------------------
# This script sets out to compute 
# classification accuracy for each set for
# each problem
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R to have been 
# run first
#-----------------------------------------

#---------------------------------------
# Author: Trent Henderson, 13 April 2023
#---------------------------------------

load("data/good_keepers.Rda")

# Get train-test split labels

load("data/TimeSeriesData.Rda")

train_test_ids <- TimeSeriesData %>%
  dplyr::select(c(problem, id, set_split)) %>%
  distinct()

rm(TimeSeriesData) # Clean up environment as dataframe is large

#---------------- Classification accuracy -----------------

# Non-z-scored

outputs <- good_keepers %>%
  purrr::map_dfr(~ fit_all_classifiers(problem_name = .x, tt_labels = train_test_ids, n_resamples = 30, by_set = TRUE, z_scored = FALSE))

save(outputs, file = "data/outputs.Rda")

outputs_aggregate <- good_keepers %>%
  purrr::map_dfr(~ fit_all_classifiers(problem_name = .x, tt_labels = train_test_ids, n_resamples = 30, by_set = FALSE, z_scored = FALSE))

save(outputs_aggregate, file = "data/outputs_aggregate.Rda")
rm(features, outputs, outputs_aggregate)

# z-scored

outputs_z <- good_keepers %>%
  purrr::map_dfr(~ fit_all_classifiers(problem_name = .x, tt_labels = train_test_ids, n_resamples = 30, by_set = TRUE, z_scored = TRUE))

save(outputs_z, file = "data/outputs_z.Rda")

outputs_z_aggregate <- good_keepers %>%
  purrr::map_dfr(~ fit_all_classifiers(problem_name = .x, tt_labels = train_test_ids, n_resamples = 30, by_set = FALSE, z_scored = TRUE))

save(outputs_z_aggregate, file = "data/outputs_z_aggregate.Rda")
rm(features, outputs_z, outputs_z_aggregate, good_keepers)
