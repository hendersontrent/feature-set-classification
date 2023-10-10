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

good_keepers %>%
  purrr::map_dfr(~ fit_all_classifiers(problem_name = .x, tt_labels = train_test_ids, n_resamples = 100, by_set = TRUE, z_scored = FALSE))

good_keepers %>%
  purrr::map_dfr(~ fit_all_classifiers(problem_name = .x, tt_labels = train_test_ids, n_resamples = 100, by_set = FALSE, z_scored = FALSE))

# z-scored

good_keepers %>%
  purrr::map_dfr(~ fit_all_classifiers(problem_name = .x, tt_labels = train_test_ids, n_resamples = 100, by_set = TRUE, z_scored = TRUE))

good_keepers %>%
  purrr::map_dfr(~ fit_all_classifiers(problem_name = .x, tt_labels = train_test_ids, n_resamples = 100, by_set = FALSE, z_scored = TRUE))

#---------------- Bind together -----------------

#-------------
# Non-z-scored
#-------------

# By set

data_files <- list.files("data/classifiers/by_set", full.names = TRUE, pattern = "\\.Rda")
outputs_tmp <- vector(mode = "list", length = length(data_files))

for(d in data_files){
  load(d)
  outputs_tmp[[match(d, data_files)]] <- outputs
}

outputs <- do.call("rbind", outputs_tmp)
save(outputs, file = "data/outputs.Rda")
rm(outputs)

# Aggregate

data_files <- list.files("data/classifiers/aggregate", full.names = TRUE, pattern = "\\.Rda")
outputs_aggregate <- vector(mode = "list", length = length(data_files))

for(d in data_files){
  load(d)
  outputs_aggregate[[match(d, data_files)]] <- outputs
}

outputs_aggregate <- do.call("rbind", outputs_aggregate)
save(outputs_aggregate, file = "data/outputs_aggregate.Rda")
rm(outputs)

#---------
# z-scored
#---------

# By set

data_files <- list.files("data/z-score-classifiers/by_set", full.names = TRUE, pattern = "\\.Rda")
outputs_z <- vector(mode = "list", length = length(data_files))

for(d in data_files){
  load(d)
  outputs_z[[match(d, data_files)]] <- outputs
}

outputs_z <- do.call("rbind", outputs_z)
save(outputs_z, file = "data/outputs.Rda")
rm(outputs)

# Aggregate

data_files <- list.files("data/z-score-classifiers/aggregate", full.names = TRUE, pattern = "\\.Rda")
outputs_aggregate_z <- vector(mode = "list", length = length(data_files))

for(d in data_files){
  load(d)
  outputs_aggregate_z[[match(d, data_files)]] <- outputs
}

outputs_aggregate_z <- do.call("rbind", outputs_aggregate_z)
save(outputs_aggregate_z, file = "data/outputs_aggregate_z.Rda")
