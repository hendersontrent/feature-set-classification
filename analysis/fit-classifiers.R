#------------------------------------------
# This script sets out to compute 
# classification accuracy for catch24 for
# each problem
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R to have been 
# run first
#-----------------------------------------

#---------------------------------------
# Author: Trent Henderson, 13 April 2023
#---------------------------------------

#---------------- Classification accuracy -----------------

# Non-z-scored

load("data/feature-calcs/bound/features.Rda")

outputs <- unique(features$problem) %>%
  purrr::map_dfr(~ fit_all_classifiers(features, problem_name = .x, n_resamples = 30, by_set = TRUE))

save(outputs, file = "data/outputs.Rda")

outputs_aggregate <- unique(features$problem) %>%
  purrr::map_dfr(~ fit_all_classifiers(features, problem_name = .x, n_resamples = 30, by_set = FALSE))

save(outputs_aggregate, file = "data/outputs_aggregate.Rda")
rm(features, outputs, outputs_aggregate)

# z-scored

load("data/feature-calcs/bound/features_z.Rda")

outputs_z <- unique(features$problem) %>%
  purrr::map_dfr(~ fit_all_classifiers(features, problem_name = .x, n_resamples = 30, by_set = TRUE))

save(outputs_z, file = "data/outputs_z.Rda")

outputs_z_aggregate <- unique(features$problem) %>%
  purrr::map_dfr(~ fit_all_classifiers(features, problem_name = .x, n_resamples = 30, by_set = FALSE))

save(outputs_z_aggregate, file = "data/outputs_z_aggregate.Rda")
rm(features, outputs_z, outputs_z_aggregate)
